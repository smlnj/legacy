(* mlriscGen.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Translate CPS to MLRISC.
 *
 * This version of MLRiscGen also injects GC types to the MLRISC backend.
 * I've also reorganized it a bit and added a few comments
 * so that I can understand it.
 *)

signature MLRISCGEN =
  sig

  (* The result is a thunk around the address of the resulting code
   * object's entry point.  The client must promise to first call
   * "finish" before forcing it.
   *)
    val codegen : {
	    funcs: CPS.function list,
	    limits:  CPS.lvar -> int * int,
	    source: string
	  } -> (unit -> int)

  end

functor MLRiscGen (
    structure MachineSpec: MACH_SPEC
    structure Ext        : SMLNJ_MLTREE_EXT
    structure Regs       : CPSREGS
		 	   where T.Region = CPSRegions
	                     and T.Constant = SMLNJConstant
		  	     and T.Extension = Ext
    structure ClientPseudoOps : SMLNJ_PSEUDO_OPS
    structure PseudoOp   : PSEUDO_OPS
			    where T = Regs.T
			      and Client = ClientPseudoOps
    structure MLTreeComp : MLTREECOMP
			   where TS.T = Regs.T
                             and TS.S.P = PseudoOp
    structure Flowgen    : CONTROL_FLOWGRAPH_GEN
			   where S = MLTreeComp.TS.S
			     and I = MLTreeComp.I
			     and CFG = MLTreeComp.CFG
    structure InvokeGC   : INVOKE_GC
			   where TS = MLTreeComp.TS
			     and CFG = Flowgen.CFG

    structure Cells      : CELLS
    structure CCalls     : C_CALLS
			   where T = Regs.T
    val compile : Flowgen.CFG.cfg -> unit
 ) : MLRISCGEN = struct

    structure M  = Regs.T		(* MLTree *)
    structure E  = Ext			(* Extensions *)
    structure C  = CPS
    structure P  = C.P			(* CPS primitive operators *)
    structure R  = CPSRegions		(* Regions *)
    structure PT = R.PT			(* PointsTo *)
    structure CG = Control.CG		(* Compiler Control *)
    structure MS = MachineSpec		(* Machine Specification *)
    structure D  = MS.ObjDesc		(* ML Object Descriptors *)
    structure TS = MLTreeComp.TS	(* MLTREE streams *)
    structure CPs = ClientPseudoOps
    structure PB = PseudoOpsBasisTyp
    structure An = MLRiscAnnotations
    structure CB = CellsBasis
    structure Tbl = IntHashTable

  (* Argument passing *)
    structure ArgP = ArgPassing(
	structure Cells = Cells
	structure C = Regs
	structure MS = MachineSpec)

    structure Frag = Frag(M)      (* Decompose a compilation unit into clusters *)

    structure MemAliasing = MemAliasing(Cells) (* Memory aliasing *)

  (* C-Calls handling *)
    structure CPSCCalls = CPSCCalls(
	structure MS = MachineSpec
	structure C = Regs
	structure MLTreeComp = MLTreeComp
	structure Cells = Cells
	structure CCalls = CCalls)

  (* lift selected CPS datatypes *)
    datatype cty = datatype C.cty
    datatype value = datatype C.value
    datatype accesspath = datatype C.accesspath

    fun error msg = MLRiscErrorMsg.error("MLRiscGen", msg)

  (*
   * Debugging
   *)
    fun printCPSFun cps = (
	  Control.Print.say "*********************************************** \n";
	  PPCps.printcps0 cps;
	  Control.Print.say "*********************************************** \n";
	  Control.Print.flush())
    val print = Control.Print.say

  (*
   * GC Safety
   *)

  (* How to annotate GC information *)
    structure GCCells = GCCells(
	structure C = Cells
	structure GC = SMLGCType)

    val TAGINT = SMLGCType.TAGGED_INT
    val INT    = SMLGCType.INT
(* REAL32: *)
    val REAL64 = SMLGCType.REAL64  (* untagged floats *)
    val PTR    = SMLGCType.PTR     (* boxed objects *)
    val NO_OPT = [#create An.NO_OPTIMIZATION ()]

    val enterGC = GCCells.setGCType

    fun sameRegAs x y = CB.sameCell (x, y)

    val annPTR = #create An.MARK_REG(fn r => enterGC(r,PTR))
    val annINT = #create An.MARK_REG(fn r => enterGC(r,INT))
    val annTAGINT = #create An.MARK_REG(fn r => enterGC(r,TAGINT))
    val annREAL64 = #create An.MARK_REG(fn r => enterGC(r,REAL64))

    fun ctyToAnn (C.NUMt{tag=true, ...}) = annTAGINT
      | ctyToAnn (C.NUMt{tag=false, ...}) = annINT
      | ctyToAnn (C.FLTt 64) = annREAL64
(* REAL32: FIXME *)
      | ctyToAnn (C.FLTt n) = raise Fail(concat[
	    "ctyToAnn: FLTt ", Int.toString n, " is unsupported"
	  ])
      | ctyToAnn _ = annPTR

  (* Convert kind to gc type *)
    fun kindToGCty (C.P.INT sz) = if (sz = Target.defaultIntSz) then TAGINT else INT
      | kindToGCty (C.P.UINT sz) = if (sz = Target.defaultIntSz) then TAGINT else INT
      | kindToGCty _ = error "kindToGCty: bogus kind"

  (* convert CPS type to gc type *)
    fun ctyToGCty (C.FLTt 64) = REAL64
      | ctyToGCty (C.FLTt n) = raise Fail(concat[
	    "ctyToGCty: FLTt ", Int.toString n, " is unsupported"
	  ])
      | ctyToGCty (C.NUMt{tag=true, ...}) = TAGINT
      | ctyToGCty (C.NUMt{tag=false, ...}) = INT
      | ctyToGCty _ = PTR

  (* Make a GC livein/liveout annotation *)
    fun gcAnnotation (an, args, ctys) = let
	  fun collect (M.GPR(M.REG(_,r))::args,cty::ctys,gctys) =
		collect(args,ctys,(r,ctyToGCty cty)::gctys)
	    | collect (M.FPR(M.FREG(_,r))::args,cty::ctys,gctys) =
		collect(args,ctys,(r,ctyToGCty cty)::gctys)
	    | collect (_::args,_::ctys,gctys) = collect(args,ctys,gctys)
	    | collect ([], [], gctys) = gctys
	    | collect _ = error "gcAnnotation"
	  in
	    an (collect (args, ctys, []))
	  end

  (*
   * These are the type widths of ML.  They are hardwired for now.
   *)
(* QUESTION: do we care about the redundancy between Target.mlValueSz and MS.wordBitWidth? *)
    val pty = MS.wordBitWidth (* size of ML's pointer *)
    val ity = MS.wordBitWidth (* size of ML's integer *)
    val fty = 64 (* size of ML's real number *)
    val ws = MS.wordByteWidth
    val addrTy = MachineSpec.addressBitWidth	(* naturalsize of address arithmetic *)

    val zero = M.LI 0
    val one  = M.LI 1
    val two  = M.LI 2
    val allOnes = M.LI(ConstArith.bNot(ity, 0))			 (* machine-word all 1s *)
    val allOnes' = M.LI(ConstArith.bNot(Target.defaultIntSz, 0)) (* tagged-int all 1s *)
    val signBit = M.LI(IntInf.<<(1, Word.fromInt ity - 0w1))
    val mlZero = one (* tagged zero *)
    val offp0 = C.OFFp 0
    val LI = M.LI
    fun LI' i = LI (M.I.fromInt(ity, i))
    fun LW' w = LI (M.I.fromWord(ity, w))

    val constBaseRegOffset = LI' MachineSpec.constBaseRegOffset

  (* CPS tagged integer constants *)
    local
      val ty = {sz = Target.defaultIntSz, tag = true}
    in
    fun cpsInt n = C.NUM{ival = IntInf.fromInt n, ty = ty}
    end (* local *)

  (*
   * The allocation pointer.  This must be a register
   *)
    val M.REG(_,allocptrR) = Regs.allocptr

  (*
   * Dedicated registers.
   *)
    val dedicated' =
	  map (fn r => M.GPR(M.REG(ity,r))) Regs.dedicatedR @
	  map (fn f => M.FPR(M.FREG(fty,f))) Regs.dedicatedF

    val dedicated = (case Regs.exhausted
	   of NONE => dedicated'
	    | SOME cc => M.CCR cc :: dedicated'
	  (* end case *))

  (*
   * This flag controls whether extra MLRISC optimizations should be
   * performed.  By default, this is off.
   *)
    val mlrisc   = Control.MLRISC.mkFlag ("mlrisc", "whether to do MLRISC optimizations")

  (*
   * If this flag is on then annotate the registers with GC type info.
   * Otherwise use the default behavior.
   *)
    val gctypes  = Control.MLRISC.mkFlag ("mlrisc-gc-types", "whether to use GC type info")

  (*
   * If this flag is on then perform optimizations before generating gc code.
   * If this flag is on then gctypes must also be turned on!
   * Otherwise use the default behavior.
   *)
    val gcsafety = Control.MLRISC.mkFlag ("mlrisc-gcsafety",
					  "whether to optimize before generating GC code")

  (*
   * If this flag is on then split the entry block.
   * This should be on for SSA optimizations.
   *)
    val splitEntry = Control.MLRISC.mkFlag ("split-entry-block", "whether to split entry block")

  (*
   * This dummy annotation is used to get an empty block
   *)
    val EMPTY_BLOCK = #create An.EMPTY_BLOCK ()

    val newLabel = Label.anon

  (* branch annotated with probability *)
    fun branchWithProb(br, NONE) = br
      | branchWithProb(br, SOME prob) =
	  M.ANNOTATION(br, #create MLRiscAnnotations.BRANCH_PROB prob)

  (*
   * A CPS register may be implemented as a physical
   * register or a memory location.  The function assign moves a
   * value v into a register or a memory location.
   *)
    fun assign (M.REG(ty,r), v) = M.MV(ty, r, v)
      | assign (M.LOAD(ty, ea, mem), v) = M.STORE(ty, ea, v, mem)
      | assign _ = error "assign"

  (* effective address: reg + offset *)
    fun ea (r, 0) = r
      | ea (r, n) = M.ADD(addrTy, r, LI' n)

  (* scaled effective address *)
    fun indexEA (r, 0) = r
      | indexEA (r, n) = M.ADD(addrTy, r, LI'(n*ws))

  (*
   * Tagged integer optimizations.
   * Note: if the tagging scheme changes then we'll have to redo these.
   *)
    fun addTag e   = M.ADD(ity, e, one)
    fun stripTag e = M.SUB(ity, e, one)
    fun orTag e    = M.ORB(ity, e, one)

  (* zero-extend and sign-extend to full machine-word width *)
    fun zeroExtend (sz, e) = M.ZX (ity, sz, e)
    fun signExtend (sz, e) = M.SX (ity, sz, e)

  (* convert unsigned CPS comparison to MLRISC *)
    fun unsignedCmp oper = (case oper
	   of P.GT => M.GTU
	    | P.GTE => M.GEU
	    | P.LT => M.LTU
	    | P.LTE => M.LEU
	    | P.EQL => M.EQ
	    | P.NEQ => M.NE
	  (* end case *))

  (* convert signed CPS comparison to MLRISC *)
    fun signedCmp oper = (case oper
	   of P.GT => M.GT
	    | P.GTE  => M.GE
	    | P.LT => M.LT
	    | P.LTE  => M.LE
	    | P.NEQ => M.NE
	    | P.EQL => M.EQ
	  (* end case *))

    fun branchToLabel lab = M.JMP(M.LABEL lab,[])

  (*
   * The main codegen function.
   *)
    fun codegen args = let
	  val { funcs : C.function list, limits:C.lvar -> (int*int), source } = args
	  val maxAlloc = #1 o limits
	  val splitEntry = !splitEntry
	(*
	 * These functions generate new virtual register names and
	 * mark expressions with their gc types.
	 * When the gc-safety feature is turned on, we'll use the
	 * versions of newReg that automatically update the GCMap.
	 * Otherwise, we'll just use the normal version.
	 *)
	  val gctypes = !gctypes

	  val (newReg, newRegWithCty, newRegWithKind, newFreg) = if gctypes
		then let
		  val newReg  = GCCells.newCell CB.GP
		  val newFreg = GCCells.newCell CB.FP
		  fun newRegWithCty cty = newReg(ctyToGCty cty)
		  fun newRegWithKind kind = newReg(kindToGCty kind)
		  in
		    (newReg, newRegWithCty, newRegWithKind, newFreg)
		  end
	       else (Cells.newReg, Cells.newReg, Cells.newReg, Cells.newFreg)

	  fun markPTR e = if gctypes then M.MARK(e, annPTR) else e
	  fun markINT e = if gctypes then M.MARK(e, annINT) else e
	  fun markREAL64 e = if gctypes then M.FMARK(e, annREAL64) else e
	  fun markGC (e, cty) = if gctypes then M.MARK(e, ctyToAnn cty) else e
	  fun markNothing e = e

	(*
	 * Known functions have parameters passed in fresh temporaries.
	 * We also annotate the gc types of these temporaries.
	 *)
	  fun known [] = []
	    | known (cty::rest) = (case cty
		 of C.FLTt 64 => M.FPR(M.FREG(fty, newFreg REAL64))
(* REAL32: FIXME *)
		  | C.FLTt n => raise Fail(concat["known: FLTt ", Int.toString n, " is unsupported"])  (* REAL32: FIXME *)
		  | C.NUMt{tag=true, ...} => M.GPR(M.REG(ity, newReg TAGINT))
		  | C.NUMt{tag=false, ...} => M.GPR(M.REG(ity, newReg INT))
		  | _ => M.GPR(M.REG(pty,newReg PTR))
		(* end case *)) :: known rest

	(*
	 * labelTbl is a mapping of function names (CPS.lvars) to labels.
	 * If the flag splitEntry is on, we also distinguish between external and
	 * internal labels, make sure that no directly branches go to the
	 * external labels.
	 *)
	  exception LabelBind
	  val labelTbl : Label.label Tbl.hash_table = Tbl.mkTable(32, LabelBind)
	  val functionLabel = Tbl.lookup labelTbl
	  val addLabelTbl = Tbl.insert labelTbl

	(*
	 * typTbl is a mapping of CPS.lvars to CPS types
	 *)
	  exception TypTbl
	  val typTbl  : C.cty Tbl.hash_table = Tbl.mkTable(32, TypTbl)
	  val addTypBinding = Tbl.insert typTbl
	  val typmap = Tbl.lookup typTbl

	(*
	 * mkGlobalTables define the labels and cty for all CPS functions
	 *)
	  fun mkGlobalTables (fk, f, _, _, _) = (
	      (* internal label *)
		addLabelTbl (f, newLabel());
	      (* external entry label *)
		if splitEntry
		  then (case fk
		     of (C.CONT | C.ESCAPE) =>
			  addLabelTbl (~f-1, Label.label(Int.toString f) ())
		      | _ => ()
		    (* end case *))
		  else ();
		case fk
		 of C.CONT => addTypBinding(f, C.CNTt)
		  | _ => addTypBinding(f, CPSUtil.BOGt)
		(* end case *))

	(* compute branch probabilities for the functions *)
	  val brProb = CpsBranchProb.branchProb funcs

	(*
	 * Function for generating code for one cluster.
	 *)
	fun genCluster cluster = let
	      val _ = if !Control.debugging then app PPCps.printcps0 cluster else ()

	    (*
	     * The mltree stream
	     *)
	      val stream as TS.S.STREAM{
		      beginCluster,  (* start a cluster *)
		      endCluster,    (* end a cluster *)
		      emit,          (* emit MLTREE stm *)
		      defineLabel,   (* define a local label *)
		      entryLabel,    (* define an external entry *)
		      exitBlock,     (* mark the end of a procedure *)
		      pseudoOp,      (* emit a pseudo op *)
		      annotation,    (* add an annotation *)
		      ...
		    } = MLTreeComp.selectInstructions (Flowgen.build ())

	   (*
	    * If there are raw C Calls (i.e., RCC is present), then we need to
	    * use the virtual frame pointer
	    *)
	      local
		fun hasRCC([]) = false
		  | hasRCC((_,_,_,_,cexp)::rest) =
		      CPSUtil.hasRCC(cexp) orelse hasRCC(rest)
	      in
	      val vfp = not MS.framePtrNeverVirtual andalso hasRCC(cluster)
	      val _ = ClusterAnnotation.useVfp := vfp
	      end

	    (*
	     * This is the GC comparison test used.  We have a choice of signed
	     * and unsigned comparisons.  This usually doesn't matter, but some
	     * architectures work better in one way or the other, so we are given
	     * a choice here.   For example, the Alpha has to do extra for unsigned
	     * tests, so on the Alpha we use signed tests.
	     *)
	      val gcTest = if Regs.signedGCTest
		    then M.CMP(pty, M.GT, Regs.allocptr, Regs.limitptr vfp)
		    else M.CMP(pty, M.GTU, Regs.allocptr, Regs.limitptr vfp)

	      val clusterSize = length cluster

	    (* per-cluster tables *)

	    (*
	     * genTbl -- is used to retrieve the parameter passing
	     * conventions once a function has been compiled.
	     *)
	      exception GenTbl
	      val genTbl : Frag.frag Tbl.hash_table = Tbl.mkTable(clusterSize, GenTbl)
	      val addGenTbl = Tbl.insert genTbl
	      val lookupGenTbl = Tbl.lookup genTbl

	    (*
	     * {fp,gp}RegTbl -- mapping of lvars to registers
	     *)
	      exception RegMap
	      val fpRegTbl : M.fexp Tbl.hash_table = Tbl.mkTable(2, RegMap)
	      val gpRegTbl : M.rexp Tbl.hash_table = Tbl.mkTable(32, RegMap)
	      val addExpBinding = Tbl.insert gpRegTbl
	      fun addRegBinding(x,r) = addExpBinding(x,M.REG(ity,r))
	      val addFregBinding = Tbl.insert fpRegTbl

	    (*
	     * The following function is used to translate CPS into
	     * larger trees.  Definitions marked TREEIFY can be forward
	     * propagated to their (only) use.   This can drastically reduce
	     * register pressure.
	     *)
	      datatype treeify = TREEIFY | TREEIFIED | COMPUTE | DEAD
	      exception UseCntTbl
	      val useCntTbl : treeify Tbl.hash_table = Tbl.mkTable(32, UseCntTbl)
	      fun treeify i = getOpt (Tbl.find useCntTbl i, DEAD)
	      val addCntTbl = Tbl.insert useCntTbl
	      fun markAsTreeified r = addCntTbl(r, TREEIFIED)

	    (*
	     * Reset the bindings and use count tables. These tables
	     * can be reset at the same time.
	     *)
	      fun clearTables() = (
		    Tbl.clear gpRegTbl;
		    Tbl.clear fpRegTbl;
		    Tbl.clear useCntTbl)

	    (*
	     * memDisambiguation uses the new register counters,
	     * so this must be reset here.
	     *)
	      val _ = Cells.reset()
(* NOTE: by default, this function is a no-op, because Control.CG.memDisambiguate is false *)
	      val memDisambig = MemAliasing.analyze(cluster)

	    (*
	     * Points-to analysis projection.
	     *)
	      fun pi (x as ref(PT.TOP _), _) = x
		| pi (x, i) = PT.pi(x, i)

	      val memDisambigFlag = !CG.memDisambiguate

	      fun getRegion e =
		  if memDisambigFlag then
		     (case e of
			CPS.VAR v => memDisambig v
		      | _ => R.readonly
		     )
		  else R.memory

	      fun getRegionPi (e, i) = if memDisambigFlag
		    then (case e
		       of CPS.VAR v => pi(memDisambig v, i)
			| _ => R.readonly)
		  else R.memory

	      fun dataptrRegion v = getRegionPi(v, 0)

	    (* fun arrayRegion(x as ref(PT.TOP _)) = x
		 | arrayRegion x = PT.weakSubscript x *)
	    (* For safety, let's assume it's the global memory right now *)
	      fun arrayRegion _ = R.memory

	    (* This keeps track of all the advanced offset on the hp
	     * since the beginning of the CPS function.
	     * This is important for generating the correct address offset
	     * for newly allocated records.
	     *)
	      val advancedHP = ref 0

	    (*
	     * Function grabty lookups the CPS type of a value expression in CPS.
	     *)
	      fun grabty (C.VAR v) = typmap v
		| grabty (C.LABEL v) = typmap v
		| grabty (C.NUM{ty, ...}) = C.NUMt ty
		| grabty (C.VOID) = C.FLTt 64 (* why? *)
		| grabty _ = CPSUtil.BOGt

	    (*
	     * The baseptr contains the start address of the entire
	     * compilation unit.  This function generates the address of
	     * a label that is embedded in the same compilation unit.  The
	     * generated address is relative to the baseptr.
	     *
	     * Note: For GC safety, we considered this to be an object reference
	     *)
	      fun laddr (lab, k) = let
		    val e = M.ADD(addrTy, Regs.baseptr vfp,
			      M.LABEXP(M.ADD(addrTy, M.LABEL lab,
				LI'(k - MachineSpec.constBaseRegOffset))))
		    in
		      markPTR e
		    end

	    (*
	     * The following function looks up the MLTREE expression associated
	     * with a general purpose value expression.
	     *)
	      val lookupGpRegTbl = Tbl.lookup gpRegTbl

	    (*
	     * This function resolve the address computation of the
	     * form M.CONST k, where offset is a reference to the
	     * kth byte allocated since the beginning of the CPS function.
	     *)
	      fun resolveHpOffset (M.CONST(absoluteHpOffset)) = let
		    val tmpR = newReg PTR
		    val offset = absoluteHpOffset - !advancedHP
		    in
		      emit(M.MV(pty, tmpR, M.ADD(addrTy, Regs.allocptr, LI' offset)));
		      M.REG(pty, tmpR)
		    end
		| resolveHpOffset e = e

	      fun regbind (C.VAR v) = resolveHpOffset(lookupGpRegTbl v)
		| regbind (C.NUM{ival, ty={tag=true, ...}}) = LI(ival+ival+1)
		| regbind (C.NUM{ival, ...}) = LI ival
		| regbind (C.LABEL v) =
		    laddr(functionLabel(if splitEntry then ~v-1 else v), 0)
		| regbind _ = error "regbind"

	    (*
	     * This version allows the value to be further propagated
	     *)
	      fun resolveHpOffset' (M.CONST(absoluteHpOffset)) = let
		    val offset = absoluteHpOffset - !advancedHP
		    in
		      markPTR(M.ADD(addrTy, Regs.allocptr, LI' offset))
		    end
		| resolveHpOffset' e = e

	      fun regbind' (C.VAR v) = resolveHpOffset'(lookupGpRegTbl v)
		| regbind' (C.NUM{ival, ty={tag=true, ...}}) = LI(ival+ival+1)
		| regbind' (C.NUM{ival, ...}) = LI ival
		| regbind' (C.LABEL v) =
		    laddr(functionLabel(if splitEntry then ~v - 1 else v), 0)
		| regbind' _ = error "regbind'"

	    (*
	     * The following function looks up the MLTREE expression associated
	     * with a floating point value expression.
	     *)
	      val lookupFpRegTbl = Tbl.lookup fpRegTbl
	      fun fregbind (C.VAR v) = lookupFpRegTbl v
		| fregbind _ = error "fregbind"

	    (* On entry to a function, the parameters will be in formal
	     * parameter passing registers. Within the body of the function, they
	     * are moved immediately to fresh temporary registers. This ensures
	     * that the life time of the formal paramters is restricted to the
	     * function body and is critical in avoiding artificial register
	     * interferences.
	     *)
	    fun initialRegBindingsEscaping (vl, rl, tl) = let
		  fun eCopy(x::xs, M.GPR(M.REG(_,r))::rl, rds, rss, xs', rl') = let
			val t = newReg PTR
			in
			  addRegBinding(x, t);
			  eCopy(xs, rl, t::rds, r::rss, xs', rl')
			end
		    | eCopy(x::xs, r::rl, rds, rss, xs', rl') =
			eCopy(xs, rl, rds, rss, x::xs', r::rl')
		    | eCopy([], [], [], [], xs', rl') = (xs', rl')
		    | eCopy([], [], rds, rss, xs', rl') = (
			emit(M.COPY(ity, rds, rss)); (xs', rl'))
		    | eCopy (([], _::_, _, _, _, _) | (_::_, [], _, _, _, _)) =
			error "eCopy"

		  fun eOther(x::xs, M.GPR(r)::rl, xs', rl') = let
			val t = newReg PTR
			in
			  addRegBinding(x, t); emit(M.MV(ity, t, r));
			  eOther(xs, rl, xs', rl')
			end
		    | eOther(x::xs, (M.FPR(M.FREG(_,f)))::rl, xs', rl') =
			eOther(xs, rl, x::xs', f::rl')
		    | eOther([], [], xs, rl) = (xs, rl)
		    | eOther (_, M.FPR _ :: _, _, _) =
			error "eOther: FPR but not FREG"
		    | eOther (_, M.CCR _ :: _, _, _) =
			error "eOther: CCR"
		    | eOther (([], _::_, _, _) | (_::_, [], _, _)) =
			error "eOther"

		  fun eFcopy([], []) = ()
		    | eFcopy(xs, rl) = let
		        val fs = map (fn _ => newFreg REAL64) xs
		        in
			  ListPair.app (fn (x,f) => addFregBinding(x,M.FREG(fty,f))) (xs,fs);
			  emit(M.FCOPY(fty, fs, rl))
		        end
		  val (vl', rl') = eCopy(vl, rl, [], [], [], [])
		  in
		    eFcopy(eOther(vl', rl', [], []));
		    ListPair.app addTypBinding (vl, tl)
		  end (* initialRegBindingsEscaping *)

	      fun initialRegBindingsKnown (vl, rl, tl) = let
		    fun f(v, M.GPR(reg as M.REG _)) = addExpBinding(v, reg)
		      | f(v, M.FPR(freg as M.FREG _)) = addFregBinding(v, freg)
		      | f _ = error "initialRegBindingsKnown.f"
		    in
		      ListPair.app f (vl, rl);
		      ListPair.app addTypBinding (vl, tl)
		    end (* initialRegBindingsKnown *)

	    (* Keep allocation pointer aligned on odd boundary
	     * Note: We have accounted for the extra space this eats up in
	     *    limit.sml
	     *)
	      fun updtHeapPtr 0 = ()
		| updtHeapPtr hp = let
		    fun advBy hp = (
			  advancedHP := !advancedHP + hp;
			  emit(M.MV(pty, allocptrR, M.ADD(addrTy, Regs.allocptr, LI' hp))))
		    in
		      if Word.andb(Word.fromInt hp, Word.fromInt ws) <> 0w0
			then advBy(hp+ws)
			else advBy hp
		    end

	      fun testLimit hp = let
		    fun assignCC (M.CC(_, cc), v) = emit(M.CCMV(cc, v))
		      | assignCC _ = error "testLimit.assign"
		    in
		      updtHeapPtr hp;
		      case Regs.exhausted
		       of NONE => ()
			| SOME cc => assignCC(cc, gcTest)
		      (* end case *)
		    end

	    (*
	     * Function to allocate an integer record
	     *   x <- [descriptor ... fields]
	     *)
	      fun allocRecord (markComp, mem, desc, fields, hp) = let
		    fun getField (v, e, C.OFFp n) = indexEA(e, n)
		      | getField (v, e, p) = getPath(getRegion v, e, p)
		    and getPath (mem, e, C.OFFp n) = indexEA(e, n)
		      | getPath (mem, e, C.SELp(n, C.OFFp 0)) =
			  markComp(M.LOAD(ity, indexEA(e, n), pi(mem, n)))
		      | getPath(mem, e, C.SELp(n, p)) = let
			  val mem = pi(mem, n)
			  in
			    getPath(mem, markPTR(M.LOAD(ity, indexEA(e, n), mem)), p)
			  end
		    fun storeFields([], hp, elem) = hp
		      | storeFields((v, p)::fields, hp, elem) = (
			  emit(M.STORE(ity,
				       M.ADD(addrTy, Regs.allocptr, LI' hp),
				       getField (v, regbind' v, p), pi(mem, elem)));
			  storeFields (fields, hp+ws, elem+1))
		   in
		     emit(M.STORE(ity, ea(Regs.allocptr, hp), desc, pi(mem, ~1)));
		     storeFields(fields, hp+ws, 0);
		     hp+ws
		   end

	    (*
	     * Functions to allocate a floating point record
	     *   x <- [descriptor ... fields]
	     *)
(* REAL32: FIXME *)
	      fun allocFrecord (mem, desc, fields, hp) = let
		    fun fea (r, 0) = r
		      | fea (r, n) = M.ADD(addrTy, r, LI'(n*8))
		    fun fgetField (v, C.OFFp 0) = fregbind v
		      | fgetField (v, C.OFFp _) = error "allocFrecord.fgetField"
		      | fgetField (v, p) = fgetPath(getRegion v, regbind' v, p)
		    and fgetPath (mem, e, C.OFFp _) = error "allocFrecord.fgetPath"
		      | fgetPath (mem, e, C.SELp(n, C.OFFp 0)) =
			   markREAL64(M.FLOAD(fty, fea(e, n), pi(mem, n)))
		      | fgetPath (mem, e, C.SELp(n, p)) =
			let val mem = pi(mem, n)
			in  fgetPath(mem, markPTR(M.LOAD(ity, indexEA(e, n), mem)),p)
			end
		    fun fstoreFields([], hp, elem) = hp
		      | fstoreFields((v, p)::fields, hp, elem) = (
			  emit(M.FSTORE(fty, M.ADD(addrTy, Regs.allocptr, LI' hp),
					fgetField(v, p), pi(mem, elem)));
			  fstoreFields(fields, hp+8, elem+1))
		    in
		      emit(M.STORE(ity, ea(Regs.allocptr, hp), desc, pi(mem, ~1)));
		      fstoreFields(fields, hp+ws, 0);
		      hp+ws
		    end

	    (* Allocate a header pair for a known-length vector or array *)
	      fun allocHeaderPair (hdrDesc, mem, dataPtr, len, hp) = (
		    emit(M.STORE(ity, ea(Regs.allocptr, hp), LI hdrDesc, pi(mem,~1)));
		    emit(M.STORE(ity, ea(Regs.allocptr, hp+ws), M.REG(ity, dataPtr),pi(mem, 0)));
		    emit(M.STORE(ity, ea(Regs.allocptr, hp+2*ws), LI'(len+len+1), pi(mem, 1)));
		    hp+ws)

	    (* tagging operations for words and integers *)
	      fun tagUnsigned e = let
		    fun double r = M.ADD(ity, r, r)
		    in
		      case e
		       of M.REG _ => addTag(double e)
			| _ => let
			    val tmp = newReg PTR (* XXX ??? *)
			    in
			      M.LET(M.MV(ity, tmp, e), addTag(double(M.REG(ity,tmp))))
			    end
		      (* end case *)
		    end
	      fun tagSigned e = let
		    fun double r = M.ADDT(ity, r, r)	(* trapping add for Overflow *)
		    in
		      case e
		       of M.REG _ => addTag(double e)
			| _ => let
			    val tmp = newReg PTR (* XXX ??? *)
			    in
			      M.LET(M.MV(ity, tmp, e), addTag(double(M.REG(ity,tmp))))
			    end
		      (* end case *)
		    end
	      fun tag (false, e) = tagUnsigned e
		| tag (true, e) = tagSigned e

	      fun untagUnsigned (C.NUM{ty={tag=true, ...}, ival}) = LI ival
		| untagUnsigned (C.NUM _) = error "untagUnsigned: boxed int"
		| untagUnsigned v = M.SRL(ity, regbind v, one)

	      fun untagSigned (C.NUM{ty={tag=true, ...}, ival}) = LI ival
		| untagSigned (C.NUM _) = error "untagSigned: boxed int"
		| untagSigned v = M.SRA(ity, regbind v, one)

	      fun untag (true, e) = untagSigned e
		| untag (false, e) = untagUnsigned e

	    (*
	     * Tagged integer operators
	     *)
	      fun tagIntAdd (addOp, C.NUM{ival=k, ...}, w) = addOp(ity, LI(k+k), regbind w)
		| tagIntAdd (addOp, w, v as C.NUM _) = tagIntAdd(addOp, v, w)
		| tagIntAdd (addOp, v, w) = addOp(ity, regbind v, stripTag(regbind w))

	      fun tagIntSub (subOp, C.NUM{ival=k, ...}, w) = subOp(ity, LI(k+k+2), regbind w)
		| tagIntSub (subOp, v, C.NUM{ival=k, ...}) = subOp(ity, regbind v, LI(k+k))
		| tagIntSub (subOp, v, w) = addTag(subOp(ity, regbind v, regbind w))

	      fun tagIntMul (signed, mulOp, v, w) = let
		    fun f (C.NUM{ival=k, ...}, C.NUM{ival=j, ...}) = (LI(k+k), LI j)
		      | f (C.NUM{ival=k, ...}, w) = (untag(signed,w), LI(k+k))
		      | f (v, w as C.NUM _) = f(w, v)
		      | f (v, w) = (stripTag(regbind v), untag(signed,w))
		    val (v, w) = f(v, w)
		    in
		      addTag(mulOp(ity, v, w))
		    end

	      fun tagIntDiv (signed, drm, v, w) = let
		    val (v, w) = (case (v, w)
			   of (C.NUM{ival=k, ...}, C.NUM{ival=j, ...}) => (LI k, LI j)
			    | (C.NUM{ival=k, ...}, w) => (LI k, untag(signed, w))
			    | (v, C.NUM{ival=k, ...}) => (untag(signed, v), LI k)
			    | (v, w) => (untag(signed, v), untag(signed, w))
			  (* end case *))
		    in
		    (* The only way a tagged-int div can overflow is when the result
		     * gets retagged, therefore we can use M.DIVS instead of M.DIVT.
		     *)
		      tag (signed,
			   if signed then M.DIVS (drm, ity, v, w) else M.DIVU (ity, v, w))
		    end

	      fun tagIntRem (signed, drm, v, w) = let
		    val (v, w) = (case (v, w)
			   of (C.NUM{ival=k, ...}, C.NUM{ival=j, ...}) => (LI k, LI j)
			    | (C.NUM{ival=k, ...}, w) => (LI k, untag(signed, w))
			    | (v, C.NUM{ival=k, ...}) => (untag(signed, v), LI k)
			    | (v, w) => (untag(signed, v), untag(signed, w))
			  (* end case *))
		    in
		      tag (false,		(* cannot overflow, so we tag like unsigned *)
			   if signed then M.REMS (drm, ity, v, w) else M.REMU (ity, v, w))
		    end

	      fun tagIntXor (C.NUM{ival=k, ...}, w) = M.XORB(ity, LI(k+k), regbind w)
		| tagIntXor (w, v as C.NUM _) = tagIntXor (v,w)
		| tagIntXor (v, w) = addTag (M.XORB(ity, regbind v, regbind w))

	      fun tagIntLShift (C.NUM{ival=k, ...}, w) =
		    addTag (M.SLL(ity, LI(k+k), untagUnsigned w))
		| tagIntLShift (v, C.NUM{ival=k, ...}) =
		    addTag(M.SLL(ity,stripTag(regbind v), LI k))
		| tagIntLShift (v,w) =
		    addTag(M.SLL(ity,stripTag(regbind v), untagUnsigned w))

	      fun tagIntRShift (rshiftOp, v, C.NUM{ival=k, ...}) =
		    orTag(rshiftOp(ity, regbind v, LI k))
		| tagIntRShift (rshiftOp, v, w) =
		    orTag(rshiftOp(ity, regbind v, untagUnsigned w))

	      fun getObjDescriptor v =
		    M.LOAD(ity, M.SUB(pty, regbind v, LI' ws), getRegionPi(v, ~1))

	      fun getObjLength v =
		    M.SRL(ity, getObjDescriptor v, LW'(D.tagWidth - 0w1))

	    (* scale-and-add, where the second argument is a tagged integer *)
	      fun scale1 (a, C.NUM{ival=0, ...}) = a
		| scale1 (a, C.NUM{ival, ...}) = M.ADD(ity, a, LI ival)
		| scale1 (a, i) = M.ADD(ity, a, untagSigned i)

	      fun scale4 (a, C.NUM{ival=0, ...}) = a
		| scale4 (a, C.NUM{ival, ...}) = M.ADD(ity, a, LI(ival*4))
		| scale4 (a, i) = M.ADD(ity, a, M.SLL(ity, untagSigned i, two))

	      fun scale8 (a, C.NUM{ival=0, ...}) = a
		| scale8 (a, C.NUM{ival, ...}) = M.ADD(ity, a, LI(ival*8))
		| scale8 (a, i) = M.ADD(ity, a, M.SLL(ity, stripTag(regbind i), two))

	    (* scale by the target word size *)
	      val scaleWord = (case ws
		     of 4 => scale4
		      | 8 => scale8
		      | _ => error "scaleWord"
		    (* end case *))

	    (*
	     * Note: because formals are moved into fresh temporaries,
	     * (formals intersection actuals) is empty.
	     *
	     * Do the treeified computation first so as to prevent extra
	     * interferences from being created.
	     *)
	      fun callSetup (formals, actuals) = let
		    fun isTreeified (C.VAR r) = (treeify r = TREEIFIED)
		      | isTreeified _ = false
		    fun gather ([], [], cpRd, cpRs, fcopies, treeified, moves) = (
			  app emit treeified;
			  case (cpRd,cpRs)
			    of ([],[]) => ()
			     | _ => emit(M.COPY(ity, cpRd, cpRs));
			  case fcopies
			    of [] => ()
			     | _ => emit(M.FCOPY(fty, map #1 fcopies, map #2 fcopies));
			  app emit moves)
		      | gather (M.GPR(M.REG(ty,rd))::fmls,act::acts,cpRd,cpRs,f,t,m) = (
			  case regbind act
			   of M.REG(_,rs) => gather(fmls,acts,rd::cpRd,rs::cpRs,f,t,m)
			    | e => if isTreeified act
				  then gather(fmls, acts, cpRd, cpRs, f, M.MV(ty, rd, e)::t, m)
				  else gather(fmls, acts, cpRd, cpRs, f, t, M.MV(ty, rd, e)::m)
			  (* end case *))
		      | gather (M.GPR(M.LOAD(ty,ea,r))::fmls,act::acts,cpRd,cpRs,f,t,m) =
			(* Always store them early! *)
			  gather(fmls, acts, cpRd, cpRs, f, M.STORE(ty,ea,regbind act,r)::t, m)
		      | gather (M.FPR(M.FREG(ty,fd))::fmls,act::acts,cpRd,cpRs,f,t,m) = (
			  case fregbind act
			   of M.FREG(_,fs) => gather(fmls, acts, cpRd, cpRs, (fd,fs)::f, t, m)
			    | e => if isTreeified act
				  then gather(fmls, acts, cpRd, cpRs, f, M.FMV(ty, fd, e)::t, m)
				  else gather(fmls, acts, cpRd, cpRs, f, t, M.FMV(ty, fd, e)::m)
			  (* end case *))
		      | gather _ = error "callSetup.gather"
		    in
		      gather(formals, actuals, [], [], [], [], [])
		    end (* callSetup *)

	    (* add to storelist, the address where a boxed update has occured *)
	      fun recordStore (tmp, hp) = (
		    emit (M.STORE(pty, M.ADD(addrTy, Regs.allocptr, LI' hp), tmp, R.storelist));
		    emit (M.STORE(pty, M.ADD(addrTy, Regs.allocptr, LI'(hp+ws)),
				  Regs.storeptr vfp, R.storelist));
		    emit (assign(Regs.storeptr vfp, M.ADD(addrTy, Regs.allocptr, LI' hp))))

	    (* generate a floating-point comparison of the given size. *)
	      fun floatCmp (oper, sz, v, w) = let
		    val v = fregbind v
		    val w = fregbind w
		    fun fcmp (cc, a, b) = M.FCMP(sz, cc, a, b)
		    in
		      case oper
		       of P.F_EQ => fcmp (M.==, v, w)
			| P.F_ULG => fcmp (M.?<>, v, w)
			| P.F_UN => fcmp (M.?, v, w)
			| P.F_LEG => fcmp (M.<=>, v, w)
			| P.F_GT => fcmp (M.>, v, w)
			| P.F_GE  => fcmp (M.>=, v, w)
			| P.F_UGT => fcmp (M.?>, v, w)
			| P.F_UGE => fcmp (M.?>=, v, w)
		      (* NOTE: on the x86 and amd64, it is more efficient to test
		       * (a > b) than (b < a), because the latter requires an initial
		       * test for unordered arguments (see MLRISC/amd64/mltree/amd64-gen.sml
		       * for details).
		       *)
			| P.F_LT => fcmp (M.>, w, v)
			| P.F_LE  => fcmp (M.<=, v, w)
			| P.F_ULT => fcmp (M.?<, v, w)
			| P.F_ULE => fcmp (M.?<=, v, w)
			| P.F_LG => fcmp (M.<>, v, w)
			| P.F_UE  => fcmp (M.?=, v, w)
		      (* end case *)
		    end

	    (*
	     * This function initializes a CPS function before we generate
	     * code for it.   Its tasks include:
	     * 1. Add type bindings for each definition. This is used to determine
	     *    the parameter passing convention for standard functions.
	     * 2. Compute the number of uses for each variable.  This is
	     *    used in the forward propagation logic.
	     * 3. Check whether the base pointer is needed.
	     *      It is needed iff
	     *       a.  There is a reference to LABEL
	     *       b.  It uses SWITCH (the jumptable requires the basepointer)
	     * 4. Generate the gc tests for STANDARD and KNOWN functions
	     * 5. Check to see if floating point allocation is being performed
	     *    in the function.  If so, we will align the allocptr.
	     *)
	      fun genCPSFunction (lab, kind, f, params, formals, tys, e) = let
		    val add = addTypBinding
		    fun addUse v = (case treeify v
			   of DEAD => addCntTbl(v, TREEIFY)
			    | TREEIFY => addCntTbl(v, COMPUTE)
			    | COMPUTE => ()
			    | _ => error "addUse"
			  (* end case *))
		    val hasFloats = ref false (* default is no *)
		    val needBasePtr = ref false

		    fun addValue (VAR v) = addUse v
		      | addValue (LABEL _) = needBasePtr := true
		      | addValue _ = ()

		    fun addValues vs = List.app addValue vs

		    fun addRecValues [] = ()
		      | addRecValues ((VAR v,_)::l) = (addUse v; addRecValues l)
		      | addRecValues ((LABEL v,_)::l) = (needBasePtr := true; addRecValues l)
		      | addRecValues(_::l) = addRecValues l

		    fun init e = (case e
			   of C.RECORD(k,vl,x,e) => (
				case k
				 of (C.RK_FCONT | C.RK_RAW64BLOCK) => hasFloats := true
				  | _ => ()
				(* end case *);
				addRecValues vl; add(x, CPSUtil.BOGt);
				init e)
			    | C.SELECT(_,v,x,t,e) => (addValue v; add(x,t); init e)
			    | C.OFFSET(_,v,x,e) => (addValue v; add(x, CPSUtil.BOGt); init e)
			    | C.SWITCH(v,_,el) => (needBasePtr := true; addValue v; app init el)
			    | C.SETTER(_,vl,e) => (addValues vl; init e)
			    | C.LOOKER(looker,vl,x,t,e) => (
				 addValues vl;
			       (* floating subscript cannot move past a floating update.
				* For now subscript operations cannot be treeified.
				* This is hacked by making it (falsely) used
				* more than once.
				*)
				 case looker
				  of (P.NUMSUBSCRIPT{kind=P.FLOAT _} | P.RAWLOAD {kind=P.FLOAT _}) =>
				       addCntTbl(x,COMPUTE)
				   | _ => ()
				 (* end case *);
				 add(x,t); init e)
			    | C.ARITH(_,vl,x,t,e) => (addValues vl; add(x,t); init e)
			    | C.RCC(_,_,_,vl,wl,e) => (addValues vl; app add wl; init e)
			    | C.PURE(p,vl,x,t,e) => (
				case p
				 of P.WRAP(P.FLOAT _) => hasFloats := true
				  | _ => ()
				(* end case *);
				addValues vl; add(x,t); init e)
			    | C.BRANCH(_,vl,_,e1,e2) => (addValues vl; init e1; init e2)
			    | C.APP(v,vl) => (addValue v; addValues vl)
			    | _ => error "genCPSFunction"
			  (* end case *))
		    in
		      (* Print debugging information *)
		      if !CG.printit then printCPSFun(kind,f,params,tys,e) else ();

		      (* Move parameters *)
		      case kind
		       of C.KNOWN => (
			    defineLabel lab;
			    init e;
			    initialRegBindingsEscaping(params, formals, tys))
			| C.KNOWN_CHECK => (
			    defineLabel lab;
			    (* gc test *)
			    if !mlrisc andalso !gcsafety
			      then InvokeGC.optimizedKnwCheckLimit stream {
				  maxAlloc = ws*maxAlloc f, regfmls = formals, regtys = tys,
				  return = branchToLabel lab
				}
			      else InvokeGC.knwCheckLimit stream {
				  maxAlloc = ws*maxAlloc f, regfmls=formals, regtys = tys,
				  return = branchToLabel lab
				};
			    init e;
			    initialRegBindingsEscaping(params, formals, tys))
			| _ => let (* Standard function *)
			    val regfmls = formals
			    val (linkreg, regfmlsTl) = (case formals
				   of (M.GPR linkreg::regfmlsTl) => (linkreg, regfmlsTl)
				    | _ => error "no linkreg for standard function"
				  (* end case *))
			    val entryLab = if splitEntry then functionLabel(~f - 1) else lab
			    in
			      if splitEntry
				then (
				  entryLabel entryLab;
				  annotation EMPTY_BLOCK;
				  defineLabel lab)
				else entryLabel lab;
			      clearTables();
			      init e;
			      if !needBasePtr
				then let
				  val baseval = M.ADD(addrTy, linkreg,
					M.LABEXP(M.SUB(addrTy, constBaseRegOffset, M.LABEL entryLab)))
				  in
				    emit(assign(Regs.baseptr(vfp), baseval))
				  end
				else ();
			      InvokeGC.stdCheckLimit stream {
				  maxAlloc = ws * maxAlloc f, regfmls = regfmls,
				  regtys = tys, return = M.JMP(linkreg,[])
				};
			      initialRegBindingsEscaping (List.tl params, regfmlsTl, List.tl tys)
			    end
		      (* end case *);
		    (* Align the allocation pointer if necessary *)
		      if !hasFloats andalso not Target.is64
			then emit(M.MV(pty, allocptrR, M.ORB(pty, Regs.allocptr, LI' ws)))
			else ();
		    (* Generate code *)
		      advancedHP := 0;
		      gen (e, 0)
(*+DEBUG*)
			handle ex => (
			  print(concat["***** exception (", exnMessage ex, ")\n"]);
			  printCPSFun(kind, f, params, tys, e);
			  raise ex)
(*-DEBUG*)
		    end (* genCPSFunction *)

	    (*
	     * Generate code for `x := e; k`, where `r` is the register to hold `x`.
	     *)
	      and define (r, x, e, k, hp) = (
		    addRegBinding(x, r);
		    emit(M.MV(ity, r, e));
		    gen(k, hp))

	      and def (gc, x, e, k, hp) = define(newReg gc, x, e, k, hp)

	      and defWithKind (kind, x, e, k, hp) = define(newRegWithKind kind, x, e, k, hp)

	      and defTAGINT (x, e, k, hp) = def(TAGINT, x, e, k, hp)
	      and defINT (x, e, k, hp) = def(INT, x, e, k, hp)
	      and defBoxed (x, e, k, hp) = def(PTR, x, e, k, hp)

	    (*
	     * Generate code for x : cty := e; k
	     *)
	      and treeifyDef(x, e, cty, k, hp) = (case treeify x
		     of COMPUTE => define(newRegWithCty cty, x, e, k, hp)
		      | TREEIFY => (
			  markAsTreeified x;
			  addExpBinding(x, markGC(e, cty));
			  gen(k, hp))
		      | DEAD => gen(k, hp)
		      | _ => error "treeifyDef"
		    (* end case *))

	    (*
	     * Generate code for
	     *    x := allocptr + offset; k
	     * where offset is the address offset of a newly allocated record.
	     * If x is only used once, we try to propagate that to its use.
	     *)
	      and defAlloc (x, offset, k, hp) =
		    defBoxed(x, M.ADD(addrTy, Regs.allocptr, LI' offset), k, hp)

	    (* Generate code for
	     *    x := allocptr + offset; k
	     * Forward propagate until it is used.
	     *)
	      and treeifyAlloc (x, offset : int, k, hp) = (case treeify x
		     of COMPUTE => defAlloc(x, offset, k, hp)
		      | TREEIFY => let
			(* Note, don't mark this as treeified since it has low
			 * register pressure.
			 *)
			  val absoluteAllocOffset = offset + !advancedHP
			  in
			    addExpBinding (x, M.CONST(absoluteAllocOffset));
			    gen (k, hp)
			  end
		      | DEAD => gen(k, hp)
		      | _    => error "treeifyAlloc"
		    (* end case *))

	      and computef64 (x, e, k, hp : int) = let
		    val f = newFreg REAL64
		    in
		      addFregBinding (x, M.FREG(fty, f));
		      emit (M.FMV(fty, f, e));
		      gen (k, hp)
		    end

	    (*
	     * x <- e where e contains an floating-point value
	     *)
	      and treeifyDefF64 (x, e, k, hp) = (case treeify x
		     of DEAD => gen(k, hp)
		      | TREEIFY => (markAsTreeified x; addFregBinding(x,e); gen(k, hp))
		      | COMPUTE => computef64(x, e, k, hp)
		      | _    => error "treeifyDefF64"
		    (* end case *))

	      and nop (x, v, e, hp) = defTAGINT(x, regbind v, e, hp)

	      and copy (gc, x, v, k, hp) = let
		    val dst = newReg gc
		    in
		      addRegBinding(x, dst);
		      case regbind v
			of M.REG(_,src) => emit(M.COPY(ity, [dst], [src]))
			 | e => emit(M.MV(ity, dst, e))
		      (* end case *);
		      gen(k, hp)
		    end

	      and copyM (sz, x, v, k, hp) = if (sz <= Target.defaultIntSz)
		    then copy(TAGINT, x, v, k, hp)
		    else copy(INT, x, v, k, hp)

	    (* normal branches *)
	      and branch (cv, cmp, [v, w], yes, no, hp) = let
		    val trueLab = newLabel ()
		    in  (* is single assignment great or what! *)
		      emit (branchWithProb
			     (M.BCC(M.CMP(ity, cmp, regbind v, regbind w), trueLab),
			     brProb cv));
		      genCont (no, hp);
		      genlab (trueLab, yes, hp)
		    end
		| branch _ = error "branch"

	    (* branch if x is boxed *)
	      and branchOnBoxed(cv, x, yes, no, hp) = let
		    val lab = newLabel()
		    val cmp = M.CMP(ity, M.NE, M.ANDB(ity, regbind x, one), zero)
		    in
		      emit (branchWithProb(M.BCC(cmp, lab), brProb cv));
		      genCont (yes, hp);
		      genlab (lab, no, hp)
		    end

	    (* branch if are identical strings v, w of length n *)
	      and branchStreq (n, v, w, yes, no, hp) = let
		  (* round number of bytes up to ws bytes *)
		    val n = ((IntInf.toInt n + ws - 1) div ws) * ws
		    val false_lab = newLabel ()
		    val r1 = newReg INT
		    val r2 = newReg INT
		    fun cmpWord i = M.CMP(ity, M.NE,
			  M.LOAD(ity, M.ADD(ity,M.REG(ity, r1), i), R.readonly),
			  M.LOAD(ity, M.ADD(ity,M.REG(ity, r2), i), R.readonly))
		    fun unroll i = if (i = n)
			  then ()
			  else (
			    emit(M.BCC(cmpWord(LI' i), false_lab));
			    unroll (i + ws))
		    in
		      emit (M.MV(ity, r1, M.LOAD(ity, regbind v, R.readonly)));
		      emit (M.MV(ity, r2, M.LOAD(ity, regbind w, R.readonly)));
		      unroll 0;
		      genCont (yes, hp);
		      genlab (false_lab, no, hp)
		    end

	      and arithINT (oper, v, w, x, e, hp) =
		    defINT(x, oper(ity, regbind v, regbind w), e, hp)

	      and shiftINT (oper, v, w, x, e, hp) =
		    defINT(x, oper(ity, regbind v, untagUnsigned w), e, hp)

	      and genCont (e, hp) = let
		    val save = !advancedHP
		    in
		      gen(e, hp);
		      advancedHP := save
		    end

	      and genlab (lab, e, hp) = (defineLabel lab; gen(e, hp))

	      and genlabCont (lab, e, hp) = (defineLabel lab; genCont(e, hp))

	    (* Allocate a normal record *)
	      and mkRecord (vl, w, e, hp) = let
		    val len = length vl
		    val desc = D.makeDesc' (len, D.tag_record)
		    in
		      treeifyAlloc(w,
			allocRecord(markPTR, memDisambig w, LI desc, vl, hp),
			  e, hp+ws+len*ws)
		    end

	    (* Allocate a record with machine-int-sized components *)
	      and mkIntBlock (vl, w, e, hp) = let
		    val len = length vl
		    val desc = D.makeDesc' (len, D.tag_raw)
		    in
		      treeifyAlloc(w,
			allocRecord(markINT, memDisambig w, LI desc, vl, hp),
			  e, hp+ws+len*ws)
		    end

	    (* Allocate a floating point record *)
(* REAL32: FIXME *)
	      and mkFblock (vl, w, e, hp) = let
		    val len = List.length vl
		    val desc = D.makeDesc'(len+len, D.tag_raw64)
		    (* At initialization the allocation pointer is aligned on
		     * an odd-word boundary, and the heap offset set to zero. If an
		     * odd number of words have been allocated then the heap pointer
		     * is misaligned for this record creation.
		     *)
		    val hp = if ws = 4 andalso Word.andb(Word.fromInt hp, 0w4) <> 0w0
			    then hp+4
			    else hp
		    in  (* The components are floating point *)
		      treeifyAlloc(w,
			allocFrecord(memDisambig w, LI desc, vl, hp),
			  e, hp+ws+len*8)
		    end

	    (* Allocate a vector *)
	      and mkVector (vl, w, e, hp) = let
		    val len = length vl
		    val hdrDesc = D.desc_polyvec
		    val dataDesc = D.makeDesc'(len, D.tag_vec_data)
		    val dataPtr = newReg PTR
		    val mem = memDisambig w
		    val hp' = hp + ws + len*ws
		    in  (* The components are boxed *)
		      (* Allocate the data *)
		      allocRecord(markPTR, mem, LI dataDesc, vl, hp);
		      emit(M.MV(pty, dataPtr, ea(Regs.allocptr, hp+ws)));
		      (* Now allocate the header pair *)
		      treeifyAlloc(w,
			 allocHeaderPair(hdrDesc, mem, dataPtr, len, hp+ws+len*ws),
			    e, hp'+3*ws)
		    end

	    (*
	     * Floating point select
	     *)
(* REAL32: FIXME *)
	      and f64select (i, v, x, e, hp) =
		    treeifyDefF64(x,
		      M.FLOAD(fty, scale8(regbind v, cpsInt i), R.real),
		      e, hp)

	    (*
	     * Non-floating point select
	     *)
	      and select (i, v, x, t, e, hp) =
		    treeifyDef(x,
		      M.LOAD(ity, scaleWord(regbind v, cpsInt i), getRegionPi(v, i)),
		      t, e, hp)

	    (*
	     * Funny select; I don't know that this does
	     *)
	      and funnySelect (i, k, x, t, e, hp) = let
		    val unboxedfloat = MS.unboxedFloats
		    fun isFlt t = if unboxedfloat
			  then (case t of FLTt _ => true | _ => false)
			  else false
		    fun fallocSp(x,e,hp) = (
			  addFregBinding(x, M.FREG(fty, newFreg REAL64)); gen(e, hp))
		  (* warning: the following generated code should never be
		     executed; its semantics is completely screwed up !
		   *)
		    in
		      if isFlt t
			then fallocSp(x, e, hp)
			else defINT(x, LI k, e, hp)(* BOGUS *)
		    end

	    (*
	     * Call an external function
	     *)
	      and externalApp (f, args, hp) = let
		    val ctys = map grabty args
		    val formals = ArgP.standard{fnTy=typmap f, vfp=vfp, argTys=ctys}
		    val dest = (case formals
			   of (M.GPR dest::_) => dest
			    | _ => error "externalApp: dest"
			  (* end case *))
		    in
		      callSetup (formals, args);
		      if gctypes
			then annotation(gcAnnotation(#create GCCells.GCLIVEOUT, formals, ctys))
			else ();
		      testLimit hp;
		      emit (M.JMP(dest, []));
		      exitBlock (formals @ dedicated)
		    end (* externalApp *)

	    (*
	     * Call an internal function
	     *)
	      and internalApp(f, args, hp) = (case lookupGenTbl f
		     of Frag.KNOWNFUN(ref(Frag.GEN formals)) => (
			  updtHeapPtr hp;
			  callSetup (formals, args);
			  emit (branchToLabel(functionLabel f)))
		      | Frag.KNOWNFUN(r as ref(Frag.UNGEN(f,vl,tl,e))) => let
			  val formals = known tl
			  val lab = functionLabel f
			  in
			    r := Frag.GEN formals;
			    updtHeapPtr hp;
			    callSetup (formals, args);
			    genCPSFunction (lab, C.KNOWN, f, vl, formals, tl, e)
			  end
		      | Frag.KNOWNCHK(r as ref(Frag.UNGEN(f,vl,tl,e))) => let
			  val formals = if MS.fixedArgPassing
				then ArgP.fixed{argTys=tl, vfp=vfp}
				else known tl
			  val lab = functionLabel f
			  in
			    r := Frag.GEN formals;
			    callSetup (formals, args);
			    testLimit hp;
			    genCPSFunction (lab, C.KNOWN_CHECK, f, vl, formals, tl, e)
			  end
		     | Frag.KNOWNCHK(ref(Frag.GEN formals)) => (
			  callSetup (formals, args);
			  testLimit hp;
			  emit (branchToLabel(functionLabel f)))
		     | Frag.STANDARD{fmlTyps, ...} => let
			  val formals = ArgP.standard{fnTy=typmap f, argTys=fmlTyps, vfp=vfp}
			  in
			    callSetup(formals, args);
			    testLimit hp;
			    emit(branchToLabel(functionLabel f))
			  end
		    (* end case *))

	      and rawload (kind, i, x, e, hp) = (case kind
		     of P.INT sz => if (sz = ity)
			    then defINT (x, M.LOAD (ity, i, R.memory), e, hp)
			  else if (sz < ity)
			    then defINT (x, signExtend (sz, M.LOAD (sz, i, R.memory)), e, hp)
			    else error ("rawload: unsupported INT " ^ Int.toString sz)
		      | P.UINT sz => if (sz = ity)
			    then defINT (x, M.LOAD (ity, i, R.memory), e, hp)
			  else if (sz < ity)
			    then defINT (x, zeroExtend (sz, M.LOAD (sz, i, R.memory)), e, hp)
			    else error ("rawload: unsupported UINT " ^ Int.toString sz)
		      | P.FLOAT 32 =>
(* REAL32: FIXME *)
			  treeifyDefF64 (x, M.CVTF2F (64, 32, M.FLOAD (32, i, R.memory)), e, hp)
		      | P.FLOAT 64 => treeifyDefF64 (x, M.FLOAD (64, i, R.memory), e, hp)
		      | P.FLOAT sz => error ("rawload: unsupported float size: " ^ Int.toString sz)
		    (* end case *))

	      and rawstore (kind, i, x) = (case kind
		     of P.INT sz => if (sz <= ity)
			(* value is `ity` bits, but only `sz` bits are stored *)
			  then emit (M.STORE (sz, i, regbind x, R.memory))
			  else error ("rawstore: unsupported INT " ^ Int.toString sz)
		      | P.UINT sz => if (sz <= ity)
			(* value is `ity` bits, but only `sz` bits are stored *)
			  then emit (M.STORE (sz, i, regbind x, R.memory))
			  else error ("rawstore: unsupported INT " ^ Int.toString sz)
		      | P.FLOAT 32 => emit (M.FSTORE (32, i, fregbind x, R.memory))
		      | P.FLOAT 64 => emit (M.FSTORE (64, i, fregbind x, R.memory))
		      | P.FLOAT sz => error ("rawstore: unsupported float size: " ^ Int.toString sz)
		    (* end case *))

	    (*
	     * Generate code
	     *)
		(** RECORD **)
	      and gen (C.RECORD(C.RK_VECTOR, vl, w, e), hp) = mkVector(vl, w, e, hp)
		| gen (C.RECORD(C.RK_FCONT, vl, w, e), hp) = mkFblock(vl, w, e, hp)
		| gen (C.RECORD(C.RK_RAW64BLOCK, vl, w, e), hp) = mkFblock(vl, w, e, hp)
		| gen (C.RECORD(C.RK_RAWBLOCK, vl, w, e), hp) = mkIntBlock(vl, w, e, hp)
		| gen (C.RECORD(_, vl, w, e), hp) = mkRecord(vl, w, e, hp)

		(*** SELECT ***)
		| gen (C.SELECT(i, NUM{ty={tag=true, ...}, ival}, x, t, e), hp) =
		    funnySelect(IntInf.fromInt i, ival, x, t, e, hp)
		| gen (C.SELECT(i, v, x, FLTt 64, e), hp) = f64select(i, v, x, e, hp) (* REAL32: *)
		| gen (C.SELECT(i, v, x, t, e), hp) = select(i, v, x, t, e, hp)

		(*** OFFSET ***)
		| gen (C.OFFSET(i, v, x, e), hp) =
		    defBoxed(x, scaleWord(regbind v, cpsInt i), e, hp)

		(*** APP ***)
		| gen (C.APP(NUM{ty={tag=true, ...}, ...}, args), hp) = updtHeapPtr hp
		| gen (C.APP(VAR f, args), hp) = externalApp(f, args, hp)
		| gen (C.APP(LABEL f, args), hp) = internalApp(f, args, hp)

		(*** SWITCH ***)
		| gen (C.SWITCH(NUM _, _, _), hp) = error "SWITCH on constant"
		| gen (C.SWITCH(v, _, l), hp) = let
		    val lab = newLabel ()
		    val labs = map (fn _ => newLabel()) l
		    val tmpR = newReg INT val tmp = M.REG(ity,tmpR)
		    in
		      emit (M.MV(ity, tmpR, laddr(lab, 0)));
		      emit (M.JMP(M.ADD(addrTy, tmp, M.LOAD(pty, scaleWord(tmp, v), R.readonly)), labs));
		      pseudoOp (PB.DATA_READ_ONLY);
		      pseudoOp (PB.EXT(CPs.JUMPTABLE{base=lab, targets=labs}));
		      pseudoOp PB.TEXT;
		      ListPair.app (fn (lab, e) => genlabCont(lab, e, hp)) (labs, l)
		    end

		(*** PURE ***)
(* REAL32: FIXME *)
		| gen (C.PURE(P.INT_TO_REAL{from, to=64}, [v], x, _, e), hp) =
		    if (from <= Target.defaultIntSz)
		      then treeifyDefF64 (x, M.CVTI2F(fty,ity,untagSigned(v)), e, hp)
		      else treeifyDefF64 (x, M.CVTI2F(fty,ity,regbind v), e, hp)
(* REAL32: FIXME *)
		| gen (C.PURE(P.PURE_ARITH{oper, kind=P.FLOAT 64}, [v], x, _, e), hp) = let
		    val r = fregbind v
		    in
		      case oper
		       of P.NEG => treeifyDefF64 (x, M.FNEG(fty,r), e, hp)
			| P.FABS => treeifyDefF64 (x, M.FABS(fty,r), e, hp)
			| P.FSQRT => treeifyDefF64 (x, M.FSQRT(fty,r), e, hp)
			| P.FSIN => computef64 (x, M.FEXT(fty, E.FSINE r), e, hp)
			| P.FCOS => computef64 (x, M.FEXT(fty, E.FCOSINE r), e, hp)
			| P.FTAN => computef64 (x, M.FEXT(fty, E.FTANGENT r), e, hp)
			| _ => error "unexpected primop in pure unary float64"
		      (* end case *)
		    end
		| gen (C.PURE(P.PURE_ARITH{oper, kind=P.FLOAT 64}, [v,w], x, _, e), hp) = let
		    val v = fregbind v
		    val w = fregbind w
		    in
		      case oper
		       of P.ADD => treeifyDefF64 (x, M.FADD(fty, v, w), e, hp)
			| P.MUL => treeifyDefF64 (x, M.FMUL(fty, v, w), e, hp)
			| P.SUB => treeifyDefF64 (x, M.FSUB(fty, v, w), e, hp)
			| P.FDIV => treeifyDefF64 (x, M.FDIV(fty, v, w), e, hp)
			| _ => error "unexpected primop in pure binary float64"
		      (* end case *)
		    end
		| gen (C.PURE(P.PURE_ARITH{oper=P.ORB, kind}, [v,w], x, _, e), hp) =
		    defWithKind(kind, x, M.ORB(ity, regbind v, regbind w), e, hp)
		| gen (C.PURE(P.PURE_ARITH{oper=P.ANDB, kind}, [v,w], x, _, e), hp) =
		    defWithKind(kind, x, M.ANDB(ity, regbind v, regbind w), e, hp)
		| gen (C.PURE(P.PURE_ARITH{oper, kind}, [v,w], x, ty, e), hp) = (case kind
		     of P.INT sz => if (sz <= Target.defaultIntSz)
			  then (case oper
			     of P.XORB => defTAGINT(x, tagIntXor(v,w), e, hp)
			      | P.LSHIFT => defTAGINT(x, tagIntLShift(v,w), e, hp)
			      | P.RSHIFT => defTAGINT(x, tagIntRShift(M.SRA,v,w),e,hp)
			      | P.ADD => defTAGINT(x, tagIntAdd(M.ADD, v, w), e, hp)
			      | P.SUB => defTAGINT(x, tagIntSub(M.SUB, v, w), e, hp)
			      | P.MUL => defTAGINT(x, tagIntMul(true, M.MULS, v, w), e, hp)
			      | _ => error "gen: PURE INT TAGGED"
			    (* end case *))
			  else (case oper
			     of P.XORB  => arithINT(M.XORB, v, w, x, e, hp)
			      | P.LSHIFT => shiftINT(M.SLL, v, w, x, e, hp)
			      | P.RSHIFT => shiftINT(M.SRA, v, w, x, e, hp)
			      | _ => error "gen: PURE INT"
			    (* end case *))
		      | P.UINT sz => if (sz <= Target.defaultIntSz)
			  then (case oper
			     of P.ADD => defTAGINT(x, tagIntAdd(M.ADD, v, w), e, hp)
			      | P.SUB => defTAGINT(x, tagIntSub(M.SUB, v, w), e, hp)
			      | P.MUL => defTAGINT(x, tagIntMul(false, M.MULU, v, w), e, hp)
			    (* we now explicitly defend agains div by 0 in translate, so these
			     * two operations can be treated as pure op:
			     *)
			      | P.QUOT => defTAGINT(x, tagIntDiv(false, M.DIV_TO_ZERO, v, w), e, hp)
			      | P.REM => defTAGINT(x, tagIntRem(false, M.DIV_TO_ZERO, v, w), e, hp)
			      | P.XORB => defTAGINT(x, tagIntXor(v, w), e, hp)
			      | P.LSHIFT => defTAGINT(x, tagIntLShift(v, w), e, hp)
			      | P.RSHIFT => defTAGINT(x, tagIntRShift(M.SRA, v, w), e, hp)
			      | P.RSHIFTL => defTAGINT(x, tagIntRShift(M.SRL, v, w), e, hp)
			      | _ => error "gen: PURE UINT TAGGED"
			    (* end case *))
			  else (case oper
			     of P.ADD => arithINT(M.ADD, v, w, x, e, hp)
			      | P.SUB => arithINT(M.SUB, v, w, x, e, hp)
			      | P.MUL => arithINT(M.MULU, v, w, x, e, hp)
			    (* we now explicitly defend agains div by 0 in translate, so these
			     * two operations can be treated as pure op:
			     *)
			      | P.QUOT => arithINT(M.DIVU, v, w, x, e, hp)
			      | P.REM => arithINT(M.REMU, v, w, x, e, hp)
			      | P.XORB => arithINT(M.XORB, v, w, x, e, hp)
			      | P.LSHIFT => shiftINT(M.SLL, v, w, x, e, hp)
			      | P.RSHIFT => shiftINT(M.SRA, v, w, x, e, hp)
			      | P.RSHIFTL => shiftINT(M.SRL, v, w, x, e, hp)
			      | _ => error "gen:PURE UINT 32"
			    (* end case *))
		      | _ => error "unexpected numkind in pure binary arithop"
		    (* end case *))
		| gen (C.PURE(P.PURE_ARITH{oper=P.NOTB, kind}, [v], x, _, e), hp) = let
		    val sz = (case kind
			   of P.UINT sz => sz
			    | _ => error "unexpected numkind in pure notb arithop")
		    in
		      if (sz <= Target.defaultIntSz)
			then defTAGINT(x, M.SUB(ity, zero, regbind v), e, hp)
			else defINT(x, M.XORB(ity, regbind v, allOnes), e, hp)
		    end
		| gen (C.PURE(P.PURE_ARITH{oper=P.NEG, kind}, [v], x, _, e), hp) = let
		    val sz = (case kind
			   of P.UINT sz => sz
			    | _ => error "unexpected numkind in pure ~ arithop")
		    in
		      if (sz <= Target.defaultIntSz)
			then defTAGINT (x, M.SUB (ity, two, regbind v), e, hp)
			else defINT (x, M.SUB(ity, zero, regbind v), e, hp)
		    end
		| gen (C.PURE(P.COPY{from=8, to}, [v], x, _, e), hp) =
		    if (to <= Target.defaultIntSz)
		      then copy (TAGINT, x, v, e, hp)
		      else defINT (x, M.SRL(ity, regbind v, one), e, hp)
		| gen (C.PURE(P.COPY{from, to}, [v], x, _, e), hp) =
		    if (from = to)
		      then copyM(from, x, v, e, hp)
		    else if (from = Target.defaultIntSz) andalso (to = ity)
		      then defINT (x, M.SRL(ity, regbind v, one), e, hp)
		      else error "gen:PURE:COPY"
		| gen (C.PURE(P.COPY_INF _, _, _, _, _), hp) =
		    error "gen:PURE:COPY_INF"
		| gen (C.PURE(P.EXTEND{from=8, to}, [v], x, _ ,e), hp) = let
		    val sa = IntInf.fromInt(Target.defaultIntSz - 8)
		    in
		      if (to <= Target.defaultIntSz)
			then defTAGINT (x, M.SRA(ity, M.SLL(ity, regbind v, LI sa), LI sa), e, hp)
			else defINT (x, M.SRA(ity, M.SLL(ity, regbind v, LI sa), LI(sa+1)), e, hp)
		    end
		| gen (C.PURE(P.EXTEND{from, to}, [v], x, _ ,e), hp) =
		    if (from = to)
		      then copyM(from, x, v, e, hp)
		    else if (from = Target.defaultIntSz) andalso (to = ity)
		      then defINT (x, M.SRA(ity, regbind v, one), e, hp)
		      else error "gen:PURE:EXTEND"
		| gen (C.PURE(P.EXTEND_INF _, _, _, _, _), hp) =
		    error "gen:PURE:EXTEND_INF"
		| gen (C.PURE(P.TRUNC{from, to}, [v], x, _, e), hp) =
		    if (from = to)
		      then copyM(from, x, v, e, hp)
		    else if (to = 8)
		      then if (from <= Target.defaultIntSz)
			then defTAGINT (x, M.ANDB(ity, regbind v, LI 0x1ff), e, hp) (* mask includes tag bit *)
			else defTAGINT (x, tagUnsigned(M.ANDB(ity, regbind v, LI 0xff)), e, hp)
		    else if (from = ity) andalso (to = Target.defaultIntSz)
		      then defTAGINT (x, M.ORB(ity, M.SLL(ity, regbind v, one), one), e, hp)
		      else error "gen:PURE:trunc"
		| gen (C.PURE(P.TRUNC_INF _, _, _, _, _), hp) =
		    error "gen:PURE:TRUNC_INF"
		| gen (C.PURE(P.OBJLENGTH, [v], x, _, e), hp) =
		    defTAGINT(x, orTag(getObjLength v), e, hp)
		| gen (C.PURE(P.LENGTH, [v], x, t, e), hp) = select(1, v, x, t, e, hp)
		| gen (C.PURE(P.SUBSCRIPTV, [v, ix as NUM{ty={tag=true, ...}, ...}], x, t, e), hp) = let
		  (* get data pointer *)
		    val mem  = dataptrRegion v
		    val a    = markPTR(M.LOAD(ity, regbind v, mem))
		    val mem' = arrayRegion mem
		    in
		      defBoxed(x, M.LOAD(ity, scaleWord(a, ix), mem'), e, hp)
		    end
		| gen (C.PURE(P.SUBSCRIPTV, [v, w], x, _, e), hp) = let
		  (* get data pointer *)
		    val mem  = dataptrRegion v
		    val a    = markPTR(M.LOAD(ity, regbind v, mem))
		    val mem' = arrayRegion mem
		    in
		      defBoxed(x, M.LOAD(ity, scaleWord(a, w), mem'), e, hp)
		    end
(* FIXME: for some reason, `INT 8` has been used for word8/char vectors; we
 * keep it around for now to support porting, but it really should be
 * sign extending the result.
 *)
		| gen (C.PURE(P.PURE_NUMSUBSCRIPT{kind=(P.UINT 8|P.INT 8)}, [v,i], x, _, e), hp) = let
		  (* get data pointer *)
		    val mem  = dataptrRegion v
		    val a    = markPTR(M.LOAD(ity, regbind v, mem))
		    val mem' = arrayRegion mem
		    in
		      defTAGINT(x,tagUnsigned(M.LOAD(8,scale1(a, i), mem')), e, hp)
		    end
		| gen (C.PURE(P.GETTAG, [v], x, _, e), hp) =
		    defTAGINT(x,
		      tagUnsigned(M.ANDB(ity, getObjDescriptor v, LI(D.powTagWidth-1))),
		      e, hp)
		| gen (C.PURE(P.MKSPECIAL, [i, v], x, _, e), hp) = let
		    val desc = (case i
			   of NUM{ty={tag=true, ...}, ival} => LI(D.makeDesc(ival, D.tag_special))
			    | _ => M.ORB(ity, M.SLL(ity, untagSigned i, LW' D.tagWidth), LI D.desc_special)
			  (* end case *))
		    in  (* What gc types are the components? *)
		      treeifyAlloc(x,
			  allocRecord(markNothing, memDisambig x,
				      desc, [(v, offp0)], hp),
			  e, hp+8)
		    end
		| gen (C.PURE(P.MAKEREF, [v], x, _, e), hp) = let
		    val tag = LI D.desc_ref
		    val mem = memDisambig x
		    in
		      emit(M.STORE(ity, M.ADD(addrTy, Regs.allocptr, LI' hp), tag, mem));
		      emit(M.STORE(ity, M.ADD(addrTy, Regs.allocptr, LI'(hp+ws)), regbind' v, mem));
		      treeifyAlloc(x, hp+ws, e, hp+2*ws)
		    end
		| gen (C.PURE(P.BOX, [u], w, _, e), hp) = copy(PTR, w, u, e, hp)
		| gen (C.PURE(P.UNBOX, [u], w, _, e), hp) = copy(INT, w, u, e, hp)
		| gen (C.PURE(P.WRAP kind, [u], w, _, e), hp) = (case kind
		     of P.FLOAT 64 => mkFblock([(u, offp0)],w,e,hp)
(* REAL32: FIXME *)
		      | P.INT sz => if (sz < ity)
			    then error "wrap for tagged ints is not implemented"
			  else if (sz = ity)
			    then mkIntBlock([(u, offp0)], w, e, hp)
			    else error(concat["wrap(INT ", Int.toString sz, ") is not implemented"])
		      | _ => error "wrap: bogus kind"
		    (* end case *))
		| gen (C.PURE(P.UNWRAP kind, [u], w, _, e), hp) = (case kind
		     of P.FLOAT 64 => f64select(0,u,w,e,hp)
(* REAL32: FIXME *)
		      | P.INT sz => if (sz < ity)
			    then error "unwrap for tagged ints is not implemented"
			  else if (sz = ity)
			    then select(0, u, w, NUMt{sz=sz, tag=false}, e, hp)
			    else error(concat["unwrap(INT ", Int.toString sz, ") is not implemented"])
		      | _ => error "unwrap: bogus kind"
		    (* end case *))

		    (* Note: the gc type is unsafe! XXX *)
		| gen (C.PURE(P.CAST,[u],w,_,e), hp) = copy(PTR, w, u, e, hp)
		| gen (C.PURE(P.GETCON,[u],w,t,e), hp) = select(0,u,w,t,e,hp)
		| gen (C.PURE(P.GETEXN,[u],w,t,e), hp) = select(0,u,w,t,e,hp)
		| gen (C.PURE(P.GETSEQDATA, [u], x, t, e), hp) = select(0,u,x,t,e,hp)
		| gen (C.PURE(P.RECSUBSCRIPT, [v, NUM{ty={tag=true, ...}, ival}], x, t, e), hp) =
		    select(IntInf.toInt ival, v, x, t, e, hp)
		| gen (C.PURE(P.RECSUBSCRIPT, [v, w], x, _, e), hp) = let
		  (* no indirection! *)
		    val mem = arrayRegion(getRegion v)
		    in
		      defTAGINT(x, M.LOAD(ity, scaleWord(regbind v, w), mem), e, hp)
		    end
		| gen (C.PURE(P.RAW64SUBSCRIPT, [v, i], x, _, e), hp) = let
		    val mem = arrayRegion(getRegion v)
		    in
		      treeifyDefF64(x, M.FLOAD(fty,scale8(regbind v, i), mem), e, hp)
		    end
		| gen (C.PURE(P.NEWARRAY0, [_], x, t, e), hp) = let
		    val hdrDesc = D.desc_polyarr
		    val dataDesc = D.desc_ref
		    val dataPtr = newReg PTR
		    val hdrM = memDisambig x
		    val (tagM, valM) = (hdrM, hdrM) (* Allen *)
		    in  (* gen code to allocate "ref()" for array data *)
		      emit(M.STORE(ity, M.ADD(addrTy, Regs.allocptr, LI' hp),
				   LI dataDesc, tagM));
		      emit(M.STORE(ity, M.ADD(addrTy, Regs.allocptr, LI'(hp+ws)),
				   mlZero, valM));
		      emit(M.MV(pty, dataPtr, M.ADD(addrTy,Regs.allocptr,LI'(hp+ws))));
		      (* gen code to allocate array header *)
		      treeifyAlloc(x,
			 allocHeaderPair(hdrDesc, hdrM, dataPtr, 0, hp+2*ws),
			    e, hp+5*ws)
		    end
		| gen (C.PURE(P.RAWRECORD NONE, [NUM{ty={tag=true, ...}, ival}], x, _, e), hp) =
		    (* allocate space for CPS spilling *)
		    treeifyAlloc(x, hp, e, hp + IntInf.toInt ival * ws) (* no tag! *)
		| gen (C.PURE(P.RAWRECORD(SOME rk), [NUM{ty={tag=true, ...}, ival}], x, _, e), hp) = let
		  (* allocate an uninitialized record with a tag *)
		    val (tag, fp) = (case rk (* tagged version *)
			   of (C.RK_FCONT | C.RK_RAW64BLOCK) => (D.tag_raw64, true)
			    | C.RK_RAWBLOCK => (D.tag_raw, false)
			    | C.RK_VECTOR => error "rawrecord VECTOR unsupported"
			    | _ => (D.tag_record, false)
			  (* end case *))
		  (* len of record in 32-bit words *)
(* 64BIT: FIXME *)
		    val len = if ws = 4 andalso fp then ival+ival else ival
		  (* record descriptor *)
		    val desc = D.makeDesc(len, tag)
		  (* Align floating point *)
(* 64BIT: REAL32: FIXME *)
		    val hp = if ws = 4 andalso fp
			     andalso Word.andb(Word.fromInt hp, 0w4) <> 0w0
			  then hp+4
			  else hp
		    val mem = memDisambig x
		    in
		    (* store tag now! *)
		      emit(M.STORE(ity, ea(Regs.allocptr, hp), LI desc, pi(mem, ~1)));
		    (* assign the address to x *)
		      treeifyAlloc(x, hp+ws, e, hp+(IntInf.toInt len)*ws+ws)
		    end

		(*** ARITH ***)
		| gen (C.ARITH(P.IARITH{sz=sz, oper=P.INEG}, [v], x, _, e), hp) = (
		    updtHeapPtr hp;
		    if (sz <= Target.defaultIntSz)
		      then defTAGINT(x, M.SUBT(ity, two, regbind v), e, 0)
		      else defINT(x, M.SUBT(ity, zero, regbind v), e, 0))
		| gen (C.ARITH(P.IARITH{sz=sz, oper}, [v, w], x, _, e), hp) = (
		    updtHeapPtr hp;
		    if (sz <= Target.defaultIntSz)
		      then (case oper
			 of P.IADD => defTAGINT(x, tagIntAdd(M.ADDT, v, w), e, 0)
			  | P.ISUB => defTAGINT(x, tagIntSub(M.SUBT, v, w), e, 0)
			  | P.IMUL => defTAGINT(x, tagIntMul(true, M.MULT, v, w), e, 0)
			  | P.IDIV => defTAGINT(x, tagIntDiv(true, M.DIV_TO_NEGINF, v, w), e, 0)
			  | P.IMOD => defTAGINT(x, tagIntRem(true, M.DIV_TO_NEGINF, v, w), e, 0)
			  | P.IQUOT => defTAGINT(x, tagIntDiv(true, M.DIV_TO_ZERO, v, w), e, 0)
			  | P.IREM => defTAGINT(x, tagIntRem(true, M.DIV_TO_ZERO, v, w), e, 0)
			  | _ => error(concat["gen: ", PPCps.arithopToString oper, " TAG INT"])
			(* end case *))
		      else (case oper
			 of P.IADD => arithINT(M.ADDT, v, w, x, e, 0)
			  | P.ISUB => arithINT(M.SUBT, v, w, x, e, 0)
			  | P.IMUL => arithINT(M.MULT, v, w, x, e, 0)
			  | P.IDIV => arithINT(fn(ty,x,y)=>M.DIVT(M.DIV_TO_NEGINF,ty,x,y),
					     v, w, x, e, 0)
			  | P.IMOD => arithINT(fn(ty,x,y)=>M.REMS(M.DIV_TO_NEGINF,ty,x,y),
					     v, w, x, e, 0)
			  | P.IQUOT => arithINT(fn(ty,x,y)=>M.DIVT(M.DIV_TO_ZERO,ty,x,y),
					     v, w, x, e, 0)
			  | P.IREM => arithINT(fn(ty,x,y)=>M.REMS(M.DIV_TO_ZERO,ty,x,y),
					     v, w, x, e, 0)
			  | _ => error(concat["gen: ", PPCps.arithopToString oper, " INT"])
			(* end case *)))
		| gen (C.ARITH(P.TESTU{from, to}, [v], x, _, e), hp) =
		  (* Note: for testu operations we use a somewhat arcane method
		   * to generate traps on overflow conditions. A better approach
		   * would be to generate a trap-if-negative instruction available
		   * on a variety of machines, e.g. mips and sparc (maybe others).
		   *)
		    if (from = to)
		      then let
			val gc = if (from < ity) then TAGINT else INT
			val xreg = newReg gc
			val vreg = regbind v
			in
			  updtHeapPtr hp;
			  emit(M.MV(ity, xreg, M.ADDT(ity, vreg, signBit)));
			  def(gc, x, vreg, e, 0)
			end
		    else if (from = ity) andalso (to = Target.defaultIntSz)
		      then let (* native word to tagged int conversion *)
			val vreg = regbind v
			val tmp = newReg INT
			val tmpR = M.REG(ity, tmp)
			val lab = newLabel ()
			in
			  emit(M.MV(ity, tmp, allOnes'));
			  updtHeapPtr hp;
			  emit(branchWithProb(
			    M.BCC(M.CMP(ity, M.LEU, vreg, tmpR),lab),
			    SOME Probability.likely));
			(* generate a trap by adding allOnes' to itself.  This code assumes that
			 * ity = Target.defaultIntSz+1.
			 *)
			  emit(M.MV(ity, tmp, M.ADDT(ity, tmpR, tmpR)));
			  defineLabel lab;
			  defTAGINT(x, tagUnsigned(vreg), e, 0)
			end
		      else error "gen:ARITH:TESTU with unexpected precisions (not implemented)"
		| gen (C.ARITH(P.TEST{from, to}, [v], x, _, e), hp) =
		    if (from = to)
		      then copyM(from, x, v, e, hp)
		    else if (from = ity) andalso (to = Target.defaultIntSz)
		      then (updtHeapPtr hp; defTAGINT(x, tagSigned(regbind v), e, 0))
		      else error "gen:ARITH:TEST with unexpected precisions (not implemented)"
		| gen (C.ARITH(P.TEST_INF _, _, _, _, _), hp) =
		    error "gen:ARITH:TEST_INF"

		(*** LOOKER ***)
		| gen (C.LOOKER(P.DEREF, [v], x, _, e), hp) = let
		    val mem = arrayRegion(getRegion v)
		    in
		      defBoxed (x, M.LOAD(ity, regbind v, mem), e, hp)
		    end
		| gen (C.LOOKER(P.SUBSCRIPT, [v,w], x, _, e), hp) = let
		  (* get data pointer *)
		    val mem  = dataptrRegion v
		    val a    = markPTR(M.LOAD(ity, regbind v, mem))
		    val mem' = arrayRegion mem
		    in
		      defBoxed (x, M.LOAD(ity, scaleWord(a, w), mem'), e, hp)
		    end
(* FIXME: for some reason, `INT 8` has been used for word8/char arrays; we
 * keep it around for now to support porting, but it really should be
 * sign extending the result.
 *)
		| gen (C.LOOKER(P.NUMSUBSCRIPT{kind=(P.UINT 8|P.INT 8)}, [v, i], x, _, e), hp) = let
		  (* get data pointer *)
		    val mem  = dataptrRegion v
		    val a    = markPTR(M.LOAD(ity, regbind v, mem))
		    val mem' = arrayRegion mem
		    in
		      defTAGINT(x, tagUnsigned(M.LOAD(8,scale1(a, i), mem')), e, hp)
		    end
(* REAL32: FIXME *)
		| gen (C.LOOKER(P.NUMSUBSCRIPT{kind=P.FLOAT 64}, [v,i], x, _, e), hp) = let
		  (* get data pointer *)
		    val mem  = dataptrRegion v
		    val a    = markPTR(M.LOAD(ity, regbind v, mem))
		    val mem' = arrayRegion mem
		    in
		      treeifyDefF64(x, M.FLOAD(fty,scale8(a, i), mem'), e, hp)
		    end
		| gen (C.LOOKER(P.GETHDLR,[],x,_,e), hp) = defBoxed(x, Regs.exnptr vfp, e, hp)
		| gen (C.LOOKER(P.GETVAR, [], x, _, e), hp) = defBoxed(x, Regs.varptr vfp, e, hp)
		| gen (C.LOOKER(P.GETSPECIAL, [v], x, _, e), hp) =
		  (* special tag is in length field; we assume it is unsigned *)
		    defTAGINT(x, orTag(getObjLength v), e, hp)
		| gen (C.LOOKER(P.RAWLOAD{ kind }, [i], x, _, e), hp) =
		    rawload (kind, regbind i, x, e, hp)
		| gen (C.LOOKER(P.RAWLOAD{ kind }, [i,j], x, _, e), hp) =
		    rawload (kind, M.ADD(addrTy,regbind i, regbind j), x, e, hp)

		(*** SETTER ***)
		| gen (C.SETTER(P.RAWUPDATE(FLTt 64),[v,i,w],e),hp) = (
(* REAL32: FIXME *)
		    emit(M.FSTORE(fty, scale8(regbind' v, i), fregbind w,R.memory));
		    gen(e, hp))
		| gen (C.SETTER(P.RAWUPDATE _, [v,i,w], e), hp) = (
		    emit(M.STORE(ity, scaleWord(regbind' v, i), regbind' w, R.memory));
		    gen(e, hp))
		| gen (C.SETTER(P.ASSIGN, [a as VAR arr, v], e), hp) = let
		    val ea = regbind a
		    val mem = arrayRegion(getRegion a)
		    in
		      recordStore(ea, hp);
		      emit(M.STORE(ity, ea, regbind v, mem));
		      gen(e, hp+2*ws)
		    end
		| gen (C.SETTER(P.UNBOXEDASSIGN, [a, v], e), hp) = let
		    val mem = arrayRegion(getRegion a)
		    in
		      emit(M.STORE(ity, regbind a, regbind v, mem));
		      gen(e, hp)
		    end
		| gen (C.SETTER(P.UPDATE, [v,i,w], e), hp) = let
		  (* get data pointer *)
		    val mem  = dataptrRegion v
		    val a    = markPTR(M.LOAD(ity, regbind v, mem))
		    val tmpR = Cells.newReg() (* derived pointer! *)
		    val tmp  = M.REG(ity, tmpR)
		    val ea   = scaleWord(a, i)  (* address of updated cell *)
		    val mem' = arrayRegion mem
		    in
		      emit(M.MV(ity, tmpR, ea));
		      recordStore(tmp, hp);
		      emit(M.STORE(ity, tmp, regbind w, mem'));
		      gen(e, hp+2*ws)
		    end
		| gen (C.SETTER(P.UNBOXEDUPDATE, [v, i, w], e), hp) = let
		  (* get data pointer *)
		    val mem  = dataptrRegion v
		    val a    = markPTR(M.LOAD(ity, regbind v, mem))
		    val mem' = arrayRegion mem
		    in
		      emit(M.STORE(ity, scaleWord(a, i), regbind w, mem'));
		      gen(e, hp)
		    end
(* FIXME: for some reason, `INT 8` has been used for word8/char arrays; we
 * keep it around for now to support porting, but it really should be
 * sign extending the result.
 *)
		| gen (C.SETTER(P.NUMUPDATE{kind=(P.UINT 8|P.INT 8)}, [s,i,v], e), hp) = let
		  (* get data pointer *)
		    val mem  = dataptrRegion v
		    val a    = markPTR(M.LOAD(ity, regbind s, mem))
		    val ea   = scale1(a, i)
		    val mem' = arrayRegion mem
		    in
		      emit(M.STORE(8, ea, untagUnsigned v, mem'));
		      gen(e, hp)
		    end
		| gen (C.SETTER(P.NUMUPDATE{kind=P.FLOAT 64},[v,i,w],e), hp) = let
		  (* get data pointer *)
		    val mem  = dataptrRegion v
		    val a    = markPTR(M.LOAD(ity, regbind v, mem))
		    val mem' = arrayRegion mem
		    in
		      emit(M.FSTORE(fty,scale8(a, i), fregbind w, mem'));
		      gen(e, hp)
		    end
		| gen (C.SETTER(P.SETSPECIAL, [v, i], e), hp) = let
		    val ea = M.SUB(ity, regbind v, LI ws)
		    val i' = (case i
			   of NUM{ty={tag=true, ...}, ival} => LI(D.makeDesc(ival, D.tag_special))
			    | _ => M.ORB(ity, M.SLL(ity, untagSigned i, LW' D.tagWidth),
					 LI D.desc_special)
			  (* end case *))
		    val mem = getRegionPi(v, 0)
		    in
		      emit(M.STORE(ity, ea, i', mem));
		      gen(e, hp)
		    end
		| gen (C.SETTER(P.SETHDLR,[x],e), hp) =
		    (emit(assign(Regs.exnptr(vfp), regbind x)); gen(e, hp))
		| gen (C.SETTER(P.SETVAR,[x],e), hp) =
		    (emit(assign(Regs.varptr(vfp), regbind x)); gen(e, hp))
		| gen (C.SETTER (P.RAWSTORE { kind }, [i, x], e), hp) = (
		    rawstore (kind, regbind i, x); gen (e, hp))
		| gen (C.SETTER (P.RAWSTORE { kind }, [i, j, x], e), hp) = (
		    rawstore (kind, M.ADD(addrTy, regbind i, regbind j), x);
		    gen (e, hp))

		| gen (C.RCC(arg as (_, _, _, _, wtl, e)), hp) = let
		    val {result, hp} = CPSCCalls.c_call {
			    stream = stream, regbind = regbind,
			    fregbind = fregbind, typmap = typmap,
			    vfp = vfp, hp = hp
			  } arg
		    in
		      case (result, wtl)
		       of ([], [(w, _)]) => defTAGINT (w, mlZero, e, hp) (* void result *)
			| ([M.FPR x],[(w,C.FLTt 64)]) => treeifyDefF64 (w, x, e, hp) (* REAL32: *)
			      (* more sanity checking here ? *)
			| ([M.GPR x],[(w, C.NUMt{tag=false, ...})]) => defINT (w, x, e, hp)
			| ([M.GPR x],[(w, C.PTRt _)]) => defBoxed (w, x, e, hp)
			| ([M.GPR x1, M.GPR x2],
			   [(w1, C.NUMt{tag=false, ...}), (w2, C.NUMt{tag=false, ...})]
			  ) => let
			    val (r1, r2) = (newReg INT, newReg INT)
			    in
			      addRegBinding(w1, r1);
			      addRegBinding(w2, r2);
			      emit(M.MV(ity,r1,x1));
			      emit(M.MV(ity,r2,x2));
			      gen(e,hp)
			    end
			| _ => error "RCC: bad results"
		      (* end case *)
		    end

		(*** BRANCH  ***)
(* this case should never arise, since CPS contraction handles it? *)
		| gen (C.BRANCH(P.CMP{oper, kind}, [NUM v, NUM k], _, e, d), hp) =
raise Fail "unexpected constant branch"
		| gen (C.BRANCH(P.CMP{oper, kind=P.INT _}, vw, p, e, d), hp) =
		    branch(p, signedCmp oper, vw, e, d, hp)
		| gen (C.BRANCH(P.CMP{oper, kind=P.UINT _}, vw, p, e, d), hp) =
		    branch(p, unsignedCmp oper, vw, e, d, hp)
		| gen (C.BRANCH(P.FCMP{oper, size}, [v,w], p, d, e), hp) = let
		    val trueLab = newLabel ()
		    val cmp = floatCmp(oper, size, v, w)
		    in
		      emit(M.BCC(cmp, trueLab));
		      genCont(e, hp);
		      genlab(trueLab, d, hp)
		    end
(* REAL32: FIXME *)
		| gen (C.BRANCH(P.FSGN 64, [v], p, d, e), hp) = let
		    val trueLab = newLabel ()
		    val r = fregbind v
		    val r' = newReg INT
		    val rReg = M.REG(ity, r')
		  (* address of the word that contains the sign bit *)
		    val addr = if MachineSpec.bigEndian
			  then M.ADD(addrTy, Regs.allocptr, LI' hp)
			  else M.ADD(pty, rReg, LI'((fty - pty) div 8))
		    in
		      emit(M.MV(ity, r', M.ADD(addrTy, Regs.allocptr, LI' hp)));
		      emit(M.FSTORE(fty,rReg,r,R.memory));
		      emit(M.BCC(M.CMP(ity, M.LT, M.LOAD(ity, addr, R.memory), zero), trueLab));
		      genCont(e, hp);
		      genlab(trueLab, d, hp)
		    end
		| gen (C.BRANCH(P.BOXED, [x], p, a, b), hp) = branchOnBoxed(p, x, a, b, hp)
		| gen (C.BRANCH(P.UNBOXED, [x], p, a, b), hp) = branchOnBoxed(p, x, b, a, hp)
		| gen (C.BRANCH(P.PEQL, vw, p, e, d), hp) = branch(p, M.EQ, vw, e, d, hp)
		| gen (C.BRANCH(P.PNEQ, vw, p, e, d), hp) = branch(p, M.NE, vw, e, d, hp)
		| gen (C.BRANCH(P.STRNEQ, [NUM{ty={tag=true, ...}, ival},v,w], p, d, e), hp) =
		    branchStreq(ival, v, w, e, d, hp)
		| gen (C.BRANCH(P.STREQL, [NUM{ty={tag=true, ...}, ival},v,w],p,d,e), hp) =
		    branchStreq(ival, v, w, d, e, hp)
		| gen (e, hp) = (PPCps.prcps e; print "\n"; error "genCluster.gen")

	      fun fragComp() = let
	            fun continue() = fcomp (Frag.next())
		    and fcomp (NONE) = ()
		      | fcomp (SOME(_, Frag.KNOWNFUN _)) = continue()
		      | fcomp (SOME(_, Frag.KNOWNCHK _)) = continue()
		      | fcomp (SOME(_, Frag.STANDARD{func=ref NONE, ...})) = continue()
		      | fcomp (SOME(lab,
			  Frag.STANDARD{func as ref(SOME (zz as (k,f,vl,tl,e))),
			...})) = let
		          val formals = ArgP.standard{fnTy=typmap f, argTys=tl, vfp=vfp}
			  in
			    func := NONE;
			    pseudoOp(PB.ALIGN_SZ 2);
			    genCPSFunction(lab, k, f, vl, formals, tl, e);
			    continue()
			  end
	      in
		fcomp (Frag.next())
	      end (* fragComp *)

	    (*
	     * execution starts at the first CPS function -- the frag
	     * is maintained as a queue.
	     *)
	      fun initFrags (start::rest : C.function list) = let
		    fun init(func as (fk, f, _, _, _)) =
			  addGenTbl (f, Frag.makeFrag(func, functionLabel f))
		    in
		      app init rest;
		      init start
		    end
		| initFrags [] = error "initFrags"

	    (*
	     * Create cluster annotations.
	     * Currently, we only need to enter the appropriate
	     * gc map information.
	     *)
	      fun clusterAnnotations() = let
		    val cellinfo = if gctypes
			  then let
			    fun enter(M.REG(_,r),ty) = enterGC(r, ty)
			      | enter _ = ()
			    in
			      enterGC(allocptrR, SMLGCType.ALLOCPTR);
			      enter(Regs.limitptr vfp, SMLGCType.LIMITPTR);
			      enter(Regs.baseptr vfp, PTR);
			      enter(Regs.stdlink vfp, PTR);
			      [#create An.PRINT_CELLINFO(GCCells.printType)]
			    end
			  else []
		    in
		      if vfp then #set An.USES_VIRTUAL_FRAME_POINTER ((), cellinfo)
		      else cellinfo
		    end
	      in
		initFrags cluster;
		beginCluster 0;
		pseudoOp PB.TEXT;
		fragComp();
		InvokeGC.emitLongJumpsToGCInvocation stream;
		compile(endCluster(clusterAnnotations()))
	      end (* genCluster *)

	fun finishCompilationUnit file = let
	      val stream = MLTreeComp.selectInstructions (Flowgen.build ())
	      val TS.S.STREAM{beginCluster, pseudoOp, endCluster, ...} = stream
	      in
		Cells.reset();
		ClusterAnnotation.useVfp := false;
		beginCluster 0;
		pseudoOp PB.TEXT;
		InvokeGC.emitModuleGC stream;
		pseudoOp (PB.DATA_READ_ONLY);
		pseudoOp (PB.EXT(CPs.FILENAME file));
		compile (endCluster NO_OPT)
	      end

	fun entrypoint ((_,f,_,_,_)::_) () = Label.addrOf (functionLabel f)
	  | entrypoint [] () = error "entrypoint: no functions"
	in
	  app mkGlobalTables funcs;
	  app genCluster (Cluster.cluster funcs);
	  finishCompilationUnit source;
	  entrypoint funcs
	end (* codegen *)

  end (* MLRiscGen *)
