(* invokegc.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module is responsible for generating code to invoke the
 * garbage collector.  This new version is derived from the functor CallGC.
 * It can handle derived pointers as roots and it can also be used as
 * callbacks.  These extra facilities are neccessary for global optimizations
 * in the presence of GC.
 *
 * -- Allen
 *)

functor InvokeGC (
    structure MS    : MACH_SPEC
    structure C     : CPSREGS
		        where T.Region=CPSRegions
    structure TS    : MLTREE_STREAM
		        where T = C.T
    structure CFG   : CONTROL_FLOW_GRAPH
		        where P = TS.S.P
  ) : INVOKE_GC = struct

    structure CB = CellsBasis
    structure S  = CB.SortedCells
    structure T  = C.T
    structure D  = MS.ObjDesc
    structure R  = CPSRegions
    structure SL = SortedList
    structure Cells = C.C
    structure CFG = CFG
    structure TS = TS

    val ity = MS.wordBitWidth
    val wordSz = MS.wordByteWidth
    val (logWordSz, align) = (case wordSz of 4 => (2, true) | 8 => (3, false))

    fun error msg = ErrorMsg.impossible("InvokeGC."^msg)

    type t = {
	maxAlloc : int,
	regfmls  : T.mlrisc list,
	regtys   : CPS.cty list,
	return   : T.stm
      }

    type stream = (T.stm, T.mlrisc list, CFG.cfg) TS.stream

    val debug = Control.MLRISC.mkFlag ("debug-gc", "GC invocation debug mode")

    val addrTy = MS.addressBitWidth

    val ZERO_FREQ       = #create MLRiscAnnotations.EXECUTION_FREQ 0
    val CALLGC          = #create MLRiscAnnotations.CALLGC ()
    val NO_OPTIMIZATION = #create MLRiscAnnotations.NO_OPTIMIZATION ()

   (* The following datatype is used to encapsulates
    * all the information needed to generate code to invoke gc.
    * The important fields are:
    *    known     -- is the function a known (i.e. internal) function
    *    optimized -- if this is on, gc code generation is delayed until
    *                 we have performed all optimizations.  This is false
    *                 for normal SML/NJ use.
    *    lab       -- a list of labels that belongs to the call gc block
    *    boxed, float, int -- roots partitioned by types
    *    regfmls   -- the roots
    *    ret       -- how to return from the call gc block.
    *)
    datatype gcInfo
      = GCINFO of {
	    known     : bool,            (* known function ? *)
	    optimized : bool,            (* optimized? *)
	    lab       : Label.label ref, (* labels to invoke GC *)
	    boxed     : T.rexp list,     (* locations with boxed values *)
	    int       : T.rexp list,     (* locations with untagged int values *)
	    float     : T.fexp list,     (* locations with float values *)
	    regfmls   : T.mlrisc list,   (* all live registers *)
	    ret       : T.stm            (* how to return *)
	  }
      | MODULE of {
            info : gcInfo,
            addrs : Label.label list ref (* addrs associated with long jump *)
	  }

   (*====================================================================
    * Implementation/architecture specific stuff starts here.
    *====================================================================*)

   (* Extra space in allocation space
    * The SML/NJ runtime system leaves around 1K words of extra space
    * in the allocation space for safety.
    *)
     val skidPad = MS.valueSize * 1024
     val pty = MS.wordBitWidth

     val vfp = false			(* don't use virtual frame ptr here *)

     val unit = T.LI 1		      (* representation of ML's unit;
                                       * this is used to initialize registers.
                                       *)
     fun LI i = T.LI (T.I.fromInt(pty, i))

   (*
    * Callee-save registers
    * All callee save registers are used in the gc calling convention.
    *)
     val calleesaves = List.take(C.miscregs, MS.numCalleeSaves)

   (*
    * registers that are the roots of gc.
    *)
     val gcParamRegs = C.stdlink(vfp)::C.stdclos(vfp)::C.stdcont(vfp)::C.stdarg(vfp)
	   ::calleesaves

   (*
    * How to call the call the GC
    *)
     val gcCall = let
	   val use = map T.GPR gcParamRegs
	   val def = case C.exhausted of NONE => use | SOME cc => T.CCR cc::use
	   val call = T.CALL{
		   funct=T.LOAD(pty,
			    T.ADD(addrTy,C.frameptr vfp, LI MS.startgcOffset),
			    R.stack),
		   targets=[], defs=def, uses=use, region=R.stack,
		   pops=0
		}

	 (* mark it with a CALLGC annotation *)
	   val call = T.ANNOTATION(call, CALLGC)
	   in
	     T.ANNOTATION(call, #create MLRiscAnnotations.COMMENT "call gc")
	   end

   (*
    * record descriptors
    *)
     fun unboxedDesc words = D.makeDesc'(words, D.tag_raw64)
     fun boxedDesc words   = D.makeDesc'(words, D.tag_record)

  (* the allocation pointer must always in a register! *)
    val allocptrR = (case C.allocptr
	   of T.REG(_, allocptrR) => allocptrR
	    | _ => error "allocptr must be a register"
	  (* end case *))

  (* what type of comparison to use for GC test? *)
    val gcCmp = if C.signedGCTest then T.GT else T.GTU

    val unlikely = #create MLRiscAnnotations.BRANCH_PROB Probability.unlikely

    val normalTestLimit = T.CMP(pty, gcCmp, C.allocptr, C.limitptr vfp)

  (*====================================================================
   * Private state
   *====================================================================*)
  (* gc info required for standard functions within the cluster *)
    val clusterGcBlocks = ref([]: gcInfo list)

  (* gc info required for known functions within the cluster *)
    val knownGcBlocks = ref([]: gcInfo list)

  (* gc info required for modules *)
    val moduleGcBlocks = ref ([]: gcInfo list)

  (*====================================================================
   * Auxiliary functions
   *====================================================================*)

  (*
   * Convert a list of rexps into a set of registers and memory offsets.
   * Memory offsets must be relative to the frame pointer.
   *)
    fun set bindings = let
	  val theVfp = C.vfp
	  val theFp = (case C.frameptr false
		 of T.REG (_, theFp) => theFp
		  | _ => error "theFp"
		(* end case *))
	(* At this point, theVfp will always eventually end up
	 * being theFp, but mlriscGen might pass in references to theVfp
	 * anyway (because of some RCC that happens to be in the cluster).
	 * Therefor, we test for either the real frame pointer (theFp) or
	 * the virtual frame pointer (theVfp) here. *)
	  fun isFramePtr fp = CB.sameColor (fp, theFp) orelse
			      CB.sameColor (fp, theVfp)
	  fun live (T.REG(_,r)::es, regs, mem) = live(es, r::regs, mem)
	    | live (T.LOAD(_, T.REG(_, fp), _)::es, regs, mem) = if isFramePtr fp
		  then live(es, regs, 0::mem)
	          else error "set:LOAD"
	    | live (T.LOAD(_, T.ADD(_, T.REG(_, fp), T.LI i), _)::es, regs, mem) = if isFramePtr fp
	          then live(es, regs, T.I.toInt(pty,i)::mem)
	          else error "set:LOAD"
	    | live ([], regs, mem) = (regs, mem)
	    | live _ = error "live"
	  val (regs, mem) = live(bindings, [], [])
	  in
	    {regs=S.return(S.uniq regs), mem=SL.uniq mem}
	  end

    fun difference ({regs=r1, mem=m1}, {regs=r2, mem=m2}) =
          {regs=S.difference(r1,r2), mem=SL.difference(m1,m2)}

    fun setToString {regs, mem} = concat[
            "{", String.concatWithMap " " CB.toString regs, " ",
	    String.concatWithMap " " Int.toString mem, "}"
	  ]

  (* The client communicates root pointers to the gc via the following set
   * of registers and memory locations.
   *)
    val gcrootSet = set gcParamRegs
    val aRoot     = hd(#regs gcrootSet)
    val aRootReg  = T.REG(pty,aRoot)

  (*
   * This function generates a gc limit check.
   * It returns the label to the GC invocation block.
   *)
    fun checkLimit (emit, maxAlloc) = let
	  val lab = Label.anon()
	  fun gotoGC(cc) = emit(T.ANNOTATION(T.BCC(cc, lab), unlikely))
	  in
	    if maxAlloc < skidPad
	      then (case C.exhausted
		 of SOME cc => gotoGC cc
		  | NONE => gotoGC normalTestLimit
		(* end case *))
	      else let
		val shiftedAllocPtr = T.ADD(addrTy,C.allocptr,LI(maxAlloc-skidPad))
		val shiftedTestLimit = T.CMP(pty, gcCmp, shiftedAllocPtr, C.limitptr vfp)
		in
		  case C.exhausted
		   of SOME(cc as T.CC(_,r)) => (emit(T.CCMV(r, shiftedTestLimit)); gotoGC(cc))
		    | NONE => gotoGC(shiftedTestLimit)
		    | _ => error "checkLimit"
		  (* end case *)
		end;
	    lab
	  end

    val baseOffset = T.LI(IntInf.fromInt MS.constBaseRegOffset)

  (*
   * This function recomputes the base pointer address.
   *)
    fun computeBasePtr(emit,defineLabel,annotation) = let
	  val returnLab = Label.anon()
	  val baseExp =
	        T.ADD(addrTy, C.gcLink vfp, T.LABEXP(T.SUB(addrTy, baseOffset, T.LABEL returnLab)))
	  in
	    defineLabel returnLab;
	    annotation(ZERO_FREQ);
	    emit (case C.baseptr vfp
	       of T.REG(ty, bpt) => T.MV(ty, bpt, baseExp)
		| T.LOAD(ty, ea, mem) => T.STORE(ty, ea, baseExp, mem)
		| _ => error "computeBasePtr")
	  end

  (*====================================================================
   * Main functions
   *====================================================================*)
    fun init () = (
	  clusterGcBlocks        := [];
	  knownGcBlocks          := [];
	  moduleGcBlocks         := [])

  (*
   * Partition the root set into types
   *)
    fun split ([], [], boxed, int, float) = {boxed=boxed, int=int, float=float}
      | split (T.GPR r::rl, CPS.NUMt{tag=false, ...}::tl, b, i, f) = split(rl, tl, b, r::i, f)
      | split (T.GPR r::rl, CPS.FLTt _::tl, b, i, f) = error "split: T.GPR"
      | split (T.GPR r::rl, _::tl, b, i, f) = split(rl,tl,r::b,i,f)
      | split (T.FPR r::rl, CPS.FLTt _::tl, b, i, f) = split(rl,tl,b,i,r::f)
      | split _ = error "split"

    fun genGcInfo
	(clusterRef, known, optimized)
	(TS.S.STREAM{emit,...} : stream)
	{maxAlloc, regfmls, regtys, return} = let
	(* partition the root set into the appropriate classes *)
          val {boxed, int, float} = split(regfmls, regtys, [], [], [])
	  in
	    clusterRef := GCINFO{
		known     = known,
		optimized = optimized,
		lab       = ref (checkLimit(emit,maxAlloc)),
		boxed     = boxed,
		int       = int,
		float     = float,
		regfmls   = regfmls,
		ret       = return
	      } :: (!clusterRef)
	  end

  (*
   * Check-limit for standard functions, i.e.~functions with
   * external entries.
   *)
    val stdCheckLimit = genGcInfo (clusterGcBlocks, false, false)

  (*
   * Check-limit for known functions, i.e.~functions with entries from
   * within the same cluster.
   *)
    val knwCheckLimit = genGcInfo (knownGcBlocks, true, false)

  (*
   * Check-limit for optimized, known functions.
   *)
    val optimizedKnwCheckLimit = genGcInfo (knownGcBlocks, true, true)

  (*
   * An array for checking cycles
   *)
    local
      val N = 1 + foldr (fn (r,n) => Int.max(CB.registerNum r, n)) 0 (#regs gcrootSet)
    in
    val clientRoots = Array.array(N, ~1)
    val stamp       = ref 0
    end (* local *)

  (*
   * Datatype binding describes the contents a gc root.
   *)
    datatype binding
      = Reg of CB.cell			(* integer register *)
      | Freg of CB.cell			(* floating point register*)
      | Mem of T.rexp * R.region	(* integer memory register *)
      | Record of {
	    boxed : bool,		(* is it a boxed record *)
	    words : int,		(* how many words *)
	    reg : CB.cell,		(* address of this record *)
	    regTmp : CB.cell,		(* temp used for unpacking *)
	    fields : binding list	(* its fields *)
	  }

  (*
   * This function packs boxed, int and float into gcroots.
   * gcroots must be non-empty.  Return a function to unpack.
   *)
    fun pack (emit, gcroots, boxed, int, float) = let
	(*
	 * Translates rexp/fexp into bindings.
	 * Note: client roots from memory (XXX) should NOT be used without
	 * fixing a potential cycle problem in the parallel copies below.
	 * Currently, all architectures, including the x86, do not uses
	 * the LOAD(...) form.  So we are safe.
	 *)
	  fun bind (T.REG(32, r)) = Reg r
	    | bind (T.REG(64, r)) = Reg r
(* 64BIT: FIXME *)
	    | bind (T.LOAD(32, ea, mem)) = Mem(ea, mem)  (* XXX *)
	    | bind _ = error "bind"
(* REAL32: FIXME *)
	  fun fbind (T.FREG(64, r)) = Freg r
	    | fbind _ = error "fbind"

	  val st     = !stamp
	  val cyclic = st + 1
	  val _      = if st > 100000 then stamp := 0 else stamp := st + 2
	  val N = Array.length clientRoots
	  fun markClients [] = ()
	    | markClients (T.REG(_, r)::rs) = let
		val rx = CB.registerNum r
		in
		  if rx < N then Array.update(clientRoots, rx, st) else ();
		  markClients rs
		end
	    | markClients(_::rs) = markClients rs
	  fun markGCRoots [] = ()
	    | markGCRoots (T.REG(_, r)::rs) = let
		val rx = CB.registerNum r
		in
		  if Array.sub(clientRoots, rx) = st
		    then Array.update(clientRoots, rx, cyclic)
		    else ();
		  markGCRoots rs
	        end
	    | markGCRoots(_::rs) = markGCRoots rs

	  val _ = markClients boxed
	  val _ = markClients int
	  val _ = markGCRoots gcroots
	(*
	 * First, we pack all unboxed roots, if any, into a record.
	 *)
          val boxedStuff = (case (int, float)
		 of ([], []) => map bind boxed
                  | _ => let
		    (* number of 8-byte words; note `align` is false on 64-bit targets *)
		      val qwords = if align
			    then length float + (length int + 1) div 2
			    else length float + length int
		      in
		      (* align the allocptr if we have floating point roots *)
			case float
			 of [] => ()
			  | _  => if align
			      then emit(T.MV(addrTy, allocptrR, T.ORB(addrTy, C.allocptr, LI 4)))
			      else ()
			(* end case *);
		      (* If we have int or floating point stuff, package them
		       * up into a raw record.  Floating point stuff have to come first.
		       *)
                        Record{
			    boxed = false, reg = Cells.newReg(),
                            regTmp = Cells.newReg(),
                            words = if align then qwords + qwords else qwords,
                            fields = map fbind float @ map bind int
			  } :: map bind boxed
		      end
		(* end case *))
	(*
	 * Then, we check whether we have enough gc roots to store boxedStuff.
	 * If so, we are safe. Otherwise, we have to pack up some of the
	 * boxed stuff into a record too.
	 *)
	  val nBoxedStuff = length boxedStuff
	  val nGcRoots    = length gcroots

	  val bindings = if nBoxedStuff <= nGcRoots
		then boxedStuff (* good enough *)
		else let (* package up some of the boxed stuff *)
		  val extra = nBoxedStuff - nGcRoots + 1
		  val (packUp, don'tPackUp) = List.splitAt (boxedStuff, extra)
		  in
		    Record{
			boxed=true, words=length packUp,
			regTmp=Cells.newReg(),
			reg=Cells.newReg(), fields=packUp
		      } :: don'tPackUp
		  end

	  fun copy ([], _) = ()
	    | copy (dst, src) = emit(T.COPY(ity, dst, src))

	(*
	 * The following routine copies the client roots into the real gc roots.
	 * We have to make sure that cycles have correctly handled.  So we
	 * can't do a copy at a time!  But see XXX below.
	 *)
	  fun prolog (hp, unusedRoots, [], rds, rss) = let
		fun init [] = ()
		  | init(T.REG(ty, rd)::roots) = (emit(T.MV(ty, rd, unit)); init roots)
		  | init(T.LOAD(ty, ea, mem)::roots) = (emit(T.STORE(ty, ea, unit, mem)); init roots)
		  | init _ = error "init"
		in
		(* update the heap pointer if we have done any allocation *)
		  if hp > 0
		    then emit(T.MV(addrTy, allocptrR, T.ADD(addrTy, C.allocptr, LI hp)))
		    else ();
		(* emit the parallel copies *)
		  copy (rds, rss);
		  (*
		   * Any unused gc roots have to be initialized with unit.
		   * The following MUST come last.
		   *)
		  init unusedRoots
		end
	    | prolog(hp, T.REG(_,rd)::roots, Reg rs::bs, rds, rss) =
	      (* copy client root rs into gc root rd  *)
		prolog(hp, roots, bs, rd::rds, rs::rss)
	    | prolog(hp, T.REG(_,rd)::roots, Record(r as {reg,...})::bs,rds,rss) = let
	      (* make a record then copy *)
		val hp = makeRecord(hp, r)
		in
		  prolog(hp, roots, bs, rd::rds, reg::rss)
		end
	    | prolog _ = error "prolog"

	(* Make a record and put it in reg *)
	  and makeRecord (hp, {boxed, words, reg, fields, ...}) = let
		fun disp n = T.ADD(addrTy, C.allocptr, LI n)
		fun alloci (hp, e) = emit(T.STORE(ity, disp hp, e, R.memory))
		fun allocf (hp, e) = emit(T.FSTORE(64, disp hp, e, R.memory))
		fun alloc (hp, []) = ()
		  | alloc (hp, b::bs) = (case b
		       of Reg r => (alloci(hp, T.REG(ity,r)); alloc(hp+wordSz, bs))
			| Record{reg, ...} => (alloci(hp, T.REG(ity,reg)); alloc(hp+wordSz, bs))
			| Mem(ea, m) => (alloci(hp, T.LOAD(ity,ea,m)); alloc(hp+wordSz,bs))
			| Freg r => (allocf(hp, T.FREG(64,r)); alloc(hp+8, bs))
		      (* end case *))
		fun evalArgs ([], hp) = hp
		  | evalArgs (Record r::args, hp) = evalArgs(args, makeRecord(hp, r))
		  | evalArgs (_::args, hp) = evalArgs(args, hp)
		(* MUST evaluate nested records first *)
		val hp   = evalArgs(fields, hp)
		val desc = if boxed then boxedDesc words else unboxedDesc words
		in
		  emit (T.STORE(ity, disp hp, T.LI desc, R.memory));
		  alloc (hp+wordSz, fields);
		  emit (T.MV(addrTy, reg, disp(hp+wordSz)));
		  hp + wordSz + Word.toIntX(Word.<<(Word.fromInt words,Word.fromInt logWordSz))
		end

	(* Copy the gc roots back to client roots.
	 * Again, to avoid potential cycles, we generate a single
	 * parallel copy that moves the gc roots back to the client roots.
	 *)
	  fun epilog ([], unusedGcRoots, rds, rss) = copy(rds, rss)
	    | epilog (Reg rd::bs, T.REG(_,rs)::roots, rds, rss) =
		epilog (bs, roots, rd::rds, rs::rss)
	    | epilog (Record{fields,regTmp,...}::bs, T.REG(_,r)::roots, rds, rss) = let
	      (* unbundle record *)
		val _   = emit(T.COPY(ity, [regTmp], [r]))
		val (rds, rss) = unpack(regTmp, fields, rds, rss)
		in
		  epilog (bs, roots, rds, rss)
		end
	    | epilog (b::bs, r::roots, rds, rss) = (
		assign (b, r); (* XXX *)
		epilog (bs, roots, rds, rss))
	    | epilog _ = error "epilog"

	  and assign (Reg r, e)        = emit(T.MV(ity, r, e))
	    | assign (Mem(ea, mem), e) = emit(T.STORE(ity, ea, e, mem))
	    | assign _ = error "assign"

	(* unpack fields from record *)
          and unpack (recordR, fields, rds, rss) = let
		val record = T.REG(ity, recordR)
		fun disp n = T.ADD(addrTy, record, LI n)
		fun sel n = T.LOAD(ity, disp n, R.memory)
		fun fsel n = T.FLOAD(64, disp n, R.memory)
		val N = Array.length clientRoots
	      (* unpack normal fields *)
		fun unpackFields(n, [], rds, rss) = (rds, rss)
		  | unpackFields(n, Freg r::bs, rds, rss) = (
		      emit(T.FMV(64, r, fsel n));
		      unpackFields(n+8, bs, rds, rss))
		  | unpackFields(n, Mem(ea, mem)::bs, rds, rss) = (
		      emit(T.STORE(ity, ea, sel n, mem));  (* XXX *)
		      unpackFields(n+wordSz, bs, rds, rss))
		  | unpackFields(n, Record{regTmp, ...}::bs, rds, rss) = (
		      emit(T.MV(ity, regTmp, sel n));
		      unpackFields(n+wordSz, bs, rds, rss))
		  | unpackFields(n, Reg rd::bs, rds, rss) = let
		      val rdx = CB.registerNum rd
		      in
			if rdx < N andalso Array.sub(clientRoots, rdx) = cyclic
			  then let
			    val tmpR = Cells.newReg()
			    in
			      (* print "WARNING: CYCLE\n"; *)
			      emit(T.MV(ity, tmpR, sel n));
			      unpackFields(n+wordSz, bs, rd::rds, tmpR::rss)
			    end
			  else (
			    emit(T.MV(ity, rd, sel n));
			    unpackFields(n+wordSz, bs, rds, rss))
		      end

	      (* unpack nested record *)
		fun unpackNested(_, [], rds, rss) = (rds, rss)
		  | unpackNested(n, Record{fields, regTmp, ...}::bs, rds, rss) = let
		      val (rds, rss) = unpack(regTmp, fields, rds, rss)
		      in
			unpackNested(n+wordSz, bs, rds, rss)
		      end
		  | unpackNested(n, Freg _::bs, rds, rss) =
		      unpackNested(n+8, bs, rds, rss)
		  | unpackNested(n, _::bs, rds, rss) =
		      unpackNested(n+wordSz, bs, rds, rss)

		val (rds, rss)= unpackFields(0, fields, rds, rss)
		in
		  unpackNested(0, fields, rds, rss)
		end (* unpack *)

	(* generate code *)
          in
	    prolog (0, gcroots, bindings, [], []);
	  (* return the unpack function *)
	    fn () => epilog(bindings, gcroots, [], [])
	  end

  (*
   * The following auxiliary function generates the actual call gc code.
   * It packages up the roots into the appropriate
   * records, call the GC routine, then unpack the roots from the record.
   *)
    fun emitCallGC {
	    stream=TS.S.STREAM{emit, annotation, defineLabel, ...},
	    known, boxed, int, float, ret
	} = let
	  fun setToMLTree {regs, mem} =
		map (fn r => T.REG(ity,r)) regs @
		map (fn i => T.LOAD(ity, T.ADD(addrTy, C.frameptr vfp, LI(i)), R.memory)) mem

	(* IMPORTANT NOTE:
	 * If a boxed root happens be in a gc root register, we can remove
	 * this root since it will be correctly targeted.
	 *
	 * boxedRoots are the boxed roots that we have to move to the
	 * appropriate registers.  gcrootSet are the registers that are
	 * available for communicating to the collector.
	 *)

	  val boxedSet   = set boxed
	  val boxedRoots = difference(boxedSet,gcrootSet)  (* roots *)
	  val gcrootAvail = difference(gcrootSet,boxedSet) (* gcroots available *)

	  fun mark call = if !debug
		then T.ANNOTATION(call, #create MLRiscAnnotations.COMMENT (concat[
                    "roots=", setToString gcrootAvail, " boxed=", setToString boxedRoots
		  ]))
                else call

	(* convert them back to MLTREE *)
	  val boxed   = setToMLTree boxedRoots
	  val gcroots = setToMLTree gcrootAvail

	(* If we have any remaining roots after the above trick, we have to
	 * make sure that gcroots is not empty.
	 *)
	  val (gcroots, boxed) = (case (gcroots, int, float, boxed)
		 of ([], [], [], []) => ([], []) (* it's okay *)
		  | ([], _, _, _) => ([aRootReg], boxed @ [aRootReg])
		  (* put aRootReg last to reduce register pressure
		   * during unpacking
		   *)
		  | _  => (gcroots, boxed)
		(* end case *))
	  val unpack = pack(emit, gcroots, boxed, int, float)
	  in
	    annotation CALLGC;
	    annotation NO_OPTIMIZATION;
	    annotation ZERO_FREQ;
	    emit (mark gcCall);
	    if known then computeBasePtr(emit,defineLabel,annotation) else ();
	    annotation NO_OPTIMIZATION;
	    unpack();
	    emit ret
	  end

   (*
    * The following function is responsible for generating only the
    * callGC code.
    *)
     fun callGC stream {regfmls, regtys, ret} = let
           val {boxed, int, float} = split(regfmls, regtys, [], [], [])
           in
	      emitCallGC{stream=stream, known=true, boxed=boxed, int=int, float=float, ret=ret}
	   end

  (*
   * This function emits a comment that pretty prints the root set.
   * This is used for debugging only.
   *)
    fun rootSetToString{boxed, int, float} = let
	fun extract(T.REG(ity, r)) = r
	  | extract _ = error "extract"
	fun fextract(T.FREG(64, f)) = f
	  | fextract _ = error "fextract"
	fun listify title f [] = ""
	  | listify title f l  =
	      title^foldr (fn (x,"") => f x
			    | (x,y)  => f x ^", "^y) "" (S.uniq l)^" "
	in
	  listify "boxed=" CB.toString (map extract boxed)^
	  listify "int=" CB.toString (map extract int)^
	  listify "float=" CB.toString (map fextract float)
	end

   (*
    * The following function is responsible for generating actual
    * GC calling code, with entry labels and return information.
    *)
    fun invokeGC (stream, externalEntry) gcInfo = let
	  val TS.S.STREAM{emit,defineLabel,entryLabel,exitBlock,annotation,...} = stream
	  val {known, optimized, boxed, int, float, regfmls, ret, lab} = (case gcInfo
		 of GCINFO info => info
		  | MODULE{info=GCINFO info,...} => info
		  | _ => error "invokeGC:gcInfo"
		(* end case *))
          val liveout = if optimized then [] else regfmls
	  in
	    if externalEntry then entryLabel (!lab) else defineLabel (!lab);
	  (* When the known block is optimized, no actual code is generated
	   * until later.
	   *)
	    if optimized
	      then (
		annotation (#create MLRiscAnnotations.GCSAFEPOINT
		  (if !debug then rootSetToString{boxed=boxed, int=int, float=float} else ""));
		emit ret)
	      else emitCallGC {
		  stream=stream, known=known, boxed=boxed, int=int, float=float, ret=ret
		};
	    exitBlock (case C.exhausted of NONE    => liveout | SOME cc => T.CCR cc::liveout)
	  end

   (*
    * The following function checks whether two root sets have the
    * same calling convention.
    *)
    fun sameCallingConvention (
	  GCINFO{boxed=b1, int=i1, float=f1, ret=T.JMP(ret1, _),...},
	  GCINFO{boxed=b2, int=i2, float=f2, ret=T.JMP(ret2, _),...}
	) = let
	  fun eqEA (T.REG(_, r1), T.REG(_, r2)) = CB.sameColor(r1,r2)
	    | eqEA (T.ADD(sz1,T.REG(_,r1),T.LI i), T.ADD(sz2,T.REG(_,r2),T.LI j)) =
		(sz1 = sz2) andalso T.I.EQ(sz1,i,j) andalso CB.sameColor(r1,r2)
	    | eqEA _ = false
	  fun eqR (T.REG(_,r1), T.REG(_,r2)) = CB.sameColor(r1,r2)
	    | eqR (T.LOAD(_,ea1,_), T.LOAD(_,ea2,_)) = eqEA(ea1, ea2)
	    | eqR _ = false
	  fun eqF (T.FREG(_,f1), T.FREG(_,f2)) = CB.sameColor(f1,f2)
	    | eqF (T.FLOAD(_,ea1,_), T.FLOAD(_,ea2,_)) = eqEA(ea1, ea2)
	    | eqF _ = false
	  in
	    ListPair.all eqR (b1, b2)
	      andalso eqR(ret1, ret2)
	      andalso ListPair.all eqR (i1, i2)
	      andalso ListPair.all eqF (f1, f2)
	  end
      | sameCallingConvention _ = false

  (*
   * The following function is called once at the end of compiling a cluster.
   * Generates long jumps to the end of the module unit for
   * standard functions, and directly invokes GC for known functions.
   * The actual GC invocation code is not generated yet.
   *)
    fun emitLongJumpsToGCInvocation (stream as TS.S.STREAM{emit,defineLabel,exitBlock,...}) = let
	(* GC code can be shared if the calling convention is the same
	 * Use linear search to find the gc subroutine.
	 *)
	  fun find(info as GCINFO{lab as ref l, ...}) = let
	        fun search(MODULE{info=info', addrs}::rest) = if sameCallingConvention(info, info')
		      then addrs := l :: (!addrs)
		      else search rest
		  | search [] = let (* no matching convention *)
		      val label = Label.anon()
		      in
			lab := label;
                        moduleGcBlocks := MODULE{info=info, addrs=ref[l]} :: (!moduleGcBlocks)
		      end
		  | search _ = error "search"
		in
		  search (!moduleGcBlocks)
		end
	    | find _ = error "find"
	(*
	 * Generate a long jump to all external callgc routines
	 *)
	  fun longJumps (MODULE{addrs=ref [],...}) = ()
	    | longJumps (MODULE{info=GCINFO{lab,boxed,int,float,...}, addrs}) = let
		val regRoots  = map T.GPR (int @ boxed)
		val fregRoots = map T.FPR float
		val liveOut   = regRoots @ fregRoots
		val l         = !lab
		in
		  app defineLabel (!addrs) before addrs := [];
		  emit(T.JMP(T.LABEL l, []));
		  exitBlock liveOut
		end
	    | longJumps _ = error "longJumps"

	  in
	    app find (!clusterGcBlocks)
	      before clusterGcBlocks := [];
	    app longJumps (!moduleGcBlocks);
	    app (invokeGC(stream, false)) (!knownGcBlocks)
	      before knownGcBlocks := []
	  end (* emitLongJumpsToGC *)

  (*
   * The following function is called to generate module specific
   * GC invocation code
   *)
    fun emitModuleGC stream =
	  app (invokeGC(stream,true)) (!moduleGcBlocks)
	    before moduleGcBlocks := []

  end
