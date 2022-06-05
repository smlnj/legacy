(* memAliasing.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Perform memory aliasing analysis.
 *
 * The old memory disambiguation module discards aliasing information
 * across CPS function boundaries, which made it not very useful for the
 * optimizations I have in mind.
 *
 * This is an alternative module that (hopefully) does the right thing.
 * The algorithm is inspired by Steensgaard's work on flow insensitive
 * points-to analysis, but has been hacked to deal with target level issues.
 *
 * Some target level issues
 * ------------------------
 * In the source level two CPS allocations cannot be aliased by definition.
 * When allocations are translated into target code, however, they become
 * stores to fixed offsets from the heap pointer.  Two allocation stores
 * that may write to the same offset are aliased.  Allocation stores that are
 * in disjoint program paths may be assigned the same heap allocation offset.
 * We have to mark these as aliased since we want to allow speculative writes
 * to the allocation space.
 *
 * Representing heap offsets
 * -------------------------
 *
 *
 * Language
 * --------
 * e ::= x <- v.i; k           /* select */
 *    |  x <- v+i; k           /* offset */
 *    |  x <- [v1,...vn]^hp; k /* record allocation at heap pointer hp */
 *    |  x <- !v; k            /* dereference */
 *    |  v1 := v2; k           /* update */
 *    |  f(v1,...,vn)          /* tail call */
 *
 * Since the analysis is flow insensitive, the branch constructs are
 * irrelevant.
 *
 * -- Allen
 *)

signature MEM_ALIASING = sig

    val analyze : CPS.function list -> (CPS.lvar -> CPSRegions.region)

  end

functor MemAliasing (Cells : CELLS) : MEM_ALIASING =
  struct
    structure C  = CPS
    structure P  = CPS.P
    structure PT = PointsTo

    fun error msg = MLRiscErrorMsg.error("MemAliasing",msg)

    val cellSz = Cells.cellSize
    val cellShift = (case cellSz of 4 => 0w2 | 8 => 0w3)

  (*
   * The following functions advance the heap pointer.
   * These functions are highly dependent on the runtime system and
   * how data structures are represented.
   * IMPORTANT: we are assuming that the new array representation is used.
   *)
    fun recordSize (n, hp) = (n + 1) * cellSz + hp

 (* a record to hold n 64-bit floats; it needs to be 8-byte aligned on 32-bit systems. *)
(* REAL32: FIXME *)
    val frecordSize = if (cellSz = 8)
	  then fn (n, hp) => hp + 8 * (n + 1)
	  else fn (n, hp) => (
	    if Word.andb(Word.fromInt hp, 0w4) <> 0w0
	      then hp + 8 * (n + 1)
	      else hp + 4 + 8 * n)

  (* vector is 3-word header object + data header + data *)
    fun vectorSize (n, hp) = hp + cellSz * (n + 4)

  (* storage needed to wrap an integer or float *)
    fun wrapSize (P.FLOAT 64, hp) = frecordSize(1, hp)
(* REAL32: FIXME *)
      | wrapSize (P.INT sz, hp) =
	  if (sz <= Target.defaultIntSz)
	    then hp
(* NOTE: we might want padding for 64-bit ints on 32-bit machines in the future *)
	    else hp + cellSz * (1 + sz div Target.mlValueSz)
      | wrapSize _ = error "wrapSize: bogus number kind"

    fun allocRecord (C.RK_RAW64BLOCK, vs, hp) = frecordSize(length vs, hp)
      | allocRecord (C.RK_FCONT, vs, hp)  = frecordSize(length vs, hp)
      | allocRecord (C.RK_VECTOR, vs, hp) = vectorSize(length vs, hp)
      | allocRecord (_, vs, hp) = recordSize(length vs, hp)

    val storeListSize = 2 * cellSz	(* store-list elements *)
    val array0Size    = 5 * cellSz	(* zero-length array *)

    exception NotFound

    val top = CPSRegions.memory

  (*
   * Analyze a set of CPS functions
   *)
    fun analyze(cpsFunctions) = let
	  fun sizeOf (C.RECORD(rk,vs,x,k),hp) = sizeOf(k,allocRecord(rk,vs,hp))
	    | sizeOf (C.SELECT(off,v,x,cty,k),hp) = sizeOf(k,hp)
	    | sizeOf (C.OFFSET(off,v,x,k),hp) = sizeOf(k,hp)
	    | sizeOf (C.APP(f,vs),hp) = hp
	    | sizeOf (C.FIX _,hp) = error "sizeOf: FIX"
	    | sizeOf (C.SWITCH(v,x,ks),hp) = sizeOfs(ks,hp)
	    | sizeOf (C.BRANCH(p,_,x,k1,k2),hp) = Int.max(sizeOf(k1,hp),sizeOf(k2,hp))
	    | sizeOf (C.SETTER(P.ASSIGN,vs,k),hp) = sizeOf(k,hp+storeListSize)
	    | sizeOf (C.SETTER(P.UPDATE,vs,k),hp) = sizeOf(k,hp+storeListSize)
	    | sizeOf (C.SETTER(_,vs,k),hp) = sizeOf(k,hp)
	    | sizeOf (C.PURE(P.MKSPECIAL,vs,x,cty,k),hp) = sizeOf(k, hp + 2*cellSz)
	    | sizeOf (C.PURE(P.MAKEREF,vs,x,cty,k),hp) = sizeOf(k, hp + 2*cellSz)
	    | sizeOf (C.PURE(P.WRAP kind, _, _, _, k), hp) = sizeOf(k, wrapSize(kind, hp))
	    | sizeOf (C.PURE(P.NEWARRAY0,vs,x,cty,k),hp) = sizeOf(k,hp+array0Size)
	    | sizeOf (C.PURE(p,vs,x,cty,k),hp) = sizeOf(k,hp)
	    | sizeOf (C.ARITH(a,vs,x,cty,k),hp) = sizeOf(k,hp)
	    | sizeOf (C.LOOKER(lk,vs,x,cty,k),hp) = sizeOf(k,hp)
	    | sizeOf (C.RCC(_,_,_,_,_,k),hp) = sizeOf(k,hp)

	  and sizeOfs ([],hp)    = hp
	    | sizeOfs (k::ks,hp) = Int.max(sizeOf(k,hp),sizeOfs(ks,hp))

	  val locMap = IntHashTable.mkTable(37, NotFound) (* lvar -> loc *)
	  val look   = IntHashTable.lookup locMap
	  val find   = IntHashTable.find locMap
	  val bind   = IntHashTable.insert locMap

	  val newMem = Cells.newCell CellsBasis.MEM

	  val _      = PT.reset newMem

	  fun newRef _ = ref(PT.SCELL(newMem(),ref []))

	  val exnptr = PT.newSRef() (* exception handler *)
	  val varptr = PT.newSRef() (* var ptr *)

	  fun lookup x = (case find x
		 of SOME r => r
		  | NONE => let val r = newRef() in bind(x,r); r end
		(* end case *))

	  fun defineFunction (fk, f, args, _, cexp) = let
	        val xs = map (fn x => let val r = newRef() in bind(x,r); r end) args
	        in
		  bind(f, PT.mkLambda xs)
		end

	  val off0 = C.OFFp 0

          fun process(fk, f, args, _, cexp) = let
	      (* create a table of allocation offset locations *)
		val table = Array.tabulate(sizeOf(cexp, 0) div 4, newRef)

		fun select (i,C.VAR v,x) = bind(x,PT.pi(lookup v,i))
		  | select (i,_,x)       = ()

		fun offset (i,C.VAR v,x) = bind(x,PT.offset(lookup v,i))
		  | offset (i,_,x)       = ()

		fun value (C.VAR v) = lookup v
		  | value _         = newRef()

		fun apply (C.VAR f,args) = PT.app(lookup f,map value args)
		  | apply _             = ()

		fun getPath (v,C.OFFp 0) = value v
		  | getPath (v,C.OFFp n) = PT.offset(value v, n)
		  | getPath (v,C.SELp(n,path)) = PT.pi(getPath(v,path),n)

		fun getPaths ([],hp) = []
		  | getPaths ((v,path)::vs,hp) = let
		      val r  = Array.sub(table,hp)
		      val r' = getPath(v,path)
		      in
			PT.unify(r,r'); r::getPaths(vs,hp+1)
		      end

		fun getF64Paths ([],hp) = []
		  | getF64Paths ((v,path)::vs,hp) = let
		      val r1  = Array.sub(table,hp)
		      val r2  = Array.sub(table,hp+1)
		      val r'  = getPath(v,path)
		      in
			PT.unify(r1,r'); PT.unify(r2,r');
			r'::getF64Paths(vs,hp+2)
		      end

		(* How to make a record *)
		fun mkRec (f,getPaths,x,vs,hp) = let
		      val i = Word.toInt(Word.>>(Word.fromInt hp,cellShift))
		      val r = f(SOME(Array.sub(table,i)),getPaths(vs,i+1))
		      in
			bind(x,r)
		      end
		fun mkFRecord (x,vs,hp) = mkRec(PT.mkRecord,getF64Paths,x,vs,hp)
		fun mkVector (x,vs,hp) = mkRec(PT.mkRecord,getPaths,x,vs,hp)
		fun mkNormalRecord (x,vs,hp) = mkRec(PT.mkRecord,getPaths,x,vs,hp)

		fun mkRecord (C.RK_RAW64BLOCK,x,vs,hp) = mkFRecord(x,vs,hp)
		  | mkRecord (C.RK_FCONT,x,vs,hp) = mkFRecord(x,vs,hp)
		  | mkRecord (C.RK_VECTOR,x,vs,hp) = mkVector(x,vs,hp)
		  | mkRecord (_,x,vs,hp) = mkNormalRecord(x,vs,hp)

		fun makeTop m = (PT.unify(m, top); top)

		(* CPS Pure Primitives *)
		fun arrayptr v = PT.pi(value v, 0)

		fun mkspecial (x,v,hp) = mkNormalRecord(x,[(v,off0)],hp)
		fun mkWrap (P.FLOAT 64, x, v, hp) = mkFRecord(x, [(v, off0)], hp)
		  | mkWrap (P.INT sz, x, v, hp) = if (sz <= Target.defaultIntSz)
		      then ()
		      else mkNormalRecord(x, [(v, off0)], hp)
		  | mkWrap _ = error "mkWrap: bogus number kind"
		fun makeref (x,v,hp) = mkNormalRecord(x,[(v,off0)],hp)
		fun newarray0 (x,hp) =
		      bind(x,PT.mkRecord(NONE,[PT.mkRecord(NONE,[])]))

		fun objlength (x,v) = bind(x, PT.pi(value v, ~1))
		fun length (x,v) = bind(x, PT.pi(value v, 1))
		fun arraysub (x,a,i) = makeTop(PT.weakSubscript(arrayptr a))
		fun subscriptv (x,a,i) = arraysub(x,a,i)
		fun subscript (x,a,i) = arraysub(x,a,i)
		fun pure_numsubscript (x,a,i) = arraysub(x,a,i)
		fun gettag (x,v) = bind(x,PT.pi(value v, ~1))
		fun numsubscript8 (x,a,i) = arraysub(x,a,i)
		fun numsubscriptf64 (x,a,i) = arraysub(x,a,i)
		fun getcon (x,v) = bind(x, PT.pi(value v,0))
		fun getexn (x,v) = bind(x, PT.pi(value v,0))
		fun recsubscript (x,a,i) = arraysub(x,a,i)
		fun raw64subscript (x,a,i) = arraysub(x,a,i)

		(* CPS Looker Primitives *)
		fun deref (x,v) = makeTop(PT.strongSubscript(value v, 0))
		fun gethdlr x = bind(x, PT.strongSubscript(exnptr, 0))
		fun getvar x = bind(x, PT.strongSubscript(varptr, 0))

		(* CPS Setter Primitives *)
		fun supdate (a,x) = PT.strongUpdate(value a, 0, makeTop(value x))
		fun wupdate (a,x) = PT.weakUpdate(value a, makeTop(value x))

		fun arrayupdate (a,i,x) = PT.weakUpdate(arrayptr a,value x)

		fun assign (a,x) = supdate(a,x)
		fun unboxedassign (a,x) = supdate(a,x)
		fun update (a,i,x) = arrayupdate(a,i,x)
		fun unboxedupdate (a,i,x) = arrayupdate(a,i,x)
		fun numupdate (a,i,x) = arrayupdate(a,i,x)
		fun numupdateF64 (a,i,x) = arrayupdate(a,i,x)
		fun sethdlr x = PT.strongUpdate(exnptr, 0, value x)
		fun setvar  x = PT.strongUpdate(varptr, 0, value x)

		(* I don't know whether the following makes any sense...
		 * Basically, I want to ignore this aliasing analysis
		 * as far as raw access is concerned.  (The invariant is
		 * that raw access NEVER occurs to any memory location
		 * that ML "knows" about.  -- Blume (2000/1/1) *)
		fun rawstore (a, x) = ()
		fun rawload (a, x) = top

		fun infer (C.RECORD(rk,vs,x,k),hp) =
		      (mkRecord(rk,x,vs,hp); infer(k,allocRecord(rk,vs,hp)))
		  | infer (C.SELECT(i,v,x,cty,k),hp) = (select(i,v,x); infer(k,hp))
		  | infer (C.OFFSET(i,v,x,k),hp) = (offset(i,v,x); infer(k,hp))
		  | infer (C.APP(f,vs),hp) = apply(f,vs)
		  | infer (C.FIX _,hp) = error "infer: FIX"
		  | infer (C.SWITCH(v,x,ks),hp) = infers(ks,hp)
		  | infer (C.BRANCH(p,_,x,k1,k2),hp) = (infer(k1,hp); infer(k2,hp))
		    (*
		     * These things are misnamed! There is nothing pure about them!
		     *)
		  | infer (C.PURE(P.OBJLENGTH, [v], x, _, k), hp) =
		      (objlength(x, v); infer(k, hp))
		  | infer (C.PURE(P.LENGTH, [v], x, _, k), hp) =
		      (length(x, v); infer(k, hp))
		  | infer (C.PURE(P.SUBSCRIPTV,[a,i],x,_,k),hp) =
		      (subscriptv(x, a, i); infer(k, hp))
		  | infer (C.PURE(P.PURE_NUMSUBSCRIPT{kind=P.INT 8},[a,i],x,_,k),hp) =
		      (pure_numsubscript(x, a, i); infer(k, hp))
		  | infer (C.PURE(P.GETTAG, [v], x, _, k), hp) =
		      (gettag(x, v); infer(k, hp))
		  | infer (C.PURE(P.MKSPECIAL,[i,v],x,cty,k),hp) =
		      (mkspecial(x,v,hp); infer(k,hp+8))
		  | infer (C.PURE(P.MAKEREF,[v],x,cty,k),hp) =
		      (makeref(x,v,hp); infer(k,hp+8))
		  | infer (C.PURE(P.WRAP kind, [v], x, cty, k), hp) = (
		      mkWrap(kind, x, v, hp);
		      infer(k, wrapSize(kind, hp)))
		  | infer (C.PURE(P.GETCON,[v],x,_,k), hp) =
		      (getcon(x, v); infer(k, hp))
		  | infer (C.PURE(P.GETEXN,[v],x,_,k), hp) =
		      (getexn(x, v); infer(k, hp))
		  | infer (C.PURE(P.RECSUBSCRIPT,[a,i],x,_,k), hp) =
		      (recsubscript(x,a,i); infer(k, hp))
		  | infer (C.PURE(P.RAW64SUBSCRIPT,[a,i],x,_,k), hp) =
		      (raw64subscript(x,a,i); infer(k, hp))
		  | infer (C.PURE(P.NEWARRAY0,_,x,cty,k),hp) =
		      (newarray0(x,hp); infer(k,hp+array0Size))
		  | infer (C.PURE(p,vs,x,cty,k),hp) = infer(k,hp)

		  | infer (C.ARITH(a,vs,x,cty,k),hp) = infer(k,hp)

		    (* Lookers *)
		  | infer (C.LOOKER(P.DEREF,[v],x,_,k),hp) = (deref(x,v); infer(k,hp))
		  | infer (C.LOOKER(P.GETHDLR,[],x,_,k),hp) = (gethdlr x; infer(k,hp))
		  | infer (C.LOOKER(P.SUBSCRIPT,[a,i],x,_,k),hp) =
		      (subscript(x,a,i); infer(k,hp))
		  | infer (C.LOOKER(P.NUMSUBSCRIPT{kind=P.INT 8},[a,i],x,_,k),hp) =
		      (numsubscript8(x,a,i); infer(k,hp))
		  | infer (C.LOOKER(P.NUMSUBSCRIPT{kind=P.FLOAT 64},[a,i],x,_,k),hp) =
		      (numsubscriptf64(x,a,i); infer(k,hp))

		  | infer (C.LOOKER(P.GETVAR,[],x,_,k),hp) = (getvar x; infer(k,hp))

		  | infer (C.LOOKER (P.RAWLOAD _, [a], x, _, k), hp) =
		      (rawload (x, a); infer(k,hp))

		    (* Setters *)
		  | infer (C.SETTER(P.ASSIGN, [a,v], k),hp) =
		      (assign(a,v); infer(k,hp+storeListSize))
		  | infer (C.SETTER(P.UNBOXEDASSIGN, [a,v], k),hp) =
		      (unboxedassign(a,v); infer(k,hp))
		  | infer (C.SETTER(P.UPDATE, [a,i,v], k),hp) =
		      (update(a,i,v); infer(k,hp+storeListSize))
		  | infer (C.SETTER(P.UNBOXEDUPDATE, [a,i,v], k), hp) =
		      (unboxedupdate(a,i,v); infer(k,hp))
		  | infer (C.SETTER(P.NUMUPDATE{kind=P.INT _}, [a,i,v], k),hp) =
		      (numupdate(a,i,v); infer(k,hp))
		  | infer (C.SETTER(P.NUMUPDATE{kind=P.FLOAT 64}, [a,i,v], k),hp) = (* REAL32: FIXME *)
		      (numupdateF64(a,i,v); infer(k,hp))

		  | infer (C.SETTER(P.SETHDLR, [x], k), hp) = (sethdlr x; infer(k,hp))
		  | infer (C.SETTER(P.SETVAR, [x], k), hp) = (setvar x; infer(k,hp))
		  | infer (C.SETTER (P.RAWSTORE _, [a, x], k), hp) =
		      (rawstore (a, x); infer (k, hp))

		  | infer (e, hp) =
		      (PPCps.prcps e; print "\n"; error "infer")

		and infers ([],hp) = ()
		  | infers (k::ks,hp) = (infer(k,hp); infers(ks,hp))
	    in
	      infer (cexp, 0)
	    end (* process *)

	  in
	  (* NOTE: the default for this flag is currently false *)
	    if !Control.CG.memDisambiguate
	      then (
		CPSRegions.reset();
		app defineFunction cpsFunctions;
		app process cpsFunctions;
		fn r => look r handle _ => top)
              else (fn _ => top)
	  end

    end
