(* memDisambig.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature MEM_DISAMBIGUATION = sig
  val build : CPS.function list -> (int -> CPSRegions.region)
end

functor MemDisambiguate(structure Cells: CELLS) : MEM_DISAMBIGUATION = struct
  structure C = CPS
  structure P = CPS.P
  structure R = CPSRegions

  fun error msg = ErrorMsg.impossible ("MemDisambiguate." ^ msg)

  exception MemDisambig and FormalsTbl
  val newRegion = Cells.newCell Cells.MEM

  fun build(frags) = let
    (* mapping of lvars to a list of regions that define it *)
    (* mappings can only be RVAR, COPY, or RECORD *)
    val regionTbl : R.region Intmap.intmap = Intmap.new(16, MemDisambig)
    val enterRegion = Intmap.add regionTbl
    val lookupRegion = Intmap.map regionTbl
    fun peekRegion v = SOME(Intmap.map regionTbl v) handle _ => NONE
    fun addRegion(arg as (x,v)) = (Intmap.rmv regionTbl x; enterRegion arg)

    (* compute the size of a CPS assuming that the allocation
     * pointer has been appropriately aligned.
     *)
    fun sizeOf(cexp, hp) = let
      val storeListSz = 8
      fun frecord len = let
	val hp = if Word.andb(Word.fromInt hp, 0w4) <> 0w0 then hp+4 else hp
      in hp + 8*len + 4
      end
      fun record len = 4 + 4*len
    in
      case cexp
      of C.RECORD(C.RK_RAW64BLOCK, vl, _, e) => sizeOf(e, frecord(length vl))
       | C.RECORD(C.RK_FCONT, vl, _, e)  => sizeOf(e, frecord(length vl))
       | C.RECORD(C.RK_VECTOR, vl, _, e) => sizeOf(e, hp+record(length vl + 3))
       | C.RECORD(_, vl, _, e) => sizeOf(e, hp + record(length vl))
       | C.SELECT(_, _, _, _, e) => sizeOf(e, hp)
       | C.OFFSET(_,_,_,e) => sizeOf(e, hp)
       | C.SWITCH(_,_,el) =>
	  List.foldl Int.max 0 (map (fn e => sizeOf(e, hp)) el)
       | C.SETTER(P.update,_,e) => sizeOf(e, hp+storeListSz)
       | C.SETTER(_, _, e) => sizeOf(e, hp)
       | C.PURE(P.fwrap, _, _, _, e) => sizeOf(e, hp+frecord(1))
       | C.PURE(P.mkspecial, _, _, _, e) => sizeOf(e, hp+8)
       | C.PURE(P.makeref, _, _, _, e) => sizeOf(e, hp+8)
       | C.PURE(P.i32wrap, _, _, _, e) => sizeOf(e, hp+record(2))	(* 64BIT: FIXME *)
       | C.PURE(P.newarray0, _, _, _, e) => sizeOf(e, hp+(4*5))
       | C.PURE(_, _, _, _, e) => sizeOf(e, hp)
       | C.ARITH(_, _, _, _, e) => sizeOf(e, hp)
       | C.LOOKER(_,_,_,_,e) => sizeOf(e, hp)
       | C.BRANCH(_,_,_,a,b) => Int.max(sizeOf(a, hp), sizeOf(b, hp))
       | C.APP _ => hp
       | C.FIX _ => error "sizeOf: FIX"
    end

    val offp0 = C.OFFp 0

    fun funBody(_, _, _, _, cexp) = let
      val regionIdTbl =
	Array.tabulate(Int.quot(sizeOf(cexp, 0), 4), fn _ => newRegion())

      fun regionId hp = R.RVAR(Array.sub(regionIdTbl, Int.quot(hp, 4)))

      fun traceRoot(C.LABEL _) = R.RO_MEM
	| traceRoot(C.VAR v) = (lookupRegion v handle MemDisambig => R.RO_MEM)
        | traceRoot _ = R.RO_MEM

      fun iter(cexp, hp) = let

        fun desc hp = (regionId(hp), R.RO_MEM, offp0)

        fun record(vl, x, e) = let
	  fun fields ([], _) = []
	    | fields ((v, ap)::vl, hp) =
	       (regionId(hp), traceRoot v, ap)::fields(vl, hp+4)
        in
	  enterRegion(x, R.RECORD(desc(hp)::fields(vl, hp+4)));
	  iter(e, hp + 4 + 4*length vl)
        end

        fun frecord(vl, x, e) = let
	  fun regionPair hp = R.REGIONS(regionId hp, regionId(hp+4))
	  fun fields([], _) = []
	    | fields((v, ap)::vl, hp) =
	       (regionPair hp, traceRoot v, ap) :: fields(vl, hp+8)
	  val hp = if Word.andb(Word.fromInt hp, 0w4) <> 0w0 then hp+4 else hp
        in
	  enterRegion(x, R.RECORD(desc(hp)::fields(vl, hp+4)));
	  iter (e, hp + 4 + 8*length vl)
        end

	fun recordSlots((d, R.RECORD vl, _)::rest) =
	     R.REGIONS(d, recordSlots (vl@rest))
	  | recordSlots((d, R.OFFSET(_, vl), _)::rest) =
	     R.REGIONS(d, recordSlots (vl@rest))
	  | recordSlots [(d, _, _)] = d
	  | recordSlots((d, _, _)::rest) = R.REGIONS(d, recordSlots rest)

	fun update(C.VAR a, C.VAR v, e) =
	     (case (peekRegion a, peekRegion v)
	      of (NONE, NONE) => enterRegion(a, R.MUTABLE(R.RW_MEM, R.RO_MEM))
	       | (NONE, SOME(R.RECORD rl)) =>
		   enterRegion(a, R.MUTABLE(R.RW_MEM, recordSlots rl))
	       | (SOME _, NONE) => ()
	       | (SOME(R.MUTABLE(def,use)), SOME(R.RECORD rl)) =>
		   addRegion(a, R.MUTABLE(def, R.REGIONS(use, recordSlots rl)))
              (*esac*);
	      iter(e, hp))
	  | update(_, _, e) = iter(e, hp)

	fun select(C.VAR v, i, x, e) =
	    (case peekRegion v
	     of SOME(R.RECORD vl) => let
		  val (_, region, ap) = List.nth(vl, i+1)
		in enterRegion(x, R.trace(region, ap))
		end
	      | SOME(R.OFFSET(j, vl)) => let
		  val (_, region, ap) = List.nth(vl, i+j+1)
		in enterRegion(x, R.trace(region, ap))
		end
	      | SOME(R.MUTABLE _) => error "select"
	      | _ => ()
	     (*esac*);
	     iter(e, hp))
	  | select(_, _, _, e) = iter(e, hp)

	fun offset(C.VAR v, i, x, e) =
	    (case peekRegion v
	     of SOME(R.RECORD vl) => enterRegion(x, R.OFFSET(i, vl))
	      | SOME(R.OFFSET(j, vl)) => enterRegion(x, R.OFFSET(i+j, vl))
	      | SOME(R.MUTABLE _) => error "offset"
	      | _  => ()
	     (*esac*);
	     iter(e, hp))
	  | offset(_, _, _, e) = iter(e, hp)
      in
        case cexp
	of C.RECORD(C.RK_FBLOCK, vl, x, e) => frecord(vl, x, e)
	 | C.RECORD(C.RK_FCONT, vl, x, e) => frecord(vl, x, e)
	 | C.RECORD(C.RK_VECTOR, vl, x, e) => let
	    val y = LambdaVar.mkLvar()
	    in
	      record (vl, y,
		C.RECORD(
		  C.RK_RECORD, [(C.VAR y, offp0), (C.INT(length vl), offp0)],
		  x, e))
	    end
	 | C.RECORD(rk, vl, x, e) => record(vl, x, e)
	 | C.SELECT(i, v, x, _, e) => select(v, i, x, e)
	 | C.OFFSET(i, v, x, e) => offset(v, i, x, e)
	 | C.APP _ => ()
	 | C.FIX(fl, e) => error "FIX"
	 | C.SWITCH(_, _, el) => List.app (fn e => iter(e, hp)) el
	 | C.BRANCH(_, _, _, e1, e2) => (iter(e1, hp); iter(e2, hp))
	 | C.SETTER(P.update, [a,_,v], e) => update(a, v, e)
	 | C.SETTER(P.numupdate{kind=P.FLOAT 64}, [a,i,v], e) => update(a, v, e)
	 | C.SETTER(_, _, e) => iter(e, hp)
	 | C.LOOKER(_, _, _, _, e) => iter(e, hp)
	 | C.ARITH(_, _, _, _, e) => iter(e, hp)
	 | C.PURE(P.mkspecial, [i,v], x, _, e) => record([(v, offp0)], x, e)
	 | C.PURE(P.fwrap, [u], x, _, e) => frecord([(u, offp0)], x, e)
	 | C.PURE(P.i32wrap, [u], x, _, e) =>		(* 64BIT: FIXME *)
	     record([(u, offp0),(C.INT 0, offp0)], x, e)
	 | C.PURE(P.makeref, [v], x, _, e) => let
	     val uses =
	       case v
	       of C.VAR lvar =>
		    (case peekRegion lvar
	             of NONE => R.RO_MEM
	              | SOME(R.RECORD vl) => recordSlots vl
		      | SOME(R.OFFSET(_, vl)) => recordSlots vl
		      | SOME(R.MUTABLE(def, use)) => def
		      | SOME r => r
		     (*esac*))
	        | _ => R.RO_MEM
	     val defs =
	       R.REGIONS(R.RW_MEM,
			 R.REGIONS(regionId(hp), regionId(hp+4)))
	   in
	     enterRegion(x, R.MUTABLE(defs, uses));
	     iter(e, hp+8)
           end
	 | C.PURE(P.newarray0, _, w, _, e) => let
	    val y = LambdaVar.mkLvar()
	    in
	      iter (
		C.RECORD(C.RK_RECORD, [(C.INT 0, offp0)], y,
		  C.RECORD(C.RK_RECORD, [(C.VAR y, offp0), (C.INT 0, offp0)], w, e)),
		hp)
	    end
	 | C.PURE(_, _, _, _, e) => iter(e, hp)
	(*esac*)
      end
    in iter(cexp, 0)
    end (* funBody *)
  in
    app funBody frags;
    fn v => lookupRegion v handle _ => R.RO_MEM
  end (*memDisambig*)
end

