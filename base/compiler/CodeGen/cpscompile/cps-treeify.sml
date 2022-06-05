(* cps-treeify.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature CPS_TREEIFY = sig
  datatype treeify = TREEIFY | COMPUTE | DEAD

  val usage : CPS.function list -> (CPS.lvar -> treeify)
end


structure CpsNoTreeify : CPS_TREEIFY =
struct
  datatype treeify = TREEIFY | COMPUTE | DEAD
  val usage = fn _ => fn _ => COMPUTE
end



structure CpsTreeify : CPS_TREEIFY =
struct
  structure C = CPS

  datatype treeify = TREEIFY | COMPUTE | DEAD

  fun error msg = ErrorMsg.impossible ("FPStack." ^ msg)

  fun usage fl = let
   (* Table to record number of uses *)
    exception UseCntTbl
    val useCntTbl : treeify  Intmap.intmap = Intmap.new(32, UseCntTbl)
    val uses = Intmap.mapWithDefault (useCntTbl,DEAD)
    val addCntTbl = Intmap.add useCntTbl
    fun addUse v =
      case uses v
       of DEAD => addCntTbl(v, TREEIFY)
        | TREEIFY => addCntTbl(v, COMPUTE)
	| _ => ()

    fun addValue(C.VAR v) = addUse v
      | addValue _ = ()
    fun addValues [] = ()
      | addValues(C.VAR v::vl) = (addUse v; addValues vl)
      | addValues(_::vl) = addValues vl

    fun cntUsesCps(C.RECORD(_, vl, w, e)) =
	 (addValues (map #1 vl); cntUsesCps e)
      | cntUsesCps(C.SELECT(i, v, x, _, e)) = (addValue v; cntUsesCps e)
      | cntUsesCps(C.OFFSET(i, v, x, e)) = (addValue v; cntUsesCps e)
      | cntUsesCps(C.APP(v, vl)) = (addValue v; addValues vl)
      | cntUsesCps(C.FIX _) = error "pass1: FIX"
      | cntUsesCps(C.SWITCH(v, _, el)) = (addValue v; app cntUsesCps el)
      | cntUsesCps(C.BRANCH(_, vl, _, c1, c2)) =
	 (addValues vl; cntUsesCps c1; cntUsesCps c2)
      | cntUsesCps(C.SETTER(_, vl, e)) = (addValues vl; cntUsesCps e)
      | cntUsesCps(C.LOOKER(looker, vl, x, _, e)) =
	 (addValues vl;
	  (* floating subscript cannot move past a floating update.
	   * For now subscript operations cannot be treeified.
	   * This is hacked by making it (falsely) used more than once.
	   *)
	  case looker
	   of C.P.numsubscript{kind=C.P.FLOAT _} => (addUse x; addUse x)
	    | _ => ()
          (*esac*);
	  cntUsesCps e)
      | cntUsesCps(C.ARITH(_, vl, _, _, e)) = (addValues vl; cntUsesCps e)
      | cntUsesCps(C.PURE(_, vl, _, _, e)) = (addValues vl; cntUsesCps e)
  in
    app (fn (_, _, _, _, e) => cntUsesCps e) fl;
    uses

  end
end
