(* alignment.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ALIGNMENT =
  sig

  (* given a cluster, returns a predicate on functions that is true if the allocation
   * pointer needs to be 64-bit aligned on entry to the function.
   *)
    val build : CPS.function list -> (CPS.lvar -> bool)

  end

structure Alignment : ALIGNMENT =
  struct

    structure C = CPS
    structure Set = IntRedBlackSet

    exception Alignment

    fun error msg = ErrorMsg.impossible ("Alignment." ^ msg)

    fun build cluster = if Target.is64
	  then (fn _ => false)
	  else let
	    fun hasFloats (C.RECORD(C.RK_FCONT, _, _, _)) = true
	      | hasFloats (C.RECORD(C.RK_RAW64BLOCK, _, _, _)) = true
	      | hasFloats (C.RECORD(_, _, _, e)) = hasFloats e
	      | hasFloats (C.SELECT(_, _, _, _, e)) = hasFloats e
	      | hasFloats (C.OFFSET(_, _, _, e)) = hasFloats e
	      | hasFloats (C.APP _) = false
	      | hasFloats (C.FIX _) = error "hasFloats: FIX"
	      | hasFloats (C.SWITCH(_, _, el)) = List.exists hasFloats el
	      | hasFloats (C.BRANCH(_, _, _, e1, e2)) = hasFloats e1 orelse hasFloats e2
	      | hasFloats (C.SETTER(_, _, e)) = hasFloats e
	      | hasFloats (C.LOOKER(_, _, _, _, e)) = hasFloats e
	      | hasFloats (C.ARITH(_, _, _, _, e)) = hasFloats e
	      | hasFloats (C.PURE(C.P.fwrap, _, _, _, _)) = true
	      | hasFloats (C.PURE(_, _, _, _, e)) = hasFloats e
	    fun doFunction ((_,f,_,_,e), set) =
		  if hasFloats e then Set.add(set, f) else set
	    val funcsThatHaveFloats = List.foldl doFunction [] cluster
	    in
	      fn f => Set.member(funcsThatHaveFloats, f)
	    end (* build *)

  end
