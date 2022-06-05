(* finalizer.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

functor Finalizer (Obj : FINALIZED_OBJ) : FINALIZER =
  struct

    structure Obj = Obj
    structure W = System.Unsafe.Weak

    val objList = ref ([] : (Obj.object W.weak * Obj.objinfo) list)

    fun registerObj (obj, info) = objList := (W.weak obj, info) :: !objList

    fun prune ([], live, dead) = (live, dead)
      | prune ((x as (wp, info))::r, live, dead) = (case (W.strong wp)
	   of (SOME _) => prune(r, x::live, dead)
	    | NONE => prune(r, live, info::dead)
	  (* end case *))

    fun getDead () = let
	  val (live, dead) = prune (!objList, [], [])
	  in
	    objList := live;
	    dead
	  end

    fun finalize () = let
	  fun reap ([], live) = live
	    | reap ((x as (wp, info))::r, live) = (case (W.strong wp)
               of (SOME _) => reap(r, x::live)
                | NONE => (Obj.finalize info; reap(r, live))
              (* end case *))
	  in
	    objList := reap (! objList, [])
	  end

  end (* functor Finalizer *)
