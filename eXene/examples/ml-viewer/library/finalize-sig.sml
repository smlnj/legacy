(* finalize-sig.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

signature FINALIZED_OBJ =
  sig

    type object
    type objinfo

    val finalize : objinfo -> unit

  end (* FINALIZED_OBJ *)

signature FINALIZER =
  sig

    structure Obj : FINALIZED_OBJ

    val registerObj : (Obj.object * Obj.objinfo) -> unit
	(* register an object for finalization.  It is important that the
	 * object info not contain any reference to the object, otherwise
	 * the object will never become free.
	 *)

    val getDead : unit -> Obj.objinfo list
	(* return a list of registered dead objects, and remove them from
	 * the registry.
	 *)

    val finalize : unit -> unit
	(* finalize all registered dead objects and remove them from the
	 * registry.
	 *)

  end (* FINALIZER *)

