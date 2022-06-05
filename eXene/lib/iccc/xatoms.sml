(* xatoms.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Operations on atoms.
 *)

structure XAtoms : sig

    val internAtom : Display.display -> string -> XProtTypes.atom
    val lookupAtom : Display.display -> string -> XProtTypes.atom option
    val nameOfAtom : Display.display -> XProtTypes.atom -> string

  end = struct

    fun intern dpy arg = XReply.decodeInternAtomReply (
	  CML.sync (Display.dpyRequestReply dpy (XRequest.encodeInternAtom arg)))

    fun internAtom dpy name = intern dpy {name = name, only_if_exists = false}

    fun lookupAtom dpy name = (case (intern dpy {name = name, only_if_exists = true})
	   of (XProtTypes.XAtom 0w0) => NONE
	    | xa => SOME xa
	  (* end case *))

    fun nameOfAtom dpy atom = XReply.decodeGetAtomNameReply (
	  CML.sync (
	    Display.dpyRequestReply dpy (XRequest.encodeGetAtomName{atom = atom})))

  end (* XAtoms *)
