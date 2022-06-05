(* get-dpy-sig.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * Utility code for getting the display name and authentication information.
 *)

signature GET_DPY =
  sig

  (* parse a string specifying a X display into its components. *)
    val parseDisplay : string -> {
	    host : string,
	    dpy : string,
	    screen : string
	  }

  (* given an optional display name, return the display and authentication
   * information.  If the argument is NONE, then we use the DISPLAY environment
   * variable if it is defined, and "" if it is not defined.
   *)
    val getDpy : string option -> (string * EXeneBase.authentication option)

  end;

