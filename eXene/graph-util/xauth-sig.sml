(* xauth-sig.sml
 *
 * Support for X11 authentication.
 *)

signature X_AUTH = sig

  (* the different network protocol families *)
    val familyInternet : int
    val familyDECnet : int
    val familyChaos : int
    val familyLocal : int
    val familyWild : int

    val authFileName : unit -> string
	(* return the default name of the authentication file (either
	 * specified by the XAUTHORITY environment variable, or the
	 * file $HOME/.Xauthority.  If neither XAUTHORITY or HOME 
	 * are defined, then ".Xauthority" is returned.
	 *)

    val getAuthByAddr : {
	    family : int, addr : string, dpy : string
	  } -> EXeneBase.authentication option
	(* searches the default authentication file for the first entry that
	 * matches the family, network address and display number.  If no
	 * such match is found, then NONE is returned.  The * value familyWild
	 * matches anything, as do the empty strings when given for addr or dpy.
	 *)

    val getBestAuthByAddr : {
	    family : int, addr : string, dpy : string, authNames : string list
	  } -> EXeneBase.authentication option
	(* this similar to getAuthByAddr, except that a list of acceptable
	 * authentication methods is specified by the list authNames.  It
	 * returns the matching authentication info that matches the earliest
	 * name on the list.  NONE is returned if no match is found.
	 *)

    val readAuthFile : (EXeneBase.authentication -> bool) -> string
	  -> EXeneBase.authentication list
	(* read the specified authentication file and return a list of
	 * entries that satisfy the given predicate.
	 *)

  end;
