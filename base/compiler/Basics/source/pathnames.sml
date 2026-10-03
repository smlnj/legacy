(* pathnames.sml
 *
 *   Trimming of file names in diagnostic messages.
 *
 *   If the environment variable SMLNJ_TRIM_PREFIX is set, that prefix
 *   is removed from file names.  This keeps build-directory paths out
 *   of the source locations recorded in stabilized libraries.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *)
structure Pathnames : sig
    val trim : string -> string
end = struct
    fun trim path = (case OS.Process.getEnv "SMLNJ_TRIM_PREFIX"
	   of SOME prefix =>
		if prefix <> "" andalso String.isPrefix prefix path
		  then String.extract (path, size prefix, NONE)
		  else path
	    | NONE => path
	  (* end case *))
end
