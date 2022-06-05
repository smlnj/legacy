(* lib-base-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

signature LIB_BASE =
  sig

    exception Unimplemented of string
	(* raised to report unimplemented features *)
    exception Impossible of string
	(* raised to report internal errors *)
    exception BadArg of string
	(* raised to report semantically incorrect arguments.  For consistency,
	 * the string should include the library module and function names.
	 * The function badArg is provided for this purpose.
	 *)

    val badArg : {module : string, func : string, msg : string} -> 'a
	(* raise the exception BadArg with a standard format message. *)

    datatype relation = Equal | Less | Greater
	(* this is returned by collating functions *)

    val version : {major : int, minor : int, date : string}
    val versionName : string

  end (* LIB_BASE *)

