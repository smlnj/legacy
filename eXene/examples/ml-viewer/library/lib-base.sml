(* lib-base.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

structure LibBase : LIB_BASE =
  struct

  (* raised to report unimplemented features *)
    exception Unimplemented of string

  (* raised to report internal errors *)
    exception Impossible of string

  (* raised to report semantically incorrect arguments.  For consistency,
   * the string should include the library module and function names.
   * The function badArg is provided for this purpose.
   *)
    exception BadArg of string

  (* raise the exception BadArg with a standard format message. *)
    fun badArg {module, func, msg} =
	  raise (BadArg(implode[module, ".", func, ": ", msg]))

  (* this is returned by collating functions *)
    datatype relation = Equal | Less | Greater

    val version = {major = 0, minor = 1, date = "February 1, 1993"}
    val versionName = "SML/NJ Library, Version 0.1, February 1, 1993"

  end (* LibBase *)

