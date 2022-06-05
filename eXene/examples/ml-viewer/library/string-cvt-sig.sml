(* string-cvt-sig.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Basic routines to convert strings to other primitive types.
 * All of these ignore leading whitespace.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

signature STRING_CVT =
  sig

    exception Convert
	(* raised on badly formated input *)

    datatype radix = Bin | Oct | Dec | Hex

  (* String to integer conversion functions. *)
    val strToInt : radix -> (string * int) -> (int * int)
    val atoi     : string -> int
    val xatoi    : string -> int
    val oatoi    : string -> int

  (* String to real conversion functions. *)
    val strToReal : (string * int) -> (real * int)
    val atof      : string -> real

  (* String to bool conversion functions. *)
    val strToBool : (string * int) -> (bool * int)
    val atob      : string -> bool

  end (* STRING_CVT *)

