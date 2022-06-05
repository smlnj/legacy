(* makestring-sig.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * Basic value to string conversions.
 *
 * AUTHOR:  Emden Gansner & John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    erg@ulysses.att.com & jhr@research.att.com
 *)

signature MAKESTRING =
  sig

    val padLeft : (string * int) -> string
	(* pad a string on the left with blanks to specified width *)

    val padRight : (string * int) -> string
	(* pad a string on the right with blanks to specified width *)

    val boolToStr : bool -> string

    val intToBin : int -> string	(* binary representation *)
    val intToOct : int -> string	(* octal representation *)
    val intToStr : int -> string	(* decimal representation *)
    val intToHex : int -> string	(* hexadecimal representation *)

    exception BadPrecision
	(* raised by real to string conversions, if the precision is < 0. *)

    val realToFloStr : (real * int) -> string
	(* convert a real number to a string of the form [~]ddd.ddd, where
	 * the precision (number of fractional digits) is specified by the
	 * second argument.
	 *)
    val realToSciStr : (real * int) -> string
	(* convert a real number to a string of the form [~]d.dddE[~]dd, where
	 * the precision (number of fractional digits) is specified by the
	 * second argument.
	 *)

  (* Low-level real to string conversion routines. For F and E format, the precision
   * specifies the number of fractional digits with 0's appended if necessary.
   * For G format, precision specifies the number of significant digits, but
   * trailing 0's in the fractional part are dropped.
   *)
    val realFFormat : (real * int) -> {sign : bool, mantissa : string}
    val realEFormat : (real * int) -> {sign : bool, mantissa : string, exp : int}
    val realGFormat : (real * int)
	  -> {sign : bool, whole : string, frac : string, exp : int option}

  end (* MAKESTRING *)
