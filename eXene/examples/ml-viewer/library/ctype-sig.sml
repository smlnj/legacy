(* ctype-sig.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Predicates on characters.  This is modelled after the Unix C libraries.  
 * Each predicate comes in two forms; one that works on integers, and one
 * that works on an arbitrary character in a string.  The meanings of these
 * predicates are documented in Section 3 of the Unix manual.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

signature CTYPE =
  sig

  (* predicates on indexed strings *)
    val isAlpha    : (string * int) -> bool
    val isUpper    : (string * int) -> bool
    val isLower    : (string * int) -> bool
    val isDigit    : (string * int) -> bool
    val isXDigit   : (string * int) -> bool
    val isAlphaNum : (string * int) -> bool
    val isSpace    : (string * int) -> bool
    val isPunct    : (string * int) -> bool
    val isGraph    : (string * int) -> bool
    val isPrint    : (string * int) -> bool
    val isCntrl    : (string * int) -> bool
    val isAscii    : (string * int) -> bool

  (* predicates on integer coding of Ascii values *)
    val isAlphaOrd    : int -> bool
    val isUpperOrd    : int -> bool
    val isLowerOrd    : int -> bool
    val isDigitOrd    : int -> bool
    val isXDigitOrd   : int -> bool
    val isAlphaNumOrd : int -> bool
    val isSpaceOrd    : int -> bool
    val isPunctOrd    : int -> bool
    val isPrintOrd    : int -> bool
    val isCntrlOrd    : int -> bool
    val isAsciiOrd    : int -> bool
    val isGraphOrd    : int -> bool

  (* conversion routines *)
    val toAscii : string -> string
    val toUpper : string -> string
    val toLower : string -> string
    val toAsciiOrd : int -> int
    val toUpperOrd : int -> int
    val toLowerOrd : int -> int

  end (* CTYPE *)

