(* string-util-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.
 *
 * A bunch of string operations.  These roughly correspond to a merging of
 * the BSD strings.h and System V string.h interfaces.  Many of these functions
 * take either a "string" or "int -> bool" value as a specifier of a set of
 * characters.
 *
 * AUTHOR:
 *   John Reppy
 *   Cornell University
 *   Ithaca, NY 14853
 *   jhr@cs.cornell.edu
 *)

signature STRING_UTIL =
  sig

    exception NotFound

  (* scan a string from left to right for the first occurance of a character
   * in the set to the right of the starting index.  If no such character is
   * found, raise the NotFound exception.
   *)
    val index : string -> (string * int) -> int
    val indexp : (int -> bool) -> (string * int) -> int

  (* scan a string from right to left for the first occurance of a character
   * in the set to the left of the starting index.  If no such character is
   * found, raise the NotFound exception.
   *)
    val revindex : string -> (string * int) -> int
    val revindexp : (int -> bool) -> (string * int) -> int

  (* "spanp pred s" returns the length of the initial segment of s satisfying the
   * predicate p.
   *)
    val spanp : (int -> bool) -> (string * int) -> int
    val span : string -> (string * int) -> int

  (* "cspanp pred s" returns the length of the initial segment of s that doesn't
   * satisfy the predicate p.
   *)
    val cspanp : (int -> bool) -> (string * int) -> int
    val cspan : string -> (string * int) -> int

  (* tokenize a string *)
    val tokenizep : (int -> bool) -> (string * int) -> string list
    val tokenize : string -> (string * int) -> string list

  (* find the leftmost (rightmost) occurance of a substring in a string.
   * Raise the exception NotFound if it doesn't exist.
   *)
    val findstr : (string * int * string) -> int
    val revfindstr : (string * int * string) -> int

    val strcmp : (string * string) -> LibBase.relation
	(* lexically compare two strings and return their relation *)

    val isPrefix : (string * string * int) -> bool
	(* isPrefix(s1, s2, i) returns true, if s1 is a prefix of s2[i..]. *)

    val prefixCmp : (string * int * string * int) -> (bool * bool)
	(* prefixCmp (s1, i1, s2, i2) tests whether s1[i1..] is a
	 * prefix of s2[i2..], and vice versa.
	 *)

    val unequalAt : (string * int * string * int) -> (int * int)
	(* compare two strings, returning the indecies of the
	 * first characters at which they disagree.
	 *)

    val suffix : (string * int) -> string
	(* suffix (s, i) returns s[i..].  If i is greater or equal to
	 * the length of s, then "" is returned.  If i is less than 0,
	 * then LibBase.BadArg is raised.
	 *)

    val stringTrans : (string * string) -> string -> string
	(* stringTrans (s1, s2) returns a translation function that maps
	 * each character in s1 to the corresponding character in s2.
	 *)

    val stringMap : (int -> string) -> string -> string
	(* stringMap f == (fn s => implode(map f (explode s))) *)

    val compressStr : string -> string
	(* compress ML-style escape sequences to single characters. *)

    val expandStr : string -> string
	(* expand non-printing characters to their escape sequences. *)

  end (* STRING_UTIL *)
