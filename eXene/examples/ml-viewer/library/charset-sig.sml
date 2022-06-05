(* charset-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Fast, read-only, character sets.  These are meant to be used to construct
 * predicates for the functions in Strings.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

signature CHAR_SET =
  sig

    type char_set
	(* an immutable set of characters *)

    val mkCharSet : string -> char_set
	(* make a character set consisting of the characters of the given string. *)

    val charSetOfList : int list -> char_set
	(* make a character set consisting of the characters whose ordinals are
	 * given by the list of integers.
	 *)

    val charsOfSet : char_set -> string
	(* return a string representation of a character set *)

    val inSetOrd : char_set -> int -> bool
	(* return true if the character with of the given ordinal is in the set *)
    val inSet : char_set -> (string * int) -> bool
	(* (inSet c (s, i)) is equivalent to (inSetOrd c (ordof(s, i))) *)

  end (* CHAR_SET *)

