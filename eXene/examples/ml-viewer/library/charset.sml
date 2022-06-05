(* charset.sml
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

structure CharSet : CHAR_SET =
  struct

    structure BA = ByteArray

  (* an immutable set of characters *)
    datatype char_set = CS of BA.bytearray

  (* make a character set consisting of the characters of the given string. *)
    fun mkCharSet s = let
	  val ba = BA.array(256, 0)
	  fun ins i = (BA.update(ba, ordof(s, i), 1); ins(i+1))
	  in
	    (ins 0) handle _ => ();
	    CS ba
	  end

  (* make a character set consisting of the characters whose ordinals are
   * given by the list of integers.
   *)
    fun charSetOfList l = let
	  val ba = BA.array(256, 0)
	  fun ins (c::r) = (BA.update(ba, c, 1); ins r)
	    | ins [] = ()
	  in
	    (ins l)
	      handle _ => LibBase.badArg{
		  module="CharSet", func="charSetOfList", msg="range error"
		};
	    CS ba
	  end

  (* return a string representation of a character set *)
    fun charsOfSet (CS ba) = let
	  fun f (~1, l) = implode l
	    | f (i, l) = if (BA.sub(ba, i) = 1)
		then f (i-1, (chr i)::l)
		else f (i-1, l)
	  in
	    f (255, [])
	  end

  (* return true if the character with of the given ordinal is in the set *)
    fun inSetOrd (CS ba) i = (BA.sub(ba, i) = 1)

  (* (inSet c (s, i)) is equivalent to (inSetOrd c (ordof(s, i))) *)
    fun inSet (CS ba) (s, i) = (BA.sub(ba, ordof(s, i)) = 1)

  end (* CharSet *)

