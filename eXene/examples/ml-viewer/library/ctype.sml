(* ctype.sml
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

structure CType : CTYPE =
  struct

  (* For each character code we have an 8-bit vector, which is interpreted
   * as follows:
   *   0x01  ==  set for upper-case letters
   *   0x02  ==  set for lower-case letters
   *   0x04  ==  set for digits
   *   0x08  ==  set for white space characters
   *   0x10  ==  set for punctuation characters
   *   0x20  ==  set for control characters
   *   0x40  ==  set for hexadecimal characters
   *   0x80  ==  set for SPACE
   *)
    val ctypeTbl = "\
	    \\032\032\032\032\032\032\032\032\032\040\040\040\040\040\032\032\
	    \\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\
	    \\136\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
	    \\068\068\068\068\068\068\068\068\068\068\016\016\016\016\016\016\
	    \\016\065\065\065\065\065\065\001\001\001\001\001\001\001\001\001\
	    \\001\001\001\001\001\001\001\001\001\001\001\016\016\016\016\016\
	    \\016\066\066\066\066\066\066\002\002\002\002\002\002\002\002\002\
	    \\002\002\002\002\002\002\002\002\002\002\002\016\016\016\016\032\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	  \"
    fun inSet (c, s) = (Bits.andb(ordof(ctypeTbl, c), s) <> 0)

  (* predicates on integer coding of Ascii values *)
    fun isAlphaOrd i    = inSet(i, 0x03) handle _ => false
    fun isUpperOrd i    = inSet(i, 0x01) handle _ => false
    fun isLowerOrd i    = inSet(i, 0x02) handle _ => false
    fun isDigitOrd i    = inSet(i, 0x04) handle _ => false
    fun isXDigitOrd i   = inSet(i, 0x40) handle _ => false
    fun isAlphaNumOrd i = inSet(i, 0x07) handle _ => false
    fun isSpaceOrd i    = inSet(i, 0x08) handle _ => false
    fun isPunctOrd i    = inSet(i, 0x10) handle _ => false
    fun isGraphOrd i    = inSet(i, 0x17) handle _ => false
    fun isPrintOrd i    = inSet(i, 0x97) handle _ => false
    fun isCntrlOrd i    = inSet(i, 0x20) handle _ => false
    fun isAsciiOrd i    = ((0 <= i) andalso (i < 128))

  (* predicates on indexed strings *)
    fun isAlpha (s, i)    = inSet(ordof(s, i), 0x03)
    fun isUpper (s, i)    = inSet(ordof(s, i), 0x01)
    fun isLower (s, i)    = inSet(ordof(s, i), 0x02)
    fun isDigit (s, i)    = inSet(ordof(s, i), 0x04)
    fun isXDigit (s, i)   = inSet(ordof(s, i), 0x40)
    fun isAlphaNum (s, i) = inSet(ordof(s, i), 0x07)
    fun isSpace (s, i)    = inSet(ordof(s, i), 0x08)
    fun isPunct (s, i)    = inSet(ordof(s, i), 0x10)
    fun isGraph (s, i)    = inSet(ordof(s, i), 0x17)
    fun isPrint (s, i)    = inSet(ordof(s, i), 0x97)
    fun isCntrl (s, i)    = inSet(ordof(s, i), 0x20)
    fun isAscii (s, i)    = (ordof(s, i) < 128)

  (* conversion routines *)
    fun toAsciiOrd i = Bits.andb(i, 0x7F)
    fun toUpperOrd i = if (isLowerOrd i) then (i - 32) else i
    fun toLowerOrd i = if (isUpperOrd i) then (i + 32) else i
    local
      fun cvt cvtOrd s = (case (size s)
	     of 1 => chr (cvtOrd (ord s))
	      | n => let
		    fun f (~1, l) = implode l
		      | f (i, l) = f (i-1, chr (cvtOrd (ordof (s, i))) :: l)
		    in
		      f (n-1, [])
		    end
	    (* end case *))
    in
    val toAscii = cvt toAsciiOrd
    val toUpper = cvt toUpperOrd
    val toLower = cvt toLowerOrd
    end (* local *)

  end (* end CType *)

