(* xvalid.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories. See COPYRIGHT file for details.
 *)

structure XValid : sig

    val valid8		: int -> bool
    val validWord8	: word -> bool
    val validSigned8	: int -> bool
    val valid16		: int -> bool
    val validWord16	: word -> bool
    val validSigned16	: int -> bool

  end = struct

    val not8 = Word.notb 0wxFF
    val not16 = Word.notb 0wxFFFF

    fun andb (i, mask) = Word.andb(Word.fromInt i, mask)

    fun validWord8 w = (Word.andb(w, not8) = 0w0) 
    fun valid8 i = validWord8 (Word.fromInt i)
    fun validSigned8 i = (i < 128) andalso (i >= ~128)

    fun validWord16 w = (Word.andb(w, not16) = 0w0)
    fun valid16 i = validWord16 (Word.fromInt i)
    fun validSigned16 i = (i < 32768) andalso (i >= ~32768)

  end (* XValid *)
