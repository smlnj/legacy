(* keysym.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
 * and the Massachusetts Institute of Technology, Cambridge, Massachusetts.
 *
 * Symbolic names for the common keysyms in the X11 standard.  This is a situation
 * where SML doesn't really have the necessary features (e.g., symbolic constants),
 * so it is pretty ugly.
 *)

structure Keysym =
  struct

    local
      structure S : sig
	  datatype keysym = NoSymbol | KEYSYM of int
	end = XProtTypes
    in
    open S
    end

    val voidSymbol = KEYSYM 0xFFFFFF

    datatype char_set
      = CS_Latin1 | CS_Latin2 | CS_Latin3 | CS_Latin4
      | CS_Kana | CS_Arabic | CS_Cyrillic | CS_Greek
      | CS_Technical | CS_Special | CS_Publishing | CS_Apl
      | CS_Hebrew | CS_Keyboard | CS_Void

    fun charSetOf (KEYSYM 0xFFFFFF) = CS_Void
      | charSetOf NoSymbol = CS_Void
      | charSetOf (KEYSYM ks) = (case Word.andb(Word.fromInt ks, 0wxff00)
	 of 0w0 => CS_Latin1 | 0w1 => CS_Latin2
	  | 0w2 => CS_Latin3 | 0w3 => CS_Latin4
	  | 0w4 => CS_Kana | 0w5 => CS_Arabic
	  | 0w6 => CS_Cyrillic | 0w7 => CS_Greek
	  | 0w8 => CS_Technical | 0w9 => CS_Special
	  | 0w10 => CS_Publishing | 0w11 => CS_Apl
	  | 0w12 => CS_Hebrew | 0w255 => CS_Keyboard
	  | _ => MLXError.impossible "[Keysym.charSetOf: unknown character set]")

  end (* KeySym *)
