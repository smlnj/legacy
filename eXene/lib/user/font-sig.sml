(* font.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * These are the font related types and operations supported by eXene (except
 * for text drawing).
 *)

signature FONT =
  sig

    structure EXB : EXENE_BASE

    type font

  (* font drawing direction *)
    datatype font_draw_dir = FontLeftToRight | FontRightToLeft

  (* font properties *)
    datatype font_prop = FontProp of {
	name : EXB.atom,	  (* the name of the property *)
	value : Word32.word	  (* the property value: interpret according to the *)
				  (* property. *)
      }

  (* per-character font info *)
    datatype char_info = CharInfo of {
	left_bearing : int,
	right_bearing : int,
	char_wid : int,
	ascent : int,
	descent : int,
	attributes : word
      }

    exception FontNotFound
    exception NoCharInfo
    exception FontPropNotFound

    val openFont : EXB.display -> string -> font

    val fontPropertyOf : font -> EXB.atom -> Word32.word
    val fontInfoOf : font -> {
	    min_bounds : char_info,
	    max_bounds : char_info,
	    min_char : int,
	    max_char : int
	  }
    val charInfoOf : font -> int -> char_info
    val textWidth : font -> string -> int
    val charWidth : font -> char -> int
    val substrWidth : font -> (string * int * int) -> int
    val charPositions : font -> string -> int list
    val textExtents : font -> string -> {
	      dir : font_draw_dir,
	      font_ascent : int, font_descent : int,
	      overall_info : char_info
	    }
    val fontHt : font -> {ascent : int, descent : int}

  end (* FONT *)
