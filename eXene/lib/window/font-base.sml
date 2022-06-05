(* font-base.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * The basic definitions for fonts.
 *)

structure FontBase =
  struct
    local
      open XProtTypes
    in

    datatype font_info
      = FINFO8 of {
	  min_bounds : char_info,
	  max_bounds : char_info,
	  min_char : int,
	  max_char : int,
	  default_char : int,
	  draw_dir : font_draw_dir,
	  all_chars_exist : bool,
	  font_ascent : int,
	  font_descent : int,
	  properties : font_prop list,
	  char_info : int -> char_info
	}
      | FINFO16 of {
	  min_bounds : char_info,
	  max_bounds : char_info,
	  min_char : int,
	  max_char : int,
	  default_char : int,
	  draw_dir : font_draw_dir,
	  min_byte1 : int,
	  max_byte1 : int,
	  all_chars_exist : bool,
	  font_ascent : int,
	  font_descent : int,
	  properties : font_prop list,
	  char_info : int -> char_info
	}

    datatype font = FONT of {
	id : XProtTypes.font_id,
	xdpy : XDisplay.xdisplay,	(* the display this font belongs to. *)
	info : font_info
      }

  (* identity test *)
    fun sameFont (
	  FONT{id=id1, xdpy=XDisplay.XDPY{conn=c1, ...}, ...},
	  FONT{id=id2, xdpy=XDisplay.XDPY{conn=c2, ...}, ...}
        ) = ((id1 = id2) andalso XIo.sameConn(c1, c2))

    exception NoCharInfo  (* raised by the char_info functions *)

    exception FontPropNotFound

  (* find the given property of a font *)
    fun fontPropertyOf (FONT{info, ...}) atom = let
	  val props = (case info
	       of FINFO8{properties, ...} => properties
		| FINFO16{properties, ...} => properties)
	  fun look [] = raise FontPropNotFound
	    | look ((FontProp{name, value})::r) =
		if (name = atom) then value else look r
	  in
	    look props
	  end

  (* return the non-character specific info for the font *)
    fun fontInfoOf (FONT{info=(FINFO8 x), ...}) = {
	    min_bounds = #min_bounds x,
	    max_bounds = #max_bounds x,
	    min_char = #min_char x,
	    max_char = #max_char x
	  }
      | fontInfoOf (FONT{info=(FINFO16 x), ...}) = {
	    min_bounds = #min_bounds x,
	    max_bounds = #max_bounds x,
	    min_char = #min_char x,
	    max_char = #max_char x
	  }

  (* return the character info about a character in the given font *)
    fun charInfoOf (FONT{info, ...}) = (
	  case info
	   of FINFO8{char_info, ...} => char_info
	    | FINFO16{char_info, ...} => char_info)

  (* Return the width of a character in the given font *)
    fun charWidth font = let
	  val infoOf = charInfoOf font
	  fun width c = let
		val CharInfo{char_wid, ...} = infoOf(Char.ord c)
		in
		  char_wid
		end handle _ => 0
	  in
	    width
	  end

  (* Return the width of a string in the given font *)
    fun textWidth font = let
	  val charWid = charWidth font
	  fun width s = let
		val len = String.size s
		fun width' (w, i) = if (i < len)
		      then width' (w + charWid(String.sub(s, i)), i+1)
		      else w
		in
		  width' (0, 0)
		end
	  in
	    width
	  end

  (* Return the width of the substring s[i..i+n-1] in the given font *)
    fun substrWidth font = let
	  val charWid = charWidth font
	  fun width (s, i, n) = let
		val len = Int.min(size s, i+n)
		fun width' (w, i) = if (i < len)
		      then width' (w + charWid(String.sub(s, i)), i+1)
		      else w
		in
		  width' (0, i)
		end
	  in
	    width
	  end

  (* Return the per-character width of a string in the given font.
   * For a string of length n, this returns a list of length n+1.
   *)
    fun charPositions font = let
	  val charWid = charWidth font
	  fun positions s = let
		val len = String.size s
		fun width (l, w, i) = if (i < len)
		      then let
			val wid = w + charWid (String.sub(s, i))
			in
			  width (wid::l, wid, i + 1)
			end
		      else rev l
		in
		  width ([0], 0, 0)
		end
	  in
	    positions
	  end

  (* Return the extents of the given string in the given font, which is a record
   * with the fields
   *     dir : font_draw_dir,
   *     font_ascent : int,
   *     font_descent : int,
   *     overall_info : char_info
   * The dir, font_ascent and font_descent fields give the font properties.  The
   * overall_info field describes the bounding box of the string if written at
   * the origin. The upper left corner of the bounding box is at
   *    (left_bearing, -ascent)
   * the dimensions of the bounding box are
   *    (right_bearing - left_bearing, ascent + descent).
   * The width is the sum of the widths of all the characters in the string. 
   *)
    fun textExtents (FONT{info, ...}) s = let
	  val (infoOf, dir, font_ascent, font_descent) = (case info
	       of FINFO8{char_info, draw_dir, font_ascent, font_descent, ...} =>
		    (char_info, draw_dir, font_ascent, font_descent)
		| FINFO16{char_info, draw_dir, font_ascent, font_descent, ...} =>
		    (char_info, draw_dir, font_ascent, font_descent))
	  val len = String.size s
	  fun min (a : int, b) = if (a < b) then a else b
	  fun max (a : int, b) = if (a > b) then a else b
	  fun ordOf i = Char.ord(String.sub(s, i))
	  fun getInfo i = (SOME(infoOf (ordOf i))) handle _ => NONE
	  fun accumNone i = if (i < len)
		then (case (getInfo i)
		 of NONE => accumNone(i+1)
		  | SOME(CharInfo info) => accum({
			ascent = #ascent info,
			descent = #descent info,
			lbear = #left_bearing info,
			rbear = #right_bearing info,
			wid = #char_wid info
		      }, i+1))
		else {ascent=0, descent=0, lbear=0, rbear=0, wid=0}
	  and accum (arg as {ascent, descent, lbear, rbear, wid}, i) = if (i < len)
		then (case (getInfo i)
		 of NONE => accum(arg, i+1)
		  | SOME(CharInfo info) => accum({
			ascent = max(ascent, #ascent info),
			descent = max(descent, #descent info),
			lbear = min(lbear, wid + #left_bearing info),
			rbear = max(rbear, wid + #right_bearing info),
			wid = wid + (#char_wid info)
		      }, i+1))
		else arg
	  val {ascent, descent, lbear, rbear, wid} = accumNone 0
	  in {
	    dir = dir,
	    font_ascent = font_ascent,
	    font_descent = font_descent,
	    overall_info = CharInfo{
		ascent = ascent,
		descent = descent,
		char_wid = wid,
		left_bearing = lbear,
		right_bearing = rbear,
		attributes = 0w0
	      }
	  } end

    fun fontHt (FONT{info=FINFO8{font_ascent, font_descent, ...}, ...}) =
	  {ascent = font_ascent, descent = font_descent}
      | fontHt (FONT{info=FINFO16{font_ascent, font_descent, ...}, ...}) =
	  {ascent = font_ascent, descent = font_descent}

    end (* local *)
  end (* Font *)
