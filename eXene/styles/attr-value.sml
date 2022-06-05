(* attr-value.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Types to add: FontList, StdCursor, Atom, Tile
 *)

structure AttrValue : ATTR_VALUE =
  struct

    datatype attr_type
      = AT_Str
      | AT_Int
      | AT_Real
      | AT_Bool
      | AT_Font
      | AT_Color

    datatype attr_value
      = AV_Str of string
      | AV_Int of int
      | AV_Real of real
      | AV_Bool of bool
      | AV_Font of EXeneBase.font
      | AV_Color of EXeneBase.color
      | AV_NoValue

    exception BadAttrValue

  (* strip leading and trailing whitespace from a string. *)
    fun strip s = let
	  fun front i = if (CType.isSpace(s, i)) then front(i+1) else i
	  fun back i = if (CType.isSpace(s, i-1)) then back(i-1) else i
	  val start = front 0
	  val len = back(size s) - start
	  in
	    if (len < size s)
	      then substring(s, start, len)
	      else s
	  end
	    handle _ => "" (* all white space *)

    fun cvtBool s = (case (strip s)
	   of ("true"|"yes"|"Yes"|"on"|"On") => true
	    | ("false"|"no"|"No"|"off"|"Off") => false
	    | _ => raise BadAttrValue
	  (* end case *))

    fun cvtInt s = let
	  val s = strip s
	  val start = if (CType.isDigit(s, 0)) then 0 else 1
	  in
	    if (ordof(s, start) = ord "0")
	      then (case ordof(s, start+1)
		 of (88 (* #"X" *) | 120 (* #"x" *)) => StringCvt.xatoi s
		  | _ => StringCvt.oatoi s
		(* end case *))
	      else StringCvt.atoi s
	  end
	    handle _ => raise BadAttrValue

    fun cvtReal s = (#1 (StringCvt.strToReal(strip s, 0)))
	  handle _ => raise BadAttrValue

  (* convert a string to a color_spec *)
    fun cvtColorSpec s = let
	  val s = strip s
	  fun split n = let
		val shift = (4-n)*4
		fun extract (s, i) =
		      Bits.lshift(StringCvt.xatoi(substring(s, i, n)), shift)
		in
		  EXeneBase.CMS_RGB{
		      red = debug("red", extract(s, 1)),
		      green = debug("green", extract(s, 1+n)),
		      blue = debug("blue", extract(s, 1+n+n))
		    }
		end
	  in
	    if (ordof(s, 0) = 35 (* #"#" *))
	      then (case (size s)
		 of 4 => split 1	(* "#RGB" *)
		  | 7 => split 2	(* "#RRGGBB" *)
		  | 10 => split 3	(* "#RRRGGGBBB" *)
		  | 13 => split 4	(* "#RRRRGGGGBBBB" *)
		  | _ => raise BadAttrValue
		(* end case *))
	      else EXeneBase.CMS_Name s
	  end
	    handle _ => raise BadAttrValue

  (* convert a string to the specified kind of style attribute value;
   * this raises BadAttrValue if the string has the wrong format.
   *)
    fun cvtAttrValue scr = let
	  val openFont = Font.openFont (EXeneBase.displayOfScr scr)
	  fun cvtFont s = (openFont(strip s)) handle _ => raise BadAttrValue
	  val colorOfScr = EXeneBase.colorOfScr scr
	  fun cvt (value, AT_Str) = AV_Str value
	    | cvt (value, AT_Int) = AV_Int(cvtInt value)
	    | cvt (value, AT_Real) = AV_Real(cvtReal value)
	    | cvt (value, AT_Bool) = AV_Bool(cvtBool value)
	    | cvt (value, AT_Font) = AV_Font(cvtFont value)
	    | cvt (value, AT_Color) = AV_Color(colorOfScr(cvtColorSpec value))
	  in
	    cvt
	  end (* cvtAttrValue *)

    fun getInt (AV_Int i) = i | getInt _ = raise BadAttrValue
    fun getReal (AV_Real r) = r | getReal _ = raise BadAttrValue
    fun getBool (AV_Bool b) = b | getBool _ = raise BadAttrValue
    fun getString (AV_Str s) = s | getString _ = raise BadAttrValue
    fun getColor (AV_Color c) = c | getColor _ = raise BadAttrValue
    fun getFont (AV_Font f) = f | getFont _ = raise BadAttrValue

    fun getIntOpt (AV_Int i) = SOME i | getIntOpt _ = NONE
    fun getRealOpt (AV_Real r) = SOME r | getRealOpt _ = NONE
    fun getBoolOpt (AV_Bool b) = SOME b | getBoolOpt _ = NONE
    fun getStringOpt (AV_Str s) = SOME s | getStringOpt _ = NONE
    fun getColorOpt (AV_Color c) = SOME c | getColorOpt _ = NONE
    fun getFontOpt (AV_Font f) = SOME f | getFontOpt _ = NONE

  end; (* AttrValue *)
