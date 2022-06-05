(* attrs.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Types to add: FontList, Atom
 *
 * Alley Stoughton, April 2006: added shifting when forming CMS_RGB values
 *)

structure Attrs : ATTRS =
  struct

    structure F = Format
    structure SS = Substring

    type attr_name = Quark.quark

    val attr_active                     = Quark.quark "active"
    val attr_aspect                     = Quark.quark "aspect"
    val attr_arrowDir                   = Quark.quark "arrowDir"
    val attr_background                 = Quark.quark "background"
    val attr_borderColor                = Quark.quark "borderColor"
    val attr_borderWidth                = Quark.quark "borderWidth"
    val attr_color                      = Quark.quark "color"
    val attr_current                    = Quark.quark "current"
    val attr_cursor                     = Quark.quark "cursor"
    val attr_font                       = Quark.quark "font"
    val attr_fontList                   = Quark.quark "fontList"
    val attr_fontSize                   = Quark.quark "fontSize"
    val attr_foreground                 = Quark.quark "foreground"
    val attr_fromValue                  = Quark.quark "fromValue"
    val attr_gravity                    = Quark.quark "gravity"
    val attr_halign                     = Quark.quark "halign"
    val attr_height                     = Quark.quark "height"
    val attr_iconName                   = Quark.quark "iconName"
    val attr_isActive                   = Quark.quark "isActive"
    val attr_isSet                      = Quark.quark "isSet"
    val attr_isVertical                 = Quark.quark "isVertical"
    val attr_label                      = Quark.quark "label"
    val attr_length                     = Quark.quark "length"
    val attr_padx                       = Quark.quark "padx"
    val attr_pady                       = Quark.quark "pady"
    val attr_readyColor                 = Quark.quark "readyColor"
    val attr_relief                     = Quark.quark "relief"
    val attr_repeatDelay                = Quark.quark "repeatDelay"
    val attr_repeatInterval             = Quark.quark "repeatInterval"
    val attr_rounded                    = Quark.quark "rounded"
    val attr_scale                      = Quark.quark "scale"
    val attr_selectColor		= Quark.quark "selectColor"
    val attr_selectBackground           = Quark.quark "selectBackground"
    val attr_selectBorderWidth          = Quark.quark "selectBorderWidth"
    val attr_selectForeground           = Quark.quark "selectForeground"
    val attr_showValue                  = Quark.quark "showValue"
    val attr_state                      = Quark.quark "state"
    val attr_text                       = Quark.quark "text"
    val attr_thumbLength                = Quark.quark "thumbLength"
    val attr_tickInterval               = Quark.quark "tickInterval"
    val attr_tile                       = Quark.quark "tile"
    val attr_title                      = Quark.quark "title"
    val attr_toValue                    = Quark.quark "toValue"
    val attr_type                       = Quark.quark "type"
    val attr_valign                     = Quark.quark "valign"
    val attr_width                      = Quark.quark "width"

    datatype attr_type
      = AT_Str
      | AT_Int
      | AT_Real
      | AT_Bool
      | AT_Font
      | AT_Color
      | AT_ColorSpec
      | AT_Tile
      | AT_Cursor
      | AT_HAlign
      | AT_VAlign
      | AT_Relief
      | AT_ArrowDir
      | AT_Gravity

    datatype attr_value
      = AV_Str of string
      | AV_Int of int
      | AV_Real of real
      | AV_Bool of bool
      | AV_Font of EXeneBase.font
      | AV_Color of EXeneBase.color
      | AV_ColorSpec of EXeneBase.color_spec
      | AV_Tile of EXeneBase.tile
      | AV_Cursor of EXeneBase.cursor
      | AV_HAlign of WidgetBase.halign
      | AV_VAlign of WidgetBase.valign
      | AV_Relief of ThreeD.relief
      | AV_ArrowDir of WidgetBase.arrow_dir
      | AV_Gravity of WidgetBase.gravity
      | AV_NoValue

    type attr_ctxt = {
	scr : EXeneBase.screen,
	tilef : string -> EXeneBase.tile
      }

    exception BadAttrValue
    exception NoConversion

    fun sameType (AV_Str _, AT_Str) = true
      | sameType (AV_Int _, AT_Int) = true
      | sameType (AV_Real _, AT_Real) = true
      | sameType (AV_Bool _, AT_Bool) = true
      | sameType (AV_Font _, AT_Font) = true
      | sameType (AV_Color _, AT_Color) = true
      | sameType (AV_Tile _, AT_Tile) = true
      | sameType (AV_Cursor _, AT_Cursor) = true
      | sameType (AV_HAlign _, AT_HAlign) = true
      | sameType (AV_VAlign _, AT_VAlign) = true
      | sameType (AV_Relief _, AT_Relief) = true
      | sameType _ = false

    fun sameValue (AV_Str a, AV_Str b) = a = b
      | sameValue (AV_Int a, AV_Int b) = a = b
      | sameValue (AV_Real a, AV_Real b) = Real.==(a, b)
      | sameValue (AV_Bool a, AV_Bool b) = a = b
      | sameValue (AV_Font a, AV_Font b) = EXeneBase.sameFont(a,b)
      | sameValue (AV_Color a, AV_Color b) = EXeneBase.sameColor(a,b)
      | sameValue (AV_Tile a, AV_Tile b) = EXeneBase.sameTile(a,b)
      | sameValue (AV_Cursor a, AV_Cursor b) = EXeneBase.sameCursor(a,b)
      | sameValue (AV_HAlign a, AV_HAlign b) = a = b
      | sameValue (AV_VAlign a, AV_VAlign b) = a = b
      | sameValue (AV_Relief a, AV_Relief b) = a = b
      | sameValue (AV_NoValue,AV_NoValue) = true
      | sameValue _ = false

  (* strip leading and trailing whitespace from a string. *)
    fun sstrip s = 
          SS.dropr Char.isSpace (SS.dropl Char.isSpace (SS.full s))
    fun strip s = SS.string (sstrip s)
    fun skipWS s = SS.dropl Char.isSpace (SS.full s)

    fun cvtBool s = (case (strip s)
	   of ("true"|"yes"|"Yes"|"on"|"On") => true
	    | ("false"|"no"|"No"|"off"|"Off") => false
	    | _ => raise BadAttrValue
	  (* end case *))

    fun cvtInt s = let
	  val s = StringCvt.skipWS SS.getc (SS.full s)
	  val start = if Char.isDigit(SS.sub (s, 0)) then 0 else 1
	  val rad = if (SS.sub(s, start) = #"0")
	              then (case SS.sub(s, start+1)
		         of (#"X" | #"x") => StringCvt.HEX
		          | _ => StringCvt.OCT
		        (* end case *))
	              else StringCvt.DEC
	  in
            case Int.scan rad SS.getc s of
              NONE => raise BadAttrValue
            | SOME (n, _) => n
	  end
	    handle _ => raise BadAttrValue

    fun cvtReal s = (#1 (valOf (Real.scan SS.getc (skipWS s))))
	  handle _ => raise BadAttrValue

  (* convert a string to a color_spec *)
    fun cvtColorSpec s = let
	  val s = sstrip s
	  fun split n = let
		fun extract i =
                      #1(valOf(Word.scan StringCvt.HEX SS.getc (SS.slice(s, i, SOME n))))
                (* Alley: shift originally was missing *)
                fun shift m = Word.<<(m, Word.fromInt(16 - n * 4))
		in
		  EXeneBase.CMS_RGB{
		      red = shift(extract 1),
		      green = shift(extract(1+n)),
		      blue = shift(extract(1+n+n))
		    }
		end
	  in
	    if (SS.sub(s, 0) = #"#")
	      then (case (SS.size s)
		 of 4 => split 1	(* "#RGB" *)
		  | 7 => split 2	(* "#RRGGBB" *)
		  | 10 => split 3	(* "#RRRGGGBBB" *)
		  | 13 => split 4	(* "#RRRRGGGGBBBB" *)
		  | _ => raise BadAttrValue
		(* end case *))
	      else EXeneBase.CMS_Name (SS.string s)
	  end
	    handle _ => raise BadAttrValue

  (* convert between strings and  horizontal alignments *)
    fun cvtHAlign "left" = WidgetBase.HLeft
      | cvtHAlign "right" = WidgetBase.HRight
      | cvtHAlign "center" = WidgetBase.HCenter
      | cvtHAlign _ = WidgetBase.HCenter            (* ??? *)

    fun halignToStr WidgetBase.HLeft = "left"
      | halignToStr WidgetBase.HRight = "right"
      | halignToStr WidgetBase.HCenter = "center"

  (* convert between strings and vertical alignments *)
    fun cvtVAlign "top" = WidgetBase.VTop
      | cvtVAlign "bottom" = WidgetBase.VBottom
      | cvtVAlign "center" = WidgetBase.VCenter
      | cvtVAlign _ = WidgetBase.VCenter            (* ??? *)

    fun valignToStr WidgetBase.VTop = "top"
      | valignToStr WidgetBase.VBottom = "bottom"
      | valignToStr WidgetBase.VCenter = "center"

  (* convert strings and reliefs *)
    fun cvtRelief "raised" = ThreeD.Raised
      | cvtRelief "ridge" = ThreeD.Ridge
      | cvtRelief "groove" = ThreeD.Groove
      | cvtRelief "flat" = ThreeD.Flat
      | cvtRelief "sunken" = ThreeD.Sunken
      | cvtRelief _ = ThreeD.Sunken        (* ??? *)

    fun reliefToStr (ThreeD.Flat) = "flat"
      | reliefToStr (ThreeD.Raised) = "raised"
      | reliefToStr (ThreeD.Ridge) = "ridge"
      | reliefToStr (ThreeD.Groove) = "groove"
      | reliefToStr (ThreeD.Sunken) = "sunken"

  (* convert strings and arrow directions *)
    fun cvtArrowDir "down" = WidgetBase.AD_Down
      | cvtArrowDir "left" = WidgetBase.AD_Left
      | cvtArrowDir "right" = WidgetBase.AD_Right
      | cvtArrowDir _ = WidgetBase.AD_Up (* ??? *)

    fun arrowDirToStr WidgetBase.AD_Down = "down"
      | arrowDirToStr WidgetBase.AD_Left = "left"
      | arrowDirToStr WidgetBase.AD_Right = "right"
      | arrowDirToStr WidgetBase.AD_Up = "up"

  (* convert strings and gravity *)
    fun cvtGravity "north" = WidgetBase.North
      | cvtGravity "south" = WidgetBase.South
      | cvtGravity "east" = WidgetBase.East
      | cvtGravity "west" = WidgetBase.West
      | cvtGravity "northeast" = WidgetBase.NorthEast
      | cvtGravity "northwest" = WidgetBase.NorthWest
      | cvtGravity "southeast" = WidgetBase.SouthEast
      | cvtGravity "southwest" = WidgetBase.SouthWest
      | cvtGravity _ = WidgetBase.Center (* ??? *)

    fun gravityToStr WidgetBase.North = "north"
      | gravityToStr WidgetBase.South = "south"
      | gravityToStr WidgetBase.East = "east"
      | gravityToStr WidgetBase.West = "west"
      | gravityToStr WidgetBase.NorthEast = "northeast"
      | gravityToStr WidgetBase.NorthWest = "northwest"
      | gravityToStr WidgetBase.SouthEast = "southeast"
      | gravityToStr WidgetBase.SouthWest = "southwest"
      | gravityToStr WidgetBase.Center = "center"

    val colorFmt = F.format "#%04x%04x%04x"
    fun colorToStr c = let
          val EXeneBase.RGB{red,blue,green} = EXeneBase.rgbOfColor c
          in colorFmt [F.WORD red, F.WORD green, F.WORD blue] end
    fun colorSpecToStr (EXeneBase.CMS_RGB{red,green,blue}) =
          colorFmt [F.WORD red, F.WORD green, F.WORD blue]
      | colorSpecToStr _ = raise NoConversion

  (* convert a string to a std_cursor - FIX: better encoding *)
    fun cvtStdCursor name =
          case strip name
           of "x_cursor" => StdCursor.x_cursor
            | "arrow" => StdCursor.arrow
            | "based_arrow_down" => StdCursor.based_arrow_down
            | "based_arrow_up" => StdCursor.based_arrow_up
            | "boat" => StdCursor.boat
            | "bogosity" => StdCursor.bogosity
            | "bottom_left_corner" => StdCursor.bottom_left_corner
            | "bottom_right_corner" => StdCursor.bottom_right_corner
            | "bottom_side" => StdCursor.bottom_side
            | "bottom_tee" => StdCursor.bottom_tee
            | "box_spiral" => StdCursor.box_spiral
            | "center_ptr" => StdCursor.center_ptr
            | "circle" => StdCursor.circle
            | "clock" => StdCursor.clock
            | "coffee_mug" => StdCursor.coffee_mug
            | "cross" => StdCursor.cross
            | "cross_reverse" => StdCursor.cross_reverse
            | "crosshair" => StdCursor.crosshair
            | "diamond_cross" => StdCursor.diamond_cross
            | "dot" => StdCursor.dot
            | "dotbox" => StdCursor.dotbox
            | "double_arrow" => StdCursor.double_arrow
            | "draft_large" => StdCursor.draft_large
            | "draft_small" => StdCursor.draft_small
            | "draped_box" => StdCursor.draped_box
            | "exchange" => StdCursor.exchange
            | "fleur" => StdCursor.fleur
            | "gobbler" => StdCursor.gobbler
            | "gumby" => StdCursor.gumby
            | "hand1" => StdCursor.hand1
            | "hand2" => StdCursor.hand2
            | "heart" => StdCursor.heart
            | "icon" => StdCursor.icon
            | "iron_cross" => StdCursor.iron_cross
            | "left_ptr" => StdCursor.left_ptr
            | "left_side" => StdCursor.left_side
            | "left_tee" => StdCursor.left_tee
            | "leftbutton" => StdCursor.leftbutton
            | "ll_angle" => StdCursor.ll_angle
            | "lr_angle" => StdCursor.lr_angle
            | "man" => StdCursor.man
            | "middlebutton" => StdCursor.middlebutton
            | "mouse" => StdCursor.mouse
            | "pencil" => StdCursor.pencil
            | "pirate" => StdCursor.pirate
            | "plus" => StdCursor.plus
            | "question_arrow" => StdCursor.question_arrow
            | "right_ptr" => StdCursor.right_ptr
            | "right_side" => StdCursor.right_side
            | "right_tee" => StdCursor.right_tee
            | "rightbutton" => StdCursor.rightbutton
            | "rtl_logo" => StdCursor.rtl_logo
            | "sailboat" => StdCursor.sailboat
            | "sb_down_arrow" => StdCursor.sb_down_arrow
            | "sb_h_double_arrow" => StdCursor.sb_h_double_arrow
            | "sb_left_arrow" => StdCursor.sb_left_arrow
            | "sb_right_arrow" => StdCursor.sb_right_arrow
            | "sb_up_arrow" => StdCursor.sb_up_arrow
            | "sb_v_double_arrow" => StdCursor.sb_v_double_arrow
            | "shuttle" => StdCursor.shuttle
            | "sizing" => StdCursor.sizing
            | "spider" => StdCursor.spider
            | "spraycan" => StdCursor.spraycan
            | "star" => StdCursor.star
            | "target" => StdCursor.target
            | "tcross" => StdCursor.tcross
            | "top_left_arrow" => StdCursor.top_left_arrow
            | "top_left_corner" => StdCursor.top_left_corner
            | "top_right_corner" => StdCursor.top_right_corner
            | "top_side" => StdCursor.top_side
            | "top_tee" => StdCursor.top_tee
            | "trek" => StdCursor.trek
            | "ul_angle" => StdCursor.ul_angle
            | "umbrella" => StdCursor.umbrella
            | "ur_angle" => StdCursor.ur_angle
            | "watch" => StdCursor.watch
            | "xterm" => StdCursor.xterm
            | _ => raise BadAttrValue

  (* convert a string to the specified kind of style attribute value;
   * this raises BadAttrValue if the string has the wrong format.
   *)
    fun cvtString {scr,tilef} = let
          open EXeneBase
	  val openFont = Font.openFont (displayOfScr scr)
	  fun cvtTile s = (tilef (strip s)) handle _ => raise BadAttrValue
	  fun cvtFont s = (openFont(strip s)) handle _ => raise BadAttrValue
	  fun cvtCursor s = (stdCursor (displayOfScr scr) (cvtStdCursor s)) 
                              handle _ => raise BadAttrValue
	  val colorOfScr = EXeneBase.colorOfScr scr
	  fun cvt (value, AT_Str) = AV_Str value
	    | cvt (value, AT_Int) = AV_Int(cvtInt value)
	    | cvt (value, AT_Real) = AV_Real(cvtReal value)
	    | cvt (value, AT_Bool) = AV_Bool(cvtBool value)
	    | cvt (value, AT_Font) = AV_Font(cvtFont value)
	    | cvt (value, AT_Color) = AV_Color(colorOfScr(cvtColorSpec value))
	    | cvt (value, AT_ColorSpec) = AV_ColorSpec(cvtColorSpec value)
	    | cvt (value, AT_Tile) = AV_Tile(cvtTile value)
	    | cvt (value, AT_Cursor) = AV_Cursor(cvtCursor value)
	    | cvt (value, AT_HAlign) = AV_HAlign(cvtHAlign value)
	    | cvt (value, AT_VAlign) = AV_VAlign(cvtVAlign value)
	    | cvt (value, AT_Relief) = AV_Relief(cvtRelief value)
	    | cvt (value, AT_ArrowDir) = AV_ArrowDir(cvtArrowDir value)
	    | cvt (value, AT_Gravity) = AV_Gravity(cvtGravity value)
	  in
	    cvt
	  end (* cvtString *)

    fun mkString (AV_Str s) = s
      | mkString (AV_Int i) = Int.toString i
      | mkString (AV_Real r) = Real.fmt (StringCvt.SCI (SOME 6)) r
      | mkString (AV_Bool b) = Bool.toString b
      | mkString (AV_Color c) = colorToStr c
      | mkString (AV_ColorSpec c) = colorSpecToStr c
      | mkString (AV_HAlign a) = halignToStr a
      | mkString (AV_VAlign a) = valignToStr a
      | mkString (AV_Relief r) = reliefToStr r
      | mkString (AV_ArrowDir a) = arrowDirToStr a
      | mkString (AV_Gravity a) = gravityToStr a
      | mkString (AV_NoValue) = "NoValue"
      | mkString  _ = raise NoConversion

    fun cvtAttrValue (ctxt as {scr,...}) = let
          open EXeneBase
          val cvtString = cvtString ctxt
	  fun cvtCursor sc = (stdCursor (displayOfScr scr) sc) 
                                handle _ => raise BadAttrValue
	  fun cvt (AV_Str s, ty) = cvtString (s,ty)
	    | cvt (v, AT_Str) = AV_Str(mkString v)
	    | cvt (v as AV_Int _, AT_Int) = v
	    | cvt (AV_Int i, AT_Real) = AV_Real(real i)
	    | cvt (v as AV_Real _, AT_Real) = v
	    | cvt (AV_Real r, AT_Int) = AV_Int (Real.trunc r)  (* ??? *)
	    | cvt (v as AV_Bool _, AT_Bool) = v
	    | cvt (v as AV_Font _, AT_Font) = v
	    | cvt (v as AV_Color _, AT_Color) = v
	    | cvt (v as AV_ColorSpec _, AT_ColorSpec) = v
	    | cvt (AV_ColorSpec c, AT_Color) = 
                AV_Color(EXeneBase.colorOfScr scr c)
	    | cvt (v as AV_Tile _, AT_Tile) = v
	    | cvt (v as AV_Cursor _, AT_Cursor) = v
	    | cvt (v as AV_HAlign _, AT_HAlign) = v
	    | cvt (v as AV_VAlign _, AT_VAlign) = v
	    | cvt (v as AV_Relief _, AT_Relief) = v
	    | cvt (v as AV_ArrowDir _, AT_ArrowDir) = v
	    | cvt (v as AV_Gravity _, AT_Gravity) = v
	    | cvt _  = raise NoConversion
	  in
	    cvt
	  end (* cvtAttrValue *)

    fun getInt (AV_Int i) = i | getInt _ = raise BadAttrValue
    fun getReal (AV_Real r) = r | getReal _ = raise BadAttrValue
    fun getBool (AV_Bool b) = b | getBool _ = raise BadAttrValue
    fun getString (AV_Str s) = s | getString _ = raise BadAttrValue
    fun getColor (AV_Color c) = c | getColor _ = raise BadAttrValue
    fun getColorSpec (AV_ColorSpec c) = c | getColorSpec _ = raise BadAttrValue
    fun getFont (AV_Font f) = f | getFont _ = raise BadAttrValue
    fun getTile (AV_Tile x) = x | getTile _ = raise BadAttrValue
    fun getCursor (AV_Cursor x) = x | getCursor _ = raise BadAttrValue
    fun getHAlign (AV_HAlign x) = x | getHAlign _ = raise BadAttrValue
    fun getVAlign (AV_VAlign x) = x | getVAlign _ = raise BadAttrValue
    fun getRelief (AV_Relief x) = x | getRelief _ = raise BadAttrValue
    fun getArrowDir (AV_ArrowDir x) = x | getArrowDir _ = raise BadAttrValue
    fun getGravity (AV_Gravity x) = x | getGravity _ = raise BadAttrValue

    fun wrap f v = (SOME(f v)) handle _ => NONE
    val getIntOpt = wrap getInt
    val getRealOpt = wrap getReal
    val getBoolOpt = wrap getBool
    val getStringOpt = wrap getString
    val getColorOpt = wrap getColor
    val getColorSpecOpt = wrap getColorSpec
    val getFontOpt = wrap getFont
    val getTileOpt = wrap getTile
    val getCursorOpt = wrap getCursor
    val getHAlignOpt = wrap getHAlign
    val getVAlignOpt = wrap getVAlign
    val getReliefOpt = wrap getRelief
    val getArrowDirOpt = wrap getArrowDir
    val getGravityOpt = wrap getGravity

  end (* Attrs *)
