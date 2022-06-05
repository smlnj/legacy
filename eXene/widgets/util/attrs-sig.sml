(* attrs-sig.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

signature ATTRS =
  sig
    type attr_ctxt

    type attr_name = Quark.quark

    val attr_active : attr_name         (* "active" *)
    val attr_aspect : attr_name         (* "aspect" *)
    val attr_arrowDir : attr_name       (* "arrowDir" *)
    val attr_background : attr_name     (* "background" *)
    val attr_borderColor : attr_name    (* "borderColor" *)
    val attr_borderWidth : attr_name    (* "borderWidth" *)
    val attr_color : attr_name          (* "color" *)
    val attr_current : attr_name        (* "current" *)
    val attr_cursor : attr_name         (* "cursor" *)
    val attr_font : attr_name           (* "font" *)
    val attr_fontList : attr_name       (* "fontList" *)
    val attr_fontSize : attr_name       (* "fontSize" *)
    val attr_foreground : attr_name     (* "foreground" *)
    val attr_fromValue : attr_name      (* "fromValue" *)
    val attr_gravity : attr_name        (* "gravity" *)
    val attr_halign : attr_name         (* "halign" *)
    val attr_height : attr_name         (* "height" *)
    val attr_iconName : attr_name       (* "iconName" *)
    val attr_isActive : attr_name       (* "isActive" *)
    val attr_isSet : attr_name          (* "isSet" *)
    val attr_isVertical : attr_name     (* "isVertical" *)
    val attr_label : attr_name          (* "label" *)
    val attr_length : attr_name         (* "length" *)
    val attr_padx : attr_name           (* "padx" *)
    val attr_pady : attr_name           (* "pady" *)
    val attr_readyColor : attr_name     (* "readyColor" *)
    val attr_relief : attr_name         (* "relief" *)
    val attr_repeatDelay : attr_name    (* "repeatDelay" *)
    val attr_repeatInterval : attr_name (* "repeatInterval" *)
    val attr_rounded : attr_name        (* "rounded" *)
    val attr_scale : attr_name          (* "scale" *)
    val attr_selectColor : attr_name    (* "selectColor" *)
    val attr_selectBackground : attr_name     (* "selectBackground" *)
    val attr_selectBorderWidth : attr_name    (* "selectBorderWidth" *)
    val attr_selectForeground : attr_name     (* "selectForeground" *)
    val attr_showValue : attr_name      (* "showValue" *)
    val attr_state : attr_name          (* "state" *)
    val attr_text : attr_name           (* "text" *)
    val attr_thumbLength : attr_name    (* "thumbLength" *)
    val attr_tickInterval : attr_name   (* "tickInterval" *)
    val attr_tile : attr_name           (* "tile" *)
    val attr_title : attr_name          (* "title" *)
    val attr_toValue : attr_name        (* "toValue" *)
    val attr_type : attr_name           (* "type" *)
    val attr_valign : attr_name         (* "valign" *)
    val attr_width : attr_name          (* "width" *)

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

    exception BadAttrValue
    exception NoConversion

    val cvtString : attr_ctxt -> (string * attr_type) -> attr_value
    val cvtAttrValue : attr_ctxt -> (attr_value * attr_type) -> attr_value
    val sameValue : attr_value * attr_value -> bool
    val sameType : attr_value * attr_type -> bool

    val getInt : attr_value -> int
    val getReal : attr_value -> real
    val getBool : attr_value -> bool
    val getString : attr_value -> string
    val getColor : attr_value -> EXeneBase.color
    val getColorSpec : attr_value -> EXeneBase.color_spec
    val getFont : attr_value -> EXeneBase.font
    val getTile : attr_value -> EXeneBase.tile
    val getCursor : attr_value -> EXeneBase.cursor
    val getHAlign : attr_value -> WidgetBase.halign
    val getVAlign : attr_value -> WidgetBase.valign
    val getRelief : attr_value -> ThreeD.relief
    val getArrowDir : attr_value -> WidgetBase.arrow_dir
    val getGravity : attr_value -> WidgetBase.gravity

    val getIntOpt : attr_value -> int option
    val getRealOpt : attr_value -> real option
    val getBoolOpt : attr_value -> bool option
    val getStringOpt : attr_value -> string option
    val getColorOpt : attr_value -> EXeneBase.color option
    val getColorSpecOpt : attr_value -> EXeneBase.color_spec option
    val getFontOpt : attr_value -> EXeneBase.font option
    val getTileOpt : attr_value -> EXeneBase.tile option
    val getCursorOpt : attr_value -> EXeneBase.cursor option
    val getHAlignOpt : attr_value -> WidgetBase.halign option
    val getVAlignOpt : attr_value -> WidgetBase.valign option
    val getReliefOpt : attr_value -> ThreeD.relief option
    val getGravityOpt : attr_value -> WidgetBase.gravity option

  end (* ATTRS *)
