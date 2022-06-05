(* attr-value-sig.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

signature ATTR_VALUE =
  sig

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

    val cvtAttrValue : EXeneBase.screen -> (string * attr_type) -> attr_value
	(* convert a string to the specified kind of style attribute value;
	 * this raises BadAttrValue if the string has the wrong format.
	 *)

    val getInt : attr_value -> int
    val getReal : attr_value -> real
    val getBool : attr_value -> bool
    val getString : attr_value -> string
    val getColor : attr_value -> EXeneBase.color
    val getFont : attr_value -> EXeneBase.font

    val getIntOpt : attr_value -> int option
    val getRealOpt : attr_value -> real option
    val getBoolOpt : attr_value -> bool option
    val getStringOpt : attr_value -> string option
    val getColorOpt : attr_value -> EXeneBase.color option
    val getFontOpt : attr_value -> EXeneBase.font option

  end; (* ATTR_VALUE *)
