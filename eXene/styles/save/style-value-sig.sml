(* style-value-sig.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

signature STYLE_VALUE =
  sig

    datatype style_type
      = ST_Str
      | ST_Int
      | ST_Real
      | ST_Bool
      | ST_Font
      | ST_Color

    datatype style_value
      = SV_Str of string
      | SV_Int of int
      | SV_Real of real
      | SV_Bool of bool
      | SV_Font of EXeneBase.font
      | SV_Color of EXeneBase.color

    exception BadStyleValue

    val cvtStyleValue : EXeneBase.screen -> (string * style_type) -> style_value
	(* convert a string to the specified kind of style value; this raises
	 * BadStyleValue if the string has the wrong format.
	 *)

  end; (* STYLE_VALUE *)
