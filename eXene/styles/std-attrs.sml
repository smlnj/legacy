(* std-attrs.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Predefined attribute names.
 *)

structure StdAttrs : sig

    type attr_name = Quark.quark

    val attr_font : attr_name		(* "font" *)
    val attr_fontList : attr_name	(* "fontList" *)
    val attr_color : attr_name		(* "color" *)
    val attr_width : attr_name		(* "width" *)
    val attr_height : attr_name		(* "height" *)
    val attr_background : attr_name	(* "background" *)
    val attr_foreground : attr_name	(* "foreground" *)
    val attr_borderWidth : attr_name	(* "borderWidth" *)
    val attr_borderColor : attr_name	(* "borderColor" *)
    val attr_label : attr_name		(* "label" *)
    val attr_title : attr_name		(* "title" *)
    val attr_cursor : attr_name		(* "cursor" *)

  end = struct

    structure Q = Quark

    type attr_name = Q.quark

    val attr_font		= Q.quark "font"
    val attr_fontList		= Q.quark "fontList"
    val attr_color		= Q.quark "color"
    val attr_width		= Q.quark "width"
    val attr_height		= Q.quark "height"
    val attr_background		= Q.quark "background"
    val attr_foreground		= Q.quark "foreground"
    val attr_borderWidth	= Q.quark "borderWidth"
    val attr_borderColor	= Q.quark "borderColor"
    val attr_label		= Q.quark "label"
    val attr_title		= Q.quark "title"
    val attr_cursor		= Q.quark "cursor"

  end; (* StdAttrs *)

