(* text-widget-sig.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * A simple text widget: currently this only supports one fixed-width font (8x13).
 *)

signature TEXT_WIDGET =
  sig

    datatype char_coord = ChrCrd of {col : int, row : int}

    type text_widget

    val mkTextWidget : Widget.root -> {rows : int, cols : int} -> text_widget

    val widgetOf   : text_widget -> Widget.widget
    val charSizeOf : text_widget -> {rows : int, cols : int}
    val sizeOf     : text_widget -> Geometry.size

    val ptToCoord   : text_widget -> Geometry.point -> char_coord
    val coordToRect : text_widget -> char_coord -> Geometry.rect

    val scrollUp   : text_widget -> {from : int, nlines : int} -> unit
    val scrollDown : text_widget -> {from : int, nlines : int} -> unit

    val writeText : text_widget -> {at: char_coord, text : string} -> unit
    val highlightText : text_widget -> {at: char_coord, text : string} -> unit

    val insertLn : text_widget -> {lnum : int, text : string} -> unit
    val insertText : text_widget -> {at: char_coord, text : string} -> unit
    val insertHighlightText : text_widget -> 
      {at: char_coord, text : string} -> unit

    val deleteLn : text_widget -> int -> unit
    val deleteLns : text_widget -> {lnum : int, nlines : int} -> unit
    val deleteChars : text_widget -> {at: char_coord, cnt : int} -> unit

    val clearToEOL : text_widget -> char_coord -> unit
    val clearToEOS : text_widget -> char_coord -> unit
    val clear      : text_widget -> unit

    val moveCursor : text_widget -> char_coord -> unit
    val cursorPos  : text_widget -> char_coord
    val cursorOn   : text_widget -> unit
    val cursorOff  : text_widget -> unit

  end (* TEXT_WIDGET *)
