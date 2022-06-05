(* text-canvas-sig.sml
 *)

signature TEXT_CANVAS =
  sig
    structure W : WIDGET
      sharing W = Widget
    structure EXB : EXENE_BASE
      sharing EXB = EXeneBase

    type text_canvas
	(* a text canvas is a proto-widget for drawing text *)

    val mkTextCanvas : {
	    win : EXB.window,
	    size : Geometry.size,
	    font : Font.font,
	    foregrnd : EXB.color_spec option,
	    backgrnd : EXB.color_spec option
	  } -> text_canvas

    val clear : text_canvas -> unit
	(* clear a canvas to its background color *)

    type typeball
	(* specifies canvas, font, color, etc. for writing text *)

    datatype typeball_val
      = TBV_Font of Font.font		(* font *)
      | TBV_Lineheight of int		(* total height of line *)
      | TBV_Ascent of int		(* height of line above baseline *)
      | TBV_Underline of bool		(* underline mode *)
      | TBV_Foregrnd of EXB.color_spec	(* forground (text) color *)
      | TBV_Backgrnd of EXB.color_spec	(* background color *)
      | TBV_Undergrnd of EXB.color_spec	(* color of underline *)

    val mkTypeBall : (text_canvas * typeball_val list) -> typeball
	(* create a new typeball *)

    val defaultTypeBall : text_canvas -> typeball
	(* return the default typeball for the canvas *)

    val copyTypeBall : (typeball * typeball_val list) -> typeball
	(* copy a typeball, updating some attributes *)

    datatype text_elem
      = Text of {tb : typeball, text : string}
      | Fill of {tb : typeball, chrWid : int, pixWid : int}

    datatype char_coord = CC of {row:int, col:int}

    val pixWidthOf : text_elem -> int
	(* return the width (in pixels) of a text element *)

    val chrWidthOf : text_elem -> int
	(* return the width (in characters) of a text element *)

    val textWidth : typeball -> string -> int
	(* return the width of a text string using the given typeball *)

    val substr : (text_elem * int * int) -> text_elem
	(* return the substring of a text element *)

    val fontOf : typeball -> Font.font
	(* return the font of the typeball *)

    val blt : text_canvas
	  -> {dst_pos : Geometry.point, src_rect : Geometry.rect}
	    -> Geometry.rect list CML.event
	(* do a copyBlt on the canvas *)

    val clearRect : text_canvas -> Geometry.rect -> unit
	(* clear the specified rectangle to the background color *)

    val draw : {at : Geometry.point, elems : text_elem list} -> unit
    val drawText : typeball -> {at : Geometry.point, text: string} -> unit
    val drawFill : typeball -> {at : Geometry.point, wid : int} -> unit

(**
  (* Cursors *)
    datatype text_cursor
      = NoCursor
      | BoxCursor of ??
      | OutlineCursor of ??
      | CaretCursor of ??
      | BarCursor of ??
      | XtermCursor of ??
      | GlyphCursor of ??

    val setCursor : (text_canvas * text_cursor) -> unit
	(* set the type of the cursor *)

    val moveCursor : (text_canvas * char_coord) -> unit
	(* set the current cursor position *)

    val cursorOn : text_canvas -> unit
	(* enable display of the text cursor *)

    val cursorOff : text_canvas -> unit
	(* disable display of the text cursor *)
**)

  end; (* signature TEXT_CANVAS *)
