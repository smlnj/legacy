(* text-display-sig.sml
 *)

signature TEXT_DISPLAY =
  sig
    structure W : WIDGET
    structure TextPool : TEXT_POOL

    type text_display

    type typeball

    datatype typeball_val
      = TBV_Font of Font.font			(* font *)
      | TBV_Lineheight of int			(* total height of line *)
      | TBV_Ascent of int			(* height of line above baseline *)
      | TBV_Underline of bool			(* underline mode *)
      | TBV_Foregrnd of W.EXB.color_spec	(* forground (text) color *)
      | TBV_Backgrnd of W.EXB.color_spec	(* background color *)
      | TBV_Undergrnd of W.EXB.color_spec	(* color of underline *)

    datatype text_elem
      = Text of {tb : typeball, text : string}
      | Fill of {tb : typeball, chrWid : int, pixWid : int}

    datatype char_coord = CC of {row:int, col:int}

    sharing type TextPool.TextCanvas.typeball = typeball
    sharing type TextPool.TextCanvas.typeball_val = typeball_val
    sharing type TextPool.TextCanvas.text_elem = text_elem
    sharing type TextPool.TextCanvas.char_coord = char_coord

    val mkTextDisplay : {
	    canvas : TextPool.TextCanvas.text_canvas,
	    text : TextPool.text_pool,
	    size : Geometry.size
	  } -> text_display

    val resize : (text_display * Geometry.size) -> unit
	(* update the size of the display *)

    val sizeOf : text_display -> Geometry.size
	(* return size *)

    val mkTypeBall : (text_display * typeball_val list) -> typeball
	(* return a typeball for the display *)

    val defaultTypeBall : text_display -> typeball
	(* return the default typeball for the display. *)

    val copyTypeBall : (typeball * typeball_val list) -> typeball
	(* copy a typeball, updating some attributes *)

    val scrollV : text_display
	  -> {from : int, to : int, ht : int}
	    -> {vacated : Geometry.rect, damage : Geometry.rect list CML.event}
	(* Scroll a region vertically, returning the vacated rectangle and a list
	 * of damaged rectangles that must be redrawn.  The region coordinates
	 * are in pixels: "from" is the y-coord of the top of the region; "ht"
	 * is the height of the region; and "to" is the y-coord of the new
	 * top of the region.
	 *)

    val scrollH : text_display
	  -> {from : int, to : int, wid : int}
	    -> {vacated : Geometry.rect, damage : Geometry.rect list CML.event}
	(* Scroll a region horizontally, returning the vacated rectangle and a
	 * list of damaged rectangles that must be redrawn.  The region coordinates
	 * are in pixels: "from" is the x-coord of the l.h.s. of the region;
	 * "wid" is the width of the region; and "to" is the x-coord of new
	 * l.h.s. of the region.
	 *)

    val scrollLine : text_display
	  -> {from : char_coord, to : int, wid : int}
	    -> {vacated : Geometry.rect, damage : Geometry.rect list CML.event}
	(* Scroll the contents of a line horizontally. *)

    val scrollUp : text_display -> int
	  -> {vacated : Geometry.rect, damage : Geometry.rect list CML.event}
	(* Scroll the text vertically so that the specified row is at the
	 * top of the display (i.e., scroll the text up by the specified number
	 * of rows.
	 *)

    val scrollDown : text_display -> int
	  -> {vacated : Geometry.rect, damage : Geometry.rect list CML.event}
	(* Scroll the text vertically so that the top of the screen occupies
	 * the specified row (i.e., scroll the text down by the specified number
	 * of rows).
	 *)

    val clearRect : text_display -> Geometry.rect -> unit
	(* clear the specified rectangle *)

    val clearToEOL : text_display -> char_coord -> unit
	(* clear from the character coordinate to the end of its line *)

    val clearLines : text_display -> {start : int, stop : int} -> unit
	(* clear the lines [start..stop] *)

    val clearArea : text_display -> {start : char_coord, stop : char_coord} -> unit
	(* Clear the area from the coordinate start to the coordinate stop. *)

    val redraw : text_display -> Geometry.rect list -> unit
	(* redraw the damaged region *)

  end; (* signature TEXT_DISPLAY *)
