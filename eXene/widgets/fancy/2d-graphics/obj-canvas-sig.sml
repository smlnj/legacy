(* obj-canvas-sig.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

signature OBJ_CANVAS =
  sig

    structure W : WIDGET

  (* the different ways to specify a border; either an absolute number of
   * pixels, or proportional border specified in universal coordinate units.
   *)
    datatype border_spec
      = BORDER_None
      | BORDER_Pixel2D of (W.EXB.color * int)

  (* specification of a width; either in absolute pixels or in universal
   * coordinates.
   *)
    datatype width_spec
      = WID_Pixel of int

  (* specification of an arrow head *)
    datatype arrow_spec
      = NoArrow
      | Arrow of {len : real, wid : real}

    type object_id
    type 'a object_canvas

    type box = {minX : real, minY : real, maxX : real, maxY : real}

    val objCanvas : (W.root * W.view * W.arg list)
	  -> {
	      bbox : box option,
	      fontServer : ScalableFont.font_server,
	      sb : {hsb : Scrollbar.scrollbar, vsb : Scrollbar.scrollbar} option
	    } -> ('a object_canvas * Interact.in_env)

    val widgetOf : 'a object_canvas -> W.widget

    datatype gr_object
      = BOX of {
	  wid : real, ht : real,
	  fill : W.EXB.color option,
	  border : border_spec
	}
      | DIAMOND of {
	  wid : real, ht : real,
	  fill : W.EXB.color option,
	  border : border_spec
	}
      | ELLIPSE of {
	  wid : real, ht : real,
	  fill : W.EXB.color option,
	  border : border_spec
	}
      | LINE of {
	  pts : {x : real, y : real} list,
	  head : arrow_spec,
	  tail : arrow_spec,
	  wid : width_spec,
	  color : W.EXB.color
	}
      | TEXT of {
	  sz : int, style : ScalableFont.font_style,
	  align : (W.halign * W.valign),
	  color : W.EXB.color,
	  text : string
	}

    val placeObj : 'a object_canvas -> {
	    at : {x : real, y : real},
	    depth : int,
	    obj : gr_object,
	    info : 'a option
	  } -> object_id

    val drawLine : 'a object_canvas -> {
	    pts : {x : real, y : real} list,
	    depth : int,
	    wid : width_spec,
	    color : W.EXB.color
	  } -> object_id

    val drawLine' : 'a object_canvas -> {
	    pts : {x : real, y : real} list,
	    head : arrow_spec,
	    tail : arrow_spec,
	    depth : int,
	    wid : width_spec,
	    color : W.EXB.color
	  } -> object_id

    val delObj : 'a object_canvas -> object_id -> unit
	(* remove an object from the picture *)

    val delAll : 'a object_canvas -> unit
	(* delete all of the existing objects *)

    val update : 'a object_canvas -> unit
	(* update the display *)

    datatype pt_stream = ENTER | EXIT | PT of Geometry.point | END

    val trackObj : 'a object_canvas -> {
	    points : pt_stream CML.event,
	    obj : gr_object,
	    hotSpot : {x : real, y : real}
	  } -> {x : real, y : real} CML.event
	(* This is support for feedback.  Given a stream of pixel coordinates,
	 * track the points with the object.  The stream of points will be
	 * terminated with NONE, at which point the resulting event is enabled
	 * with the final position (in world coordinates).
	 *)

    val setCursor : 'a object_canvas -> W.EXB.cursor option -> unit
	(* set the cursor for the canvas *)

    val redraw : 'a object_canvas -> Geometry.rect list -> unit
	(* redraw damaged regions (specified as a list of rectangles) *)

    val resize : 'a object_canvas -> Geometry.rect -> unit
	(* resize the canvas *)

    val zoom : 'a object_canvas -> real -> unit
	(* Zoom the canvas: a zoom of 1.0 is no change; a zoom of 2.0 doubles
	 * the size of objects in the canvas (i.e., zooms in), while a zoom of
	 * 0.5 halves the size of objects in the canvas.  This will not zoom
	 * out beyond the canvas's bounding box.
	 *)

  end; (* OBJ_CANVAS *)
