(* world-sig.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This is the internal representation of the structured graphics
 * canvas.
 *)

signature WORLD =
  sig
    type 'a world

  (* specification of a width; either in absolute pixels or in universal
   * coordinates.
   *)
    datatype width_spec
      = WID_Pixel of int

  (* specification of an arrow head *)
    datatype arrow_spec
      = NoArrow
      | Arrow of {len : real, wid : real}

  (* the different ways to specify a border; either an absolute number of
   * pixels, or proportional border specified in universal coordinate units.
   *)
    datatype border_spec
      = BORDER_None
      | BORDER_Pixel2D of (EXeneBase.color * int)

    datatype object_desc
      = OBJ_LINE of {
	    pts : {x : real, y : real} list,
	    head : {x : real, y : real} arrow_desc,
	    tail : {x : real, y : real} arrow_desc,
	    color : EXeneBase.color,
	    width : width_spec
	  }
      | OBJ_RECT of {
	    x : real, y : real,
	    wid : real, ht : real,
	    fill : EXeneBase.color option,
	    border : border_spec
	  }
      | OBJ_ELLIPSE of {
	    x : real, y : real,
	    wid : real, ht : real,
	    fill : EXeneBase.color option,
	    border : border_spec
	  }
      | OBJ_DIAMOND of {
	    x : real, y : real,
	    wid : real, ht : real,
	    fill : EXeneBase.color option,
	    border : border_spec
	  }
      | OBJ_TEXT of {
	    x : real, y : real,
	    wid : real, ht : real,	(* the size of the text when drawn in it *)
					(* natural size and transformed to *)
					(* unscaled world coordinates (i.e., *)
					(* inches). *)
	    text : string,
	    color : EXeneBase.color,
	    ptSz : int,
	    fontStyle : ScalableFont.font_style
	  }

    and 'a arrow_desc
      = NO_ARROW
      | ARROW of {
	    p0 : 'a,
	    p1 : 'a,	(* this is the tip of the arrow *)
	    p2 : 'a
	  }

    type object_id

    type box = {maxX:real, maxY:real, minX:real, minY:real}

    val makeWorld : (
	    Widget.root * Canvas.canvas * ScalableFont.font_server
	    * EXeneBase.color * Geometry.size * box option
	  ) -> 'a world

    val canvasOfWorld : 'a world -> Canvas.canvas

    val viewOfWorld : 'a world -> WorldView.view

    val pickObj : ('a world * Geometry.point) -> (object_id * 'a) list

    val addObject : ('a world * object_desc * 'a option * int) -> object_id

    val delObject : ('a world * object_id) -> unit

    val updateCanvas : 'a world -> unit
	(* ensure that the view of the canvas reflects any object additions
	 * and deletions.
	 *)

    val vScroll : ('a world * Scrollbar.scroll_evt) -> unit
    val hScroll : ('a world * Scrollbar.scroll_evt) -> unit

    val zoom : ('a world * real) -> unit

    val redrawCanvas : ('a world * Geometry.rect list) -> unit

    val resizeCanvas : ('a world * Geometry.size) -> unit

    val infoOfObject : ('a world * object_id) -> 'a option
	(* get the associated info (if any) of an object; this raises the NotFound
	 * exception, if the ID is bogus.
	 *)

  end;

