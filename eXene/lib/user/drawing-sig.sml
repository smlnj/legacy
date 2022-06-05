(* drawing.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This signature brings together all of the drawing related types and operations.
 *)

signature DRAWING =
  sig

    structure EXB : EXENE_BASE
    structure G : GEOMETRY

    type window
    type pixmap
    type tile
    type font
    type color

  (** this may belong elsewhere **)
    datatype plane_mask = PLANEMASK of word

    type pen

  (* graphics functions *)
    datatype graphics_op
      = OP_Clr			(* 0 *)
      | OP_And			(* src AND dst *)
      | OP_AndNot		(* src AND NOT dst *)
      | OP_Copy			(* src *)
      | OP_AndInverted		(* NOT src AND dst *)
      | OP_Nop			(* dst *)
      | OP_Xor			(* src XOR dst *)
      | OP_Or			(* src OR dst *)
      | OP_Nor			(* NOT src AND NOT dst *)
      | OP_Equiv		(* NOT src XOR dst *)
      | OP_Not			(* NOT dst *)
      | OP_OrNot		(* src OR NOT dst *)
      | OP_CopyNot		(* NOT src *)
      | OP_OrInverted		(* NOT src OR dst *)
      | OP_Nand			(* NOT src OR NOT dst *)
      | OP_Set			(* 1 *)

    datatype pen_val
      = PV_Function of graphics_op
      | PV_PlaneMask of plane_mask
      | PV_Foreground of color
      | PV_Background of color
      | PV_LineWidth of int
      | PV_LineStyle_Solid
      | PV_LineStyle_OnOffDash
      | PV_LineStyle_DoubleDash
      | PV_CapStyle_NotLast
      | PV_CapStyle_Butt
      | PV_CapStyle_Round
      | PV_CapStyle_Projecting
      | PV_JoinStyle_Miter
      | PV_JoinStyle_Round
      | PV_JoinStyle_Bevel
      | PV_FillStyle_Solid
      | PV_FillStyle_Tiled
      | PV_FillStyle_Stippled
      | PV_FillStyle_OpaqueStippled
      | PV_FillRule_EvenOdd
      | PV_FillRule_Winding
      | PV_ArcMode_Chord
      | PV_ArcMode_PieSlice
      | PV_ClipByChildren
      | PV_IncludeInferiors
      | PV_Tile of tile
      | PV_Stipple of tile
      | PV_TSOrigin of G.point
      | PV_ClipOrigin of G.point
      | PV_ClipMask_None
      | PV_ClipMask of tile
      | PV_ClipMask_UnsortedRects of G.rect list
      | PV_ClipMask_YSortedRects of G.rect list
      | PV_ClipMask_YXSortedRects of G.rect list
      | PV_ClipMask_YXBandedRects of G.rect list
      | PV_DashOffset of int
      | PV_Dash_Fixed of int
      | PV_Dash_List of int list

    exception BadPenParameter

    val newPen	   : pen_val list -> pen
    val updatePen  : (pen * pen_val list) -> pen
    val defaultPen : pen

  (* Destinations for drawing operations *)
    type drawable

    val drawableOfPM : pixmap -> drawable
    val drawableOfWin : window -> drawable
    val depthOfDrawable : drawable -> int

  (* drawing operations *)

    exception BadDrawParameter

    val drawPts     : drawable -> pen -> G.point list -> unit
    val drawPtPath  : drawable -> pen -> G.point list -> unit
    val drawPt      : drawable -> pen -> G.point -> unit

    val drawLines   : drawable -> pen -> G.point list -> unit
    val drawPath    : drawable -> pen -> G.point list -> unit
    val drawSegs    : drawable -> pen -> G.line list -> unit
    val drawSeg     : drawable -> pen -> G.line -> unit

    datatype shape = ComplexShape | NonconvexShape | ConvexShape
    val fillPolygon : drawable -> pen -> {verts: G.point list, shape : shape} -> unit
    val fillPath    : drawable -> pen -> {path : G.point list, shape : shape} -> unit

    val drawRects   : drawable -> pen -> G.rect list -> unit
    val drawRect    : drawable -> pen -> G.rect -> unit
    val fillRects   : drawable -> pen -> G.rect list -> unit
    val fillRect    : drawable -> pen -> G.rect -> unit

    val drawArcs    : drawable -> pen -> G.arc list -> unit
    val drawArc     : drawable -> pen -> G.arc -> unit
    val fillArcs    : drawable -> pen -> G.arc list -> unit
    val fillArc     : drawable -> pen -> G.arc -> unit

    val drawCircle  : drawable -> pen -> {center : G.point, rad : int} -> unit
    val fillCircle  : drawable -> pen -> {center : G.point, rad : int} -> unit

  (* Text drawing *)
    datatype text = TEXT of (font * text_item list)
    and text_item
      = TXT_FONT of (font * text_item list)
      | TXT_STR of string
      | TXT_DELTA of int

    val drawString  : drawable -> pen -> font -> G.point * string -> unit
    val imageString : drawable -> pen -> font -> G.point * string -> unit
    val drawText    : drawable -> pen -> G.point * text -> unit

  (* Sources for bitblt operations *)
    datatype draw_src
      = WSRC of window
      | PMSRC of pixmap
      | TSRC of tile

    exception DepthMismatch
    exception BadPlane

    val pixelBlt : drawable -> pen -> {
	    src : draw_src, src_rect : G.rect, dst_pos : G.point
	  } -> G.rect list
    val pixelBltEvt : drawable -> pen -> {
	    src : draw_src, src_rect : G.rect, dst_pos : G.point
	  } -> G.rect list CML.event

    val bitBlt : drawable -> pen -> {
	    src : draw_src, src_rect : G.rect, dst_pos : G.point
	  } -> G.rect list
    val bitBltEvt : drawable -> pen -> {
	    src : draw_src, src_rect : G.rect, dst_pos : G.point
	  } -> G.rect list CML.event

    val planeBlt : drawable -> pen -> {
	    src : draw_src, src_rect : G.rect, dst_pos : G.point, plane : int
	  } -> G.rect list
    val planeBltEvt : drawable -> pen -> {
	    src : draw_src, src_rect : G.rect, dst_pos : G.point, plane : int
	  } -> G.rect list CML.event

    val tileBlt    : drawable -> pen -> {src : tile, dst_pos : G.point} -> unit
    val textureBlt : drawable -> pen -> {src : tile, dst_pos : G.point} -> unit

    val copyBlt : drawable -> pen -> {dst_pos : G.point, src_rect : G.rect}
	  -> G.rect list
    val copyBltEvt : drawable -> pen
	  -> {dst_pos : G.point, src_rect : G.rect}
	  -> G.rect list CML.event

    val clearArea : drawable -> G.rect -> unit
    val clearDrawable : drawable -> unit

  (* Create an unbuffered drawable. *)
    val feedback : drawable -> drawable

  (* Create an overlay drawable for the given window.  This provides concurrency
   * control on the window and its descendents during rubber-banding (using OP_Xor).
   * The first result is the overlay drawable, the second is the release operation
   * for the drawable.  By convention, the overlay dfrawable is unbuffered.
   *)
    val createOverlay : window -> {drawable : drawable, release : unit -> unit}

  end (* DRAWING *)
