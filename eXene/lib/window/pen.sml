(* pen.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Support for symbolic names for pen component values.
 *)

structure Pen =
  struct
    local
      open Geometry XProtTypes DrawTypes
    in

    exception BadPenParameter

    datatype pen_val
      = PV_Function of XProtTypes.graphics_op
      | PV_PlaneMask of XProtTypes.plane_mask
      | PV_Foreground of ColorServer.color
      | PV_Background of ColorServer.color
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
      | PV_TSOrigin of point
      | PV_ClipOrigin of point
      | PV_ClipMask_None
      | PV_ClipMask of tile
      | PV_ClipMask_UnsortedRects of rect list
      | PV_ClipMask_YSortedRects of rect list
      | PV_ClipMask_YXSortedRects of rect list
      | PV_ClipMask_YXBandedRects of rect list
      | PV_DashOffset of int
      | PV_Dash_Fixed of int
      | PV_Dash_List of int list
    end (* local *)

    local
      open Geometry XProtTypes PenRep DrawTypes ColorServer

      fun checkList chkfn l = (app (fn x => (chkfn x; ())) l; l)
      fun checkItem chkfn = fn v => if chkfn v then v else raise BadPenParameter
      val chkPt = checkItem Geometry.validPt
      val chkCard16 = Word.fromInt o (checkItem XValid.valid16)
      val chkCard8 = Word.fromInt o (checkItem XValid.valid8)
      val chkRect = checkItem Geometry.validRect
      val chkRects = checkList chkRect
      val chkCard8s = checkList chkCard8

    (* map a pen value to its slot and representation *)
      fun penValToRep (PV_Function OP_Copy) = (0, PVRep_default)
	| penValToRep (PV_Function gr_op) =
	    (0, PVRep_wire(XCvtFuns.graphOpToWire gr_op))
	| penValToRep (PV_PlaneMask(PLANEMASK mask)) = (1, PVRep_wire mask)
	| penValToRep (PV_Foreground(COLOR{pixel=PIXEL 0, ...})) = (2, PVRep_default)
	| penValToRep (PV_Foreground(COLOR{pixel=PIXEL px, ...})) =
	    (2, PVRep_wire(Word.fromInt px))
	| penValToRep (PV_Background(COLOR{pixel=PIXEL 1, ...})) = (3, PVRep_default)
	| penValToRep (PV_Background (COLOR{pixel=PIXEL px, ...})) =
	    (3, PVRep_wire(Word.fromInt px))
	| penValToRep (PV_LineWidth 0) = (4, PVRep_default)
	| penValToRep (PV_LineWidth wd) = (4, PVRep_wire(chkCard16 wd))
	| penValToRep (PV_LineStyle_Solid) = (5, PVRep_default)
	| penValToRep (PV_LineStyle_OnOffDash) = (5, PVRep_wire 0w1)
	| penValToRep (PV_LineStyle_DoubleDash) = (5, PVRep_wire 0w2)
	| penValToRep (PV_CapStyle_NotLast) = (6, PVRep_wire 0w0)
	| penValToRep (PV_CapStyle_Butt) = (6, PVRep_default)
	| penValToRep (PV_CapStyle_Round) = (6, PVRep_wire 0w2)
	| penValToRep (PV_CapStyle_Projecting) = (6, PVRep_wire 0w3)
	| penValToRep (PV_JoinStyle_Miter) = (7, PVRep_default)
	| penValToRep (PV_JoinStyle_Round) = (7, PVRep_wire 0w1)
	| penValToRep (PV_JoinStyle_Bevel) = (7, PVRep_wire 0w2)
	| penValToRep (PV_FillStyle_Solid) = (8, PVRep_default)
	| penValToRep (PV_FillStyle_Tiled) = (8, PVRep_wire 0w1)
	| penValToRep (PV_FillStyle_Stippled) = (8, PVRep_wire 0w2)
	| penValToRep (PV_FillStyle_OpaqueStippled) = (8, PVRep_wire 0w3)
	| penValToRep (PV_FillRule_EvenOdd) = (9, PVRep_default)
	| penValToRep (PV_FillRule_Winding) = (9, PVRep_wire 0w1)
	| penValToRep (PV_Tile(TILE(PM{id, ...}))) = (10, PVRep_pixmap id)
	| penValToRep (PV_Stipple(TILE(PM{id, ...}))) = (11, PVRep_pixmap id)
	| penValToRep (PV_TSOrigin pt) = (12, PVRep_point (chkPt pt))
	| penValToRep (PV_ClipByChildren) = (13, PVRep_default)
	| penValToRep (PV_IncludeInferiors) = (13, PVRep_wire 0w1)
	| penValToRep (PV_ClipOrigin(PT{x=0, y=0})) = (14, PVRep_default)
	| penValToRep (PV_ClipOrigin pt) = (14, PVRep_point (chkPt pt))
	| penValToRep (PV_ClipMask_None) = (15, PVRep_default)
	| penValToRep (PV_ClipMask(TILE(PM{id, ...}))) = (15, PVRep_pixmap id)
	| penValToRep (PV_ClipMask_UnsortedRects r) = (15, PVRep_rects(UnsortedOrder, chkRects r))
	| penValToRep (PV_ClipMask_YSortedRects r) = (15, PVRep_rects(YSortedOrder, chkRects r))
	| penValToRep (PV_ClipMask_YXSortedRects r) = (15, PVRep_rects(YXSortedOrder, chkRects r))
	| penValToRep (PV_ClipMask_YXBandedRects r) = (15, PVRep_rects(YXBandedOrder, chkRects r))
	| penValToRep (PV_DashOffset 0) = (16, PVRep_default)
	| penValToRep (PV_DashOffset n) = (16, PVRep_wire(chkCard16 n))
	| penValToRep (PV_Dash_Fixed 4) = (17, PVRep_default)
	| penValToRep (PV_Dash_Fixed n) = (17, PVRep_wire(chkCard8 n))
	| penValToRep (PV_Dash_List dashes) = (17, PVRep_dashes (chkCard8s dashes))
	| penValToRep (PV_ArcMode_Chord) = (18, PVRep_wire 0w0)
	| penValToRep (PV_ArcMode_PieSlice) = (18, PVRep_default)

    (* extract the non-default value mask from an array of reps *)
      fun extractMask arr = let
	    fun loop (m, i, b) = if (i < numPenSlots)
		  then (case Array.sub(arr, i)
		     of PVRep_default => loop (m, i+1, Word.<<(b, 0w1))
		      | _ => loop (Word.orb(m, b), i+1, Word.<<(b, 0w1))
		    (* end case *))
		  else m
	    in
	      loop (0w0, 0, 0w1)
	    end

    (* make a pen from an array of initial values and a list of new values *)
      fun mkPen (arr, valList) = let
	    fun update (slot, rep) = Array.update(arr, slot, rep)
	    in
	      app (fn v => update(penValToRep v)) valList;
	      PEN{
                  mask = extractMask arr,
                  vals = Vector.tabulate(numPenSlots, fn i => Array.sub(arr, i))
                }
	    end (* mkPen *)
    in

    val defaultPen = PenRep.defaultPen

  (* create a new drawing context from a list of values *)
    fun newPen vals = mkPen (Array.array(numPenSlots, PVRep_default), vals)

  (* create a new pen from an existing pen by functional update *)
    fun updatePen (PEN{vals, ...}, newVals) =
	  mkPen (Array.tabulate(numPenSlots, fn i => Vector.sub(vals, i)), newVals)

    end (* local *)
  end (* Pen *)
