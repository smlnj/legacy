(* draw.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Routines for drawing on windows and pixmaps.
 *)

structure Draw =
  struct

    exception BadDrawParameter

    local

      open Geometry FontBase DrawMaster Display DrawTypes

    (* extract the draw_cmd, id and depth of a drawable *)
      fun infoOfDrawable (DRAWABLE{draw_cmd, root = DWIN w}) = let
	    val WIN{id, scr_depth=SCRDEPTH{depth, ...}, ...} = w
	    in
	      {draw_cmd=draw_cmd, id=id, depth=depth}
	    end
	| infoOfDrawable (DRAWABLE{draw_cmd, root = DPM pm}) = let
	    val PM{id, scr_depth=SCRDEPTH{depth, ...}, ...} = pm
	    in
	      {draw_cmd=draw_cmd, id=id, depth=depth}
	    end

    (* extract the xid and depth of a source drawable *)
      fun infoOfSrc (WSRC(WIN{id, scr_depth=SCRDEPTH{depth, ...}, ...})) = (id, depth)
	| infoOfSrc (PMSRC(PM{id, scr_depth=SCRDEPTH{draw_cmd, depth, ...}, ...})) =
	    (id, depth)
	| infoOfSrc (TSRC(TILE(PM{id, scr_depth=SCRDEPTH{draw_cmd, depth, ...}, ...}))) =
	    (id, depth)

    (* build a drawing function from an encoding function.  The functions have
     * the basic type scheme
     *   drawable -> pen -> args -> unit
     *)
      fun drawFn f drawable pen = let
	    val {draw_cmd, id, ...} = infoOfDrawable drawable
	    in
	      fn x => draw_cmd(DMSG{dst = id, pen = pen, oper = (f x)})
	    end
      fun drawFn2 f drawable pen = let
	    val {draw_cmd, id, ...} = infoOfDrawable drawable
	    in
	      fn x => fn x' => draw_cmd(DMSG{dst = id, pen = pen, oper = (f x x')})
	    end

      fun checkList chkfn l = (app (fn x => (chkfn x; ())) l; l)
      fun checkItem chkfn = fn v => if chkfn v then v else raise BadDrawParameter
      val chkPt = checkItem Geometry.validPt
      val chkRect = checkItem Geometry.validRect
      val chkLine = checkItem Geometry.validLine
      val chkArc = checkItem Geometry.validArc
      val chkPts = checkList chkPt
      val chkRects = checkList chkRect
      val chkLines = checkList chkLine
      val chkArcs = checkList chkArc

    in

  (* Points *)
    val drawPts	    = drawFn (fn pts => DOP_PolyPoint(false, chkPts pts))
    val drawPtPath  = drawFn (fn pts => DOP_PolyPoint(true, chkPts pts))
    val drawPt	    = drawFn (fn pt => DOP_PolyPoint(false, [chkPt pt]))

  (* Lines and segments *)
    val drawLines   = drawFn (fn pts => DOP_PolyLine(false, chkPts pts))
    val drawPath    = drawFn (fn pts => DOP_PolyLine(true, chkPts pts))
    val drawSegs    = drawFn (fn lines => DOP_PolySeg (chkLines lines))
    val drawSeg	    = drawFn (fn seg => DOP_PolySeg [chkLine seg])

  (* Filled polygons *)
    val fillPolygon = drawFn (fn {verts, shape} => DOP_FillPoly(shape, false, chkPts verts))
    val fillPath    = drawFn (fn {path, shape} => DOP_FillPoly(shape, true, chkPts path))

  (* Rectangles *)
    val drawRects   = drawFn (fn rects => DOP_PolyRect (chkRects rects))
    val drawRect    = drawFn (fn rect => DOP_PolyRect[chkRect rect])
    val fillRects   = drawFn (fn rects => DOP_PolyFillRect (chkRects rects))
    val fillRect    = drawFn (fn rect => DOP_PolyFillRect[chkRect rect])

  (* Arcs *)
    val drawArcs    = drawFn (fn arcs => DOP_PolyArc (chkArcs arcs))
    val drawArc     = drawFn (fn arc => DOP_PolyArc[chkArc arc])
    val fillArcs    = drawFn (fn arcs => DOP_PolyFillArc (chkArcs arcs))
    val fillArc     = drawFn (fn arc => DOP_PolyFillArc[chkArc arc])

  (* Circles *)
    fun circleToArc {center = PT{x, y}, rad} = let
	  val diam = rad + rad
	  in
	    ARC{
		x = x-rad, y = y-rad,
		wid = diam, ht = diam,
		angle1 = 0, angle2 = 64*360
	      }
	  end
    val drawCircle = drawFn (fn arg => DOP_PolyArc [circleToArc arg])
    val fillCircle = drawFn (fn arg => DOP_PolyFillArc [circleToArc arg])

  (* Text drawing *)
    val drawString = drawFn2 (
	  fn (FONT{id, ...}) =>
	    fn (pt, s) => DOP_PolyText8(id, chkPt pt, [PTXT_text(0, s)]))
    val imageString = drawFn2 (
	  fn (FONT{id, ...}) => fn (pt, s) => DOP_ImageText8(id, chkPt pt, s))

  (* Polytext drawing *)
    datatype text = TEXT of (FontBase.font * text_item list)
    and text_item
      = TXT_FONT of (FontBase.font * text_item list)
      | TXT_STR of string
      | TXT_DELTA of int
    val drawText = let
	  open FontBase
	  fun f (pt, TEXT(FONT{id=fid, ...}, items)) = let
		fun flat (_, d, [], l) = (d, l)
		  | flat (fid, d, (TXT_STR s)::r, l) =
		      flat(fid, 0, r, PTXT_text(d, s) :: l)
		  | flat (fid, d, (TXT_DELTA d')::r, l) =
		      flat(fid, d+d', r, l)
		  | flat (_, d, [TXT_FONT(FONT{id=fid, ...}, items)], l) =
		      flat(fid, d, items, (PTXT_font fid)::l)
		  | flat (fid, d, (TXT_FONT(FONT{id=fid', ...}, items))::r, l) = let
		      val (d', l') = flat(fid', d, items, (PTXT_font fid')::l)
		      in
			flat(fid, d', r, (PTXT_font fid)::l')
		      end
		in
		  DOP_PolyText8(fid, chkPt pt, rev(#2(flat(fid, 0, items, []))))
		end
	  in
	    drawFn f
	  end
(** TODO: imageText (what does it mean?? **)


  (* BLT operations *)
    exception DepthMismatch
    exception BadPlane

    local

(** NOTE: we should probably check that src and dst are on the same display **)
      fun copyAreaFn msgFn (dst, pen, dstPos, src, src_rect) = let
	    val {id=dstId, draw_cmd=drawCmd, depth=dstDepth} = infoOfDrawable dst
	    val (srcId, srcDepth) = infoOfSrc src
	    val (msg, result) = msgFn (chkPt dstPos, srcId, src_rect)
	    in
	      if (srcDepth <> dstDepth) then raise DepthMismatch else ();
	      drawCmd (DMSG{dst = dstId, pen = pen, oper = msg});
	      result
	    end
      fun copyPlaneFn msgFn (dst, pen, dstPos, src, srcRect, plane) = let
	    val {id=dstId, draw_cmd=drawCmd, ...} = infoOfDrawable dst
	    val (srcId, srcDepth) = infoOfSrc src
	    val (msg, result) = msgFn (chkPt dstPos, srcId, srcRect, plane)
	    in
	      if ((plane < 0) orelse (srcDepth <= plane)) then raise BadPlane else ();
	      drawCmd (DMSG{dst = dstId, pen = pen, oper = msg});
	      result
	    end
      val copyArea = copyAreaFn (fn (dstPos, srcId, srcRect) => let
	    val syncV = SyncVar.iVar()
	    in
	      (DOP_CopyArea(dstPos, srcId, srcRect, syncV), syncV)
	    end)
      val copyPlane = copyPlaneFn (fn (dstPos, srcId, srcRect, plane) => let
	    val syncV = SyncVar.iVar()
	    in
	      (DOP_CopyPlane(dstPos, srcId, srcRect, plane, syncV), syncV)
	    end)
      val copyPMArea = copyAreaFn (fn arg => (DOP_CopyPMArea arg, ()))
      val copyPMPlane = copyPlaneFn (fn arg => (DOP_CopyPMPlane arg, ()))
      fun promiseEvt (drawCmd, syncV) = let
	    val syncEvt = SyncVar.iGetEvt syncV
	    in
	      CML.guard (fn () => (case (SyncVar.iGetPoll syncV)
		 of (SOME rectsFn) => CML.wrap (CML.alwaysEvt (), rectsFn)
		  | NONE => (
		      drawCmd (DMSG_Flush);
		      CML.wrap (syncEvt, fn rectsFn => rectsFn()))
		(* end case *)))
	    end
    in

    fun pixelBlt dst pen {src as (WSRC _), src_rect, dst_pos} = let
	  val DRAWABLE{draw_cmd, ...} = dst
	  val syncV = copyArea (dst, pen, dst_pos, src, src_rect)
	  in
	    draw_cmd (DMSG_Flush);
	    (SyncVar.iGet syncV)()
	  end
      | pixelBlt dst pen {src, src_rect, dst_pos} = (
	  copyPMArea (dst, pen, dst_pos, src, src_rect); [])

    fun pixelBltEvt dst pen {src as (WSRC _), src_rect, dst_pos} = let
	  val DRAWABLE{draw_cmd, ...} = dst
	  val syncV = copyArea (dst, pen, dst_pos, src, src_rect)
	  in
	    promiseEvt (draw_cmd, syncV)
	  end
      | pixelBltEvt dst pen {src, src_rect, dst_pos} = (
	  copyPMArea (dst, pen, dst_pos, src, src_rect);
	  CML.alwaysEvt [])

    fun planeBlt dst pen {src as (WSRC _), src_rect, dst_pos, plane} = let
	  val DRAWABLE{draw_cmd, ...} = dst
	  val syncV = copyPlane (dst, pen, dst_pos, src, src_rect, plane)
	  in
	    draw_cmd (DMSG_Flush);
	    (SyncVar.iGet syncV)()
	  end
      | planeBlt dst pen {src, src_rect, dst_pos, plane} = (
	  copyPMPlane (dst, pen, dst_pos, src, src_rect, plane); [])

    fun planeBltEvt dst pen {src as (WSRC _), src_rect, dst_pos, plane} = let
	  val DRAWABLE{draw_cmd, ...} = dst
	  val syncV = copyPlane (dst, pen, dst_pos, src, src_rect, plane)
	  in
	    promiseEvt (draw_cmd, syncV)
	  end	    
      | planeBltEvt dst pen {src, src_rect, dst_pos, plane} = (
	  copyPMPlane (dst, pen, dst_pos, src, src_rect, plane);
	  CML.alwaysEvt [])

    fun bitBlt dst pen {src, src_rect, dst_pos} =
	  planeBlt dst pen {src=src, src_rect=src_rect, dst_pos=dst_pos, plane=0}
    fun bitBltEvt dst pen {src, src_rect, dst_pos} =
	  planeBltEvt dst pen {src=src, src_rect=src_rect, dst_pos=dst_pos, plane=0}

    fun textureBlt dst pen {src, dst_pos} = let
	  val SIZE{wid, ht} = sizeOfTile src
	  val rect = RECT{x=0, y=0, wid=wid, ht=ht}
	  in
	    planeBlt dst pen {src=TSRC src, src_rect=rect, dst_pos=dst_pos, plane=0};
	    ()
	  end

    fun tileBlt dst pen {src, dst_pos} = let
	  val SIZE{wid, ht} = sizeOfTile src
	  val rect = RECT{x=0, y=0, wid=wid, ht=ht}
	  in
	    pixelBlt dst pen {src=TSRC src, src_rect=rect, dst_pos=dst_pos};
	    ()
	  end

    fun copyBlt drawable pen {dst_pos, src_rect} = let
	  val src = (case drawable
		 of DRAWABLE{root = DWIN w, ...} => WSRC w
		  | DRAWABLE{root = DPM pm, ...} => PMSRC pm
		(* end case *))
	  in
	    pixelBlt drawable pen {src=src, dst_pos=dst_pos, src_rect=src_rect}
	  end
    fun copyBltEvt drawable pen {dst_pos, src_rect} = let
	  val src = (case drawable
		 of DRAWABLE{root = DWIN w, ...} => WSRC w
		  | DRAWABLE{root = DPM pm, ...} => PMSRC pm
		(* end case *))
	  in
	    pixelBltEvt drawable pen {src=src, dst_pos=dst_pos, src_rect=src_rect}
	  end

    end (* local *)

  (* Clear part of a destination drawable.  For windows, this fills in the
   * background color; for pixmaps, this fills in all 0's (which is actually
   * the default forground pixel value).
   *)
    local
      val clearPen = Pen.newPen [Pen.PV_Foreground ColorServer.color0]
    in
      fun clearArea drawable = let
	    val {draw_cmd, id, ...} = infoOfDrawable drawable
	    in
	      fn rect => draw_cmd (
		  DMSG{dst = id, pen = clearPen, oper = (DOP_ClearArea (chkRect rect))})
	    end
    end (* local val clearPen ... *)

  (* Clear the whole area of a drawable *)
    fun clearDrawable dst = clearArea dst (RECT{x=0, y=0, wid=0, ht=0})

    end (* local *)

  end (* Draw *)
