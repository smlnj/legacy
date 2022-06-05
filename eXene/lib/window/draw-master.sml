(* draw-master.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * TODO
 *  - optimize the case where successive DOPs use the same pen.
 *  - all window configuration operations (Resize, Move, Pop/Push, Create &
 *    Delete) should go through the draw master.
 *)

structure DrawMaster : sig

    datatype dm_config
      = DM_Unmap
      | DM_Map
      | DM_FirstExpose

    datatype poly_text
      = PTXT_text of (int * string)
      | PTXT_font of XProtTypes.font_id

    datatype draw_op
      = DOP_PolyPoint of (bool * Geometry.point list)
      | DOP_PolyLine of (bool * Geometry.point list)
      | DOP_PolySeg of Geometry.line list
      | DOP_FillPoly of (XProtTypes.shape * bool * Geometry.point list)
      | DOP_PolyRect of Geometry.rect list
      | DOP_PolyFillRect of Geometry.rect list
      | DOP_PolyArc of Geometry.arc list
      | DOP_PolyFillArc of Geometry.arc list
      | DOP_CopyArea of (
	    Geometry.point * XProtTypes.xid * Geometry.rect *
	    (unit -> Geometry.rect list) SyncVar.ivar)
      | DOP_CopyPlane of (
	    Geometry.point * XProtTypes.xid * Geometry.rect * int *
	    (unit -> Geometry.rect list) SyncVar.ivar)
      | DOP_CopyPMArea of (Geometry.point * XProtTypes.xid * Geometry.rect)
      | DOP_CopyPMPlane of (Geometry.point * XProtTypes.xid * Geometry.rect * int)
      | DOP_ClearArea of Geometry.rect
      | DOP_PutImage of {
	    dst_pt : Geometry.point,
	    size : Geometry.size,
	    depth : int,
	    lpad : int,
	    format : XProtTypes.image_format,
	    data : Word8Vector.vector
	  }
      | DOP_PolyText8 of (XProtTypes.font_id * Geometry.point * poly_text list)
      | DOP_ImageText8 of (XProtTypes.font_id * Geometry.point * string)

    datatype destroy_item
      = DSTRY_Win of XProtTypes.win_id
      | DSTRY_PM of XProtTypes.pixmap_id

    datatype draw_msg
      = DMSG of {
	  dst : XProtTypes.xid,
	  pen : PenRep.pen,
	  oper : draw_op
	}
      | DMSG_Flush
      | DMSG_CreateOverlay of {
	  cmd_strm : draw_msg CML.chan,   (* the stream of drawing commands for *)
					    (* the overlay *)
	  release_evt : unit CML.event	    (* the overlay release event *)
	}
      | DMSG_Destroy of destroy_item
      | DMSG_BatchingOn
      | DMSG_BatchingOff

    val mkDM : (
	    dm_config CML.event *
	    GCServer.gc_server *
	    WinRegistry.registry *
	    XIo.connection
	  ) -> draw_msg -> unit

  end = struct

    datatype dm_config
      = DM_Unmap
      | DM_Map
      | DM_FirstExpose

    datatype poly_text
      = PTXT_text of (int * string)
      | PTXT_font of XProtTypes.font_id

    datatype draw_op
      = DOP_PolyPoint of (bool * Geometry.point list)
      | DOP_PolyLine of (bool * Geometry.point list)
      | DOP_PolySeg of Geometry.line list
      | DOP_FillPoly of (XProtTypes.shape * bool * Geometry.point list)
      | DOP_PolyRect of Geometry.rect list
      | DOP_PolyFillRect of Geometry.rect list
      | DOP_PolyArc of Geometry.arc list
      | DOP_PolyFillArc of Geometry.arc list
      | DOP_CopyArea of (
	    Geometry.point * XProtTypes.xid * Geometry.rect *
	    (unit -> Geometry.rect list) SyncVar.ivar)
      | DOP_CopyPlane of (
	    Geometry.point * XProtTypes.xid * Geometry.rect * int *
	    (unit -> Geometry.rect list) SyncVar.ivar)
      | DOP_CopyPMArea of (Geometry.point * XProtTypes.xid * Geometry.rect)
      | DOP_CopyPMPlane of (Geometry.point * XProtTypes.xid * Geometry.rect * int)
      | DOP_ClearArea of Geometry.rect
      | DOP_PutImage of {
	    dst_pt : Geometry.point,
	    size : Geometry.size,
	    depth : int,
	    lpad : int,
	    format : XProtTypes.image_format,
	    data : Word8Vector.vector
	  }
      | DOP_PolyText8 of (XProtTypes.font_id * Geometry.point * poly_text list)
      | DOP_ImageText8 of (XProtTypes.font_id * Geometry.point * string)

    datatype destroy_item
      = DSTRY_Win of XProtTypes.win_id
      | DSTRY_PM of XProtTypes.pixmap_id

    datatype draw_msg
      = DMSG of {
	  dst : XProtTypes.xid,
	  pen : PenRep.pen,
	  oper : draw_op
	}
      | DMSG_Flush
      | DMSG_CreateOverlay of {
	  cmd_strm : draw_msg CML.chan,   (* the stream of drawing commands for *)
					    (* the overlay *)
	  release_evt : unit CML.event	    (* the overlay release event *)
	}
      | DMSG_Destroy of destroy_item
      | DMSG_BatchingOn
      | DMSG_BatchingOff

(* +DEBUG 
fun dopToString (DOP_PolyPoint _) = "PolyPoint"
  | dopToString (DOP_PolyLine _) = "PolyLine"
  | dopToString (DOP_PolySeg _) = "PolySeg"
  | dopToString (DOP_FillPoly _) = "PolyFillPoly"
  | dopToString (DOP_PolyRect _) = "PolyRect"
  | dopToString (DOP_PolyFillRect _) = "PolyFillRect"
  | dopToString (DOP_PolyArc _) = "PolyArc"
  | dopToString (DOP_PolyFillArc _) = "PolyFillArc"
  | dopToString (DOP_CopyArea _) = "CopyArea"
  | dopToString (DOP_CopyPlane _) = "CopyPlane"
  | dopToString (DOP_CopyPMArea _) = "CopyPMArea"
  | dopToString (DOP_CopyPMPlane _) = "CopyPMPlane"
  | dopToString (DOP_ClearArea _) = "ClearArea"
  | dopToString (DOP_PutImage _) = "PutImage"
  | dopToString (DOP_PolyText8 _) = "PolyText8"
  | dopToString (DOP_ImageText8 _) = "ImageText8"
 -DEBUG *)

    local
    (* the maximum number of drawing commands to buffer before
     * flushing.
     *)
      val fullBufferSz = 16

      val ++ = Word.orb and << = Word.<<
      infix ++ <<

    (* "eq" test on pens for efficiency hack *)
      fun penEq (a : PenRep.pen, b : PenRep.pen) =
	    (((Unsafe.cast a) : int) = ((Unsafe.cast b) : int))

    (* Bit masks for the various components of a pen.  These should
     * track the slot numbers given in PenValues.
     *)
      val penFunction           = (0w1 << 0w0)
      val penPlaneMask          = (0w1 << 0w1)
      val penForeground         = (0w1 << 0w2)
      val penBackground         = (0w1 << 0w3)
      val penLineWidth          = (0w1 << 0w4)
      val penLineStyle          = (0w1 << 0w5)
      val penCapStyle           = (0w1 << 0w6)
      val penJoinStyle		= (0w1 << 0w7)
      val penFillStyle		= (0w1 << 0w8)
      val penFillRule		= (0w1 << 0w9) 
      val penTile		= (0w1 << 0w10)
      val penStipple		= (0w1 << 0w11)
      val penTileStipOrigin	= (0w1 << 0w12)
      val penSubwindowMode	= (0w1 << 0w13)
      val penClipOrigin		= (0w1 << 0w14)
      val penClipMask		= (0w1 << 0w15)
      val penDashOffset		= (0w1 << 0w16)
      val penDashList		= (0w1 << 0w17)
      val penArcMode		= (0w1 << 0w18)
      val penExposures		= 0w0 (* (0w1 << 0w19) *)

      val stdComp		(* the standard pen components used by most ops *)
	    =  penFunction
	    ++ penPlaneMask
	    ++ penSubwindowMode
	    ++ penClipOrigin
	    ++ penClipMask
	    ++ penForeground
	    ++ penBackground
	    ++ penTile
	    ++ penStipple
	    ++ penTileStipOrigin
      val stdLineComp		(* the components used by line-drawing operations *)
	    =  stdComp
	    ++ penLineWidth
	    ++ penLineStyle
	    ++ penCapStyle
	    ++ penJoinStyle
	    ++ penFillStyle
	    ++ penDashOffset
	    ++ penDashList

      fun penValsUsed (DOP_PolyPoint _)		= stdComp
        | penValsUsed (DOP_PolyLine _)		= stdLineComp
        | penValsUsed (DOP_PolySeg _)		= stdLineComp
        | penValsUsed (DOP_FillPoly _)		= (stdComp++penFillStyle)
        | penValsUsed (DOP_PolyRect _)		= stdLineComp
        | penValsUsed (DOP_PolyFillRect _)	= (stdComp++penFillStyle)
        | penValsUsed (DOP_PolyArc _)		= stdLineComp
        | penValsUsed (DOP_PolyFillArc _)	= (stdComp++penFillStyle)
        | penValsUsed (DOP_CopyArea _)		= stdComp++penExposures
        | penValsUsed (DOP_CopyPlane _)		= stdComp++penExposures
        | penValsUsed (DOP_CopyPMArea _)	= stdComp
        | penValsUsed (DOP_CopyPMPlane _)	= stdComp
        | penValsUsed (DOP_ClearArea _)		= 0w0
        | penValsUsed (DOP_PutImage _)		= stdComp
        | penValsUsed (DOP_PolyText8 _)		= (stdComp++penFillStyle)
        | penValsUsed (DOP_ImageText8 _)	= stdComp

      local
	open XRequest
      in
      fun requestDrawOp (request, requestE) = (
	    fn (dst, gc, _, DOP_PolyPoint(rel, pts)) =>
		request (encodePolyPoint{drawable=dst, gc=gc, items=pts, relative=rel})
	     | (dst, gc, _, DOP_PolyLine(rel, pts)) =>
		request (encodePolyLine{drawable=dst, gc=gc, items=pts, relative=rel})
	     | (dst, gc, _, DOP_PolySeg lines) =>
		request (encodePolySegment{drawable=dst, gc=gc, items=lines})
	     | (dst, gc, _, DOP_FillPoly(shape, rel, pts)) =>
		request (encodeFillPoly{
		    drawable=dst, gc=gc, pts=pts, relative=rel, shape=shape
		  })
	     | (dst, gc, _, DOP_PolyRect rects) =>
		request (encodePolyRectangle{drawable=dst, gc=gc, items=rects})
	     | (dst, gc, _, DOP_PolyFillRect rects) =>
		request (encodePolyFillRectangle{drawable=dst, gc=gc, items=rects})
	     | (dst, gc, _, DOP_PolyArc arcs) =>
		request (encodePolyArc{drawable=dst, gc=gc, items=arcs})
	     | (dst, gc, _, DOP_PolyFillArc arcs) =>
		request (encodePolyFillArc{drawable=dst, gc=gc, items=arcs})
	     | (dst, gc, _, DOP_CopyArea(pt, src, rect, syncV)) => let
		val (p, sz) = Geometry.originAndSzOfRect rect
		in
		  requestE (
		    encodeCopyArea{
		        gc=gc, src=src, dst=dst, src_pt=p, size=sz, dst_pt=pt
		      },
		    syncV)
		end
	     | (dst, gc, _, DOP_CopyPlane(pt, src, rect, plane, syncV)) => let
		val (p, sz) = Geometry.originAndSzOfRect rect
		in
		  requestE (
		    encodeCopyPlane{
			gc=gc, src=src, dst=dst, src_pt=p, size=sz,
			dst_pt=pt, plane=plane
		      },
		    syncV)
		end
	     | (dst, gc, _, DOP_CopyPMArea(pt, src, rect)) => let
		val (p, sz) = Geometry.originAndSzOfRect rect
		in
		  request (encodeCopyArea{
		      gc=gc, src=src, dst=dst, src_pt=p, size=sz, dst_pt=pt
		    })
		end
	     | (dst, gc, _, DOP_CopyPMPlane(pt, src, rect, plane)) => let
		val (p, sz) = Geometry.originAndSzOfRect rect
		in
		  request (encodeCopyPlane{
		      gc=gc, src=src, dst=dst, src_pt=p, size=sz, dst_pt=pt, plane=plane
		    })
		end
	     | (dst, _, _, DOP_ClearArea rect) =>
		request (encodeClearArea {win=dst, rect=rect, exposures = false})
	     | (dst, gc, _, DOP_PutImage im) => request (encodePutImage{
		    drawable = dst, gc = gc, depth = #depth im, dst = #dst_pt im,
		    size = #size im, lpad = #lpad im, format = #format im,
		    data = #data im
		  })
	     | (dst, gc, curFid, DOP_PolyText8(fid, pt, txtItems)) => let
		open XProtTypes
		val lastFid = let
		      fun f (lastFid, []) = lastFid
			| f (lastFid, (PTXT_font id)::r) = f (id, r)
			| f (lastFid, _::r) = f (lastFid, r)
		      in
			f (fid, txtItems)
		      end
		val txtItems = if (lastFid = curFid)
			then txtItems
			else txtItems @ [PTXT_font curFid]
		val txtItems = if (fid = curFid)
			then txtItems
			else (PTXT_font fid) :: txtItems
		fun splitDelta (0, l) = l
		  | splitDelta (i, l) = if (i < ~128)
			then splitDelta (i+128, ~128 :: l)
		      else if (i > 127)
			then splitDelta (i-127, 127 :: l)
			else i :: l
	      (* split a string into legal lengths for a PolyText8 command *)
		fun splitText "" = []
		  | splitText s = let
		      val n = String.size s
		      fun split (i, l) = if ((n - i) > 254)
			    then split(i+254, substring(s, i, 254) :: l)
			    else List.rev(substring(s, i, n-i) :: l)
		      in
			if (n > 254)
			  then split(0, [])
			  else [s]
		      end
		fun splitItem (PTXT_font id) = [FontItem id]
		  | splitItem (PTXT_text(delta, s)) = (
		      case (splitDelta(delta, []), splitText s)
		       of ([], []) => []
			| ([], sl) => (map (fn s => TextItem(0, s)) sl)
			| (dl, []) => (map (fn n => TextItem(n, "")) dl)
			| ([d], s::sr) => (
			    TextItem(d, s) :: (map (fn s => TextItem(0, s)) sr))
			| (d::dr, s::sr) => (
			    (map (fn n => TextItem(n, "")) dr)
			      @ (TextItem(d, s) :: (map (fn s => TextItem(0, s)) sr))))
		val doItems = foldr (fn (item, l) => (splitItem item) @ l) []
		in
		  request (encodePolyText8 {
		      drawable=dst, gc=gc, pt=pt, items=(doItems txtItems)
		    })
		end
	     | (dst, gc, _, DOP_ImageText8(_, pt, txt)) =>
		request (encodeImageText8 {drawable=dst, gc=gc, pt=pt, str=txt}))
      end (* local *)

    (* Flush a list of drawing commands out to the sequencer. This requires
     * aquiring actual server graphics contexts for the operations from
     * the GC-server.
     *)
      fun flushBuf (gcServer, conn) = let
	    datatype gc_info
	      = NoGC
	      | NoFont
	      | WithFont of XProtTypes.font_id
	      | SetFont of XProtTypes.font_id
	    val acqGC = GCServer.acquireGC gcServer
	    val relGC = GCServer.releaseGC gcServer
	    val acqGCWithFont = GCServer.acquireGCWithFont gcServer
	    val acqGCAndSetFont = GCServer.acquireGCAndSetFont gcServer
	    val relGCAndFont = GCServer.releaseGCAndFont gcServer
	    val requestDOP =
		  requestDrawOp (XIo.request conn, XIo.requestWithExposures conn)
	  (* batch the maximal sequence of operations that can safely use the
	   * same GC.  Add the batch to the batch list.
	   *)
	    fun batchGC ([], batchLst) = batchLst
	      | batchGC (ops as (firstOper :: _), batchLst) = let
		  fun gcInfoOf (DOP_ClearArea _) = NoGC
		    | gcInfoOf (DOP_PolyText8(fid, _, _)) = (WithFont fid)
		    | gcInfoOf (DOP_ImageText8(fid, _, _)) = (SetFont fid)
		    | gcInfoOf oper = NoFont
		  fun extendMask (m, oper) = (m ++ (penValsUsed oper))
		  fun f (arg as ([], _, _, _, _)) = arg
		    | f (arg as ({dst, pen, oper}::r, info, curPen, usedMask, ops)) =
			if (penEq(pen, curPen))
			  then (case (info, gcInfoOf oper)
			     of (_, NoGC) =>
				  f (r, info, curPen, usedMask, (dst, oper)::ops)
			      | (NoGC, newInfo) =>
				  f (r, newInfo, curPen, penValsUsed oper,
				      (dst, oper)::ops)
			      | (_, NoFont) =>
				  f (r, info, curPen, extendMask(usedMask, oper),
				      (dst, oper)::ops)
			      | (SetFont fid, WithFont _) =>
				  f (r, SetFont fid, curPen, extendMask(usedMask, oper),
				      (dst, oper)::ops)
			      | (_, WithFont fid) =>
				  f (r, WithFont fid, curPen, extendMask(usedMask, oper),
				      (dst, oper)::ops)
			      | (SetFont fid1, SetFont fid2) =>
				  if (fid1 = fid2)
				    then f (r, SetFont fid1, curPen,
					extendMask(usedMask, oper), (dst, oper)::ops)
				    else arg
			      | (_, SetFont fid) =>
				  f (r, SetFont fid, curPen, extendMask(usedMask, oper),
				      (dst, oper)::ops)
			    (* end case *))
			  else arg
		  val (rest, info, pen, mask, ops) =
			f (ops, NoGC, #pen firstOper, 0w0, [])
		  in
		    batchGC(rest, (info, pen, mask, ops) :: batchLst)
		  end (* batchGC *)
	    fun drawOps (gc, initialFid) = let
		  fun draw [] = ()
		    | draw ((dst, oper)::r) = (
			requestDOP (dst, gc, initialFid, oper);
			draw r)
		  in
		    draw
		  end
	    val xid0 = XProtTypes.XID 0w0
	    fun drawBatch (NoGC, _, _, ops) = drawOps (xid0, xid0) ops
	      | drawBatch (NoFont, pen, mask, ops) = let
		  val gc = acqGC{pen = pen, used = mask}
		  in
		    drawOps (gc, xid0) ops;
		    relGC gc
		  end
	      | drawBatch (WithFont fid, pen, mask, ops) = let
		  val (gc, initFid) = acqGCWithFont{pen = pen, used = mask, fid = fid}
		  in
		    drawOps (gc, initFid) ops;
		    relGCAndFont gc
		  end
	      | drawBatch (SetFont fid, pen, mask, ops) = let
		  val gc = acqGCAndSetFont{pen = pen, used = mask, fid = fid}
		  in
		    drawOps (gc, fid) ops;
		    relGCAndFont gc
		  end
	    val draw = app drawBatch
	    fun flush buf = (draw (batchGC(buf, [])); XIo.flushOut conn)
	    in
	      flush
	    end (* flushBuf *)

    (* Insert a drawing command into the buffer, checking for possible batching
     * of operations.
     * BATCHING NOT IMPLEMENTED YET
     *)
      fun batchCmd (cnt, cmd, last, rest) = (cnt+1, cmd::last::rest)

      fun destroyWin conn (DSTRY_Win winId) = (
	  XIo.request conn (XRequest.encodeDestroyWindow{win = winId});
	  XIo.flushOut conn)
        | destroyWin conn (DSTRY_PM pmId) = (
	  XIo.request conn (XRequest.encodeFreePixmap{pixmap = pmId});
	  XIo.flushOut conn)

    (* create an overlay buffer on the drawing command stream.  This buffers
     * operations aimed at locked windows, and passes the others onto the
     * draw master.  releaseEvt is enabled when the overlay is released; this
     * causes the buffer to flush its buffered messages.  An event is returned
     * that signifies the completion of the flushing process.
     *)
      fun mkOverlayBuffer (winreg, newStrm, oldStrmEvt, releaseEvt) = let
	    val flushDone = SyncVar.iVar()
	    fun release buf = (
		  List.app (fn msg => CML.send(newStrm, msg)) (List.rev buf);
		  CML.send (newStrm, DMSG_Flush);
		  SyncVar.iPut (flushDone, ()))
	    fun loop buf = let
		  fun filterMsg (dst, m) = if (WinRegistry.isLocked (winreg, dst))
			  then loop(m::buf)
			  else (CML.send (newStrm, m); loop buf)
		  fun filter (m as DMSG{dst, ...}) = filterMsg (dst, m)
		    | filter (DMSG_Flush) = (CML.send (newStrm, DMSG_Flush); loop buf)
		    | filter (DMSG_CreateOverlay _) = MLXError.impossible
			"[multiple overlays not supported]"
		    | filter (m as (DMSG_Destroy (DSTRY_Win wid))) = filterMsg (wid, m)
		    | filter _ = MLXError.impossible
			"[unsupported message in DrawMaster.mkBuffer]"
		  in
		    CML.select [
		        CML.wrap (oldStrmEvt, filter),
			CML.wrap (releaseEvt, fn () => release buf)
		      ]
		  end
	    in
	      XDebug.xspawn("OverlayBuffer", fn () => loop []);
	      SyncVar.iGetEvt flushDone
	    end (* mkOverlayBuffer *)

    in

(** Need to check state transitions to insure no deadlock **)
    fun mkDM (configEvt, gcServer, winreg, conn) = let
	  val cmdCh = CML.channel()
	  val cmdEvt = CML.recvEvt cmdCh
	  val flush = flushBuf (gcServer, conn)
	  val flushDelay = CML.timeOutEvt(Time.fromMilliseconds 40)
	  val destroy = destroyWin conn
	  fun serverUnmapped () = let
		fun handleConfig DM_Map = serverMapped (0, [])
		  | handleConfig DM_Unmap = serverUnmapped()
		  | handleConfig _ = (
		      MLXError.impossible "[DM(unmapped): bad config command]")
		fun handleCmd (DMSG_Destroy id) = (destroy id; serverUnmapped())
		  | handleCmd (DMSG_CreateOverlay{cmd_strm, release_evt}) =
		      overlay(false, cmd_strm, release_evt)
		  | handleCmd _ = serverUnmapped()
		in
(* XDebug.trace(XDebug.dmTM, fn => ["DrawMaster: serverUnmapped\n"]); *)
		  CML.select [
		      CML.wrap(cmdEvt, handleCmd),
		      CML.wrap(configEvt, handleConfig)
		    ]
		end
	  and serverMapped (_, []) = let
		fun handleConfig DM_Unmap = serverUnmapped()
		  | handleConfig DM_Map = serverMapped (0, [])
		  | handleConfig _ = (
		      MLXError.impossible "[DM(mapped): bad config command]")
		fun handleCmd (DMSG m) = serverMapped (1, [m])
		  | handleCmd (DMSG_Flush) = serverMapped (0, [])
		  | handleCmd (DMSG_CreateOverlay{cmd_strm, release_evt}) =
		      overlay(true, cmd_strm, release_evt)
		  | handleCmd (DMSG_Destroy id) = (destroy id; serverMapped (0, []))
		  | handleCmd _ =
		      MLXError.impossible "DrawMaster: user batching not supported yet"
		in
(* XDebug.trace(XDebug.dmTM, fn => ["DrawMaster: serverMapped (empty)\n"]); *)
		  CML.select [
		    CML.wrap(cmdEvt, handleCmd),
		    CML.wrap(configEvt, handleConfig)
		  ]
		end
	    | serverMapped (cnt, buf as (last::r)) =  let
		fun handleConfig DM_Unmap = serverUnmapped()
		  | handleConfig DM_Map = serverMapped (cnt, buf)
		  | handleConfig _ = (
		      MLXError.impossible "[DM(mapped): bad config command]")
		fun handleCmd (DMSG m) = serverMapped (batchCmd(cnt, m, last, r))
		  | handleCmd (DMSG_Flush) = (flush buf; serverMapped(0, []))
		  | handleCmd (DMSG_CreateOverlay{cmd_strm, release_evt}) = (
		      flush buf; overlay(true, cmd_strm, release_evt))
		  | handleCmd (DMSG_Destroy id) = (
		     flush buf; destroy id; serverMapped (0, []))
		  | handleCmd _ =
		      MLXError.impossible "DrawMaster: user batching not supported yet"
		in
(* XDebug.trace(XDebug.dmTM, fn => ["DrawMaster: serverMapped (", makestring(length buf), ")\n"]); *)
		  if (cnt > fullBufferSz)
		    then (flush buf; serverMapped(0, []))
		    else CML.select [
		        CML.wrap(flushDelay,
			  fn _ => (flush buf; serverMapped(0, []))),
		        CML.wrap(cmdEvt, handleCmd),
		        CML.wrap(configEvt, handleConfig)
		      ]
		end
	  and overlay (isMapped, newStrm, releaseEvt) = let
		val flushDoneEvt = mkOverlayBuffer (winreg, newStrm, cmdEvt, releaseEvt)
		val newCmdEvt = CML.recvEvt newStrm
		fun overlayUnmapped () = let
		      fun handleConfig DM_Map = overlayMapped (0, [])
			| handleConfig DM_Unmap = overlayUnmapped()
			| handleConfig _ = MLXError.impossible
			    "[DM(unmapped-overlay): bad config command]"
		      fun handleCmd (DMSG_Destroy id) = (
			    destroy id; overlayUnmapped())
			| handleCmd _ = overlayUnmapped()
		      in
(* XDebug.trace(XDebug.dmTM, fn => ["DrawMaster: overlayUnmapped\n"]); *)
			CML.select [
			    CML.wrap(flushDoneEvt,  fn _ => false),
			    CML.wrap(newCmdEvt, handleCmd),
			    CML.wrap(configEvt, handleConfig)
			  ]
		      end
		and overlayMapped (_, []) = let
		      fun handleConfig DM_Unmap = overlayUnmapped()
			| handleConfig DM_Map = overlayMapped (0, [])
			| handleConfig _ = MLXError.impossible
			    "[DM(mapped-overlay): bad config command]"
		      fun handleCmd (DMSG m) = overlayMapped (1, [m])
			| handleCmd (DMSG_Flush) = overlayMapped (0, [])
			| handleCmd (DMSG_Destroy id) = (destroy id; overlayMapped (0, []))
			| handleCmd _ =
			    MLXError.impossible "[DM(mapped-overlay): bad command]"
		      in
(* XDebug.trace(XDebug.dmTM, fn => ["DrawMaster: overlayMapped (empty)\n"]); *)
			CML.select [
			    CML.wrap(flushDoneEvt, fn _ => true),
			    CML.wrap(newCmdEvt, handleCmd),
			    CML.wrap(configEvt, handleConfig)
			  ]
		      end
		  | overlayMapped (cnt, buf as (last::r)) =  let
		      fun handleConfig DM_Unmap = overlayUnmapped()
			| handleConfig DM_Map = overlayMapped (cnt, buf)
			| handleConfig _ = (
			    MLXError.impossible "[DM(mapped): bad config command]")
		      fun handleCmd (DMSG m) =
			    overlayMapped (batchCmd(cnt, m, last, r))
			| handleCmd (DMSG_Flush) = (flush buf; overlayMapped(0, []))
			| handleCmd (DMSG_Destroy id) = (
			    flush buf; destroy id; overlayMapped (0, []))
			| handleCmd _ = MLXError.impossible
			    "DrawMaster: user batching not supported yet"
		      in
(* XDebug.trace(XDebug.dmTM, fn => ["DrawMaster: overlayMapped (", makestring(length buf), ")\n"]); *)
			CML.select [
			    CML.wrap(flushDoneEvt, fn _ => (flush buf; true)),
			    CML.wrap(flushDelay, fn _ => (flush buf; overlayMapped(0, []))),
			    CML.wrap(newCmdEvt, handleCmd),
			    CML.wrap(configEvt, handleConfig)
			  ]
		      end
		fun doOverlay () =
		      if isMapped then overlayMapped(0, []) else overlayUnmapped()
		in
		  if doOverlay() then serverMapped (0, []) else serverUnmapped ()
		end (* overlay *)
		
	  fun serverInit () = (case (CML.sync configEvt)
		 of DM_FirstExpose => serverMapped(0, [])
		  | _ => serverInit()
		(* end case *))
	  in
	    XDebug.xspawn("DrawMaster", serverInit);
	    fn msg => CML.send (cmdCh, msg)
	  end (* mkDM *)

    end (* local *)
  end (* DrawMaster *)
