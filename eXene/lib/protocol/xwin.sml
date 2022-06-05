(* xwin.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure XWin : sig

  (* window configuration values *)
    datatype xwin_val
      = XWV_BackgroundPixmap_None
      | XWV_BackgroundPixmap_ParentRelative
      | XWV_BackgroundPixmap of XProtTypes.pixmap_id
      | XWV_BackgroundPixel of XProtTypes.pixel
      | XWV_BorderPixmap_CopyFromParent
      | XWV_BorderPixmap of XProtTypes.pixmap_id
      | XWV_BorderPixel of XProtTypes.pixel
      | XWV_BitGravity of XProtTypes.gravity
      | XWV_WinGravity of XProtTypes.gravity
      | XWV_BackingStore of XProtTypes.backing_store
      | XWV_BackingPlanes of XProtTypes.plane_mask
      | XWV_BackingPixel of XProtTypes.pixel
      | XWV_SaveUnder of bool
      | XWV_EventMask of XProtTypes.event_mask
      | XWV_DoNotPropagateMask of XProtTypes.event_mask
      | XWV_OverrideRedirect of bool
      | XWV_ColorMap_CopyFromParent
      | XWV_ColorMap of XProtTypes.colormap_id
      | XWV_Cursor_None
      | XWV_Cursor of XProtTypes.cursor_id

    val newXWin : XIo.connection -> {
	    id : XProtTypes.win_id,
	    parent : XProtTypes.win_id,
	    in_only : bool option,
	    depth : int,
	    visual : XProtTypes.visual_id option,
	    geom : Geometry.win_geom,
	    attrs : xwin_val list
	  } -> unit
	(* Create a new X-window with the given xid *)

    val mapXWin : XIo.connection -> XProtTypes.win_id -> unit
	(* Map a window *)

    val changeXWinAttrs : XIo.connection -> (XProtTypes.win_id * xwin_val list)
	  -> unit
	(* change window attributes *)

  end = struct

    structure G = Geometry
    structure XTy = XProtTypes
    structure XDpy = XDisplay

  (* window configuration values *)
    datatype xwin_val
      = XWV_BackgroundPixmap_None
      | XWV_BackgroundPixmap_ParentRelative
      | XWV_BackgroundPixmap of XProtTypes.pixmap_id
      | XWV_BackgroundPixel of XProtTypes.pixel
      | XWV_BorderPixmap_CopyFromParent
      | XWV_BorderPixmap of XProtTypes.pixmap_id
      | XWV_BorderPixel of XProtTypes.pixel
      | XWV_BitGravity of XProtTypes.gravity
      | XWV_WinGravity of XProtTypes.gravity
      | XWV_BackingStore of XProtTypes.backing_store
      | XWV_BackingPlanes of XProtTypes.plane_mask
      | XWV_BackingPixel of XProtTypes.pixel
      | XWV_SaveUnder of bool
      | XWV_EventMask of XProtTypes.event_mask
      | XWV_DoNotPropagateMask of XProtTypes.event_mask
      | XWV_OverrideRedirect of bool
      | XWV_ColorMap_CopyFromParent
      | XWV_ColorMap of XProtTypes.colormap_id
      | XWV_Cursor_None
      | XWV_Cursor of XProtTypes.cursor_id

    fun doWinVal arr = let
	  fun update (i, x) = Array.update (arr, i, SOME x)
	  in
	    fn (XWV_BackgroundPixmap_None) => update (0, 0w0)
	     | (XWV_BackgroundPixmap_ParentRelative) => update (0, 0w1)
	     | (XWV_BackgroundPixmap(XTy.XID p)) => update (0, p)
	     | (XWV_BackgroundPixel(XTy.PIXEL p)) => update (1, Word.fromInt p)
	     | (XWV_BorderPixmap_CopyFromParent) => update (2, 0w0)
	     | (XWV_BorderPixmap(XTy.XID p)) => update (2, p)
	     | (XWV_BorderPixel(XTy.PIXEL p)) => update (3, Word.fromInt p)
	     | (XWV_BitGravity g) => update (4, XCvtFuns.gravityToWire g)
	     | (XWV_WinGravity g) => update (5, XCvtFuns.gravityToWire g)
	     | (XWV_BackingStore XTy.BS_NotUseful) => update (6, 0w0)
	     | (XWV_BackingStore XTy.BS_WhenMapped) => update (6, 0w1)
	     | (XWV_BackingStore XTy.BS_Always) => update (6, 0w2)
	     | (XWV_BackingPlanes(XTy.PLANEMASK m)) => update (7, m)
	     | (XWV_BackingPixel(XTy.PIXEL p)) => update (8, Word.fromInt p)
	     | (XWV_OverrideRedirect b) => update (9, XCvtFuns.boolToWire b)
	     | (XWV_SaveUnder b) => update (10, XCvtFuns.boolToWire b)
	     | (XWV_EventMask(XTy.XEVTMASK m)) => update (11, m)
	     | (XWV_DoNotPropagateMask(XTy.XEVTMASK m)) => update (12, m)
	     | (XWV_ColorMap_CopyFromParent) => update (13, 0w0)
	     | (XWV_ColorMap(XTy.XID x)) => update (13, x)
	     | (XWV_Cursor_None) => update (14, 0w0)
	     | (XWV_Cursor(XTy.XID x)) => update (14, x)
	  end
    val doWinValList = XCvtFuns.doValList 15 doWinVal

  (* Create a new X-window with the given xid *)
    fun newXWin conn {id, parent, in_only, depth, visual, geom, attrs} = let
	  val msg = XRequest.encodeCreateWindow {
		  win = id,
		  parent = parent,
		  input_only = in_only,
		  depth = depth,
		  visual = visual,
		  geom = geom,
		  vals = doWinValList attrs
		}
	  in
	    XIo.request conn msg
	  end

  (* Map a window *)
    fun mapXWin conn w = (XIo.request conn (XRequest.encodeMapWindow{win=w}))

  (* change window attributes *)
    fun changeXWinAttrs conn (win, attrs) = (
	  XIo.request conn (XRequest.encodeChangeWindowAttributes{
	      win = win, vals = doWinValList attrs
	    });
	  XIo.flushOut conn)

  end (* XWin *)
