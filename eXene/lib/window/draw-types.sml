(* draw-types.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Types of objects that can be drawn on (or are pixel sources).
 *)

structure DrawTypes : sig

  (* An on-screen bitmap *)
    datatype window = WIN of {
	id : XProtTypes.win_id,
	scr : Display.screen,
	scr_depth : Display.scr_depth,
	draw_cmd : DrawMaster.draw_msg -> unit
      }

  (* An off-screen bitmap *)
    datatype pixmap = PM of {  
	id : XProtTypes.pixmap_id,
	scr : Display.screen,
	sz : Geometry.size,
	scr_depth : Display.scr_depth
      }

  (* immutable pixmaps *)
    datatype tile = TILE of pixmap

    val sameWindow	: (window * window) -> bool
    val samePixmap	: (pixmap * pixmap) -> bool
    val sameTile	: (tile * tile) -> bool

  (* Sources for bitblt operations *)
    datatype draw_src
      = WSRC of window
      | PMSRC of pixmap
      | TSRC of tile

    val depthOfWin	: window -> int
    val depthOfPixmap	: pixmap -> int
    val depthOfTile	: tile -> int
    val depthOfDrawSrc	: draw_src -> int

    val geomOfWin	: window -> {
	    pos : Geometry.point, sz : Geometry.size, depth : int, border : int
	  }
    val geomOfPixmap	: pixmap  -> {
	    pos : Geometry.point, sz : Geometry.size, depth : int, border : int
	  }
    val geomOfTile	: tile  -> {
	    pos : Geometry.point, sz : Geometry.size, depth : int, border : int
	  }
    val geomOfDrawSrc	: draw_src  -> {
	    pos : Geometry.point, sz : Geometry.size, depth : int, border : int
	  }

    val sizeOfWin	: window -> Geometry.size
    val sizeOfPixmap	: pixmap -> Geometry.size
    val sizeOfTile	: tile -> Geometry.size

    datatype draw_root = DWIN of window | DPM of pixmap
    datatype drawable = DRAWABLE of {
	root : draw_root,
	draw_cmd : DrawMaster.draw_msg -> unit
      }

    val drawableOfWin	: window -> drawable
    val drawableOfPM	: pixmap -> drawable

    val depthOfDrawable : drawable -> int

    val feedback : drawable -> drawable

    val createOverlay : window -> {drawable : drawable, release : unit -> unit}

  end = struct

    structure G = Geometry
    structure XTy = XProtTypes
    structure Dpy = Display
    structure DM = DrawMaster

  (* An on-screen bitmap *)
    datatype window = WIN of {
	id : XTy.win_id,
	scr : Dpy.screen,
	scr_depth : Dpy.scr_depth,
	draw_cmd : DM.draw_msg -> unit
      }

  (* An off-screen bitmap *)
    datatype pixmap = PM of {  
	id : XTy.pixmap_id,
	scr : Dpy.screen,
	sz : G.size,
	scr_depth : Dpy.scr_depth
      }

  (* immutable pixmaps *)
    datatype tile = TILE of pixmap

  (* identity tests *)
    fun sameWindow (WIN{id=id1, scr=s1, ...}, WIN{id=id2, scr=s2, ...}) =
	  ((id1 = id2) andalso Dpy.sameScreen(s1, s2))
    fun samePixmap (PM{id=id1, scr=s1, ...}, PM{id=id2, scr=s2, ...}) =
	  ((id1 = id2) andalso Dpy.sameScreen(s1, s2))
    fun sameTile (TILE p1, TILE p2) = samePixmap(p1, p2)

  (* Sources for bitblt operations *)
    datatype draw_src
      = WSRC of window
      | PMSRC of pixmap
      | TSRC of tile

    fun depthOfWin (WIN{scr_depth=Dpy.SCRDEPTH{depth, ...}, ...}) = depth
    fun depthOfPixmap (PM{scr_depth=Dpy.SCRDEPTH{depth, ...}, ...}) = depth
    fun depthOfTile (TILE(PM{scr_depth=Dpy.SCRDEPTH{depth, ...}, ...})) = depth

    fun depthOfDrawSrc (WSRC win) = depthOfWin win
      | depthOfDrawSrc (PMSRC pm) = depthOfPixmap pm
      | depthOfDrawSrc (TSRC tile) = depthOfTile tile

    fun geomOfWin (WIN{id, scr=Dpy.SCREEN{dpy, ...}, ...}) = let
	  open XRequest XReply
	  val reply = CML.sync (Dpy.dpyRequestReply dpy (encodeGetGeometry {drawable=id}))
	  val {depth, geom=G.WGEOM{pos, sz, border}, ...} = decodeGetGeometryReply reply
	  in
	    {pos = pos, sz = sz, depth = depth, border = border}
	  end
    fun geomOfPixmap (PM{sz, scr_depth=Dpy.SCRDEPTH{depth, ...}, ...}) = {
	    pos = G.originPt, sz = sz, depth = depth, border = 0
	  }
    fun geomOfTile (TILE pm) = geomOfPixmap pm

    fun geomOfDrawSrc (WSRC w) = geomOfWin w
      | geomOfDrawSrc (PMSRC pm) = geomOfPixmap pm
      | geomOfDrawSrc (TSRC(TILE pm)) = geomOfPixmap pm

    fun sizeOfWin win = let
	  val {sz, ...} = geomOfWin win
	  in
	    sz
	  end
    fun sizeOfPixmap (PM{sz, ...}) = sz
    fun sizeOfTile (TILE pm) = sizeOfPixmap pm


  (** drawables **
   *
   * these are abstract views of drawable objects (e.g., windows or pixmaps).
   *)
    datatype draw_root = DWIN of window | DPM of pixmap
    datatype drawable = DRAWABLE of {
	root : draw_root,
	draw_cmd : DM.draw_msg -> unit
      }

  (* make a drawable from a window *)
    fun drawableOfWin (w as WIN{draw_cmd, ...}) =
	  DRAWABLE{root = DWIN w, draw_cmd = draw_cmd}

  (* make a drawable from a pixmap *)
    fun drawableOfPM (pm as PM{sz, scr_depth=Dpy.SCRDEPTH{draw_cmd, ...}, ...}) = let
	  fun drawCmd (DM.DMSG{dst, pen, oper = DM.DOP_ClearArea(G.RECT{x, y, wid, ht})}) = let
		fun clip (z, 0, max) = max - z
		  | clip (z, w, max) = if ((z + w) > max) then (max - z) else w
		val G.SIZE{wid = pmWid, ht = pmHt} = sz
		val dstRect = G.RECT{
			x = x, y = y,
			wid = clip(x, wid, pmWid), ht = clip(y, ht, pmHt)
		      }
		in
		  draw_cmd (DM.DMSG{
		      dst = dst,
		      pen = PenRep.defaultPen,
		      oper = DM.DOP_PolyFillRect[dstRect]
		    });
                (* the following is needed to avoid race between updating the
		 * pixmap and using it as the source of a blt.
		 *)
		  draw_cmd DM.DMSG_Flush
		end
	    | drawCmd dmsg = draw_cmd dmsg
	  in
	    DRAWABLE{root = DPM pm, draw_cmd=drawCmd}
	  end

    fun depthOfDrawable (DRAWABLE{root = DWIN w, ...}) = depthOfWin w
      | depthOfDrawable (DRAWABLE{root = DPM pm, ...}) = depthOfPixmap pm

  (* Create an unbuffered drawable for real-time feedback.  This basically
   * works by adding flush messages after each draw command.  There is
   * probably a better way.
   *)
    fun feedback (DRAWABLE{root as DWIN w, draw_cmd}) = DRAWABLE{
	    root = root,
	    draw_cmd = fn msg => (draw_cmd msg; draw_cmd DM.DMSG_Flush)
	  }
      | feedback d = d

  (* the following exception is raised if an attempt is made to use a stale
   * overlay drawable (i.e., one that has been released).
   *)
    exception StaleOverlay

  (* Create an overlay drawable for the given window.  This provides concurrency
   * control on the window and its descendents during rubber-banding (using OP_Xor).
   * The first result is the overlay drawable, the second is the release operation
   * for the drawable.
   *)
    fun createOverlay (w as WIN{draw_cmd, ...}) = let
	  val releaseCV = SyncVar.iVar()
	  val newDrawCh = CML.channel()
	(* the draw command for the overlay.  It raises StaleOverlay if called
	 * after the overlay is released. *)
	  val errorEvt = CML.wrap (SyncVar.iGetEvt releaseCV, fn () => raise StaleOverlay)
	  fun drawFn msg = CML.select [
		  CML.sendEvt(newDrawCh, msg),
		  errorEvt
		]
	  fun drawAndFlush msg = (drawFn msg; drawFn DM.DMSG_Flush)
	(* the function used to release the overlay.  Multiple calls are allowed,
	 * so we must handle WriteTwice.
	 *)
	  fun releaseFn () = (SyncVar.iPut(releaseCV, ()) handle _ => ())
	  in
	    draw_cmd (DM.DMSG_CreateOverlay{
		cmd_strm = newDrawCh,
		release_evt = SyncVar.iGetEvt releaseCV
	      });
	    {
	      drawable = DRAWABLE{root = DWIN w, draw_cmd = drawAndFlush},
	      release = releaseFn
	    }
	  end

  end (* DrawTypes *)
