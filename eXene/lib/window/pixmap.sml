(* pixmap.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure Pixmap =
  struct
    local
      open XRequest XDisplay Display DrawTypes
    in

    exception BadPixmapParameter

    fun createPixmap scr (sz, depth) = let
	  val SCREEN{
		  scr=SCR{xscr=XSCR{root, ...}, ...},
		  dpy=DPY{xdpy=XDPY{conn, nextXId, ...}, ...}
		} = scr
	  val scr_depth = (serversOfScrDepth (scr, depth))
            handle MLXError.XERROR _ => raise BadPixmapParameter 
	  val id = nextXId()
	  in
            if Geometry.validSize sz then () else raise BadPixmapParameter;
	    XIo.request conn (XRequest.encodeCreatePixmap{
		pixmap = id, drawable = root, size = sz, depth = depth
	      });
	    PM{id = id, scr = scr, sz = sz, scr_depth = scr_depth}
	  end

  (* Destroy a pixmap.  We do this via the draw-master, to avoid a race with any
   * pending draw requests on the pixmap.
   *)
    fun destroyPixmap (PM{id, scr_depth=SCRDEPTH{draw_cmd, ...}, ...}) =
	  draw_cmd (DrawMaster.DMSG_Destroy (DrawMaster.DSTRY_PM id))

    end
  end (* Pixmap *)

