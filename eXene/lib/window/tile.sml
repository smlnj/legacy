(* tile.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Tiles are immutable pixmaps.  They can be created from bitmap data, or
 * by bitblting a rectangle from a pixmap or window.
 *)

signature TILE =
  sig

    type tile

    val createTileFromImage : Display.screen -> Image.image -> tile
    val createTileFromAsciiData : Display.screen -> (int * string list list) -> tile
    val createTileFromPixmap : DrawTypes.pixmap -> tile

  end (* TILE *)

structure Tile : TILE=
  struct
    open DrawTypes

    fun createTileFromImage scr im = TILE(Image.createPixmapFromImage scr im)
    fun createTileFromAsciiData scr data = TILE(Image.createPixmapFromAsciiData scr data)

    fun createTileFromPixmap (pm as PM{scr, sz, scr_depth, ...}) = let
	  open Draw Geometry
	  val Display.SCRDEPTH{depth, ...} = scr_depth
	  val newPM = Pixmap.createPixmap scr (sz, depth)
	  in
	    pixelBlt (drawableOfPM newPM) Pen.defaultPen {
		src = PMSRC pm, src_rect = mkRect(originPt, sz), dst_pos = originPt
	      };
	    TILE newPM
	  end

  end (* Tile *)
