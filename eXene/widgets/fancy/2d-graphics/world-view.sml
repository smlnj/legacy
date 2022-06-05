(* world-view.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

signature WORLD_VIEW =
  sig
    type view

(* DEBUG *
val prView : view -> unit
* DEBUG *)

    val makeView : {
	    minX:real, maxX:real, minY:real, maxY:real, scale:real,
	    wid:int, ht:int
	  } -> view

    val pixelsPerInch : view -> real
	(* return the pixel scaling factor of the view (i.e., # of pixels
	 * per inch).
	 *)

    val scale : view -> real
	(* return the scaling factor (i.e., the number of inches on the screen
	 * per inch in world coordinates).
	 *)

    val hScrollView : (view * real) -> (view * int)
    val vScrollView : (view * real) -> (view * int)
	(* Scroll a view horizontally or vertically; the amount is given as a
	 * percentage of the visible whole.  Return the number of pixels to scroll
	 * and the new view.
	 *)

    val zoomView    : (view * real) -> view
	(* Zoom a view: a zoom of 1.0 is no change; a zoom of 2.0 doubles the size
	 * of objects in the view (i.e., zooms in), while a zoom of 0.5 halves
	 * the size of objects in the view.  This will not zoom out beyond the
	 * view's bounding box.
	 *)

    val viewSize : view -> {top:real, vSize:real, left:real, hSize:real}
	(* return the size and position of the view as a percentage of the
	 * total horizontal and vertical dimensions of the world.
	 *)

    val viewBBox : view -> {minX : real, maxX : real, minY : real, maxY : real}
	(* return the bounding box of the view *)

    val viewVBox : view -> {minX : real, maxX : real, minY : real, maxY : real}
	(* return the visible box *)

    val uCoordToPixel : (view * real * real) -> Geometry.point
    val uCoordToSize  : (view * real * real) -> Geometry.size
    val pixelToUCoord : (view * Geometry.point) -> (real * real)
	(* mappings from universal to screen coordinates, and back *)

    val scaleFontSz : (view * int) -> int
	(* given a font size (in points) return the appropriate font size
	 * for the current view.
	 *)

    val pixelRadius  : view -> real
	(* return the radius of a pixel in universal coordinates *)

  end;

structure WorldView : WORLD_VIEW =
  struct

    structure T2D = Transform2D

  (* the following data structure describes a view; it consists of the
   * "world" (defined by left, right, top & bottom), and a sub-region
   * of the world (defined by viewX, viewY, viewWid, viewHt).  In
   * addition, information about the mapping onto a window of pixels is
   * maintained.
   *)
    type view = {
	winWid : int,		(* the size of the view in pixels. *)
	winHt : int,
	left : real,		(* the bounding box of the world, in *)
	right : real,		(* universal coordinates *)
	top : real,
	bottom : real,
	scale : real,		(* the current scaling factor *)
	pixelScale : real,	(* the current scaling factor; this is the *)
				(* number of pixels per universal coordinate *)
	viewX : real,		(* the center of the view in universal *)
	viewY : real,		(* coordinates. *)
	viewWid : real,		(* the size of the view in universal *)
	viewHt : real,		(* coordinates. *)
	xform : T2D.xform	(* the transformation matrix that maps universal *)
				(* coordinates to pixel coordinates *)
      }

(* DEBUG *
local open Debug in
fun prView (v : view) = (
      prf ("  WINDOW: (%d, %d), scale = %f, pixelScale = %f\n", [
	  F.INT(#winWid v), F.INT(#winHt v), F.REAL(#scale v), F.REAL(#pixelScale v)
	]);
      prf ("  WORLD: l = %f, r = %f, t = %f, b = %f\n", [
	  F.REAL(#left v), F.REAL(#right v), F.REAL(#top v), F.REAL(#bottom v)
	]);
      prf ("  VIEW: x = %f, y = %f, wid = %f, ht = %f\n", [
	  F.REAL(#viewX v), F.REAL(#viewY v), F.REAL(#viewWid v), F.REAL(#viewHt v)
	]))
end
* DEBUG *)

  (* real utility functions *)
    fun min (x : real, y) = if (x < y) then x else y
    fun max (x : real, y) = if (x > y) then x else y
    fun round x = floor(x + 0.5)

    fun worldWid (v : view) = (#right v - #left v)
    fun worldHt (v : view) = (#top v - #bottom v)

  (* return the transformation matrix that maps universal coordinates to
   * pixel coordinates.
   *)
    fun transform {scale, viewX, viewY, viewWid, viewHt} = let
	  val hw = 0.5*viewWid and hh = 0.5*viewHt
	  in
	    T2D.translate (scale*hw, scale*hh,
	      T2D.scale (scale, ~scale,
		T2D.translate (~viewX, ~viewY,
		  T2D.id)))
	  end

  (* given a view, ensure that the world's bounding box is contained in the
   * view box; if not, grow the world.
   *)
    fun growWorld (v : view) = let
	  fun bbox (pos, wid) = let val half = 0.5*wid
		in
		  (pos-half, pos+half)
		end
	  val (viewLeft, viewRight) = bbox(#viewX v, #viewWid v)
	  val (viewBot, viewTop) = bbox(#viewY v, #viewHt v)
	  in
	    if ((viewLeft < #left v) orelse (#right v < viewRight)
	    orelse (viewBot < #bottom v) orelse (#top v < viewTop))
	      then {
		  winWid	= #winWid v,
		  winHt		= #winHt v,
		  left		= min(#left v, viewLeft),
		  right		= max(#right v, viewRight),
		  top		= max(#top v, viewTop),
		  bottom	= min(#bottom v, viewBot),
		  scale		= #scale v,
		  pixelScale	= #pixelScale v,
		  viewX		= #viewX v,
		  viewY		= #viewY v,
		  viewWid	= #viewWid v,
		  viewHt	= #viewHt v,
		  xform		= #xform v
		}
	      else v
	  end

  (* given the world bounding box, view size in pixels, and initial scaling
   * factor, create a view centered on the world.
   *)
    fun makeView {minX, maxX, minY, maxY, scale, wid, ht} = let
	  val worldWid = maxX - minX and worldHt = maxY - minY
	(* size of viewing area in universal coordinates *)
	  val viewWid = (real wid) / scale
	  val viewHt = (real ht) / scale
	(* center of world *)
	  val viewX = minX + (0.5 * worldWid)
	  val viewY = minY + (0.5 * worldHt)
	(* if necessary, extend the world to cover the view *)
	  val worldWid = max (worldWid, viewWid)
	  val worldHt = max (worldHt, viewHt)
	  in
	    { winWid	= wid,
	      winHt	= ht,
	      left	= viewX - (0.5 * worldWid),
	      right	= viewX + (0.5 * worldWid),
	      top	= viewY + (0.5 * worldHt),
	      bottom	= viewY - (0.5 * worldHt),
	      scale	= 1.0,
	      pixelScale= scale,
	      viewX	= viewX,
	      viewY	= viewY,
	      viewWid	= viewWid,
	      viewHt	= viewHt,
	      xform	= transform {
		  scale = scale, viewX = viewX, viewY = viewY,
		  viewWid = viewWid, viewHt = viewHt
		}
	    }
	  end

  (* return the pixel scaling factor of the view (i.e., # of pixels
   * per inch).
   *)
    fun pixelsPerInch (v : view) = #pixelScale v

  (* return the scaling factor (i.e., the number of inches on the screen
   * per inch in world coordinates).
   *)
    fun scale (v : view) = #scale v

  (* move the view in universal coordinates *)
    fun moveView (v : view, dx, dy) = let
	  fun clip (x, lo, hi) =
		if (x < lo) then lo else if (hi < x) then hi else x
	  val hw = 0.5*(#viewWid v) and hh = 0.5*(#viewHt v)
	  val viewX = clip((#viewX v) + dx, (#left v)+hw, (#right v)-hw)
	  val viewY = clip((#viewY v) + dy, (#bottom v)+hh, (#top v)-hh)
	  in
	    { winWid	= #winWid v,
	      winHt	= #winHt v,
	      left	= #left v,
	      right	= #right v,
	      top	= #top v,
	      bottom	= #bottom v,
	      scale	= #scale v,
	      pixelScale= #pixelScale v,
	      viewX	= viewX,
	      viewY	= viewY,
	      viewWid	= #viewWid v,
	      viewHt	= #viewHt v,
	      xform	= transform {
		  scale = #pixelScale v, viewX = viewX, viewY = viewY,
		  viewWid = #viewWid v, viewHt = #viewHt v
	        }
	    }
	  end

  (* Scroll a view horizontally; the amount is given as a percentage of the
   * visible whole.  Return the number of pixels to scroll, and the new view.
   *)
    fun hScrollView (v : view, pDelta) = let
	  val delta = pDelta * (#viewWid v)
	  val v' = moveView (v, delta, 0.0)
	  in
	    (v', round(((#viewX v') - (#viewX v)) * (#pixelScale v')))
	  end

  (* Scroll a view vertically; the amount is given as a percentage of the
   * whole.  Return the number of pixels to scroll, and the new view.
   *)
    fun vScrollView (v : view, pDelta) = let
	  val delta = pDelta * (#viewWid v)
	  val v' = moveView (v, 0.0, delta)
	  in
	    (v', round(((#viewY v') - (#viewY v)) * (#pixelScale v')))
	  end

  (* Zoom a view: a zoom of 1.0 is no change; a zoom of 2.0 doubles the size
   * of objects in the view (i.e., zooms in), while a zoom of 0.5 halves
   * the size of objects in the view.  This will not zoom out beyond the
   * view's bounding box.
   *)
    fun zoomView (v : view, zoom) = let
	  val zoomInv = 1.0 / zoom
	  val pixelScale = (#pixelScale v) * zoom
	  val viewWid = (#viewWid v) * zoomInv
	  val viewHt = (#viewHt v) * zoomInv
	  in
	    growWorld {
		winWid		= #winWid v,
		winHt		= #winHt v,
		left		= #left v,
		right		= #right v,
		top		= #top v,
		bottom		= #bottom v,
		scale		= zoom * (#scale v),
		pixelScale	= pixelScale,
		viewX		= #viewX v,
		viewY		= #viewY v,
		viewWid		= viewWid,
		viewHt		= viewHt,
		xform		= transform {
		    scale = pixelScale, viewX = #viewX v, viewY = #viewY v,
		    viewWid = viewWid, viewHt = viewHt
		  }
	      }
	  end (* zoomView *)

  (* return the size of the view as a percentage of the total horizontal
   * and vertical dimensions of the world.
   *)
    fun viewSize (v : view) = let
	  val wwInv = 1.0 / worldWid v and vw = #viewWid v
	  val whInv = 1.0 / worldHt v and vh = #viewHt v
	  in
	    { top = ((#top v) - (#viewY v + 0.5*vh)) * whInv,
	      vSize = vh * whInv,
	      left = ((#viewX v - 0.5*vw) - (#left v)) * wwInv,
	      hSize = vw * wwInv
	    }
	  end

  (* return the bounding box of the view *)
    fun viewBBox (v : view) = {
	    minX = (#left v), maxX = (#right v),
	    minY = (#bottom v), maxY = (#top v)
	  }

  (* return the visible box *)
    fun viewVBox (v : view) = let
	  val dx = 0.5 * (#viewWid v) and dy = 0.5 * (#viewHt v)
	  in
	    { minX = (#viewX v) - dx, maxX = (#viewX v) + dx,
	      minY = (#viewY v) - dy, maxY = (#viewY v) + dy
	    }
	  end

  (* map a universal coordinate to pixel coordinates *)
    fun uCoordToPixel (v : view, x, y) = let
	  val (x, y) = T2D.pixelize (x, y, #xform v)
	  in
	    Geometry.PT{x=x, y=y}
	  end

  (* map a universal coordinate to a size in pixels; this is like the
   * uCoordToPixel conversion, except that no translation is done.
   *)
    fun uCoordToSize ({pixelScale, ...} : view, x, y) = Geometry.SIZE{
	    wid = round(pixelScale * x), ht = round(pixelScale * y)
	  }

  (* map a pixel coordinate to universal coordinates *)
    fun pixelToUCoord (v : view, Geometry.PT{x, y}) =
	  T2D.transform (real x, real y, T2D.inverse(#xform v))

  (* given a font size (in points) return the appropriate font size
   * for the current view.
   *)
    fun scaleFontSz (v : view, ptSz) = round(real ptSz * (#scale v))

  (* return the radius of a pixel *)
    fun pixelRadius (v : view) = 0.5 / (#pixelScale v)

  end; (* WorldView *)

