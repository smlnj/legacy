(* geom.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * The basic geometric types and operations.
 *)

structure Geometry =
  struct

    local
      open XValid

      fun min (x : int, y) = if x < y then x else y
      fun max (x : int, y) = if x > y then x else y
    in

  (* geometric types (from Xlib.h) *)
    datatype point = PT of {x : int, y : int}
    datatype line = LINE of point * point
    datatype size = SIZE of {wid : int, ht : int}
    datatype rect = RECT of {x : int, y : int, wid : int, ht : int}
    datatype arc = ARC of {
	x : int, y : int,
	wid : int, ht : int,
	angle1 : int, angle2 : int
      }

  (* The geometry of a window w.r.t. its parent. *)
    datatype win_geom = WGEOM of {
	pos : point,
	sz : size,
	border : int
      }

  (* points *)
    val originPt = PT{x=0, y=0}
    fun xCoordOfPt (PT{x, ...}) = x
    fun yCoordOfPt (PT{y, ...}) = y
    fun addPt (PT{x=x1, y=y1}, PT{x=x2, y=y2}) = PT{x=(x1+x2), y=(y1+y2)}
    fun subPt (PT{x=x1, y=y1}, PT{x=x2, y=y2}) = PT{x=(x1-x2), y=(y1-y2)}
    fun scalePt (s, PT{x, y}) = PT{x=s*x, y=s*y}
    fun lessThanPt (PT{x=x1, y=y1}, PT{x=x2, y=y2}) = (x1 < x2) andalso (y1 < y2)
    fun lessEqPt (PT{x=x1, y=y1}, PT{x=x2, y=y2}) = (x1 <= x2) andalso (y1 <= y2)

  (* size operations *)
    fun addSz (SIZE{wid=w1, ht=h1}, SIZE{wid=w2, ht=h2}) = SIZE{wid=(w1+w2), ht=(h1+h2)}
    fun subSz (SIZE{wid=w1, ht=h1}, SIZE{wid=w2, ht=h2}) = SIZE{wid=(w1-w2), ht=(h1-h2)}
    fun scaleSz (s, SIZE{wid, ht}) = SIZE{wid=s*wid, ht=s*ht}
    fun addSzToPt (PT{x, y}, SIZE{wid, ht}) = PT{x=x+wid, y=y+ht}
    fun limitPt (SIZE{wid, ht}, PT{x, y}) =
	  PT{
	      x = if (x <= 0) then 0 else if (x < wid) then x else (wid-1),
	      y = if (y <= 0) then 0 else if (y < ht) then y else (ht-1)
	    }

  (* rectangles *)
    fun mkRect (PT{x, y}, SIZE{wid, ht}) = RECT{x=x, y=y, wid=wid, ht=ht}
    fun originOfRect (RECT{x, y, ...}) = PT{x=x, y=y}
    fun sizeOfRect (RECT{wid, ht, ...}) = SIZE{wid=wid, ht=ht}
    fun originAndSzOfRect (RECT{x, y, wid, ht}) = (PT{x=x, y=y}, SIZE{wid=wid, ht=ht})
    fun cornerOfRect r = addSzToPt(originAndSzOfRect r)
    fun clipPt (RECT{x=minX, y=minY, wid, ht}, PT{x, y}) =
	  PT{
	      x = if (x <= minX) then minX else if (x < minX+wid) then x else (minX+wid-1),
	      y = if (y <= minY) then minY else if (y < minY+ht) then y else (minY+ht-1)
	    }
    fun translate (RECT{x, y, wid, ht}, PT{x=px, y=py}) =
	  RECT{x=x+px, y=y+py, wid=wid, ht=ht}
    fun rtranslate (RECT{x, y, wid, ht}, PT{x=px, y=py}) =
	  RECT{x=x-px, y=y-py, wid=wid, ht=ht}
    fun intersect (RECT{x=x1, y=y1, wid=w1, ht=h1},
	    RECT{x=x2, y=y2, wid=w2, ht=h2}) =
	  ((x1 < (x2+w2)) andalso (y1 < (y2+h2))
	  andalso (x2 < (x1+w1)) andalso (y2 < (y1+h1)))
    exception Intersection
    fun intersection (RECT{x=x1, y=y1, wid=w1, ht=h1},
	    RECT{x=x2, y=y2, wid=w2, ht=h2}) = let
	  val x = max(x1, x2) and y = max(y1, y2)
	  val cx = min(x1+w1, x2+w2) and cy = min(y1+h1, y2+h2)
	  in
	    if ((x < cx) andalso (y < cy))
	      then RECT{x=x, y=y, wid=(cx-x), ht=(cy-y)}
	      else raise Intersection
	  end
    fun union (
	  r1 as RECT{x=x1, y=y1, wid=w1, ht=h1},
	  r2 as RECT{x=x2, y=y2, wid=w2, ht=h2}
	) = if ((w1 = 0) orelse (h1 = 0))
	    then r2
	  else if ((w2 = 0) orelse (h2 = 0))
	    then r1
	    else let
	      val x = min(x1, x2) and y = min(y1, y2)
	      val cx = max(x1+w1, x2+w2) and cy = max(y1+h1, y2+h2)
	      in
		RECT{x=x, y=y, wid=(cx-x), ht=(cy-y)}
	      end
    fun within (PT{x=px, y=py}, RECT{x, y, wid, ht}) =
	  ((x <= px) andalso (y <= py)
          andalso (px < (x+wid)) andalso (py < (y+ht)))
    fun inside (RECT{x=x1, y=y1, wid=w1, ht=h1}, RECT{x=x2, y=y2, wid=w2, ht=h2}) =
	  ((x2 <= x1) andalso (y2 <= y1)
	  andalso ((x1+w1) <= (x2+w2))
	  andalso ((y1+h1) <= (y2+h2)))
    fun boundBox [] = RECT{x=0, y=0, wid=0, ht=0}
      | boundBox ((PT{x, y}) :: pts) = let
          fun bb (minx, miny, maxx, maxy, []) = 
               RECT{x = minx, y = miny, wid = maxx-minx+1, ht = maxy-miny+1}
            | bb (minx, miny, maxx, maxy, (PT{x, y}) :: pts) = 
               bb (min(minx, x), min(miny, y), max(maxx, x), max(maxy, y), pts)
          in
            bb (x, y, x, y, pts)
          end

  (* Validation routines *)
    fun validPt (PT{x, y}) = (validSigned16 x) andalso (validSigned16 y)
    fun validLine (LINE(p1, p2)) = (validPt p1) andalso (validPt p2)
    fun validSize (SIZE{wid, ht}) = (valid16 wid) andalso (valid16 ht)
    fun validRect (RECT{x, y, wid, ht}) =
      (validSigned16 x) andalso (validSigned16 y) andalso 
      (valid16 wid) andalso (valid16 ht)

    fun validArc (ARC{x, y, wid, ht, angle1, angle2}) =
      (validSigned16 x) andalso (validSigned16 y) andalso 
      (valid16 wid) andalso (valid16 ht) andalso 
      (validSigned16 angle1) andalso (validSigned16 angle2)

    fun validGeom (WGEOM{pos, sz, border}) = 
      (validPt pos) andalso (validSize sz) andalso (valid16 border)

    end (* local *)
  end (* Geometry *)
