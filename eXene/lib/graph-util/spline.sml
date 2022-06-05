(* spline.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *)

structure Spline : SPLINE =
  struct

    structure G = Geometry
    open G

    fun round x = if x > 0.0 then floor (x+0.5) else ~1*floor(~x+0.5)

    fun addSeg ([], x0,y0,x1,y1) = 
          [PT{x = round x0, y = round y0}, PT{x = round x1, y = round y1}]
      | addSeg (l, x0, y0, _,_) = (PT{x = round x0, y = round y0})::l

  (* isFlat:
   * Returns true if the polygon determined by the four points
   * is flat enough. Flatness is measured by the maximum distance
   * of (x1,y1) and (x2,y2) from the line determined by (x0,y0)
   * and (x3,y3). In addition, check that p1, p2 are close to the 
   * line segment. To do this, make sure they are roughly within 
   * the circle with center (p0+p3)/2 and radius = |p3-p0|/2+flatness
   *)
    fun isFlat {x0, y0, x1, y1, x2, y2, x3, y3} = let
	  fun sqr x = x * x
	  val dx = x3 - x0 and dy = y3 - y0
	  val midx = 0.5*dx and midy = 0.5*dy and dist2 = sqr dy + sqr dx
	  val flatness2 = sqr 1.0 * dist2 and halfd2 = 0.25*dist2
	  fun inFlatRange (x,y) = sqr(dy * x - dx * y) <= flatness2
		andalso let val d = sqr(x-midx) + sqr(y-midy)
			in d<=halfd2 orelse sqr(d-halfd2)<=flatness2 end
	  in
	    inFlatRange (x1-x0, y1-y0) andalso inFlatRange (x2-x0, y2-y0)
	  end

  (* bezier:
   * Recursively compute a Bezier cubic section. If the points
   * determine a polygon flat enough to be represented as a line
   * segment, the segment is added to the list. Otherwise, the
   * the curve is bisected and each part is recursively computed,
   * with the lists concatenated.
   *
   * From "The Beta2-split: A special case of the Beta-spline Curve and
   * Surface Representation." B. A. Barsky and A. D. DeRose. IEEE, 1985,
   * as adapted by Crispin Goswell for xps.
   *)
    fun bezier (arg as {x0,y0,x1,y1,x2,y2,x3,y3}, l) = if (isFlat arg)
	  then addSeg(l, x0, y0, x3, y3)
	  else let
	    val mid_x = (x0 + x3) / 8.0 + 3.0 * (x1 + x2) / 8.0
	    val mid_y = (y0 + y3) / 8.0 + 3.0 * (y1 + y2) / 8.0
	    val l' = bezier ({
		    x0 = mid_x,                     y0 = mid_y,
		    x1 = (x1+x3) / 4.0 + x2 / 2.0,  y1 = (y1+y3) / 4.0 + y2 / 2.0,
		    x2 = (x2+x3) / 2.0,             y2 = (y2+y3) / 2.0,
		    x3 = x3,                        y3 = y3
		  }, l)
	    in
	      bezier ({
		  x0 = x0,                        y0 = y0,
		  x1 = (x0+x1) / 2.0,             y1 = (y0+y1) / 2.0,
		  x2 = (x0+x2) / 4.0 + x1 / 2.0,  y2 = (y0+y2) / 4.0 + y1 / 2.0,
		  x3 = mid_x,                     y3 = mid_y
		}, l')
	    end

  (* curve:
   * Given four points [p0,p1,p2,p3], return a list of points corresponding to 
   * to a Bezier cubic section, starting at p0, ending at p3, with p1, p2 as
   * control points.
   *
   *)
    fun curve (PT{x=x0,y=y0}, PT{x=x1,y=y1}, PT{x=x2,y=y2}, PT{x=x3,y=y3}) =
	  bezier ({
	      x0 = real x0, y0 = real y0, 
	      x1 = real x1, y1 = real y1, 
	      x2 = real x2, y2 = real y2, 
	      x3 = real x3, y3 = real y3
	    }, [])

  (* doSpline:
   * Given four points (p0,p1,p2,p3), return a list of points corresponding to 
   * to the B-spline curve section, accumulating the results on the argument list.
   * We compute the curve by determining the corresponding Bezier control points,
   * and then use the Bezier routines above.
   *
   *)
    fun doSpline (PT{x=x0,y=y0}, PT{x=x1,y=y1}, PT{x=x2,y=y2}, PT{x=x3,y=y3}, l) = let
	  val x0 = real x0 and y0 = real y0 
	  val x1 = real x1 and y1 = real y1 
	  val x2 = real x2 and y2 = real y2 
	  val x3 = real x3 and y3 = real y3
	  in
	    bezier ({
		x0 = (x0 + 4.0*x1 + x2)/6.0, y0 = (y0+ 4.0*y1 + y2)/6.0, 
		x1 = (2.0*x1 + x2)/3.0,      y1 = (2.0*y1 + y2)/3.0, 
		x2 = (x1 + 2.0*x2)/3.0,      y2 = (y1 + 2.0*y2)/3.0, 
		x3 = (x1 + 4.0*x2 + x3)/6.0, y3 = (y1 + 4.0*y2 + y3)/6.0
	      }, l)
	  end

  (* loopSpline:
   * Given a list of spline control points, generate the corresponding
   * spline. Since we accumulate on the head of the list, we assume
   * the calling function has reversed the list of control points.
   * The loop continues as long as there are 4 control points left.
   *)
    fun loopSpline (p3, p2, p1, p0::tl, l) = 
          loopSpline (p2, p1, p0, tl, doSpline(p0, p1, p2, p3, l))
      | loopSpline (_, _, _, [], l) = l 

  (* simpleBSpline:
   * Compute a simple B-spline with the given control points.
   *)
    fun simpleBSpline arg = (case (rev arg)
	   of (p3::p2::p1::tl) => loopSpline (p3,p2,p1,tl,[])
	    | _ => arg
	  (* end case *))
          
  (* bSpline:
   * Compute a B-spline using the given control points.
   * In addition, we constrain the resultant spline to connect the
   * first and last points by adding copies of these points.
   *)
    fun bSpline (arg as (p0::_::_::_)) = let
	  val (pn::tl) = rev (p0::p0::arg)
	  in
	    loopSpline (pn, pn, pn, tl, [])
	  end
      | bSpline l = l
    
  (* closedBSpline:
   * Compute a closed B-spline. This is done by repeating the first
   * three points at the end of the list.
   * Note that the first and last points of the result are the same.
   *)
    fun closedBSpline (arg as (p0::p1::p2::_)) = loopSpline(p2,p1,p0,rev arg,[])
      | closedBSpline l = l

  end (* Spline *)

