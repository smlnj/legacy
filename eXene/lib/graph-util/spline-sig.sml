(* spline-sig.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *)

signature SPLINE = sig

    structure G : GEOMETRY

    (* curve:
     * Given four points (p0,p1,p2,p3), return a list of points corresponding to 
     * to a Bezier cubic section, starting at p0, ending at p3, with p1, p2 as
     * control points.
     *)
    val curve : (G.point * G.point * G.point * G.point) -> G.point list

    (* simpleBSpline:
     * Compute a simple B-spline with the given control points.
     *)
    val simpleBSpline : G.point list -> G.point list

    (* bSpline:
     *   bSpline ([p1]@l@[pn]) == simpleBSpline ([p1,p1,p1]@l@[pn,pn,pn])
     * The replication of p1 and pn constrains the resultant spline 
     * to connect p1 and pn.
     *)
    val bSpline : G.point list -> G.point list

    (* closedBSpline:
     * Compute a closed B-spline.
     *   closedBSpline (l as a::b::c::_) = simpleBSpline (l@[a,b,c])
     * Note that the first and last points of the result are the same.
     *)
    val closedBSpline : G.point list -> G.point list

  end (* SPLINE *)
