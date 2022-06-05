(* ellipse.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 *
 * Code for producing rotated ellipses.
 *
 * Based on an ellipse generator, written by James Tough, 7th May 92
 *
 *)

structure Ellipse : ELLIPSE =
  struct
    structure G = Geometry

    exception BadAxis

    fun round x = if (x > 0.0) then floor (x+0.5) else ~1*floor(~x+0.5)

    fun doEllipse (G.PT{x=cx,y=cy}, radius_x, radius_y, angle) = let
	  val a = real radius_x
	  val b = real radius_y
	  val cphi = Math.cos angle
	  val sphi = Math.sin angle
	  val cphisqr = cphi*cphi
	  val sphisqr = sphi*sphi
	  val asqr = a*a
	  val bsqr = b*b
	  val cphisphi = cphi*sphi
	  val c1 = (cphisqr/asqr)+(sphisqr/bsqr)
	  val c2 = ((cphi*sphi/asqr)-(cphi*sphi/bsqr))/c1
	  val c3 = (bsqr*cphisqr) + (asqr*sphisqr)
	  val c4 = a * b / c3
	
          fun mkp (x,y) = G.PT{x = cx + round x, y = cy + y}

	  (* val ymax = truncate(sqrt c3) *)
	  val v1 = c4 * c4
	  val c6 = v1 + v1
	  val c3' = c3 * v1 - v1
          val d = c4 * (Math.sqrt c3)
          val firstp = mkp(~d,0)

          fun flipOn ([],l) = l
            | flipOn (i::rest,l) = flipOn(rest,i::l)

          fun merge(l1, l2, l3, l4) = flipOn(l1,l2 @ (flipOn(l3,l4)))

          fun loop (l1, l2, l3, l4, y, c3, c5, v1) =
                if c3 < 0.0 then merge(l1,l2,l3,l4)
                else let
                  val d = Math.sqrt c3
                  val xleft = c5 - d
                  val xright = c5 + d
                  in
                    loop(
                      mkp(xleft,y)::l1,
                      mkp(xright,y)::l2,
                      mkp(~xleft,~y)::l3,
                      mkp(~xright,~y)::l4,
                      y + 1,
                      c3 - v1,
                      c5 + c2,
                      v1 + c6
                    )
                  end

          in
            loop([firstp], [mkp(d,0)], [], [firstp], 1, c3', c2, v1 + c6)
          end

  (* ellipse (pt, a, b, phi) produces a list of points
   * describing the ellipse x^2 / a^2 + y^2 / b^2 = 1
   * translated to point pt and rotated phi radians 
   * counterclockwise.  If a = 0 or b = 0, it returns [].
   * Raises BadAxis if a < 0 or b < 0.
   *)
    fun ellipse (arg as (_, radius_x, radius_y, _)) =
          if ((radius_x < 0) orelse (radius_y < 0))
	    then raise BadAxis
          else if ((radius_x = 0) orelse (radius_y = 0))
	    then []
          else doEllipse arg

  end (* Ellipse *)

