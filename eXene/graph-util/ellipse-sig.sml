(* ellipse-sig.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 *
 * Code for producing rotated ellipses.
 *
 * Based on an ellipse generator, written by James Tough, 7th May 92
 *
 *)

signature ELLIPSE =
  sig
    structure G : GEOMETRY

    exception BadAxis

    val ellipse : (G.point * int * int * real) -> G.point list
      (* ellipse (pt, a, b, phi) produces a list of points
       * describing the ellipse x^2 / a^2 + y^2 / b^2 = 1
       * translated to point pt and rotated phi radians 
       * counterclockwise.  If a = 0 or b = 0, it returns [].
       * Raises BadAxis if a < 0 or b < 0.
       *)

  end; (* ELLIPSE *)
