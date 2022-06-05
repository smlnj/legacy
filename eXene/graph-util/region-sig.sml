(* region-sig.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories
 *
 * Signature for regions.
 *
 *)

signature REGION =
  sig
    structure G : GEOMETRY

    datatype fill_rule = EvenOdd | Winding
    datatype rect_overlap = RectangleOut | RectangleIn | RectanglePart

    type region

      (* Empty region *)
    val empty : region
 
      (* Returns list of rectangles composing the region.
       * The rectangles are YX banded. Specifically, the rectangles
       * are listed in non-decreasing y coordinates. Two rectangles
       * with the same y coordinate are listed in increasing x coordinate.
       * Additionally, if the y projections of any two rectangles overlap,
       * then the projections are equal. (The rectangles lie in non-overlapping
       * bands.) Within a band, the rectangles are non-contiguous.
       *)
    val rectsOf : region -> G.rect list

      (* Construct a region corresponding to the given rectangle.
       *)
    val rectangle : G.rect -> region

      (* Construct a region corresponding to the polygon described by
       * the list of points and the fill rule. 
       *)
    val polygon : G.point list * fill_rule -> region

      (* Translate a region by the given vector. *)
    val offset : region * G.point -> region

      (* shrink (r,PT{x,y}) strips a band x pixels horizontally
       * and y pixels vertically from the boundary of r. If x or y are 
       * negative, pixels are added rather than stripped in that dimension.
       *)
    val shrink : region * G.point -> region

      (* Return the smallest rectangle containing the region. *)
    val clipBox : region -> G.rect

      (* Return the region corresponding to the given set operation
       * applied to two argument regions.
       *)
    val intersect : region * region -> region
    val union : region * region -> region
    val subtract : region * region -> region
    val xor : region * region -> region

      (* True if the region is empty. *)
    val isEmpty : region -> bool

      (* True if the two regions are equal. *)
    val equal : region * region -> bool

      (* True if the two regions have non-empty intersection. *)
    val overlap : region * region -> bool

      (* True if the point lies within the region. *)
    val pointIn : region * G.point -> bool

      (* Returns RectangleIn if the rectangle is entirely contained
       *                     in the region.
       * returns RectangleOut if the rectangle is entirely outside
       *                      the region.
       * returns RectanglePart if the rectangle is partly in and partly
       *                       out of the region.
       *)
    val rectIn : region * G.rect -> rect_overlap
  end

