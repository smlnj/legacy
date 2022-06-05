(* box.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories
 *
 * Code for simple box data structure.
 *
 * A box is essentially a rectangle stored as the upper left point
 * and lower right bounding point. Note that the lower right point
 * is not actually part of the rectangle. Explicitly,
 * RECT{x,y,wid,ht} corresponds to the box 
 * BOX{x1=x,y1=y,x2=x+wid,y2=y+ht}. For certain computations (e.g.,
 * constructing regions), this representation is more useful.
 *
 *)

structure Box =
  struct

    datatype box = BOX of {x1 : int, y1 : int, x2 : int, y2 : int}

    val zeroBox = BOX{x1 = 0, y1 = 0, x2 = 0, y2 = 0}

    fun miny(BOX{y1,...}) = y1

      (* inBox:
       * returns true if point is in box
       *)
    fun inBox(BOX{x1,y1,x2,y2},Geometry.PT{x,y}) =
          x2 > x andalso x >= x1 andalso y2 > y andalso y >= y1

      (* inside:
       * returns true if first box is contained in second
       *)
    fun inside(BOX{x1,y1,x2,y2},BOX{x1=x1',y1=y1',x2=x2',y2=y2'}) =
	      x1' <= x1 andalso y1' <= y1 andalso x2' >= x2 andalso y2' >= y2

      (* overlap:
       * returns true if boxes overlap
       *)
    fun overlap(BOX{x1,y1,x2,y2},BOX{x1=x1',y1=y1',x2=x2',y2=y2'}) =
          x2 > x1' andalso x1 < x2' andalso y2 > y1' andalso y1 < y2'

      (* boundBox:
       * returns bounding box of two boxes
       *)
    fun boundBox(BOX{x1,y1,x2,y2},BOX{x1=x1',y1=y1',x2=x2',y2=y2'}) = BOX{
	    x1 = Int.min(x1,x1'),
	    x2 = Int.max(x2,x2'),
	    y1 = Int.min(y1,y1'),
	    y2 = Int.max(y2,y2')
	  }

      (* offsetBox:
       * translate box by given vector
       *)
    fun offsetBox (Geometry.PT{x,y}) (BOX{x1,y1,x2,y2}) =
          BOX{x1 = x1+x, x2 = x2+x, y1 = y1+y, y2 = y2+y}

      (* xOffsetBox:
       * horizontally translate box
       *)
    fun xOffsetBox x (BOX{x1,y1,x2,y2}) = BOX{x1 = x1+x, x2 = x2+x, y1 = y1, y2 = y2}

      (* yOffsetBox:
       * vertically translate box
       *)
    fun yOffsetBox y (BOX{x1,y1,x2,y2}) = BOX{x1 = x1, x2 = x2, y1 = y1+y, y2 = y2+y}

  end
