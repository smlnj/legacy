(* rounded-rect.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 *
 * Copyright 1988 by the Massachusetts Institute of Technology
 *
 * Routines to draw/fill rectangles with rounded corners.  The implementation
 * is lifted from the MIT X11 distribution.
 *)

structure RoundedRect : ROUNDED_RECT =
  struct
    structure G = Geometry

    open Geometry

    fun drawRoundedRect drawable pen {rect, c_wid, c_ht} = let
	  val RECT{x, y, wid=w, ht=h} = rect
	  val w2 = c_wid+c_wid
	  val h2 = c_ht+c_ht
	  val (ew, ew2) = if (w2 > w) then (0, 0) else (c_wid, w2)
	  val (eh, eh2) = if (h2 > h) then (0, 0) else (c_ht, h2)
	  in
	    Drawing.drawArcs drawable pen [
		ARC{x= x, y= y, wid= ew2, ht= eh2, angle1= 180*64, angle2= ~90*64},
		ARC{x= x+ew, y= y, wid= w - ew2, ht= 0, angle1= 180*64, angle2= ~180*64},
		ARC{x= x+w - ew2, y= y, wid= ew2, ht= eh2, angle1= 90*64, angle2= ~90*64},
		ARC{x= x+w, y= y+eh, wid= 0, ht= h - eh2, angle1= 90*64, angle2= ~180*64},
		ARC{x= x+w - ew2, y= y+h - eh2, wid= ew2, ht= eh2, angle1= 0, angle2= ~90*64},
		ARC{x= x+ew, y= y+h, wid= w - ew2, ht= 0, angle1= 0, angle2= ~180*64},
		ARC{x= x, y= y+h - eh2, wid= ew2, ht= eh2, angle1= 270*64, angle2= ~90*64},
		ARC{x= x, y= y+eh, wid= 0, ht= h - eh2, angle1= 270*64, angle2= ~180*64}
	      ]
	  end

    fun fillRoundedRect drawable pen {rect, c_wid, c_ht} = let
	  val pen = Drawing.updatePen (pen, [Drawing.PV_ArcMode_PieSlice])
	  val RECT{x, y, wid=w, ht=h} = rect
	  val w2 = c_wid+c_wid
	  val h2 = c_ht+c_ht
	  val (ew, ew2) = if (w2 > w) then (0, 0) else (c_wid, w2)
	  val (eh, eh2) = if (h2 > h) then (0, 0) else (c_ht, h2)
	  in
	    Drawing.fillArcs drawable pen [
		ARC{x= x, y= y, wid= ew2, ht= eh2, angle1= 180*64, angle2= ~90*64},
		ARC{x= x+w - ew2, y= y, wid= ew2, ht= eh2, angle1= 90*64, angle2= ~90*64},
		ARC{x= x+w - ew2, y= y+h - eh2, wid= ew2, ht= eh2, angle1= 0, angle2= ~90*64},
		ARC{x= x, y= y+h - eh2, wid= ew2, ht= eh2, angle1= 270*64, angle2= ~90*64}
	      ];
	    Drawing.fillRects drawable pen [
		RECT{x= x + ew, y= y, wid= w - ew*2, ht= h},
		RECT{x= x, y= y + eh, wid= ew, ht= h - eh2},
		RECT{x= x + w - ew, y= y + eh, wid= ew, ht= h - eh2}
	      ]
	  end

  end (* RoundedRect *)

