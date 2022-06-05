(* diamond-view.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * View for diamond-shaped button.
 *)

structure DiamondShape : SHAPE_VIEW = 
  struct

    structure W = Widget

    open Geometry

    val attrs = []

    fun drawfn (d,SIZE{wid,ht},bwid) = let
          open Drawing
          val offset = 1
          val midx = wid div 2
          val midy = ht div 2
          val verts = [PT{x=midx,y=offset},
                       PT{x=offset,y=midy},
                       PT{x=midx,y=ht-1},
                       PT{x=wid-1,y=midy}]
          fun draw (base,top,bottom) = (
                fillPolygon d base {verts=verts,shape=ConvexShape};
                ThreeD.draw3DPoly d (verts,bwid) {top=top,bottom=bottom} 
              )
          in draw end

    fun sizefn (wid,ht) =
          W.fixBounds (wid,case ht of NONE => wid | SOME h => h)

    fun config _ = (sizefn, drawfn)

  end (* DiamondShape *)

structure DiamondView = ShapeView (DiamondShape)
