(* arrow-view.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * View for arrow buttons.
 *)

structure ArrowShape : SHAPE_VIEW = 
  struct

  structure W = Widget

  val attrs = [
      (Attrs.attr_arrowDir,    Attrs.AT_ArrowDir,    Attrs.AV_ArrowDir W.AD_Up)
    ]

  open Geometry

  val offset = 1

  fun getVerts(wid,ht,W.AD_Up) = 
        [PT{x=wid div 2,y=offset-1},
         PT{x=offset-1,y=ht-offset},
         PT{x=wid-offset,y=ht-offset}]
    | getVerts(wid,ht,W.AD_Down) = 
        [PT{x=wid div 2,y=ht-offset},
         PT{x=wid-offset,y=offset},
         PT{x=offset,y=offset}]
    | getVerts(wid,ht,W.AD_Left) = 
        [PT{x=offset,y=ht div 2},
         PT{x=wid-offset,y=ht-offset},
         PT{x=wid-offset,y=offset-1}]
    | getVerts(wid,ht,W.AD_Right) = 
        [PT{x=wid-offset,y=ht div 2},
         PT{x=offset,y=offset-1},
         PT{x=offset,y=ht-offset}]

  fun size dir (wid,ht) = let
        val length = ((((wid - 2*offset)*173) + 100) div 200) + 2*offset
        val (wid,ht) = case ht of
                         SOME h => (wid,h)
                       | _ => case dir of
                                (W.AD_Down | W.AD_Up) => (wid,length)
                              | _ => (length,wid)
        in W.fixBounds (wid,ht) end

  fun drawfn dir (d,sz as SIZE{wid,ht},bwid) = let
        open Drawing
        val verts = getVerts (wid,ht,dir)
        in
         fn (base,top,bottom) => (
              fillPolygon d base {verts=verts,shape=ConvexShape};
              ThreeD.draw3DPoly d (verts,bwid) {top=top,bottom=bottom}
            )
        end

  fun config attrs = let
        val dir = Attrs.getArrowDir(attrs Attrs.attr_arrowDir)
        in
          (size dir, drawfn dir)
        end

  end (* ArrowShape *)

structure ArrowView = ShapeView (ArrowShape)
