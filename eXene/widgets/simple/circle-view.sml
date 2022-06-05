(* circle-view.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * View for circular button.
 *)

structure CircleShape : SHAPE_VIEW = 
  struct

    structure W = Widget

    open Geometry

    val attrs = []

    fun drawfn (d,SIZE{wid,ht},bwid) = let
          val wid2 = wid div 2
          val ht2 = ht div 2
          val pt = PT{x=wid2,y=ht2} (* Center point *)
          val radius = Int.min(wid2,ht2)-1
          val diam = 2*radius
          val circle = {center=pt,rad=radius-bwid}
          val angle1 = 45 * 64   (* 45 degrees *)
          val angle2 = 180 * 64   (* 180 degrees *)
          val upper = 
                ARC{x=1,y=1,wid=diam,ht=diam,angle1=angle1,angle2=angle2}
          val lower =
                ARC{x=1,y=1,wid=diam,ht=diam,angle1=angle1,angle2= ~angle2}
          fun draw(base,top,bottom) = (
                Drawing.fillArc d top upper;
                Drawing.fillArc d bottom lower;
                Drawing.fillCircle d base circle
              )
          in draw end

    fun sizefn (wid,_) = W.fixBounds (wid,wid)

    fun config _ = (sizefn,drawfn)
  
  end (* CircleShape *)

structure CircleView = ShapeView (CircleShape)
