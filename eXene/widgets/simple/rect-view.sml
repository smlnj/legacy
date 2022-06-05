(* rect-view.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * View for rectangular button.
 *)

structure RectShape : SHAPE_VIEW = 
  struct

    structure W = Widget

    val attrs = []

    fun drawfn (d,sz,bwid) = let
          val r = Geometry.mkRect(Geometry.originPt,sz)
          val drawRect = ThreeD.draw3DRect d (r,bwid)
          fun draw (base,top,bottom) = (
                Drawing.fillRect d base r;
                drawRect {top=top,bottom=bottom}
              )
          in draw end

    fun sizefn (wid,ht) =
          W.fixBounds (wid,case ht of NONE => wid | SOME h => h)

    fun config _ = (sizefn,drawfn)

end (* RectView *)

structure RectView = ShapeView (RectShape)
