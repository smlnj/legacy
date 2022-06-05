(* spot.sml
 *
 * COPYRIGHT (c) 1991,1995 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

signature SPOT = 
  sig

    structure W : WIDGET

    type spot

    val spot : (W.root * W.view) -> 
                 {color : W.EXB.rgb,
                  wid : int,
                  ht : int} -> spot
    val widgetOf : spot -> W.widget
    val setSpot : spot -> W.EXB.rgb -> unit

  end; (* SPOT *)

structure Spot : SPOT = 
  struct

    structure W = Widget
    structure A = Attrs

    open Geometry

    datatype spot = Spot of (W.widget * (W.EXB.rgb -> unit))

    fun spot (root,view) {color = W.EXB.RGB color,wid,ht} = let
          val colorOf = Root.colorOf root
          val color = W.EXB.CMS_RGB color
          val args = [(A.attr_background, A.AV_Color (colorOf color))]
          val label = Label.label (root, view, args)
          val widget = Shape.fixSize (Label.widgetOf label, SIZE{wid=wid,ht=ht})
          fun setc (W.EXB.RGB rgb) = 
                Label.setBackground label (colorOf (W.EXB.CMS_RGB rgb))
          in
            Spot(widget, setc)
          end

    fun widgetOf (Spot(w,_)) = w
    fun setSpot (Spot(_,f)) = f

  end; (* Spot *)
