(* divider.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Divider widget, for drawing horizontal or vertical line.
 *)

signature DIVIDER = 
  sig

    structure W : WIDGET

    val horzDivider : (W.root * W.view * W.arg list) -> W.widget

    val mkHorzDivider : W.root -> {
      color : W.EXB.color option,
      width : int 
    } -> W.widget

    val vertDivider : (W.root * W.view * W.arg list) -> W.widget

    val mkVertDivider : W.root -> {
      color : W.EXB.color option,
      width : int 
    } -> W.widget

  end (* DIVIDER *)

structure Divider : DIVIDER = 
  struct

    structure W = Widget
  
    fun mkSizeFn (isHorz, width) = let
          val _ = if width < 0 
                     then LibBase.failure{module="Divider",func="mkSize",msg="width < 0"}
                     else ()
          val fixD = W.fixDim width
          val stretchD = W.DIM{base=1,incr=1,min=0,nat=0,max=NONE}
          val size = if isHorz then {x_dim=stretchD, y_dim=fixD}
                     else {y_dim=stretchD, x_dim=fixD}
          in
            fn () => size
          end

    fun mkDivider isHorz root {color, width} =
            ColorRect.mkColorRect root (color, mkSizeFn (isHorz, width))

    val mkHorzDivider = mkDivider true
    val mkVertDivider = mkDivider false

    val attrs = [
        (Attrs.attr_width,          Attrs.AT_Int,      Attrs.AV_Int 1)
      ]

    fun divider isHorz (root,view,args) = let
          val attrDef = W.findAttr (W.attrs(view,attrs,args))
          val width = Attrs.getInt (attrDef Attrs.attr_width)
          val sizefn = mkSizeFn (isHorz, width)
          in ColorRect.colorRect (root,view,args) sizefn end

    val horzDivider = divider true
    val vertDivider = divider false

  end (* Divider *)

