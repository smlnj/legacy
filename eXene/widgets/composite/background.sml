(* background.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Background widget.
 * Largely obsolete, as each widget supports its own background now.
 *)

signature BACKGROUND = 
  sig

    structure W : WIDGET

    type background

    val background : (W.root * W.view * W.arg list) -> W.widget -> background

    val mkBackground : {
          color : W.EXB.color option,
          widget : W.widget
        } -> background

    val widgetOf : background -> W.widget

  end (* BACKGROUND *)

structure Background : BACKGROUND = 
  struct

    structure W = Widget

    type background = W.widget

    val attrs = [
        (Attrs.attr_background,     Attrs.AT_Color,    Attrs.AV_Str "white")
      ]

    fun mkBack (root, color, widget) =
            W.mkWidget{
                root=root,
                args= fn () => {background = SOME color}, 
                boundsOf = W.boundsFn widget,
                realize= W.realizeFn widget
              }

    fun background (root,view,args) widget = let
          val attrs = W.findAttr (W.attrs (view,attrs,args))
          val color = Attrs.getColor (attrs Attrs.attr_background)
          in mkBack (root,color,widget) end

    fun mkBackground {color, widget} = let
          val root = W.rootOf widget
          val color = (case color
		 of NONE => W.EXB.whiteOfScr (W.screenOf root)
		  | SOME color => color
		(* end case *))
          in mkBack (root, color,widget) end

    fun widgetOf w = w

  end (* Background *)

