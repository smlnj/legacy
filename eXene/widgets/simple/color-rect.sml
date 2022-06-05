(* color-rect.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Widget that fills rectangle with a color.
 *)

signature COLOR_RECT = 
  sig

    structure W : WIDGET

    val colorRect : (W.root * W.view * W.arg list) -> 
          (unit -> W.bounds) -> W.widget

    val mkColorRect : W.root -> 
          (W.EXB.color option * (unit -> W.bounds)) -> W.widget

  end (* COLOR_RECT *)

structure ColorRect : COLOR_RECT = 
  struct

    structure W = Widget

    open Geometry EXeneBase Interact Drawing

    val attrs = [
        (Attrs.attr_foreground,     Attrs.AT_Color,    Attrs.AV_Str "black")
      ]

    fun crect (root, color, boundsOf) = let
          fun realize {env, win, sz} = (ignoreAll env;())
          in
            W.mkWidget{
              root = root, 
              args = fn () => {background = SOME color},
              boundsOf = boundsOf,
              realize = realize
            }
          end

    fun colorRect (root,view,args) boundsOf = let
          val attrs = W.findAttr (W.attrs(view,attrs,args))
          val color = Attrs.getColor(attrs Attrs.attr_foreground)
          in crect (root, color, boundsOf) end

    fun mkColorRect root (colorOpt,boundsOf) = let
          val scr = W.screenOf root
          val color = case colorOpt of
                            SOME color => color
                          | NONE => blackOfScr scr
          in crect (root, color, boundsOf) end

  end (* ColorRect *)

