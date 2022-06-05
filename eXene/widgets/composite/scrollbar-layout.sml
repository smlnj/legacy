(* scroll-layout.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Wrapper for putting scrollbars around a widget.
 *)

signature SCROLL_LAYOUT =
  sig

    structure Box : BOX

    val mkSBLayout : Box.W.root -> {
          widget : Box.W.widget,
          hsb : {sb : Box.W.widget, pad : int, top : bool} option,
          vsb : {sb : Box.W.widget, pad : int, left : bool} option
        } -> Box.box_layout

  end (* SCROLL_LAYOUT *)

structure ScrollLayout : SCROLL_LAYOUT = struct

  structure Box = Box

  open CML Geometry EXeneBase Widget Box

  type hsb_desc = { sb : W.widget, pad : int, top : bool}
  type vsb_desc = { sb : W.widget, pad : int, left : bool}

  fun fixGlue pad = Box.Glue {nat=pad,min=pad,max=SOME pad}

  fun mkSBLayout root {widget, hsb=NONE, vsb=NONE} = Box.mkLayout root (Box.WBox widget)
    | mkSBLayout root {widget, hsb = SOME (hdesc : hsb_desc), vsb=NONE} = let
        val scr = screenOf root
        val {sb, pad, top} = hdesc
      in
          case (top, pad) of
            (true, 0) => Box.mkLayout root 
              (VtCenter [Box.WBox sb, Box.WBox widget])
          | (false, 0) => Box.mkLayout root
              (VtCenter [Box.WBox widget, Box.WBox sb])
          | (true, pad) => Box.mkLayout root
              (VtCenter [
                 Box.WBox sb,
                 fixGlue pad,
                 Box.WBox widget
              ])
          | (false, pad) => Box.mkLayout root
              (VtCenter [
                 Box.WBox widget,
                 fixGlue pad,
                 Box.WBox sb
              ])
      end
    | mkSBLayout root {widget, vsb = SOME (vdesc : vsb_desc), hsb=NONE} = let
        val scr = screenOf root
        val {sb, left, pad} = vdesc
      in
          case (left, pad) of
            (true, 0) => Box.mkLayout root 
              (HzCenter [Box.WBox sb, Box.WBox widget])
          | (false, 0) => Box.mkLayout root
              (HzCenter [Box.WBox widget, Box.WBox sb])
          | (true, pad) => Box.mkLayout root
              (HzCenter [
                 Box.WBox sb,
                 fixGlue pad,
                 Box.WBox widget
              ])
          | (false, pad) => Box.mkLayout root
              (HzCenter [
                 Box.WBox widget,
                 fixGlue pad,
                 Box.WBox sb
              ])
      end
    | mkSBLayout root {widget, hsb = SOME hdesc, vsb = SOME vdesc} = let
        val scr = screenOf root

        val hpad = #pad hdesc
        val hsb = #sb hdesc
        val vpad = #pad vdesc
        val vsb = #sb vdesc
        val {x_dim,...} = boundsOf vsb
        val vsz = natDim x_dim

      in
        case (#top hdesc, #left vdesc) of
          (true, true) => Box.mkLayout root 
            (VtCenter [
               HzCenter [
                 fixGlue (vsz+vpad),
                 Box.WBox hsb 
               ],
               fixGlue hpad,
               HzCenter [
                 Box.WBox vsb, 
                 fixGlue vpad,
                 Box.WBox widget
               ]
            ])
        | (false, true) => Box.mkLayout root 
            (VtCenter [
               HzCenter [
                 Box.WBox vsb, 
                 fixGlue vpad,
                 Box.WBox widget
               ],
               fixGlue hpad,
               HzCenter [
                 fixGlue (vsz+vpad),
                 Box.WBox hsb 
               ]
            ])
        | (true, false) => Box.mkLayout root 
            (VtCenter [
               HzCenter [
                 Box.WBox hsb,
                 fixGlue (vsz+vpad)
               ],
               fixGlue hpad,
               HzCenter [
                 Box.WBox widget,
                 fixGlue vpad,
                 Box.WBox vsb
               ]
            ])
        | (false, false) => Box.mkLayout root 
            (VtCenter [
               HzCenter [
                 Box.WBox widget,
                 fixGlue vpad,
                 Box.WBox vsb
               ],
               fixGlue hpad,
               HzCenter [
                 Box.WBox hsb,
                 fixGlue (vsz+vpad)
               ]
            ])
      end

end (* ScrollLayout *)

