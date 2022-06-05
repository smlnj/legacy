(* switch-view.sml
 *
 * COPYRIGHT (c) 1991,1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * View for rocker switch.
 *)

structure SwitchView : BUTTON_VIEW = 
  struct

    structure W = Widget

    structure F = Format

    open Geometry

    val on_switch_data = W.EXB.imageFromAscii (32, [[
      "0x0C000000", "0x1B000000", "0x28C00000", "0x5A300000",
      "0xA88C0000", "0xDA230000", "0xA888FFFE", "0xDA228002",
      "0xA888AAAA", "0xDA228002", "0xA888AAAA", "0xDA228002",
      "0xAC88AAAA", "0xDF228002", "0xBFC8AAAA", "0xFFF28002",
      "0xFFFCAAAA", "0xFFFF8002", "0xFFFFFFFE", "0xFFFFFFFE",
      "0x7FFFFFFC"
    ]])

    val on_switch_mask = W.EXB.imageFromAscii (32, [[
      "0x0c000000", "0x1f000000", "0x3fc00000", "0x7ff00000",
      "0xfffc0000", "0xffff0000", "0xfffffffe", "0xfffffffe",
      "0xfffffffe", "0xfffffffe", "0xfffffffe", "0xfffffffe",
      "0xfffffffe", "0xfffffffe", "0xfffffffe", "0xfffffffe",
      "0xfffffffe", "0xfffffffe", "0xfffffffe", "0xfffffffe",
      "0x7ffffffc"
    ]])

    val off_switch_data = W.EXB.imageFromAscii (32, [[
      "0x00000060", "0x000001B0", "0x00000628", "0x000018B4",
      "0x0000622A", "0x000188B6", "0xFFFE222A", "0x800288B6",
      "0xAAAA222A", "0x800288B6", "0xAAAA222A", "0x800288B6",
      "0xAAAA226A", "0x800289F6", "0xAAAA27FA", "0x80029FFE",
      "0xAAAA7FFE", "0x8003FFFE", "0xFFFFFFFE", "0xFFFFFFFE",
      "0x7FFFFFFC"
    ]])

    val off_switch_mask = W.EXB.imageFromAscii (32, [[
      "0x00000060", "0x000001f0", "0x000007f8", "0x00001ffc",
      "0x00007ffe", "0x0001fffe", "0xfffffffe", "0xfffffffe",
      "0xfffffffe", "0xfffffffe", "0xfffffffe", "0xfffffffe",
      "0xfffffffe", "0xfffffffe", "0xfffffffe", "0xfffffffe",
      "0xfffffffe", "0xfffffffe", "0xfffffffe", "0xfffffffe",
      "0x7ffffffc"
    ]])

    val attrs = [
        (Attrs.attr_color,          Attrs.AT_Color,  Attrs.AV_NoValue),
        (Attrs.attr_readyColor,     Attrs.AT_Color,  Attrs.AV_NoValue),
        (Attrs.attr_background,     Attrs.AT_Color,  Attrs.AV_Str "white"),
        (Attrs.attr_foreground,     Attrs.AT_Color,  Attrs.AV_Str "black")
      ]

    datatype button_view = SW of {
        fg : W.EXB.color,
        bg : W.EXB.color,
        color : W.EXB.color,
        readyc : W.EXB.color,
        on_src : W.EXB.tile,
        off_src : W.EXB.tile,
        on_mask : W.EXB.tile,
        off_mask : W.EXB.tile,
        inactive_on_mask : W.EXB.tile,
        inactive_off_mask : W.EXB.tile,
        iwid : int,
        iht : int
      }

    fun buttonView (root,view,args) = let
          open Attrs Drawing
          val attrs = W.findAttr (W.attrs (view,attrs,args))
          val fg = getColor(attrs attr_foreground) 
          val bg = getColor(attrs attr_background) 
          val color = case getColorOpt(attrs attr_color) of
                        NONE => bg
                      | SOME c => c
          val readyc = case getColorOpt(attrs attr_readyColor) of
                        NONE => color
                      | SOME c => c
          val scr = W.screenOf root
          val stipple = W.tile root "gray"

          val on_src = W.EXB.createTileFromImage scr on_switch_data
          val off_src = W.EXB.createTileFromImage scr off_switch_data
          val on_pattern = W.EXB.createPixmapFromImage scr on_switch_mask
          val off_pattern = W.EXB.createPixmapFromImage scr off_switch_mask
          val on_mask = W.EXB.createTileFromPixmap on_pattern
          val off_mask = W.EXB.createTileFromPixmap off_pattern

            (* create stippled masks *)
          val sz = W.EXB.sizeOfPixmap on_pattern
          val spen = newPen [
                PV_Stipple stipple, PV_FillStyle_OpaqueStippled,
                PV_Foreground W.EXB.color1, PV_Background W.EXB.color0, 
                PV_Function OP_And] 
          val _ = fillRect (drawableOfPM off_pattern) spen (mkRect(originPt,sz))
          val _ = fillRect (drawableOfPM on_pattern) spen (mkRect(originPt,sz))
          val inactive_on_mask = W.EXB.createTileFromPixmap on_pattern
          val inactive_off_mask = W.EXB.createTileFromPixmap off_pattern

          val {sz=SIZE{wid=twid,ht=tht},...} = W.EXB.geomOfTile on_src
          in
(*
            destroyPixmap on_pattern
            destroyPixmap off_pattern
*)
            SW {
             fg = fg,
             bg = bg,
             color = color,
             readyc = readyc,
             on_src = on_src,
             off_src = off_src,
             on_mask = on_mask,
             off_mask = off_mask,
             inactive_on_mask = inactive_on_mask,
             inactive_off_mask = inactive_off_mask,
             iwid = twid,
             iht = tht
            }
          end

    fun bounds (SW{iwid,iht,...}) = W.fixBounds(iwid,iht)
    fun win_args (SW{bg,...}) = {background = SOME bg}
    fun config (SW(v as {iwid,iht,color,readyc,...}), win, SIZE{wid,ht}) = let
          open Drawing
          val drawable = drawableOfWin win
          val on_src = #on_src v
          val off_src = #off_src v
          val on_mask = #on_mask v
          val off_mask = #off_mask v
          val inactive_on_mask = #inactive_on_mask v
          val inactive_off_mask = #inactive_off_mask v

          (* Compute point at which to blt centered icon *)
          val pt = PT{x=(wid-iwid) div 2,y=(ht-iht) div 2}

          val onPen = newPen [PV_Foreground (#fg v), PV_Background color,
                PV_ClipMask on_mask, PV_ClipOrigin pt]
          val offPen = updatePen(onPen, [PV_ClipMask off_mask])
          val (readyOnPen,readyOffPen) =
                 if W.EXB.sameColor(color,readyc) then (onPen,offPen)
                 else (updatePen(onPen,[PV_Background readyc]),
                       updatePen(offPen,[PV_Background readyc]))
          val inactiveOnPen = updatePen(onPen, [PV_ClipMask inactive_on_mask])
          val inactiveOffPen = updatePen(offPen,[PV_ClipMask inactive_off_mask])

          fun draw (src, pen) = (
                clearDrawable drawable;
                textureBlt drawable pen {src=src,dst_pos=pt}
              )
       
          fun setf (W.Inactive true,_,_) = draw(on_src, inactiveOnPen)
            | setf (W.Inactive false,_,_)= draw(off_src, inactiveOffPen)
            | setf (W.Active false,ready,false) = 
                if ready then draw (off_src,readyOffPen)
                else draw(off_src, offPen)
            | setf (W.Active false,ready,true) =
                if ready then draw (on_src,readyOnPen)
                else draw(on_src, onPen)
            | setf (W.Active true,ready,false) =
                if ready then draw (on_src,readyOnPen)
                else draw(on_src, onPen)
            | setf (W.Active true,ready,true) =
                if ready then draw (off_src,readyOffPen)
                else draw(off_src, offPen)
(*
          val setf = fn (a as (W.Inactive s,r,d)) =>
                  (TextIO.print(F.format "Inactive %b %b %b\n"
                      [F.BOOL s, F.BOOL r, F.BOOL d]); setf a)
                      | (a as (W.Active s,r,d)) =>
                  (TextIO.print(F.format "Active %b %b %b\n"
                      [F.BOOL s, F.BOOL r, F.BOOL d]); setf a)
*)
          in setf end

  end (* SwitchView *)
