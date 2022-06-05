(* hide-view.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * View for hideable button.
 *)

structure HideView : BUTTON_VIEW = 
  struct

    structure W = Widget

    open Geometry

    val dfltFont = "-Adobe-Helvetica-Bold-R-Normal--*-120-*"
    val dfltBorderWidth = 2

    val attrs = [
        (Attrs.attr_label,          Attrs.AT_Str,    Attrs.AV_Str ""),
        (Attrs.attr_font,           Attrs.AT_Font,   Attrs.AV_Str dfltFont),
        (Attrs.attr_color,          Attrs.AT_Color,  Attrs.AV_NoValue),
        (Attrs.attr_background,     Attrs.AT_Color,  Attrs.AV_Str "white"),
        (Attrs.attr_foreground,     Attrs.AT_Color,  Attrs.AV_Str "black")
      ]

    fun mkFontInfo font = let
          val {ascent=fonta,descent=fontd} = Font.fontHt font
          in (font, fonta, fontd) end

    fun mkTextLabel (s,font) = let
          val Font.CharInfo {left_bearing=lb,right_bearing=rb,...}
                = #overall_info (Font.textExtents font s)
          in (s,lb,rb) end

    fun sizeOfLabel ((s,lb,rb),(_,fa,fd)) =
          SIZE{wid = rb - lb + 2, ht = fa + fd + 2}

    fun setLightWidth (_,fonta,fontd) = (80 * (fonta+fontd)) div 100

    datatype button_view = BV of {
        shades : W.shades,
        fontinfo : (W.EXB.font * int * int),
        label : (string * int * int),
        fg : W.EXB.color,
        bg : W.EXB.color,
        lightSz : int,
        padx : int,
        pady : int,
        borderWidth : int,
        onColor : W.EXB.color
      }

    fun buttonView (root,view,args) = let
          open Attrs
          val attrs = W.findAttr (W.attrs(view, attrs, args))
          val fontinfo as (f,_,_) = mkFontInfo(getFont(attrs attr_font))
          val label = mkTextLabel(getString(attrs attr_label),f)
          val forec = Attrs.getColor (attrs Attrs.attr_foreground)
          val backc = Attrs.getColor (attrs Attrs.attr_background)
          val color = case Attrs.getColorOpt (attrs Attrs.attr_color) of
                        SOME c => c 
                      | _ => forec
          val lightSz = setLightWidth fontinfo
          in
            BV {
              fontinfo = fontinfo,
              label = label,
              fg = forec,
              bg = backc,
              shades = W.shades root color,
              lightSz = lightSz,
              borderWidth = dfltBorderWidth,
              padx = lightSz div 2,
              pady = lightSz div 4,
              onColor = forec
            }
          end

    fun config (BV v, win, SIZE{wid,ht}) = let
          open Drawing
          val dr = drawableOfWin win
          val {fontinfo,shades,label,borderWidth=bw,fg,onColor,...} = v
          val (font,fonta,fontd) = fontinfo
          val fonth = fonta + fontd
          val fgPen = newPen[PV_Foreground fg, PV_LineWidth 2]
          val onPen = newPen[PV_Foreground onColor]
          val pts = let
                val SIZE{wid=lwid,...} = sizeOfLabel (label,fontinfo)
                val topy = #pady v + (fonth div 2)
                val boty = ht - topy
                val lightSz = #lightSz v
                val x0 = #padx v + lightSz div 2
                val x1 = #padx v
                val x2 = wid - x1
                val x3 = #padx v + 2*lightSz + lwid
                in
                  [PT{x=x0,y=topy},PT{x=x1,y=topy},PT{x=x1,y=boty},
                   PT{x=x2,y=boty},PT{x=x2,y=topy},PT{x=x3,y=topy}]
                end

          fun drawCheck (isOn,rel) = let
                val lightSz = #lightSz v
                val y = #pady v + (fonth - lightSz) div 2
                val x = #padx v + (lightSz div 2)
                val rect = RECT{x=x,y=y,wid=lightSz,ht=lightSz}
                val arg = {rect=rect,width=bw,relief=rel}
                in
                  if isOn then (
                    fillRect dr onPen rect;
                    ThreeD.drawRect dr arg shades
                  )
                  else ThreeD.drawFilledRect dr arg shades
                end

          fun drawLine () = drawLines dr fgPen pts
          fun drawLabel () = let
                val lightSz = #lightSz v
                val (font,fonta,_) = #fontinfo v
                val (s,lb,_) = #label v
                val x = #padx v + (2*lightSz) - lb + 1
                val y = #pady v + fonta + 1
                in drawString dr fgPen font (PT{x=x,y=y}, s) end

          fun init() = (clearDrawable dr; drawLabel (); drawLine ())

          fun setf (W.Inactive s,_,_) = (init(); drawCheck(s,W.Raised))
            | setf (W.Active s,_,d) = let
                val rel = if d then W.Sunken else W.Raised
                in init(); drawCheck (s,rel) end
          in 
            setf
          end

    fun bounds (BV v) = let
          val {label,lightSz,fontinfo,padx,pady,...} = v
          val SIZE{wid,ht} = sizeOfLabel (label,fontinfo)
          val halfLight = lightSz div 2
          val wid = (2*padx + 3*lightSz + wid)
          val ht = (2*pady + 2*ht)
          val x_dim = W.flexDim wid
          val y_dim = W.fixDim ht
          in {x_dim=x_dim,y_dim=y_dim} end

    fun win_args (BV{bg,...}) = {background = SOME bg}

  end (* HideView *)
