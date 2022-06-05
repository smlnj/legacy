(* text-view.sml
 *
 * COPYRIGHT (c) 1991,1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Views for text buttons.
 *)

structure TextView : BUTTON_VIEW = 
  struct

    structure W = Widget

    open Geometry

    val dfltFontName = "8x13"

    val attrs = [
        (Attrs.attr_halign,       Attrs.AT_HAlign,  Attrs.AV_HAlign W.HCenter),
        (Attrs.attr_rounded,      Attrs.AT_Bool,    Attrs.AV_Bool false),
        (Attrs.attr_width,        Attrs.AT_Int,     Attrs.AV_NoValue),
        (Attrs.attr_height,       Attrs.AT_Int,     Attrs.AV_NoValue),
        (Attrs.attr_borderWidth,  Attrs.AT_Int,     Attrs.AV_Int 2),
        (Attrs.attr_label,        Attrs.AT_Str,     Attrs.AV_Str ""),
        (Attrs.attr_font,         Attrs.AT_Font,    Attrs.AV_Str dfltFontName),
        (Attrs.attr_color,        Attrs.AT_Color,   Attrs.AV_NoValue),
        (Attrs.attr_background,   Attrs.AT_Color,   Attrs.AV_Str "white"),
        (Attrs.attr_foreground,   Attrs.AT_Color,   Attrs.AV_Str "black")
      ]

    type fontdata = {font : W.EXB.font, fonta : int, fontd : int}
    fun mkFont font = let
          val {ascent=fonta,descent=fontd} = Font.fontHt font
          in {font=font, fonta=fonta, fontd=fontd} end

    type labeldata = {label : string, lb : int, rb : int}
    fun mkLabel (s,font) = let
          val Font.CharInfo {left_bearing=lb,right_bearing=rb,...}
                = #overall_info (Font.textExtents font s)
          in {label=s,lb=lb,rb=rb} end

    datatype button_view = BV of {
           align : W.halign,
           rounded : bool,
           width : int option,
           height : int option,
           borderWidth : int,
           labeldata : labeldata,
           fontdata : fontdata,
           stipple : W.EXB.tile,
           shades : W.shades,
           forec : W.EXB.color,
           color : W.EXB.color,
           backc : W.EXB.color
         }

    fun buttonView (root,view,args) = let
          val attrs = W.findAttr (W.attrs(view,attrs,args))
          val font = Attrs.getFont (attrs Attrs.attr_font)
          val backc = Attrs.getColor (attrs Attrs.attr_background)
          val color = case Attrs.getColorOpt (attrs Attrs.attr_color) of
                        SOME c => c
                      | _ => backc
          in
            BV {
              align = Attrs.getHAlign(attrs Attrs.attr_halign),
              rounded = Attrs.getBool(attrs Attrs.attr_rounded),
              width = Attrs.getIntOpt(attrs Attrs.attr_width),
              height = Attrs.getIntOpt(attrs Attrs.attr_height),
              borderWidth = Attrs.getInt(attrs Attrs.attr_borderWidth),
              labeldata = mkLabel(Attrs.getString(attrs Attrs.attr_label),font),
              stipple = W.tile root "gray",
              shades = W.shades root color,
              fontdata = mkFont font,
              forec = Attrs.getColor (attrs Attrs.attr_foreground),
              color = color,
              backc = backc
            }
          end

    val pad = 1
    val rpad = 4

    fun configfn (BV v, win,sz as SIZE{wid,ht}) = let
          open Drawing
          val d = drawableOfWin win
          val r = mkRect(originPt, sz)
          val {borderWidth,labeldata,fontdata,...} = v
          val textPen = newPen [PV_Foreground (#forec v)]
          val iTextPen = updatePen(textPen, [PV_FillStyle_Stippled,
                PV_Stipple (#stipple v)])
          val {base,light,dark} = #shades v
          val xoff = borderWidth + pad
          val {lb,rb,label} = labeldata
          val {fonta,fontd,font} = fontdata

          val x = case #align v of
                    W.HLeft => xoff - lb
                  | W.HRight => wid - xoff - rb - 1
                  | W.HCenter => (wid - lb - rb) div 2
          val y = (ht + fonta - fontd) div 2
          val textPt = PT{x=x,y=y}
          val border = ThreeD.draw3DRect d (r,borderWidth)

          fun draw (textPen,backPen,topPen,botPen) = (
                fillRect d backPen r;
                drawString d textPen font (textPt, label);
                border {top=topPen,bottom=botPen}
              )
        
          fun drawf (W.Active true,_,false) = draw (textPen,base,dark,light)
            | drawf (W.Active false,_,false) = draw (textPen,base,light,dark)
            | drawf (W.Active true,_,true) = draw (textPen,base,light,dark)
            | drawf (W.Active false,_,true) = draw (textPen,base,dark,light)
            | drawf (W.Inactive true,_,_) = draw (iTextPen,base,dark,light)
            | drawf (W.Inactive false,_,_) = draw (iTextPen,base,light,dark)
          in drawf end

    fun rconfigfn (BV v, win,sz as SIZE{wid,ht}) = let
          open Drawing RoundedRect
          val d = drawableOfWin win
          val r = RECT{x=0,y=0,wid=wid-1,ht=ht-1}
          val rad = Int.min(10, Int.min(wid, ht) div 6)
          val {backc,color,borderWidth,labeldata,fontdata,...} = v
          val textPen = newPen [PV_Foreground (#forec v)]
          val iTextPen = updatePen(textPen, [PV_FillStyle_Stippled,
                PV_Stipple (#stipple v)])
          val {base,light,dark} = #shades v
          val {lb,rb,label} = labeldata
          val {fonta,fontd,font} = fontdata

          val textPt = PT{x=(wid - lb - rb) div 2,y= (ht + fonta - fontd) div 2}

          fun draw (textPen,backPen,topPen,botPen) = (
                clearDrawable d;
                if W.EXB.sameColor (backc,color) then ()
                else fillRoundedRect d base {rect=r,c_wid=rad,c_ht=rad};
                drawString d textPen font (textPt, label);
                ThreeD.draw3DRoundRect d 
                  {rect=r,c_wid=rad,c_ht=rad,width=borderWidth}
                  {top=topPen,bottom=botPen}
              )
        
          fun drawf (W.Active true,_,false) = draw (textPen,base,dark,light)
            | drawf (W.Active false,_,false) = draw (textPen,base,light,dark)
            | drawf (W.Active true,_,true) = draw (textPen,base,light,dark)
            | drawf (W.Active false,_,true) = draw (textPen,base,dark,light)
            | drawf (W.Inactive true,_,_) = draw (iTextPen,base,dark,light)
            | drawf (W.Inactive false,_,_) = draw (iTextPen,base,light,dark)
          in drawf end

    fun bounds (BV v) = let
          val {rounded,borderWidth,labeldata,fontdata,...} = v
          val inset = borderWidth + (if rounded then rpad else pad)
          val {lb,rb,...} = labeldata
          val {fonta,fontd,...} = fontdata
          val lwid = rb - lb
          val lht = fonta + fontd
          val x = case #width v of
                    SOME w => w
                  | NONE => lwid + 2*inset
          val y = case #height v of
                    SOME h => h
                  | NONE => lht + 2*inset
          in {x_dim = W.flexDim x, y_dim = W.flexDim y} end

    fun config (arg as (BV{rounded=true,...},win,sz)) = rconfigfn arg
      | config arg = configfn arg

    fun win_args (BV{backc,...}) = {background = SOME backc}

  end (* TextView *)
