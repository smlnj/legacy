(* slider-view.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Slider views.
 *)

signature SLIDER_VIEW = 
  sig
    structure W : WIDGET

    type res
    type state = (int * bool * bool * bool)

    val attrs : (Attrs.attr_name * Attrs.attr_type * Attrs.attr_value) list
    val getResources : (W.root * (Attrs.attr_name -> Attrs.attr_value)) -> res
    val drawf : (W.EXB.window * Geometry.size * res) -> (state * bool) -> unit
    val ptToVal : (Geometry.size * res) -> Geometry.point -> int
    val boundsOf : res -> unit -> W.bounds

  end (* SLIDER_VIEW *)

structure SliderView : SLIDER_VIEW = 
  struct
    structure W = Widget

    open Geometry

    val min = Int.min
    val max = Int.max

    fun mkFontInfo font = let
          val {ascent=fonta,descent=fontd} = Font.fontHt font
          in (font, fonta, fontd) end

    fun mkLabel (NONE,_) = NONE
      | mkLabel (SOME s,font) = let
          val Font.CharInfo {left_bearing=lb,right_bearing=rb,...}
                = #overall_info (Font.textExtents font s)
          in SOME(s,lb,rb) end

    fun intToStr i =
          if i >= 0 then Int.toString i
          else "-"^(Int.toString (~i))

    fun getTickWidth (fromV,toV,font,fonta) = let
          fun sz (i : int) = let 
                val s = intToStr i
                val Font.CharInfo {left_bearing=lb,right_bearing=rb,...}
                      = #overall_info (Font.textExtents font s)
                in rb-lb end
          in (fonta div 2) + max(sz fromV,sz toV) end

    type state = (int * bool * bool * bool)
    type res = {
        bg : W.EXB.color,
        borderWidth : int,
        fontinfo : (W.EXB.font * int * int),
        fg : W.EXB.color,
        isVertical : bool,
        relief : W.relief,
        fromV : int,
        label : (string * int * int) option,
        length : int,
        showValue : bool,
        shades : W.shades,
        readyShades : W.shades,
        slideShades : W.shades,
        thumb : int,
        tick : int,
        toV : int,
        offset : int,
        twid : int,
        width : int
      }

    fun numTicks ({tick=0,...} : res) = 0
      | numTicks ({tick,fromV,toV,...} : res) = let
          val stop = if fromV <= toV then fn v => toV < v else fn v => toV > v
          fun loop (v,cnt) = if stop v then cnt else loop(v+tick,cnt+1)
          in loop (fromV,0) end

    local open Attrs in
    val attrs = [
        (attr_readyColor,     AT_Color,      AV_NoValue),
        (attr_background,     AT_Color,      AV_Str "white"),
        (attr_borderWidth,    AT_Int,        AV_Int 2),
        (attr_font,           AT_Font,       AV_Str "8x13"),
        (attr_foreground,     AT_Color,      AV_Str "black"),
        (attr_isVertical,     AT_Bool,       AV_Bool true),
        (attr_relief,         AT_Relief,     AV_Relief W.Flat),
        (attr_fromValue,      AT_Int,        AV_Int 0),
        (attr_label,          AT_Str,        AV_NoValue),
        (attr_length,         AT_Int,        AV_Int 100),
        (attr_showValue,      AT_Bool,       AV_Bool false),
        (attr_color,          AT_Color,      AV_NoValue),
        (attr_thumbLength,    AT_Int,        AV_Int 30),
        (attr_tickInterval,   AT_Int,        AV_Int 0),
        (attr_toValue,        AT_Int,        AV_Int 100),
        (attr_width,          AT_Int,        AV_Int 15)
      ]
    end (* local *)

    val spacing = 2

    fun getResources (root,attrs) = let
          open Attrs
          val fg = getColor(attrs attr_foreground)
          val bg = getColor(attrs attr_background)
          val shades = W.shades root bg
          val slideShades = case getColorOpt(attrs attr_color) of
                              NONE => shades
                            | SOME c => W.shades root c
          val readyShades = case getColorOpt(attrs attr_readyColor) of
                              NONE => slideShades
                            | SOME c => W.shades root c
          val toV = getInt(attrs attr_toValue)
          val fromV = getInt(attrs attr_fromValue)
          val borderWidth = getInt(attrs attr_borderWidth)
          val font = getFont(attrs attr_font)
          val fontinfo as (font,fonta,_) = mkFontInfo font
          val thumb = max(6,max(3*borderWidth,getInt(attrs attr_thumbLength)))
          val relief = getRelief(attrs attr_relief)
          fun chkTick t = if fromV <= toV then if t < 0 then ~t else t
                          else if t > 0 then ~t else t
 
          in
            {
              bg = bg,
              fg = fg,
              borderWidth = borderWidth,
              fontinfo = fontinfo,
              isVertical = getBool(attrs attr_isVertical),
              relief = relief,
              fromV = fromV,
              label = mkLabel(getStringOpt(attrs attr_label),font),
              length = max(getInt(attrs attr_length),thumb),
              showValue = getBool(attrs attr_showValue),
              shades = shades,
              slideShades = slideShades,
              readyShades = readyShades,
              thumb = thumb,
              tick = chkTick(getInt(attrs attr_tickInterval)),
              twid = getTickWidth(fromV,toV,font,fonta),
              offset = case relief of W.Flat => 0 | _ => borderWidth,
              toV = toV,
              width = max(3*borderWidth,getInt(attrs attr_width))
            }
          end

    fun hboundsOf (res : res) = let
          val (_,fonta,fontd) = #fontinfo res
          val lineHt = fonta + fontd
          val offset2 = 2*(#offset res)
          val bw2 = 2*(#borderWidth res)
          val x = max((#length res) + offset2,(numTicks res)*(#twid res))
          val y = (#width res) + offset2 + bw2 +
                  (if #tick res <> 0 then lineHt else 0) +
                  (if #showValue res then lineHt + spacing else 0) +
                  (case #label res of NONE => 0 | _ => lineHt)
          val bnds = {x_dim = W.flexDim (x+bw2), y_dim = W.fixDim y}
          in
            fn () => bnds
          end

    fun vboundsOf (res : res) = let
          val (font,fonta,fontd) = #fontinfo res
          val labelWidth = case #label res of
                             NONE => 0
                           | SOME(_,lb,rb) => rb - lb + fonta
          val twid = #twid res
          val tickWidth = if #tick res <> 0 then twid else 0
          val valueWidth = if #showValue res then twid else 0
          val offset2 = 2*(#offset res)
          val bw2 = 2*(#borderWidth res)
          val y = (#length res) + offset2 + bw2
          val x = (#width res) + offset2 + bw2 +
                  labelWidth + spacing + tickWidth + valueWidth
          val bnds = {x_dim = W.fixDim x, y_dim = W.flexDim y}
          in
            fn () => bnds
          end

    fun valToPt (sz,res : res) = let
          val {isVertical,thumb,borderWidth,offset,fromV,toV,...} = res
          val SIZE{wid,ht} = sz
          val range = real(toV - fromV)
          val prange = (if isVertical then ht else wid)
                          - thumb - 2*(offset + borderWidth)
          val rprange = real prange
          val off = thumb div 2 + offset + borderWidth
          in
            if Real.==(range, 0.0) then fn _ => off
            else fn value => let
	      val y = trunc((real(value - fromV) * rprange)/range + 0.5)
              val y = if y < 0 then 0 else if y > prange then prange else y
              in y + off end
          end

    fun drawBorder (0,_,_,_,_) = ()
      | drawBorder (bw,d,wid,ht,res : res) = let
          val r = RECT{x=0,y=0,wid=wid,ht=ht}
          val rel = #relief res
          val shades = #shades res
          in
            ThreeD.drawRect d {rect=r,relief= rel,width=bw} shades
          end

    fun drawVValue (valToPt,ht,dr,pen,res : res) (value,right) = let
          open Drawing
          val (font,fonta,fontd) = #fontinfo res
          val offset = #offset res
          val s = intToStr value
          val Font.CharInfo {right_bearing,ascent,descent,...}
                = #overall_info (Font.textExtents font s)
          val y = max((valToPt value) + (fonta div 2),offset+ascent)
          val y = min(y,ht - offset - descent)
          val x = right - right_bearing
          in drawString dr pen font (PT{x=x,y=y},s) end

    fun drawHValue (valToPt,wid,dr,pen,res : res) (value,bottom) = let
          open Drawing
          val (font,_,fontd) = #fontinfo res
          val s = intToStr value
          val Font.CharInfo {left_bearing=lb,right_bearing=rb,...}
                = #overall_info (Font.textExtents font s)
          val y = bottom - fontd
          val offset = #offset res
          val x = max((valToPt value) - (lb + rb) div 2,lb + offset)
          val x = min(x,wid - offset - rb)
          in drawString dr pen font (PT{x=x,y=y},s) end

    fun vdrawf (win,sz as SIZE{wid,ht}, res : res) = let
          open Drawing
          val d = drawableOfWin win
          val fd = feedback d
          val offset = #offset res
          val borderWidth = #borderWidth res
          val valToPt = valToPt (sz,res)
          val textPen = newPen[PV_Foreground (#fg res)]
          val drawVValue = drawVValue (valToPt,ht,d,textPen,res)
          val (font,fonta,fontd) = #fontinfo res
          val labelWid = case #label res of
                           NONE => 0
                         | SOME(_,lb,rb) => rb - lb + fonta
          val twid = #twid res
          val tickWid = if #tick res <> 0 then twid else 0
          val valueWid = if #showValue res then twid else 0
          val total = tickWid + valueWid + 2*(borderWidth+spacing)
                        + labelWid + (#width res)
          val tickRight = (wid - total) div 2 + tickWid
          val valueRight = tickRight + valueWid
          val scaleLeft = valueRight + spacing
          val labelLeft = scaleLeft + 2*borderWidth
	                    + (#width res) + (fonta div 2)

          fun drawTicks 0 = ()
            | drawTicks delta = let
                val toV = #toV res
                val fromV = #fromV res
                val stop = if fromV <= toV then fn v => toV < v 
                           else fn v => toV > v
                fun loop v =
                  if stop v then ()
                  else (drawVValue(v,tickRight);loop(v+delta))
                in loop fromV end

          fun drawLabel NONE = ()
            | drawLabel (SOME(s,_,_)) = let
                val x = labelLeft
                val y = offset + (3*fonta) div 2
                in drawString d textPen font (PT{x=x,y=y},s) end

          fun drawValue (value,dontErase) = (
                if dontErase then ()
                else let
                  val r = RECT{x= valueRight - valueWid, y= offset,
                               wid=valueWid, ht= ht - 2*offset}
                  in clearArea d r end;
                drawVValue (value,valueRight)
              )

          fun initWin () = (
                clearDrawable d;
                drawTicks (#tick res);
                drawLabel (#label res)
              )

          fun drawSlider (value,ready,down,doAll) = let
                open ThreeD
                val width = #width res
                val bw = #borderWidth res
                val shades = #shades res
                val slideShades = if ready then #readyShades res
                                  else #slideShades res
                fun drawInset(x,y,w,h,sw,rel) = let
                      val x = x+sw
                      val y = y+sw
                      val h = h-sw
                      val w = w - 2*sw
                      val r = RECT{x=x,y=y,wid=w,ht=h}
                      val r' = RECT{x=x,y=y+h,wid=w,ht=h}
                      in
                        drawFilledRect d {rect=r,width=sw,relief=rel} slideShades;
                        drawFilledRect fd {rect=r',width=sw,relief=rel} slideShades
                      end

                fun drawSlide () = let
                      val sht = (#thumb res) div 2
                      val swid = width
                      val x = scaleLeft + bw
                      val y = (valToPt value) - sht
                      val shadowWidth = max(1,bw div 2)
                      val relief = if down then W.Sunken else W.Raised
                      val r = RECT{x=x,y=y,wid=swid,ht=2*sht}
                      in
                        drawRect d {rect=r,width=bw,relief=relief} slideShades;
                        drawInset(x,y,swid,sht,shadowWidth,relief)
                      end
                in
                  if doAll then let
                    val r = RECT{x=scaleLeft,y=offset,ht=ht - 2*offset,
                                 wid= width + 2*bw}
                    in drawRect d {rect=r,width=bw,relief=W.Sunken} shades end
                  else let
                    val r = RECT{y=offset+bw,x=scaleLeft+bw,
                                 ht=ht - 2*(bw+offset), wid= width}
	            in clearArea d r end;
                  drawSlide ()
                end

          fun draw ((value,active,ready,down),doAll) = (
                if doAll then initWin() else ();
                if #showValue res then drawValue (value,doAll) else ();
                drawSlider (value,ready,down,doAll);
                if doAll then drawBorder (offset,d,wid,ht,res) else ()
              )
          in draw end

    fun hdrawf (win,sz as SIZE{wid,ht},res : res) = let
          open Drawing
          val d = drawableOfWin win
          val fd = feedback d
          val offset = #offset res
          val valToPt = valToPt (sz,res)
          val textPen = newPen[PV_Foreground (#fg res)]
          val drawHValue = drawHValue (valToPt,wid,d,textPen,res)
          val (font,fonta,fontd) = #fontinfo res
          val lineHt = fonta + fontd
          val tickHt = if #tick res <> 0 then lineHt else 0
          val valueHt = if #showValue res then lineHt + spacing else 0
          val labelHt = case #label res of NONE => 0 | _ => lineHt
          val borderWidth = #borderWidth res
          val total = tickHt + valueHt + 2*borderWidth + (#width res)
                        + labelHt
          val ticky = (ht + total) div 2 - 1
          val valuey = ticky - tickHt
          val scaley = valuey - valueHt
          val labely = scaley - 2*borderWidth - (#width res)

          fun drawTicks 0 = ()
            | drawTicks delta = let
                val toV = #toV res
                val fromV = #fromV res
                val stop = if fromV <= toV then fn v => toV < v 
                           else fn v => toV > v
                fun loop v =
                  if stop v then ()
                  else (drawHValue(v,ticky);loop(v+delta))
                in loop fromV end

          fun drawLabel NONE = ()
            | drawLabel (SOME(s,lb,rb)) = let
                val x = offset + fonta div 2
                val y = labely - fontd
                in drawString d textPen font (PT{x=x,y=y},s) end

          fun initWin () = (
                clearDrawable d;
                drawTicks (#tick res);
                drawLabel (#label res)
              )

          fun drawValue (value,dontErase) = (
                if dontErase then ()
                else let
                  val r = RECT{x= offset, y= scaley + 1,
                               wid= wid - 2*offset, 
                               ht= valuey - scaley}
                  in clearArea d r end;
                drawHValue (value,valuey)
              )

          fun drawSlider (value,ready,down,doAll) = let
                open ThreeD
                val width = #width res
                val bw = #borderWidth res
                val y = scaley - 2*bw - width + 1;
                val shades = #shades res
                val slideShades = if ready then #readyShades res
                                  else #slideShades res
                fun drawInset(x,y,w,h,sw,rel) = let
                      val x = x+sw
                      val y = y+sw
                      val w = w-sw
                      val h = h - 2*sw
                      val r = RECT{x=x,y=y,wid=w,ht=h}
                      val r' = RECT{x=x+w,y=y,wid=w,ht=h}
                      in
                        drawFilledRect d {rect=r,width=sw,relief=rel} slideShades;
                        drawFilledRect fd {rect=r',width=sw,relief=rel} slideShades
                      end

                fun drawSlide () = let
                      val swid = (#thumb res) div 2
                      val sht = width
                      val x = (valToPt value) - swid
                      val shadowWidth = max(1,bw div 2)
                      val y = y + bw
                      val relief = if down then W.Sunken else W.Raised
                      val r = RECT{x=x,y=y,wid= 2*swid,ht=sht}
                      in
                        drawRect d {rect=r,width=bw,relief=relief} slideShades;
                        drawInset(x,y,swid,sht,shadowWidth,relief)
                      end
                in
                  if doAll then let
                    val r = RECT{x=offset,y=y,wid=wid - 2*offset,
                                 ht= width + 2*bw}
                    in drawRect d {rect=r,width=bw,relief=W.Sunken} shades end
                  else let
                    val r = RECT{x=offset+bw,y=y+bw,wid=wid - 2*(bw+offset),
                                 ht= width}
	            in clearArea d r end;
                  drawSlide ()
                end

          fun draw ((value,active,ready,down),doAll) = (
                if doAll then initWin() else ();
                if #showValue res then drawValue (value,doAll) else ();
                drawSlider (value,ready,down,doAll);
                if doAll then drawBorder (offset,d,wid,ht,res) else ()
              )
          in draw end

    exception BadRange

    fun ptToVal (sz,res : res) = let
          val {isVertical,thumb,borderWidth,offset,fromV,toV,...} = res
          val SIZE{wid,ht} = sz
          val l = if isVertical then ht else wid
          val range = l - thumb - 2*(offset + borderWidth)
          val prange = real range
          val inset = thumb div 2 + offset + borderWidth
          val rrange = real(toV-fromV)
          fun mkv v = let
                val value = min(max(0,v - inset),range)
                val tmp = ((real value)*rrange)/prange + (real fromV)
                in trunc(if tmp < 0.0 then tmp - 0.5 else tmp + 0.5) end
          in
            if range <= 0 then fn _ => raise BadRange
            else if isVertical then fn (PT{y,...}) => mkv y
            else fn (PT{x,...}) => mkv x
          end

    fun boundsOf (res : res) =
          if #isVertical res then vboundsOf res else hboundsOf res

    fun drawf (arg as (_,_,res : res)) =
          if #isVertical res then vdrawf arg else hdrawf arg

  end (* SliderView *)
