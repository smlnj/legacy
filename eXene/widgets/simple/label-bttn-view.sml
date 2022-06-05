(* label-bttn-view.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Basic labeled button view.
 *)

structure LabelBttnView : BUTTON_VIEW = 
  struct

    structure W = Widget

    open CML Geometry

    datatype label_type = Text of string | Icon of W.EXB.tile

    datatype rqst = 
        SetLabel of label_type
      | SetBC of W.EXB.color
      | SetFC of W.EXB.color
      | GetBounds of W.bounds SyncVar.ivar
      | DoRealize of {
          env : Interact.in_env,
          win : W.EXB.window,
          sz : Geometry.size
        }

    datatype label_data = 
        TXT of {s : string, rb : int, lb : int}
      | ICON of W.EXB.tile

    fun mkFontInfo font = let
          val {ascent=fonta,descent=fontd} = Font.fontHt font
          in (font, fonta, fontd) end

    fun mkTextLabel (s,font) = let
          val Font.CharInfo {left_bearing=lb,right_bearing=rb,...}
                = #overall_info (Font.textExtents font s)
          in TXT{s=s,lb=lb,rb=rb} end

    fun sizeOfLabel (ICON tile,_) = W.EXB.sizeOfTile tile
      | sizeOfLabel (TXT {lb,rb,...},(_,fa,fd)) =
          SIZE{wid = rb - lb + 2, ht = fa + fd + 2}

    datatype light_type = RadioLight | CheckLight

    fun cvtLight "radio" = SOME RadioLight
      | cvtLight "check" = SOME CheckLight
      | cvtLight _ = NONE

    type light = {
           space : int, 
           size : int, 
           ltype : light_type, 
           color : W.EXB.color}

    fun mkLight (NONE, _, _, _, _) = NONE
      | mkLight (SOME lt, ICON tile, height, _, color) = let
          val SIZE{wid,ht} = W.EXB.sizeOfTile tile
          val ht = if height > 0 then height else ht
          val size = case lt of
                       CheckLight => (65*ht) div 100
                     | RadioLight => (75*ht) div 100
          in
            SOME{ltype=lt,space=ht,size=size,color=color}
          end
      | mkLight (SOME lt, _, _, (font,fonta,fontd), color) = let
          val size = case lt of
                       CheckLight => (80*(fonta + fontd)) div 100
                     | RadioLight => fonta + fontd
          val ht = size + (Font.textWidth font "0")
          in
            SOME{ltype=lt,space=ht,size=size,color=color}
          end

    datatype button_view = BV of {
        light : light option, 
        label : label_data, 
        relief : W.relief,
        fg : W.EXB.color, 
        bg : W.EXB.color, 
        readyc : W.EXB.color, 
        shades : W.shades,
        stipple : W.EXB.tile,
        borderWidth : int,
        font : (W.EXB.font * int * int),
        align : W.halign,
        width : int,
        height : int,
        padx : int,
        pady : int
      }

    val dfltFont = "-Adobe-Helvetica-Bold-R-Normal--*-120-*"

    local open Attrs in
    val attrs = [
        (attr_halign,         AT_HAlign,     AV_HAlign W.HCenter),
        (attr_tile,           AT_Tile,       AV_NoValue),
        (attr_label,          AT_Str,        AV_Str ""),
        (attr_type,           AT_Str,        AV_Str "NoLight"),
        (attr_borderWidth,    AT_Int,        AV_Int 2),
        (attr_height,         AT_Int,        AV_Int 0),
        (attr_width,          AT_Int,        AV_Int 0),
        (attr_padx,           AT_Int,        AV_Int 1),
        (attr_pady,           AT_Int,        AV_Int 1),
        (attr_font,           AT_Font,       AV_Str dfltFont),
        (attr_relief,         AT_Relief,     AV_Relief W.Raised),
        (attr_foreground,     AT_Color,      AV_Str "black"),
        (attr_color,          AT_Color,      AV_NoValue),
        (attr_readyColor,     AT_Color,      AV_NoValue),
        (attr_background,     AT_Color,      AV_Str "white")
      ]
    end

    fun buttonView (root,view,args) = let
          open Attrs Drawing
          val attrs = W.findAttr (W.attrs(view,attrs,args))
          val ltype = getString(attrs attr_type)
          val align = getHAlign(attrs attr_halign)
          val bw = getInt(attrs attr_borderWidth)
          val height = getInt(attrs attr_height)
          val width = getInt(attrs attr_width)
          val padx = getInt(attrs attr_padx)
          val pady = getInt(attrs attr_pady)
          val font as (f,_,_) = mkFontInfo(getFont(attrs attr_font))
          val label = ICON(getTile(attrs attr_tile))
                        handle _ => mkTextLabel(getString(attrs attr_label),f)
          val relief = getRelief(attrs attr_relief)
          val lab = getString(attrs attr_label)
          val fg = getColor(attrs attr_foreground)
          val bg = getColor(attrs attr_background)
          val readyc = case getColorOpt(attrs attr_readyColor) of
                        NONE => bg
                      | SOME c => c
          val setColor = case getColorOpt(attrs attr_color) of
                           NONE => fg
                         | SOME c => c
          val light = mkLight(cvtLight ltype,label,height,font,setColor)
          in
            BV {
              light = light,
              label = label,
              relief = relief,
              stipple = W.tile root "gray",
              fg = fg,
              bg = bg,
              shades = W.shades root bg,
              readyc = readyc,
              borderWidth = Int.max(0,bw),
              font = font,
              align = align,
              width = Int.max(0,width),
              height = Int.max(0,height),
              padx = Int.max(0,padx),
              pady = Int.max(0,pady)
            }
          end

    fun bounds (BV v) = let
          val {label,borderWidth,width,height,padx,pady,font,...} = v
          val lightSpace = case (#light v) of NONE => 0
                                            | SOME{space,...} => space
          val SIZE{wid,ht} = sizeOfLabel (label,font)
          val wid = if width > 0 then width
                    else (2*borderWidth + 2*padx + lightSpace + wid)
          val ht = if height > 0 then height else (2*borderWidth + 2*pady + ht)
          val x_dim = W.flexDim wid
          val y_dim = W.flexDim ht
          in {x_dim=x_dim,y_dim=y_dim} end

    fun config (BV v, win, SIZE{wid,ht}) = let
          open Drawing
          val dr = drawableOfWin win
          val {light,shades,label,borderWidth=bw,fg,bg,readyc,...} = v
          val rect = RECT{x=0,y=0,wid=wid,ht=ht}
          val xoff = bw + (#padx v)
          val backPen = newPen[PV_Foreground bg]
          val readyPen = newPen[PV_Foreground readyc]
          val normalPen = newPen[PV_Foreground fg, PV_Background bg]
          val inactivePen = newPen[PV_Foreground fg]
          val inactivePen = newPen [PV_Foreground fg,PV_Background bg,
                 PV_FillStyle_Stippled, PV_Stipple (#stipple v)]
          val lspace = case light of NONE => 0 | SOME{space,...} => space

          fun drawRadio (pen,size) isOn = let
                val ystart = ht div 2
                val half = size div 2
                val ps = [PT{x=xoff,y=ystart},PT{x=xoff+half,y=ystart+half},
                         PT{x=xoff+size,y=ystart},PT{x=xoff+half,y=ystart-half}]
                in
                  if isOn then fillPolygon dr pen {verts=ps,shape=ConvexShape}
                  else ();
                  ThreeD.drawPoly dr {pts=ps,width=bw,relief=W.Raised} shades
                end

          fun drawCheck (pen,size) isOn = let
                val r = RECT{x=xoff,y= (ht - size) div 2, wid = size, ht = size}
                in
                  if isOn then fillRect dr pen r else ();
                  ThreeD.drawRect dr {rect=r,width=bw,relief=W.Sunken} shades
                end

          val drawLabel =
            case label of
              ICON tile => let
                val SIZE{wid=twid,ht=tht} = W.EXB.sizeOfTile tile
                val sr = RECT{x=0,y=0,wid=twid,ht=tht}
                val x = case #align v of
                          W.HLeft => xoff + lspace
                        | W.HRight => wid - xoff - twid
                        | W.HCenter => (wid + lspace - twid) div 2
                val y = (ht - tht) div 2
                val arg = {src= TSRC tile,src_rect=sr,dst_pos=PT{x=x,y=y}}
                in fn pen => (bitBlt dr pen arg; ()) end
            | TXT {s,lb,rb} => let
                val (font,fonta,fontd) = #font v
                val pen = newPen[PV_Foreground fg]
                val x = case #align v of
                          W.HLeft => xoff + lspace - lb + 1
                        | W.HRight => wid - xoff - rb - 1
                        | W.HCenter => (wid + lspace - lb - rb) div 2
                val y = (ht + fonta - fontd) div 2
                in fn pen => drawString dr pen font (PT{x=x,y=y}, s) end

          fun setf (W.Inactive s,_,_) = let
                val rel = if s then W.Sunken else #relief v
                in
                  fillRect dr backPen rect;
                  drawLabel inactivePen;
                  ThreeD.drawRect dr {rect=rect,relief=rel,width=bw} shades
                end
            | setf (W.Active s,r,d) = let
                val backpen = if r then readyPen else backPen
                val rel = if s = d then #relief v else W.Sunken
                in
                  fillRect dr backpen rect;
                  drawLabel normalPen;
                  ThreeD.drawRect dr {rect=rect,relief=rel,width=bw} shades
                end

          fun lsetf drawLight (W.Inactive s,_,_) = let
                val rel = #relief v
                in
                  fillRect dr backPen rect;
                  drawLabel inactivePen;
                  drawLight s;
                  ThreeD.drawRect dr {rect=rect,relief=rel,width=bw} shades
                end
            | lsetf drawLight (W.Active s,r,d) = let
                val backpen = if r then readyPen else backPen
                val rel = if d then W.Sunken else #relief v
                in
                  fillRect dr backpen rect;
                  drawLabel normalPen;
                  drawLight s;
                  ThreeD.drawRect dr {rect=rect,relief=rel,width=bw} shades
                end

          in 
            case light of
              NONE => setf
            | SOME {ltype = CheckLight, size, color, ...} => 
                lsetf (drawCheck (newPen[PV_Foreground color],size))
            | SOME {ltype = RadioLight, size, color, ...} => 
                lsetf (drawRadio (newPen[PV_Foreground color],size))
          end
          
    fun win_args (BV{bg,...}) = {background = SOME bg}

  end (* LabelBttnView *)
