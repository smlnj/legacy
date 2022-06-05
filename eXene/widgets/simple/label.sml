(* label.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Label widget.
 *
 * TODO:
 *   allow user control over maxc, either in pixels or as character
 *)

signature LABEL = 
  sig

    structure W : WIDGET

    type label

    datatype label_type = Text of string | Icon of W.EXB.tile

    val label : (W.root * W.view * W.arg list) -> label

    val mkLabel : W.root -> {
        label : string, 
        font : string option,
        foregrnd : W.EXB.color option, 
        backgrnd : W.EXB.color option, 
        align : W.halign
      } -> label

    val widgetOf : label -> W.widget
    val setLabel : label -> label_type -> unit
    val setBackground : label -> W.EXB.color -> unit
    val setForeground : label -> W.EXB.color -> unit

  end (* LABEL *)

structure Label : LABEL = 
  struct

    structure W = Widget
    structure F = Format

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

    datatype font_info = FI of {
        font  : W.EXB.font,
        fonta : int,                  (* font ascent *)
        fontd : int,                  (* font descent *)
        maxc  : int                   (* max. char width *)
      }

    fun mkFontInfo font = let
          val {ascent,descent} = Font.fontHt font
          val {max_bounds,...} = Font.fontInfoOf font
          val Font.CharInfo {char_wid,...} = max_bounds
          in FI{font=font, fonta=ascent , fontd=descent, maxc = char_wid} end

    fun mkTextLabel (s,font) = let
          val Font.CharInfo {left_bearing=lb,right_bearing=rb,...}
                = #overall_info (Font.textExtents font s)
          in TXT{s=s,lb=lb,rb=rb} end

    datatype label_view = LV of {
        label : label_data ref, 
        fg : W.EXB.color ref, 
        bg : W.EXB.color ref, 
        shades : W.shades ref,
        relief : W.relief,
        borderWidth : int,
        font : font_info,
        align : W.halign,
        width : int,
        height : int,
        padx : int,
        pady : int
      }

    val dfltFont = "-Adobe-Helvetica-Bold-R-Normal--*-120-*"

    local open Attrs in
    val attrs = [
        (attr_halign, AT_HAlign, AV_HAlign W.HCenter),
        (attr_tile, AT_Tile, AV_NoValue),
        (attr_label, AT_Str, AV_Str ""),
        (attr_borderWidth, AT_Int, AV_Int 2),
        (attr_height, AT_Int, AV_Int 0),
        (attr_width, AT_Int, AV_Int 0),
        (attr_padx, AT_Int, AV_Int 1),
        (attr_pady, AT_Int, AV_Int 1),
        (attr_font, AT_Font, AV_Str dfltFont),
        (attr_relief, AT_Relief, AV_Relief W.Flat),
        (attr_foreground, AT_Color, AV_Str "black"),
        (attr_background, AT_Color, AV_Str "white")
      ]
    end

    fun labelView (root,view,args) = let
          open Attrs Drawing
          val attrs = W.findAttr (W.attrs(view,attrs,args))
          val align = getHAlign(attrs attr_halign)
          val bw = getInt(attrs attr_borderWidth)
          val height = getInt(attrs attr_height)
          val width = getInt(attrs attr_width)
          val padx = getInt(attrs attr_padx)
          val pady = getInt(attrs attr_pady)
          val fi as FI{font=f,...} = mkFontInfo(getFont(attrs attr_font))
          val label = ICON(getTile(attrs attr_tile))
                        handle _ => mkTextLabel(getString(attrs attr_label),f)
          val relief = getRelief(attrs attr_relief)
          val lab = getString(attrs attr_label)
          val fg = getColor(attrs attr_foreground)
          val bg = getColor(attrs attr_background)
          val shades = W.shades root bg
          in
            LV {
              label = ref label,
              fg = ref fg,
              bg = ref bg,
              shades = ref shades,
              relief = relief,
              borderWidth = Int.max(0,bw),
              font = fi,
              align = align,
              width = Int.max(0,width),
              height = Int.max(0,height),
              padx = Int.max(0,padx),
              pady = Int.max(0,pady)
            }
          end

    datatype label = Label of {widget : W.widget, rqst : rqst CML.chan}

    fun bounds lview = let
          val LV{borderWidth,width,height,padx,pady,font,...} = lview
          fun computeSz (LV{label = ref (ICON tile),...}) = let
                val SIZE{wid,ht} = W.EXB.sizeOfTile tile
                val w = if width > 0 then width else wid
                val h = if height > 0 then height else ht
                in SIZE{wid=w,ht=h} end
            | computeSz (LV{label = ref (TXT {rb,lb,s}),...}) = let
                val FI{fonta,fontd,maxc,...} = font
                val wid = rb - lb
                val lineHt = fonta + fontd
                val w = if width = 0 then wid 
                        else width*maxc
                val h = if height > 0 then height*lineHt else lineHt
                in SIZE{wid=w,ht=h} end

          val SIZE{wid,ht} = computeSz lview
          val x_dim = W.fixDim (wid + 2*(borderWidth+padx+1))
          val y_dim = W.fixDim (ht + 2*(borderWidth+pady+1))
          in {x_dim=x_dim,y_dim=y_dim} end

    fun updateLabel (lv as LV{label,...},Icon t) = (label := ICON t; lv)
      | updateLabel (lv as LV{label,font=FI{font,...},...},Text s) = 
          (label := mkTextLabel(s,font); lv)

    fun updateFg (lv as LV{fg,...}, c) = (fg := c; lv)
    fun updateBg root (lv as LV{bg,shades,...}, c) = 
          (bg := c; shades := W.shades root c; lv)

    fun draw (dr,SIZE{wid,ht}) (LV lv) = let
          open Drawing
          val {shades,relief,label,borderWidth,fg,bg,...} = lv
          val rect = RECT{x=0,y=0,wid=wid,ht=ht}
          val xoff = borderWidth + (#padx lv)
          in
            fillRect dr (newPen[PV_Foreground (!bg)]) rect;
            case !label of
              ICON tile => let
                val pen = newPen[PV_Foreground (!fg), PV_Background (!bg)]
                val SIZE{wid=twid,ht=tht} = W.EXB.sizeOfTile tile
                val sr = RECT{x=0,y=0,wid=twid,ht=tht}
                val x = case #align lv of
                          W.HLeft => xoff
                        | W.HRight => wid - xoff - twid
                        | W.HCenter => (wid - twid) div 2
                val y = (ht - tht) div 2
                val pos = PT{x=x,y=y}
                in 
                  bitBlt dr pen {src= TSRC tile,src_rect=sr,dst_pos=pos}; () 
                end
            | TXT {s,lb,rb} => let
                val FI{font,fonta,fontd,...} = #font lv
                val pen = newPen[PV_Foreground (!fg)]
                val x = case #align lv of
                          W.HLeft => xoff - lb + 1
                        | W.HRight => wid - xoff - rb - 1
                        | W.HCenter => (wid - lb - rb) div 2
                val y = (ht + fonta - fontd) div 2
                in drawString dr pen font (PT{x=x,y=y}, s) end;
            ThreeD.drawRect dr {rect=rect,relief=relief,width=borderWidth}
                  (!shades)
          end

    fun realize {env=inenv, win, sz} (root,reqc,lv) = let
          open Interact
          val InEnv{ci,co,...} = ignoreInput inenv
          val dr = Drawing.drawableOfWin win

          fun chkSize (label, label',wid,ht) =
                case (label,label') of
                  (TXT{lb,rb,...},TXT{lb=lb',rb=rb',...}) =>
                    if wid = 0 andalso rb' - lb' <> rb - lb 
                      then sync (co CO_ResizeReq)
                      else ()
                | (ICON t,ICON t') => let
                    val sz = W.EXB.sizeOfTile t
                    val sz' = W.EXB.sizeOfTile t'
                    in 
                      if (wid = 0 orelse ht = 0) andalso sz <> sz' 
                        then (TextIO.print "resize2\n"; sync (co CO_ResizeReq)) 
                        else () 
                    end
                | _ => (TextIO.print "resize3\n"; sync (co CO_ResizeReq))

          fun handleReq (SetLabel v, lv) = let
                val LV{label= ref l,width, height,...} = lv
                val lv' as LV{label = ref l',...} = updateLabel (lv,v)
                in
                  chkSize (l,l',width, height);
                  SOME lv'
                end
            | handleReq (SetBC c, lv) = SOME(updateBg root (lv,c))
            | handleReq (SetFC c, lv) = (updateFg (lv,c);SOME lv)
            | handleReq (GetBounds arg,lv) = (SyncVar.iPut(arg,bounds lv); NONE)
            | handleReq _ = NONE

          fun handleCI (CI_Redraw _, me as (lv,drawf)) = 
                (drawf lv; me)
            | handleCI (CI_Resize (RECT{wid,ht,...}), (lv,_)) = 
                (lv, draw(dr,SIZE{wid=wid,ht=ht}))
            | handleCI (_,me) = me
  
          fun loop (lv,drawf) =
                select [
                  wrap(recvEvt reqc, fn evt =>
                    case handleReq(evt,lv) of
                      NONE => loop (lv,drawf)
                    | SOME lv' => (drawf lv'; loop(lv',drawf))),
                  wrap(ci, fn evt => loop(handleCI (msgBodyOf evt,(lv,drawf))))
                ]
          in
            loop (lv,draw (dr,sz))
          end

    fun init (root,reqc,lv) = let
          fun loop lv =
                case recv reqc of
                  SetLabel l => loop(updateLabel (lv,l))
                | SetBC c => loop(updateBg root (lv,c))
                | SetFC c => loop(updateFg (lv,c))
                | DoRealize arg => realize arg (root,reqc,lv)
                | GetBounds arg => (SyncVar.iPut(arg,bounds lv); loop lv)
          in loop lv end
 
    fun label (args as (root,_,_)) = let
          val lv = labelView args
          val reqc = channel ()
          fun getBnds () = let
                val v = SyncVar.iVar ()
                in send (reqc,GetBounds v); SyncVar.iGet v end
          in
            spawn (fn () => init (root,reqc,lv));
            Label {
              widget = Widget.mkWidget{
                root=root,
                args = fn () => {background = NONE},
                boundsOf = getBnds,
                realize = fn arg => send(reqc,DoRealize arg)
              },
              rqst = reqc
            }
          end

    fun mkLabel root {label=caption,font,foregrnd,backgrnd,align} = let
          open Attrs
          val name = Styles.mkView {name = Styles.styleName ["label"],
                                    aliases = []}
          val args = [(attr_halign, AV_HAlign align),
                      (attr_label, AV_Str caption)]
          val args = case font of NONE => args
                                | SOME f => (attr_font, AV_Str f)::args
          val args = case foregrnd of 
                       NONE => args
                     | SOME fc => (attr_foreground, AV_Color fc)::args
          val args = case backgrnd of 
                       NONE => args
                     | SOME bc => (attr_background, AV_Color bc)::args
          in label (root,(name,W.styleOf root),args) end

    fun widgetOf (Label{widget,...}) = widget
    fun set msg (Label{rqst,...}) arg = send(rqst, msg arg)
    val setLabel = set SetLabel
    val setBackground = set SetBC
    val setForeground = set SetFC

  end (* Label *)
