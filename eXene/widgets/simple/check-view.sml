(* check-view.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * View for check box.
 *)

structure CheckView : BUTTON_VIEW = 
  struct

    structure W = Widget

    open Geometry

    val attrs = [
        (Attrs.attr_relief,         Attrs.AT_Relief, Attrs.AV_Relief W.Flat),
        (Attrs.attr_width,          Attrs.AT_Int,    Attrs.AV_Int 30),
        (Attrs.attr_readyColor,     Attrs.AT_Color,  Attrs.AV_NoValue),
        (Attrs.attr_color,          Attrs.AT_Color,  Attrs.AV_NoValue),
        (Attrs.attr_background,     Attrs.AT_Color,  Attrs.AV_Str "white"),
        (Attrs.attr_foreground,     Attrs.AT_Color,  Attrs.AV_Str "black")
      ]

    datatype button_view = BV of {
        relief : W.relief,
        shades : W.shades,
        stipple : W.EXB.tile,
        fg : W.EXB.color,
        bg : W.EXB.color,
        color : W.EXB.color,
        readyc : W.EXB.color,
        sz : int
      }

    fun buttonView (root,view,args) = let
          val attrs = W.findAttr (W.attrs(view, attrs, args))
          val sz = Attrs.getInt (attrs Attrs.attr_width)
          val relief = Attrs.getRelief (attrs Attrs.attr_relief)
          val forec = Attrs.getColor (attrs Attrs.attr_foreground)
          val backc = Attrs.getColor (attrs Attrs.attr_background)
          val color = case Attrs.getColorOpt (attrs Attrs.attr_color) of
                        SOME c => c 
                      | _ => forec
          val readyc = case Attrs.getColorOpt(attrs Attrs.attr_readyColor) of
                        NONE => color
                      | SOME c => c
          val stipple = W.tile root "lightGray"
          in
            BV {
              fg = forec,
              stipple = stipple,
              bg = backc,
              color = color,
              readyc = readyc,
              relief = relief,
              shades = W.shades root color,
              sz = sz
            }
          end

    fun config (BV(v as {sz,color,shades,readyc,...}), win, SIZE{wid,ht}) = let
          open Drawing
          val drawable = drawableOfWin win
          val bwid = 2 
          val pwid = 3 
          val readyp = if W.EXB.sameColor(#bg v,readyc) then NONE
                       else SOME(newPen[PV_Foreground readyc])
          val {light,base,dark} = shades
          val chkPen = newPen [PV_Foreground (#fg v), 
                PV_LineWidth 3, PV_JoinStyle_Miter]
          val stipple = #stipple v
          fun mki p = updatePen(p, [PV_FillStyle_Stippled,PV_Stipple stipple])
          val iChkPen = mki chkPen
          val ishades = {light= mki light, dark = mki dark, base = mki base}
  
          val bsz = Int.min(wid,ht) div 2
          val xstart = (wid - bsz) div 2
          val ystart = (ht - bsz) div 2
          val boxR = RECT{x=xstart,y=xstart,wid=bsz,ht=bsz}
          val drawr = 
             ThreeD.drawRect drawable {width=bwid, rect=boxR, relief= #relief v}
          val chkPts = [
            PT{x=xstart+4,y=ht div 2},
            PT{x=wid div 2,y=(ystart+bsz)-4},
            PT{x=(xstart+bsz)+4,y= ystart - (bsz div 6)}
          ]
    
          fun drawCheck pen = drawLines drawable pen chkPts

          fun drawBox (shades,back) = (
                clearDrawable drawable;
                case back of
                  SOME p => fillRect drawable p boxR
                | NONE => ();
                drawr shades
              )
            
          fun setf (W.Inactive true,_,_) = 
                (drawBox (ishades,NONE);drawCheck iChkPen)
            | setf (W.Inactive false,_,_) = drawBox (ishades,NONE)
            | setf (W.Active false,ready,false) = 
                if ready then drawBox (shades,readyp)
                else drawBox(shades, NONE)
            | setf (W.Active false,ready,true) =
                if ready then (drawBox(shades,readyp);drawCheck chkPen)
                else (drawBox(shades, NONE);drawCheck chkPen)
            | setf (W.Active true,ready,false) =
                if ready then (drawBox(shades,readyp);drawCheck chkPen)
                else (drawBox(shades, NONE);drawCheck chkPen)
            | setf (W.Active true,ready,true) =
                if ready then drawBox (shades,readyp)
                else drawBox(shades, NONE)

          in setf end

    fun bounds (BV{sz,...}) = W.fixBounds(sz,sz)
    fun win_args (BV{bg,...}) = {background = SOME bg}

  end (* CheckView *)
