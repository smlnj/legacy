(* shape-view.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Functor for producing simple shape button views.
 *)

structure ShapeTypes =
  struct
    type sizefn = int * int option -> Widget.bounds
    type drawfn = (Drawing.drawable * Geometry.size * int) ->          
          (Drawing.pen * Drawing.pen * Drawing.pen) -> unit
  end

signature SHAPE_VIEW =
  sig
    val attrs : (Attrs.attr_name * Attrs.attr_type * Attrs.attr_value) list
    val config : (Attrs.attr_name -> Attrs.attr_value) -> 
      (ShapeTypes.sizefn * ShapeTypes.drawfn)
  end

functor ShapeView (S : SHAPE_VIEW) : BUTTON_VIEW = 
  struct

    structure W = Widget

    open Geometry

    val attrs =
        (Attrs.attr_borderWidth,    Attrs.AT_Int,    Attrs.AV_Int 2)::
        (Attrs.attr_width,          Attrs.AT_Int,    Attrs.AV_Int 30)::
        (Attrs.attr_height,         Attrs.AT_Int,    Attrs.AV_NoValue)::
        (Attrs.attr_background,     Attrs.AT_Color,  Attrs.AV_Str "white")::
        (Attrs.attr_color,          Attrs.AT_Color,  Attrs.AV_NoValue)::
        (Attrs.attr_readyColor,     Attrs.AT_Color,  Attrs.AV_NoValue)::
        (Attrs.attr_foreground,     Attrs.AT_Color,  Attrs.AV_NoValue)::
        S.attrs

    datatype button_view = BV of {
        bw : int,
        shades : W.shades,
        rshades : W.shades,
        stipple : W.EXB.tile,
        drawfn : ShapeTypes.drawfn,
        fg : W.EXB.color option,
        bg : W.EXB.color,
        sz : W.bounds
      }

    fun buttonView (root,view,args) = let
          val attrs = W.findAttr (W.attrs(view, attrs, args))
          val (sizefn,drawfn) = S.config attrs
          val stipple = W.tile root "gray"
          val wid = Attrs.getInt (attrs Attrs.attr_width)
          val ht = Attrs.getIntOpt (attrs Attrs.attr_height)
          val forec = Attrs.getColorOpt (attrs Attrs.attr_foreground)
          val backc = Attrs.getColor (attrs Attrs.attr_background)
          val color = case Attrs.getColorOpt (attrs Attrs.attr_color) of
                        SOME c => c | _ => backc
          val readyc = case Attrs.getColorOpt (attrs Attrs.attr_readyColor) of
                        SOME c => c | _ => color
          val bwid = Attrs.getInt(attrs Attrs.attr_borderWidth)
          val shades = W.shades root color
          in
            BV {
              bg = backc,
              fg = forec,
              bw = bwid,
              stipple = stipple,
              shades = shades,
              rshades = if W.EXB.sameColor(color,readyc) then shades
                        else W.shades root readyc,
              drawfn = drawfn,
              sz = sizefn(wid,ht)
            }
          end

    fun config (BV v,win,sz) = let
          open Drawing
          val dr = drawableOfWin win
          val {fg,bw,drawfn,shades,rshades,...} = v
          val draw = drawfn (dr,sz,bw)

          fun addStipple p = updatePen(p, [PV_FillStyle_Stippled, 
                               PV_Stipple (#stipple v)])

          val {light,base,dark} = shades
          val {light=rlight,base=rbase,dark=rdark} = rshades
          val ilight = addStipple light
          val idark = addStipple dark
          val ibase = addStipple base

          val (fore,ifore) = 
                case fg of
                  NONE => (base,ibase)
                | SOME c => let
                    val forepen = newPen [PV_Foreground c]
                    in (forepen, addStipple forepen) end
  
          fun setf (W.Inactive true,_,_) = draw(ibase,idark,ilight)
            | setf (W.Inactive false,_,_)= draw(ibase,ilight,idark)
            | setf (W.Active false,ready,false) = 
                if ready then draw (rbase,rlight,rdark)
                else draw(base,light,dark)
            | setf (W.Active false,ready,true) =
                if ready then draw (rbase,rdark,rlight)
                else draw(base,dark,light)
            | setf (W.Active true,ready,false) =
                if ready then draw (rbase,rdark,rlight)
                else draw(base,dark,light)
            | setf (W.Active true,ready,true) =
                if ready then draw (rbase,rdark,rlight)
                else draw(base,dark,light)

          fun fsetf (W.Inactive true,_,_) = draw(ifore,ilight,idark)
            | fsetf (W.Inactive false,_,_)= draw(ibase,ilight,idark)
            | fsetf (W.Active false,ready,false) = 
                if ready then draw (rbase,rlight,rdark)
                else draw(base,light,dark)
            | fsetf (W.Active false,ready,true) =
                if ready then draw (rbase,rdark,rlight)
                else draw(base,dark,light)
            | fsetf (W.Active true,ready,false) =
                if ready then draw (fore,rlight,rdark)
                else draw(fore,light,dark)
            | fsetf (W.Active true,ready,true) =
                if ready then draw (fore,rdark,rlight)
                else draw(fore,dark,light)

          in case fg of NONE => setf | _ => fsetf end

    fun bounds (BV{sz,...}) = sz
    fun win_args (BV{bg,...}) = {background = SOME bg}

  end (* ShapeView *)
