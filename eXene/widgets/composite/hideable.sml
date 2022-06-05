(* hideable.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Widget for "iconizing" another widget.
 *)

signature HIDEABLE = 
  sig

    structure W : WIDGET

    type hideable

    val hideable : (W.root * W.view * W.arg list) -> W.widget -> hideable

    val widgetOf : hideable -> W.widget

  end (* HIDEABLE *)

structure Hideable : HIDEABLE = 
  struct

    structure W = Widget
    structure EXW = EXeneWin
    structure D = Drawing

    open Geometry

    datatype rqst =
      GetBounds of W.bounds SyncVar.ivar
    | DoRealize of { env : Interact.in_env, win : W.EXB.window, sz : size }

    datatype hideable = H of {widget : W.widget, rqst : rqst CML.chan}

    open Geometry

    val dfltFont = "-Adobe-Helvetica-Bold-R-Normal--*-120-*"
    val minBorderWidth = 4
    val ltBorderWidth = 2
    val pady = 2    (* padding above and below label *)
    val space = 10  (* spacing between light and label *)

    local open Attrs in
    val attrs = [
        (attr_borderWidth,    AT_Int,    AV_Int 10),
        (attr_label,          AT_Str,    AV_Str ""),
        (attr_font,           AT_Font,   AV_Str dfltFont),
        (attr_color,          AT_Color,  AV_NoValue),
        (attr_background,     AT_Color,  AV_Str "white"),
        (attr_foreground,     AT_Color,  AV_Str "black"),
	(attr_selectColor,    AT_Color,  AV_Str "black")
      ]
    end (* local *)

    fun mkFontInfo font = let
          val {ascent=fonta,descent=fontd} = Font.fontHt font
          in (font, fonta, fontd) end

    fun mkTextLabel (s,font) = let
          val Font.CharInfo {left_bearing=lb,right_bearing=rb,...}
                = #overall_info (Font.textExtents font s)
          in (s,lb,rb) end

    fun sizeOfLabel ((s,lb,rb),(_,fa,fd)) =
          SIZE{wid = rb - lb + 2, ht = fa + fd}

    fun setLightWidth (_,fonta,fontd) = (80 * (fonta+fontd)) div 100

    type res = {
        child : W.widget,
        shades : W.shades,
        fontinfo : (W.EXB.font * int * int),
        label : (string * int * int),
        fg : W.EXB.color,
        bg : W.EXB.color,
        lightSz : int,
        pady : int,
        borderWidth : int,
        onColor : W.EXB.color
      }

    fun mkRes (root,view,args) widget = let
          open Attrs
          val attrs = W.findAttr (W.attrs(view, attrs, args))
          val fontinfo as (f,_,_) = mkFontInfo(getFont(attrs attr_font))
          val label = mkTextLabel(getString(attrs attr_label),f)
          val borderWidth = getInt (attrs attr_borderWidth)
          val forec = getColor (attrs attr_foreground)
          val backc = getColor (attrs attr_background)
          val selectC = getColor (attrs attr_selectColor)
          val color = case getColorOpt (attrs attr_color) of
                        SOME c => c 
                      | _ => backc
          val lightSz = setLightWidth fontinfo
          in
            {
              child = widget,
              fontinfo = fontinfo,
              label = label,
              fg = forec,
              bg = backc,
              pady = pady,
              shades = W.shades root color,
              lightSz = lightSz,
              borderWidth = Int.max(borderWidth,minBorderWidth),
              onColor = selectC
            }
          end

    fun drawfn (dr, SIZE{wid,ht},v : res) = let
          open Drawing
          val (font,fonta,fontd) = #fontinfo v
          val fonth = fonta + fontd
          val txtPen = newPen[PV_Foreground (#fg v)]
          val onPen = newPen[PV_Foreground (#onColor v)]

          fun drawLight (isOn,rel) = let
                val lightSz = #lightSz v
                val y = #pady v + (fonth - lightSz) div 2
                val x = #borderWidth v
                val rect = RECT{x=x,y=y,wid=lightSz,ht=lightSz}
                val arg = {rect=rect,width=ltBorderWidth,relief=rel}
                val shades = #shades v
                in
                  if isOn then (
                    fillRect dr onPen rect;
                    ThreeD.drawRect dr arg shades
                  )
                  else ThreeD.drawFilledRect dr arg shades
                end

          fun drawGroove () = let
                val bw = #borderWidth v
                val SIZE{wid=lwid,...} = sizeOfLabel (#label v,#fontinfo v)
                val lightSz = #lightSz v
                val y = #pady v + (fonth div 2)
                val rht = ht - y - (bw div 2)
                val rect = RECT{x=bw div 2,y=y,wid=wid-bw,ht=rht}
                val clrRect = RECT{x= bw + lightSz,y= #pady v,
                                   wid= space + lwid,ht=fonth}
                val arg = {rect=rect,width=2,relief=W.Groove}
                in
                  ThreeD.drawRect dr arg (#shades v);
                  clearArea dr clrRect
                end

          fun drawLabel () = let
                val lightSz = #lightSz v
                val (font,fonta,_) = #fontinfo v
                val (s,lb,_) = #label v
                val x = #borderWidth v + lightSz + space - lb + 1
                val y = #pady v + fonta + 1
                in drawString dr txtPen font (PT{x=x,y=y}, s) end

          fun init() = (clearDrawable dr; drawGroove (); drawLabel ())

          fun draw (doInit,isOpen,down) = (
                if doInit then init() else (); 
                drawLight (isOpen,if down then W.Sunken else W.Raised) 
              )
          in 
            draw
          end

    datatype mouse_evt = MseDown | MseUp of bool

    fun mseP (m, mchan) = let
          open CML Interact
          fun downLoop isIn = 
                case msgBodyOf (sync m) of 
                  MOUSE_LastUp _ => send (mchan, MseUp isIn)
                | MOUSE_Leave _ => downLoop false
                | MOUSE_Enter _ => downLoop true
                | _ => downLoop isIn 

          fun loop () =
                (case msgBodyOf (sync m) of 
                  MOUSE_FirstDown {but,...} => (
                    send (mchan, MseDown);
                    downLoop true)
                | _ => ();
                loop ())
          in loop () end

    fun adjust (W.DIM{base,incr,min,nat,max},low) = let
           fun adj(l,mn) = if l >= low then mn else adj(l+incr,mn+1)
           val min' = adj(base+min*incr,min)
           val nat' = Int.max(nat,min')
           val max' = case max of 
                        NONE => NONE 
                      | SOME m => SOME(Int.max(m,nat'))
           in
             W.DIM{base=base,incr=incr,min=min',nat=nat',max=max'}
           end

    fun bounds (res : res, isOpen) = let
          fun incBase (W.DIM{base,incr,min,nat,max},extra) =
                  W.DIM{base=base+extra,incr=incr,min=min,nat=nat,max=max}
          val SIZE{wid,ht} = sizeOfLabel (#label res ,#fontinfo res )
          val {x_dim, y_dim} = W.boundsOf (#child res)
          val xextra = 2*(#borderWidth res)
          val topwid = xextra + (#lightSz res) + wid + space
          val x_dim = if W.minDim x_dim >= topwid then x_dim
                      else adjust(x_dim,topwid)
          val yextra = 2*(#pady res) + (#borderWidth res) + ht
          val y_dim = if isOpen then incBase (y_dim,yextra) else W.fixDim yextra
          in 
            {x_dim=incBase(x_dim,xextra),y_dim=y_dim} 
          end

    fun realize ({env, win, sz},res : res,rqst) = let
          open CML Interact
          val mchan = channel ()
          val rcvm = recvEvt mchan
          val InEnv{co=myco,...} = env
          val (my_inenv, my_outenv) = createWinEnv ()
          val InEnv{ci,m,...} = ignoreKey my_inenv

          fun childRect (SIZE{wid,ht}) = let
                val bw = #borderWidth res
                val (_,fonta,fontd) = #fontinfo res
                val yoff = #pady res + fonta + fontd
                in
                  RECT{x = bw,
                       y = yoff,
                       wid=Int.max(1,wid-bw-bw),
                       ht=Int.max(1,ht-yoff-bw)}
                end

          val crect = childRect sz
          val cwin = W.wrapCreate(win, crect,W.argsOf (#child res))
          val (cinenv, coutenv) = createWinEnv ()
          val OutEnv{co=childco,...} = coutenv
          val dr = D.drawableOfWin win

          fun handleCO (CO_ResizeReq,isOpen) = 
                if isOpen then CML.sync(myco CO_ResizeReq) else ()
            | handleCO (CO_KillReq,_) = 
                (W.EXW.destroyWin cwin;CML.sync(myco CO_KillReq))

          fun handleCI (CI_Resize (RECT{wid,ht,...}),me) = let
                val sz = SIZE{wid=wid,ht=ht}
                in
                  if #1 me then W.EXW.moveAndResizeWin cwin (childRect sz)
                  else ();
                  (#1 me, #2 me, drawfn (dr,sz,res))
                end
            | handleCI (CI_Redraw _,me as (isOpen,down,drawfn)) =
                (drawfn(true,isOpen,down); me)
            | handleCI (_,me) = me
  
          fun handleReq (GetBounds ans,isOpen) = 
                SyncVar.iPut(ans,bounds (res,isOpen))
            | handleReq _ = ()

          fun handleM (MseDown,(isOpen,_,drawfn)) =
                (drawfn(false,isOpen,true); (isOpen,true,drawfn))
            | handleM (MseUp true,(isOpen,_,drawfn)) =
                (if isOpen then EXW.unmapWin cwin else EXW.mapWin cwin; 
                 CML.sync(myco CO_ResizeReq);
                 drawfn(false,not isOpen,false);
                 (not isOpen,false,drawfn))
            | handleM (MseUp false,(isOpen,_,drawfn)) =
                (drawfn(false,isOpen,false); (isOpen,false,drawfn))

          fun main me =
                select [
                  wrap(rqst, fn r => (handleReq(r,#1 me);main me)),
                  wrap (ci, fn msg => main(handleCI(msgBodyOf msg,me))),
                  wrap (rcvm, fn m => main(handleM(m,me))),
                  wrap (childco, fn c => (handleCO (c,#1 me); main me))
                ]
          in
            CML.spawn (fn () => mseP(m,mchan));
            Router.routePair (env, my_outenv, coutenv);
            W.realizeFn (#child res) {
              env = cinenv, 
              win = cwin,
              sz = sizeOfRect crect
            };
            main (false,false,drawfn (dr,sz,res))
          end

    fun init (res : res,rqst) = let
          fun handleReq (GetBounds ans) = SyncVar.iPut(ans,bounds (res,false))
            | handleReq (DoRealize arg) = realize (arg,res,rqst)
          fun loop () = (handleReq(CML.sync rqst);loop())
          in loop () end

    fun hideable (root,view,args) widget = let
          val rqst = CML.channel()
          val res = mkRes (root,view,args) widget
          fun boundsOf () = let
                val v = SyncVar.iVar ()
                in
                  CML.send(rqst, GetBounds v);
                  SyncVar.iGet v
                end
          val w = W.mkWidget {
                    root=root,
                    args = fn () => {background = SOME (#bg res)},
                    boundsOf = boundsOf,
                    realize = fn arg => CML.send(rqst,DoRealize arg)
                  }
          in
            CML.spawn (fn () => init (res,CML.recvEvt rqst));
            H {widget=w,rqst=rqst}
          end

    fun widgetOf (H{widget,...}) = widget

  end (* Hideable *)
