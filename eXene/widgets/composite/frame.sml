(* frame.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Frame widget, for putting a border around another widget
 *)

signature FRAME = 
  sig

    structure W : WIDGET

    type frame

    val frame : (W.root * W.view * W.arg list) -> W.widget -> frame

    val mkFrame : {
          color : W.EXB.color option,
          width : int, 
          widget : W.widget
        } -> frame

    val widgetOf : frame -> W.widget
    val setColor : frame -> W.EXB.color option -> unit

  end (* FRAME *)

structure Frame : FRAME = 
  struct

    structure W = Widget
    structure D = Drawing
    structure I = Interact

    open CML Geometry

    val attrs = [
        (Attrs.attr_padx,           Attrs.AT_Int,      Attrs.AV_Int 0),
        (Attrs.attr_pady,           Attrs.AT_Int,      Attrs.AV_Int 0),
        (Attrs.attr_borderWidth,    Attrs.AT_Int,      Attrs.AV_Int 2),
        (Attrs.attr_relief,         Attrs.AT_Relief,   Attrs.AV_Relief (W.Sunken)),
        (Attrs.attr_background,     Attrs.AT_Color,    Attrs.AV_NoValue)
      ]

    type res = {
                 padx : int,
                 pady : int,
                 borderWidth : int,
                 relief : W.relief,
                 background : W.EXB.color option
               }

    datatype frame = Frame of {
        widget : W.widget,
        reqChan : W.EXB.color option chan
      }

    fun mkResources (view,args) = let
          val attrs = W.findAttr (W.attrs (view,attrs,args))
          in {
               padx = Attrs.getInt (attrs Attrs.attr_padx),
               pady = Attrs.getInt (attrs Attrs.attr_pady),
               borderWidth = Attrs.getInt (attrs Attrs.attr_borderWidth),
               relief = Attrs.getRelief(attrs Attrs.attr_relief),
               background = Attrs.getColorOpt (attrs Attrs.attr_background)
             } end

    fun frame (root, view, args) widget = let
          val res = mkResources (view,args)
          val reqChan = channel ()
          val reqEvt = CML.recvEvt reqChan
          val realizeVar = SyncVar.iVar ()

          fun fillfn W.Flat (d,r,c) = let
                val p = D.newPen [D.PV_Foreground c]
                in fn () => D.fillRect d p r end
            | fillfn rel (d,r,c) = let
                val shades as {base,...} = W.shades root c
                val arg1 = {rect=r,width= #borderWidth res,relief=rel}
                fun fill () = (
                        if (#padx res = 0) andalso (#pady res = 0) then ()
                        else D.fillRect d base r;
                        ThreeD.drawRect d arg1 shades
                      )
                in fill end

          fun size () = let
                fun incBase (W.DIM{base,incr,min,nat,max},extra) =
                  W.DIM{base=base+extra,incr=incr,min=min,nat=nat,max=max}
                val {x_dim, y_dim} = W.boundsOf widget
                val xextra = 2*(#padx res + #borderWidth res)
                val yextra = 2*(#pady res + #borderWidth res)
                in
                  {x_dim = incBase(x_dim,xextra), y_dim = incBase(y_dim,yextra)}
                end

          fun realizeFrame {env as I.InEnv{co=myco,...}, win, sz} color = let
                val (my_inenv, my_outenv) = I.createWinEnv ()
                val I.InEnv{ci=myci,...} = I.ignoreInput my_inenv

                fun childRect (SIZE{wid,ht}) = let
                      val xoff = #padx res + #borderWidth res
                      val yoff = #pady res + #borderWidth res
                      in
                        RECT{x = xoff,
                             y = yoff,
                             wid=Int.max(1,wid-(2*xoff)),
                             ht=Int.max(1,ht-(2*yoff))}
                      end

                val crect = childRect sz
                val cwin = W.wrapCreate(win, crect,W.argsOf widget)
                val (cinenv, coutenv) = I.createWinEnv ()
                val I.OutEnv{co=childco,...} = coutenv
                val drawable = D.drawableOfWin win

                fun mkFill (_,NONE) = (fn _ => D.clearDrawable drawable)
                  | mkFill (r,SOME c) = fillfn (#relief res) (drawable,r,c)

                fun main (rect, color, update) = let
                      val fill = mkFill (rect,color)

                      fun handleCO I.CO_ResizeReq = sync(myco I.CO_ResizeReq)
                        | handleCO I.CO_KillReq = W.EXW.destroyWin cwin

                      fun handleCI (I.CI_Resize (RECT{x,y,wid,ht})) =
                           (W.EXW.moveAndResizeWin cwin 
                             (childRect(SIZE{wid=wid,ht=ht}));
                           main(RECT{x=0,y=0,wid=wid,ht=ht},color,false))
                       | handleCI (I.CI_Redraw _) = fill ()
                       | handleCI _ = ()
  
                      fun loop () =
                        select [
                          wrap(reqEvt, fn c => main (rect,c,true)),
                          wrap (myci, loop o handleCI o I.msgBodyOf),
                          wrap (childco, loop o handleCO)
                        ]
                    in
                      loop(if update then fill () else ())
                    end
                  in
                    Router.routePair (env, my_outenv, coutenv);
                    W.realizeFn widget {
                      env = cinenv, 
                      win = cwin,
                      sz = sizeOfRect crect
                    };
                    W.EXW.mapWin cwin;
                    main (mkRect(originPt,sz),color,false)
                  end

          fun initLoop color =
                select [
                  wrap(SyncVar.iGetEvt realizeVar, fn arg => realizeFrame arg color),
                  wrap(recvEvt reqChan, fn c =>initLoop c)
                ]
          in
            spawn (fn () => initLoop (#background res));
            Frame {
              widget=W.mkWidget {
                root=root,
                args= fn () => {background = NONE},
                boundsOf=size, 
                realize=fn arg => SyncVar.iPut(realizeVar,arg)
              },
              reqChan = reqChan
             }
          end

    fun widgetOf (Frame{widget,...}) = widget
    fun setColor (Frame{reqChan,...}) color = send(reqChan, color)

    fun mkFrame {color,width,widget} = let
          open Attrs
          val root = W.rootOf widget
          val name = Styles.mkView {name = Styles.styleName ["frame"],
                                    aliases = []}
          val args = [ (attr_borderWidth, Attrs.AV_Int width) ]
          val args = case color of
                       NONE => args
                     | SOME c => (attr_background,  Attrs.AV_Color c)::args
          in frame (root,(name,W.styleOf root),args) widget end

  end
