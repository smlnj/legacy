(* viewport.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Viewport widget, for panning over a child widget.
 *
 * TODO:
 *   Allow child window to vary within bounds.
 *   Parameterize by child (granularity, specific scroll function)
 *)
structure Viewport : VIEWPORT = struct

  structure W = Widget
  structure CML = CML

  open CML Geometry EXeneBase EXeneWin Interact Drawing Widget

  val viewportTM  = TraceCML.traceModule(XDebug.eXeneTM, "viewport")
  fun trace f = TraceCML.trace (viewportTM, f)
  fun debug str = trace(fn () => [str])

  datatype req_msg
    = DoRealize of {
          env : in_env,
          win : window,
          sz : size
        }
    | Get
    | Set of { horz : int option, vert : int option }

  type geometry = { rect : rect, childSz : size }

  datatype reply_msg = Geometry of geometry

  datatype viewport = VP of {
      widget : widget,
      evtc : geometry event,
      reqc : req_msg chan,
      repc : reply_msg chan
    }

  fun naturalSz {x_dim,y_dim} = SIZE{wid=natDim x_dim,ht=natDim y_dim}
  fun naturalRect arg = mkRect(originPt,naturalSz arg)

  fun viewBnds (wid,ht,cbnds) = let
        fun looseDim v = DIM{base=0,incr=1,min=1,nat=v,max=NONE}
        fun bnds () = let
              val {x_dim,y_dim} = cbnds ()
              val x = case wid of NONE => natDim x_dim | SOME x => x
              val y = case ht of NONE => natDim y_dim | SOME y => y
              in
                {x_dim= looseDim x, y_dim = looseDim y}
              end
        in
          bnds
        end

    (* Adjust view's rectangle *)
  fun newOrigin ({horz,vert},RECT{x,y,wid,ht}) = let
        val x = case horz of SOME h => h | _ => x
        val y = case vert of SOME v => v | _ => y
        in RECT{x=x,y=y,wid=wid,ht=ht} end

  fun filter (inevt, outchan) = let
    val timeOut = timeOutEvt(Time.fromMilliseconds 30)
    val filterCnt = 10
    fun optSend (i,v) = if i <> filterCnt then send(outchan,v) else ()

    fun main () =
      case sync inevt of
        v as Set _ => (send(outchan,v); counter(filterCnt,v))
      | Get => (send(outchan, Get); main ())
      | _ => main ()
    and counter (0,v) = (send(outchan,v); counter(filterCnt,v))
      | counter (arg as (i,v)) =
          select [
            wrap(timeOut, fn () => (optSend arg; main())),
            wrap(inevt, fn evt =>
              case evt of
                v' as Set _ => counter(i-1,v')
              | Get => (optSend arg; send(outchan, Get); main ())
              | _ => (optSend arg; main ())
            )
          ]
  in
    main ()
  end

  fun newGeom (wid,ht,{rect=RECT{x,y,...},childSz}) = let
        val SIZE{wid=cw,ht=ch} = childSz
        fun normal (x,w,cw) =
              if x < 0 then 0
              else if x+w <= cw then x
              else Int.max(0,cw-w)
        val x' = normal (x,wid,cw)
        val y' = normal (y,ht,ch)
        val rect' = RECT{wid=wid,ht=ht,x=x',y=y'}
        in rect' end

  fun mk_viewport (wid,ht,widget) = let
    val root = rootOf widget
    val scr = screenOf root
    val reqChan = channel () 
    val repChan = channel ()
    val evtChan = channel ()

    fun realizeView {env as InEnv{co=myco,...}, win, sz} (geom : geometry) = let
      val my_win = win
      val filtChan = channel ()
      val (my_inenv, my_outenv) = createWinEnv ()
      val InEnv{ci=myci,...} = ignoreInput my_inenv
      val r as RECT{x,y,...} = #rect geom

      val crect = naturalRect(boundsOf widget)
      val cwin = wrapCreate(my_win, crect,argsOf widget)
      val (cinenv, coutenv as OutEnv{co=childco,...}) = createWinEnv ()

      fun handleCI (CI_Resize (RECT{wid,ht,...}), geom) = 
            {rect=newGeom (wid,ht,geom),childSz= #childSz geom}
        | handleCI (_,geom) = geom

      fun handleCO (CO_ResizeReq,{rect,childSz}) =
            {rect=rect,childSz=sizeOfRect (naturalRect (boundsOf widget))}
        | handleCO (CO_KillReq,g) = (destroyWin cwin; g)

      fun handleReq (Set arg,{rect,childSz} : geometry) = let
            val r as RECT{x,y,...} = newOrigin (arg,rect)
            in
              if r <> rect then moveWin cwin (PT{x= ~x,y= ~y}) else ();
              {rect=r,childSz= childSz}
            end
        | handleReq (Get, geom) = (send(repChan, Geometry geom); geom)
        | handleReq (_,geom) = geom

     fun loop (geom as {childSz,rect}) = let
            fun doCI evt = let
                  val geom as {rect=rect',...} = handleCI (msgBodyOf evt,geom)
                  val origin' as PT{x,y} = originOfRect rect'
                  in
                    if origin' <> originOfRect rect
                       then (
                         moveWin cwin (PT{x= ~x,y= ~y}); 
                         changed {rect=rect',childSz=childSz}
                       )
                    else if sizeOfRect rect <> sizeOfRect rect'
                       then changed {rect=rect',childSz=childSz}
                    else loop geom
                  end

            fun doCO evt = let
                  val geom' as {rect=rect',
                                childSz=childSz'} = handleCO (evt, geom)
                  val origin' as PT{x,y} = originOfRect rect'
                  in if childSz <> childSz'
                        then (
                          let val SIZE{wid=cw,ht=ch} = childSz'
                              val cr = RECT{x= ~x,y= ~y,wid=cw,ht=ch}
                          in moveAndResizeWin cwin cr;
                          (* moveWin cwin (PT{x= ~x,y= ~y}); *)
                             changed geom'
                          end
                        )
                     else loop geom
                  end

            in
              (select [
                wrap (myci, doCI),
                wrap (childco, doCO),
                wrap (recvEvt filtChan, fn arg => loop(handleReq (arg, geom)))
              ])
            end
      and changed geom =
        (select [
          wrap (sendEvt(evtChan,geom), fn () => loop geom),
          wrap (myci, fn evt => changed(handleCI (msgBodyOf evt,geom))),
          wrap (childco, fn arg => changed(handleCO (arg,geom))),
          wrap (recvEvt filtChan, fn arg => changed(handleReq (arg, geom)))
        ])

    in
      Router.routePair (env, my_outenv, coutenv);
      moveWin cwin (PT{x= ~x,y= ~y});
      realizeFn widget {
        env = cinenv, 
        win = cwin,
        sz = sizeOfRect crect
      };
      spawn(fn () => filter(recvEvt reqChan, filtChan));
      mapWin cwin;
      changed {rect=mkRect(PT{x=x,y=y},sz),childSz=sizeOfRect crect}
    end

    fun initGeom () = let
      val SIZE{wid=cwid,ht=cht} = naturalSz(boundsOf widget)
      val wid = case wid of NONE => cwid | SOME w => w
      val ht = case ht of NONE => cht | SOME h => h
    in
      {rect=RECT{x=0,y=0,wid=wid,ht=ht},childSz=SIZE{wid=cwid,ht=cht}}
    end

    fun initLoop (geom : geometry) = let
          fun update ({horz,vert}) = let
                val RECT{x,y,wid,ht} = #rect geom
                val x' = case horz of SOME h => h | _ => x
                val y' = case vert of SOME v => v | _ => y
                val SIZE{wid=cwid,ht=cht} = naturalSz(boundsOf widget)
                in
                  {rect=RECT{x=x',y=y',wid=wid,ht=ht},
                   childSz=SIZE{wid=cwid,ht=cht}}
                end
          in
            case recv reqChan of
              Set arg => initLoop (update arg)
            | Get => (send(repChan, Geometry geom); initLoop geom)
            | DoRealize arg => realizeView arg geom
          end
  in
    spawn (fn () => initLoop (initGeom ()));
    VP {
      widget = mkWidget{
        root = root, 
        args = argsFn widget,
        boundsOf = viewBnds (wid,ht,boundsFn widget),
        realize = fn arg => send (reqChan, DoRealize arg)
      },
      evtc = recvEvt evtChan,
      reqc = reqChan,
      repc = repChan
    }
  end

  fun mkViewport widget = mk_viewport (NONE,NONE,widget)

  val attrs = [
      (Attrs.attr_width,        Attrs.AT_Int,      Attrs.AV_NoValue),
      (Attrs.attr_height,       Attrs.AT_Int,      Attrs.AV_NoValue),
      (Attrs.attr_background,   Attrs.AT_Color,    Attrs.AV_Str "white")
    ]

  fun viewport (root,view,args) widget = let
        val attrs = W.findAttr (W.attrs(view,attrs,args))
        val wid = Attrs.getIntOpt (attrs Attrs.attr_width)
        val ht = Attrs.getIntOpt (attrs Attrs.attr_height)
        val color = Attrs.getColor (attrs Attrs.attr_background)
        in mk_viewport (wid,ht,widget) end
  
  fun widgetOf (VP{widget,...}) = widget

  fun setHorz (VP{reqc,repc,...}) arg = send (reqc,Set{horz=SOME arg,vert=NONE})

  fun setVert (VP{reqc,repc,...}) arg = send (reqc,Set{vert=SOME arg,horz=NONE})

  fun setOrig (VP{reqc,repc,...}) (PT{x,y}) = 
        send (reqc,Set{vert=SOME y,horz=SOME x})

  fun getGeometry (VP{reqc,repc,...}) =
        (send (reqc,Get); case recv repc of Geometry g => g )

  fun evtOf (VP{evtc,...}) = evtc

end (* Viewport *)

