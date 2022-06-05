(* shell.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Shell widget to provide interface between X library/window manager
 * and widgets.
 *
 * TODO: Allow mapping/unmapping of shells
 *       Cleanup and complete shell resource usage
 *)

signature SHELL = 
  sig

    structure W : WIDGET

    type shell

    (* type wm_args = { 
     *   win_name : string option, 
     *   icon_name : string option
     * }
     *)
    type wm_args
    val mkWMArgs : {win_name : string option, icon_name : string option} -> wm_args

    (* type hints = {
     *   size_hints : size_hints list,
     *   wm_hints : wm_hints list
     * }
     *)
    type hints
    val mkHints : {size_hints : W.EXW.ICCC.size_hints list, wm_hints : W.EXW.ICCC.wm_hints list } 
                    -> hints

    val shell : (W.root * W.view * W.arg list) -> W.widget -> shell
    
    val mkShell : W.widget * W.EXB.color option * wm_args -> shell
    val mkShellAt : W.G.rect -> W.widget * W.EXB.color option * wm_args -> shell
    
    val mkTransientShell : W.EXB.window -> 
          W.widget * W.EXB.color option * wm_args -> shell
    val mkTransientShellAt : W.G.rect -> W.EXB.window ->
          W.widget * W.EXB.color option * wm_args -> shell

    val setWMHints : shell -> hints -> unit
    val init : shell -> unit
    val map : shell -> unit
    val unmap : shell -> unit
    val destroy : shell -> unit
    
    (* added by ddeboer: *)
    val deleteEvent : shell -> unit CML.event
    (* end added *)
    
  end (* SHELL *)

structure Shell : SHELL = 
  struct

    structure W = Widget
    structure ICCC = ICCC

    local open CML Geometry EXeneBase Interact EXeneWin Widget ICCC in

    type hints = {
      size_hints : size_hints list,
      wm_hints : wm_hints list
      (* class_hints : {res_class : string, res_name : string} option *)
    }
    fun mkHints a = a
    datatype shell_msg = Init | Destroy | Map of bool | Hints of hints

    (* modified by ddeboer; original:
    datatype shell = Shell of (shell_msg chan) *)
    datatype shell = Shell of (shell_msg chan * unit chan)
    (* end modified *)
    
    fun setSizeHints {x_dim=x_dim as DIM xdim,y_dim=y_dim as DIM ydim} = let
          fun minSz () = let
                val minx = minDim x_dim
                val miny = minDim y_dim
                in
                  SIZE{wid=Int.max(1,minx),ht=Int.max(1,miny)}
                end
          fun maxSz () = (maxDim x_dim, maxDim y_dim)
          fun incSz () = (#incr xdim, #incr ydim)

          val MAX = 65535

          fun doInc () =
                case incSz () of
                  (1,1) => []
                | (x, 1) => [HINT_PResizeInc (SIZE{wid=x,ht=1})]
                | (1, y) => [HINT_PResizeInc (SIZE{wid=1,ht=y})]
                | (x, y) => [HINT_PResizeInc (SIZE{wid=x,ht=y})]

          fun doMin () = let
                val minsz = minSz ()
                in
                  [HINT_PMinSize minsz,HINT_PBaseSize minsz]
                end

          fun doMax () =
                case maxSz () of
                  (NONE,NONE) => []
                | (SOME x, NONE) => [HINT_PMaxSize (SIZE{wid=x,ht=MAX})]
                | (NONE, SOME y) => [HINT_PMaxSize (SIZE{wid=MAX,ht=y})]
                | (SOME x, SOME y) => [HINT_PMaxSize (SIZE{wid=x,ht=y})]
          in
            (doInc())@(doMax())@(doMin())
          end

(* DEBUG
    val setSizeHints = fn arg => let
          val pr = XDebug.pr1
          val arglist = setSizeHints arg
          fun pritem (HINT_PResizeInc sz) = pr("inc = "^(Db.sztos sz)^"\n")
            | pritem (HINT_PMaxSize sz) = pr("max = "^(Db.sztos sz)^"\n")
            | pritem (HINT_PMinSize sz) = pr("min = "^(Db.sztos sz)^"\n")
            | pritem _ = ()
          in
            app pritem arglist;
            arglist
          end
*)

    type wm_args = { win_name : string option, icon_name : string option }
    fun mkWMArgs a = a

    fun placement (NONE,sz) = (originPt,sz)
      | placement (SOME(RECT{x,y,wid,ht}),SIZE{wid=dfltwid,ht=dfltht}) =
          (PT{x=x,y=y},
           SIZE{wid= if wid > 0 then wid else dfltwid,
                ht= if ht > 0 then ht else dfltht})

    fun mk_shell crwin rectopt (widget, colorOpt, wm_args : wm_args) = let
          val root = rootOf widget
          val reqChan = channel ()
          val scr = screenOf root
          val color = 
            case colorOpt of 
              NONE => whiteOfScr scr
            | SOME color => color
      
          (* added by ddeboer: *)
          val delCh = CML.channel()
          (* end added *)
      
          fun setProtocols win = 
                (* modified, ddeboer, to include WM_TAKE_FOCUS. *)
                setWMProtocols win [(ICCC.internAtom (displayOf root) "WM_DELETE_WINDOW") (*,
                                    (ICCC.internAtom (displayOf root) "WM_TAKE_FOCUS") *)]
               
          fun init (hintlist,mapped) = let
                val bnds as {x_dim,y_dim} = boundsOf widget
                val dfltsize = SIZE{wid=natDim x_dim,ht=natDim y_dim}
                val (origin,size) = placement(rectopt,dfltsize)
                (* modified by ddeboer; original: 
                val (twin, inEnv) = crwin widget { ... *)
                val (twin, inEnv, inDelChOpt) = crwin widget {
                  geom=WGEOM{pos=origin, sz=size, border=0},
                  backgrnd = color,
                  border = color   (* not used *)
                }

                fun sendHint {size_hints, wm_hints} =
                      setWMProperties twin {
                        argv = [],
                        win_name = NONE,
                        icon_name = NONE,
                        size_hints = size_hints,
                        wm_hints = wm_hints,
                        class_hints = NONE
                      }

                val _ = setWMProperties twin {
                          argv = SMLofNJ.getArgs(),
                          win_name = #win_name wm_args,
                          icon_name = #icon_name wm_args,
                          size_hints = setSizeHints bnds,
                          wm_hints = [],
                          class_hints = NONE
                        }
                val _ = app sendHint (rev hintlist)
                val _ = setProtocols twin

                val (my_inenv, my_outenv) = createWinEnv ()
    
                val cwin = wrapCreate(twin, mkRect(originPt,size),argsOf widget)
                val (cinenv, coutenv as OutEnv{co,...}) = createWinEnv ()
                val childco = wrapQueue co
                val InEnv{ci=myci,...} = ignoreInput my_inenv
    
                fun zombie () =
                      zombie (select [
                        wrap (myci, fn _ => ()),
                        wrap (recvEvt reqChan, fn _ => ()),
                        wrap (childco, fn _ => ())
                      ])
          
                fun handleCO CO_ResizeReq = let
                      val (bnds as {x_dim, y_dim}) = boundsOf widget
                      in
                  setWMProperties twin {
                    argv = [],
                    win_name = NONE,
                    icon_name = NONE,
                    size_hints = setSizeHints bnds,
                    wm_hints = [],
                    class_hints = NONE
                  };                
                  resizeWin twin (SIZE{wid=natDim x_dim,ht=natDim y_dim})
                      end
                  | handleCO CO_KillReq = (destroyWin twin; zombie())
          
                fun handleCI (CI_Resize (RECT{wid,ht,...})) = 
                      resizeWin cwin (SIZE{wid=wid,ht=ht})
                  | handleCI CI_OwnDeath = zombie ()
                  | handleCI (CI_ChildDeath _) = zombie ()
                  | handleCI (CI_Redraw _) = ()
                  | handleCI _ = ()
          
                fun mapTopWin (false,true) = (mapWin twin; true)
                  | mapTopWin (true, false) = (withdrawWin twin; false)
                  | mapTopWin (_,b) = b
                      
                fun handleReq mapped =
                      fn Init => mapped
                       | Destroy => (destroyWin twin; zombie ())
                       | Hints hint => (sendHint hint; mapped)
                       | Map arg => mapTopWin (mapped,arg)
        
                fun loop mapped =
                       (select [
                           wrap (myci, handleCI o msgBodyOf),
                           (* added by ddeboer: *)
                           (case inDelChOpt of
                            SOME inDelCh => 
                                wrap (recvEvt inDelCh, 
                                 fn () => (CML.send (delCh, ())))
                              | NONE => never), 
                           (* end added *)
                           wrap (recvEvt reqChan, loop o (handleReq mapped)),
                           wrap (childco, handleCO)
                         ]; 
                       loop mapped)
                in
                  Router.routePair (inEnv, my_outenv, coutenv);
                  realizeFn widget {
                    env = cinenv, 
                    win = cwin,
                    sz = size
                  };
                  mapWin cwin;
                  loop (mapTopWin(false, mapped))
                end
  
          fun initLoop (arg as (hintlist,mapped)) =
            case recv reqChan of
              Init => init arg
            | Destroy => initLoop arg
            | Hints hint => initLoop (hint::hintlist,mapped)
            | Map mapped' => initLoop (hintlist,mapped')
  
          in
            XDebug.xspawn ("shell", fn () => initLoop ([],true));
            (* modified by ddeboer; original:
            Shell reqChan *) 
            (Shell (reqChan, delCh))
          end
  
    local
      (* modified by ddeboer; original: 
      fun simple wdgt = createSimpleTopWin (screenOf(rootOf wdgt)) 
      fun trans w _ = createTransientWin w*)
      fun simple wdgt g = 
        let
        val (win,inEnv,delCh) = (createSimpleTopWin (screenOf(rootOf wdgt)) g)
        in (win,inEnv,SOME delCh) end
      fun trans w _ g = 
        let
        val (win,inEnv) = createTransientWin w g
        in (win,inEnv,NONE) end (* end modified *)
    in
    fun mkShellAt r = mk_shell simple (SOME r)
    val mkShell = mk_shell simple NONE
    fun mkTransientShellAt r w = mk_shell (trans w) (SOME r)
    fun mkTransientShell w = mk_shell (trans w) NONE
    val attrs = [
        (Attrs.attr_title,          Attrs.AT_Str,    Attrs.AV_NoValue),
        (Attrs.attr_iconName,       Attrs.AT_Str,    Attrs.AV_NoValue),
        (Attrs.attr_background,     Attrs.AT_Color,  Attrs.AV_NoValue)
      ]

    fun shell (root, view ,args) widget = let
          val attrs = W.findAttr (W.attrs (view, attrs,args))
          val win_name = Attrs.getStringOpt (attrs Attrs.attr_title)
          val icon_name = Attrs.getStringOpt (attrs Attrs.attr_iconName)
          val pos = NONE (* FIX to lookup geometry *)
          val color = Attrs.getColorOpt (attrs Attrs.attr_background)
          val args = {win_name = win_name, icon_name = icon_name}
          in mk_shell simple pos (widget, color, args) end 

    end (* local *)

    (* following modified by ddeboer; original:
    fun init (Shell ch) = send (ch, Init)
    fun destroy (Shell ch) = send (ch, Destroy)
    fun unmap (Shell ch) = send(ch, Map false)
    fun map (Shell ch) = send(ch, Map true)
    fun setWMHints (Shell ch) arg = send (ch, Hints arg) *)
    fun init (Shell (ch,dch)) = send (ch, Init)
    fun destroy (Shell (ch,dch)) = send (ch, Destroy)
    fun unmap (Shell (ch,dch)) = send(ch, Map false)
    fun map (Shell (ch,dch)) = send(ch, Map true)
    fun setWMHints (Shell (ch,dch)) arg = send (ch, Hints arg)
    fun deleteEvent (Shell (ch,dch)) = (recvEvt dch)
    (* end modified *)
    
    end (* local *)
  end (* Shell *)
