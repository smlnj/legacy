(* toggle-ctrl.sml
 *
 * COPYRIGHT (c) 1991,1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Protocol for toggles.
 *
 * TODO: Allow disabling of highlighting 
 *)

signature TOGGLE_CTRL = 
  sig

    structure W : WIDGET

    val toggle : (W.root * W.view * W.arg list) -> ToggleType.toggle

    val commandToggle : (W.root * W.view * W.arg list) 
          -> (bool -> unit) -> ToggleType.toggle

  end (* TOGGLE_CTRL *)

functor ToggleCtrl (BV : BUTTON_VIEW) : TOGGLE_CTRL = 
  struct

    structure W = Widget
    structure TT = ToggleType

    open CML Geometry ButtonBase

    val attrs = [
        (Attrs.attr_isActive,         Attrs.AT_Bool,  Attrs.AV_Bool true),
        (Attrs.attr_isSet,            Attrs.AT_Bool,  Attrs.AV_Bool false)
      ]

    fun realize {env=inenv, win, sz} (state,(reqc,evtc,bv)) = let
          open Interact
          val InEnv{m,ci,...} = ignoreKey inenv
          val mchan = channel ()
          val rcvm = recvEvt mchan
          val drawf = BV.config(bv,win,sz)

          fun handleReq (GetActive v,state) = 
                (SyncVar.iPut (v, getActive state); state)
            | handleReq (SetActive arg,state) = setActive (arg,state)
            | handleReq (GetBounds arg,state) = 
                (SyncVar.iPut(arg,BV.bounds bv); state)
            | handleReq (GetArgs arg,state) = 
                (SyncVar.iPut(arg,BV.win_args bv); state)
            | handleReq (GetState v,state) =
                (SyncVar.iPut (v, getState state); state)
            | handleReq (SetState arg, state) = setState (arg, state)
            | handleReq (_, state) = state
  
          fun handleCI (CI_Redraw _, me as (state,drawf)) = 
                (drawf state; me)
            | handleCI (CI_Resize (RECT{wid,ht,...}), (state,_)) = 
                (state, BV.config (bv,win,SIZE{wid=wid,ht=ht}))
            | handleCI (_,me) = me
  
          fun handleM (MseIn v,me as ((s,r,false),drawf)) =
                if v = r then me
                else let
                  val state' = (s,v,false)
                  in
                    drawf state';
                    send(evtc,if v then TT.ToggleReady else TT.ToggleNormal);
                    (state',drawf)
                  end
            | handleM (MseIn v,((s,_,true),drawf)) = let
                val state' = (s,v,true)
                in
                  drawf state';
                  (state',drawf)
                end
            | handleM (MseDown bttn,((s,_,_),drawf)) = let
                val state' = (s,true,true)
                in
                  drawf state';
                  (state',drawf)
                end
            | handleM (MseUp bttn,(state,drawf)) =
                  if #2 state
                    then let
                      val state' = (flip (#1 state),true,false)
                      in
                        drawf state';
                        send(evtc,TT.Toggle(getState state'));
                        (state',drawf)
                      end
                    else let
                      val state' = (#1 state, false, false)
                      in
                        drawf state';
                        send(evtc,TT.ToggleNormal);
                        (state',drawf)
                      end

          fun activeCmdP (me as (state,drawf)) =
                select [
                  wrap(recvEvt reqc, fn evt => let 
                    val state' = handleReq (evt,state) 
                    in
                      if state' = state then activeCmdP me
                      else (
                        drawf state'; 
                        if getActive state' 
                          then (
                            send(evtc,TT.Toggle(getState state'));
                            activeCmdP (state',drawf)
                          )
                          else (
                            if #2 state orelse #3 state then send(evtc,TT.ToggleNormal) else ();
                            inactiveCmdP (state',drawf)
                          )
                      )
                    end),
                  wrap(rcvm, fn m => activeCmdP(handleM(m,me))),
                  wrap(ci, fn evt => activeCmdP(handleCI (msgBodyOf evt,me)))
                ]

          and inactiveCmdP (me as (state,drawf)) =
                select [
                  wrap(recvEvt reqc, fn evt => let 
                    val state' = handleReq (evt,state) 
                    in
                      if state = state' then inactiveCmdP me
                      else (
                        drawf state';
                        if getActive state' 
                          then activeCmdP (state',drawf)
                          else (
                            send(evtc,TT.Toggle(getState state'));
                            inactiveCmdP (state',drawf)
                          )
                      )
                    end),
                  wrap(rcvm, fn (MseIn v) => inactiveCmdP ((#1 state,v, #3 state),drawf)
                              | _ => inactiveCmdP me),
                  wrap(ci, fn evt => inactiveCmdP(handleCI (msgBodyOf evt,me)))
                ]
          in
            spawn (fn () => mseP(m,mchan));
            if getActive state then activeCmdP(state,drawf)
            else inactiveCmdP(state,drawf)
          end

    fun init (env as (reqc,evtc,bv)) state = let
          fun loop state =
                case recv reqc of
                  GetActive v => (SyncVar.iPut (v, getActive state); loop state)
                | SetActive arg => loop (setActive (arg,state))
                | GetState v => (SyncVar.iPut (v, getState state); loop state)
                | SetState arg => loop(setState (arg, state))
                | DoRealize arg => realize arg (state,env)
                | GetBounds arg => (SyncVar.iPut(arg,BV.bounds bv); loop state)
                | GetArgs arg => (SyncVar.iPut(arg,BV.win_args bv); loop state)
          in loop state end

    fun toggle (root,view,args) = let
          open Attrs
          val attrs = W.findAttr(W.attrs(view,attrs,args))
          val reqc = channel ()
          val evtc = channel ()
          val state = mkWState(getBool(attrs attr_isActive),
                               getBool(attrs attr_isSet))
          val bv = BV.buttonView (root,view,args)
          fun getval msg () = let
                val v = SyncVar.iVar ()
                in send (reqc,msg v); SyncVar.iGet v end
          in
            spawn (fn () => init (reqc,evtc,bv) (state,false,false));
            TT.TOGGLE {
              widget = Widget.mkWidget{
                root=root,
                args = getval GetArgs,
                boundsOf = getval GetBounds,
                realize = fn arg => send(reqc,DoRealize arg)
              },
              evt = recvEvt evtc,
              rqst = reqc
            }
          end

    fun commandToggle (root,view,args) action = let
          val TT.TOGGLE{widget,rqst,evt} = toggle (root,view,args)
          fun listener () =
            listener (case sync evt of
              TT.Toggle b => action b
            | _ => ()
            )
          in
            spawn listener;
            TT.TOGGLE {
              widget = widget,
              rqst = rqst,
              evt = SyncVar.iGetEvt(SyncVar.iVar())
            }
          end
  
  end (* ToggleCtrl *)
