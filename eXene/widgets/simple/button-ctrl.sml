(* button-ctrl.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Protocol for buttons.
 *
 * TODO: Allow disabling of highlighting 
 *)

signature BUTTON_CTRL = 
  sig

    structure W : WIDGET

    val button : (W.root * W.view * W.arg list) -> ButtonType.button

    val commandBtn : (W.root * W.view * W.arg list) -> 
          (unit -> unit) -> ButtonType.button

  end (* BUTTON_CTRL *)

functor ButtonCtrl (BV : BUTTON_VIEW) : BUTTON_CTRL = 
  struct

    structure W = Widget
    structure BT = ButtonType

    open CML Geometry ButtonBase
  
    val attrs = [
        (Attrs.attr_repeatDelay,      Attrs.AT_Int,   Attrs.AV_NoValue),
        (Attrs.attr_repeatInterval,   Attrs.AT_Int,   Attrs.AV_Int 100),
        (Attrs.attr_isActive,         Attrs.AT_Bool,  Attrs.AV_Bool true),
        (Attrs.attr_isSet,            Attrs.AT_Bool,  Attrs.AV_Bool false)
      ]

    fun timerP (bttn, outch, inch, delay, interval) () = let
          fun signal () =
                select[
                  wrap(sendEvt(outch,BT.BtnDown bttn), fn () => wait(timeOutEvt interval)),
                  wrap(recvEvt inch, exit)
                ]
          and wait (timeEvt) =
                select[
                  wrap(timeEvt, signal),
                  wrap(recvEvt inch, exit)
                ]
          in wait(timeOutEvt delay) end

    fun realize {env=inenv, win, sz} (state,(quanta,reqc,
        (* next line type added ddeboer: *)
        evtc: ButtonType.button_act CML.chan,
        bv)) = let
          open Interact
          val InEnv{m,ci,...} = ignoreKey inenv
          val mchan = channel ()
          val timec = channel ()
          val rcvm = recvEvt mchan
          val drawf = BV.config(bv,win,sz)
          val q = (case quanta of 
                    NONE => NONE
                  | SOME(d,i) => SOME(d,i,channel()))

          fun handleReq (GetActive v,state) = 
                (SyncVar.iPut (v, getActive state); state)
            | handleReq (SetActive arg,state) = setActive (arg,state)
            | handleReq (GetBounds arg,state) = 
                (SyncVar.iPut(arg,BV.bounds bv); state)
            | handleReq (GetArgs arg,state) = 
                (SyncVar.iPut(arg,BV.win_args bv); state)
            | handleReq (_,state) = state
  
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
                    send(evtc,if v then BT.BtnReady else BT.BtnNormal);
                    (state',drawf)
                  end
            | handleM (MseIn v,((s,r,true),drawf)) = let
                val state' = (s,v,true)
                in
                  drawf state';
                  (state',drawf)
                end
            | handleM (MseDown bttn,((s,r,isdown),drawf)) = let
                val state' = (s,true,true)
                in
                  drawf state';
                  send(evtc,BT.BtnDown bttn);
                  case q of
                    NONE => ()
                  | SOME(d,i,tc) => 
                      (spawn(timerP(bttn,timec,tc,d,i)); ());
                  (state',drawf)
                end
            | handleM (MseUp bttn,((s,isin,isdown),drawf)) = let
                val state' = (s,isin,false)
                in
                  drawf state';
                  send(evtc,if isin then BT.BtnUp bttn else BT.BtnNormal);
                  case q of
                    NONE => ()
                  | SOME(_,_,tc) => send(tc,());
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
                        if #2 state' orelse #3 state' then send(evtc,BT.BtnNormal) else ();
                        inactiveCmdP (state',drawf)
                      )
                    end),
                  wrap(rcvm, fn m => activeCmdP(handleM(m,me))),
                  wrap(recvEvt timec, fn m => (send(evtc,m);activeCmdP me)),
                  wrap(ci, fn evt => activeCmdP(handleCI (msgBodyOf evt,me)))
                ]

          and inactiveCmdP (me as (state,drawf)) =
                select [
                  wrap(recvEvt reqc, fn evt => let 
                    val state' = handleReq (evt,state) 
                    in
                      if state' = state then inactiveCmdP me
                      else (
                        drawf state';
                        if #2 state' then send(evtc,BT.BtnReady) else ();
                        activeCmdP (state',drawf)
                      )
                    end),
                  wrap(rcvm, fn (MseIn v) => inactiveCmdP ((#1 state,v,#3 state),drawf)
                              | _ => inactiveCmdP me),
                  wrap(ci, fn evt => inactiveCmdP(handleCI (msgBodyOf evt,me)))
                ]
          in
            spawn (fn () => mseP(m,mchan));
            if getActive state then activeCmdP(state,drawf)
            else inactiveCmdP(state,drawf)
          end

    fun init (env as (quanta,reqc,evtc,bv)) state = let
          fun loop state =
                case recv reqc of
                  GetActive v => (SyncVar.iPut (v, getActive state); loop state)
                | SetActive arg => loop (setActive (arg,state))
                | DoRealize arg => realize arg (state,env)
                | GetBounds arg => (SyncVar.iPut(arg,BV.bounds bv); loop state)
                | GetArgs arg => (SyncVar.iPut(arg,BV.win_args bv); loop state)
                | _ => loop state
          in loop state end

    fun button (root,view,args) = let
          open Attrs
          val attrs = W.findAttr(W.attrs(view,attrs,args))
          val evtc = channel ()
          val reqc = channel ()
          val quanta = (case getIntOpt(attrs attr_repeatDelay)
        of NONE => NONE
         | SOME d => let
                    val i = getInt(attrs attr_repeatInterval)
            val millisecs = Time.fromMilliseconds o Int.toLarge
                    in
              SOME(millisecs d, millisecs i)
            end
           (* end case *))
          val state = mkWState(getBool(attrs attr_isActive),
                               getBool(attrs attr_isSet))
          val bv = BV.buttonView (root,view,args)
          fun getval msg () = let
                val v = SyncVar.iVar ()
                in send (reqc,msg v); SyncVar.iGet v end
          in
            spawn (fn () => init (quanta,reqc,evtc,bv) (state,false,false));
            BT.Button {
              widget = Widget.mkWidget{
                root=root,
                args = getval GetArgs,
                boundsOf = getval GetBounds,
                realize = fn arg => send(reqc,DoRealize arg)
              },
              rqst = reqc,
              (* modified by ddeboer; original: *)
              (* evt = recvEvt evtc *)
              evt = (*(WidgetBase.wrapQueue )*)recvEvt evtc(*))*)
            }
          end

    fun commandBtn args action = let
          val BT.Button{widget,rqst,evt} = button args
          fun listener () =
            listener (case sync evt of
              BT.BtnUp btn => action ()
            | _ => ()
            )
          in
            spawn listener;
            BT.Button {
              widget = widget,
              rqst = rqst,
              evt = SyncVar.iGetEvt(SyncVar.iVar())
            }
          end
  
  end (* ButtonCtrl *)
