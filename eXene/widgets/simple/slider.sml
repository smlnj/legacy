(* slider.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Analogue slider.
 *)

signature SLIDER = 
  sig

    structure CML : CML
    structure W : WIDGET

    type slider
    type range = {from : int, to : int}

    val slider : (W.root * W.view * W.arg list) -> slider

    val widgetOf : slider -> W.widget
    val evtOf : slider -> int CML.event
    val setValue : slider -> int -> unit
    val getValue : slider -> int
    val getRange : slider -> range
    val setActive : (slider * bool) -> unit
    val getActive : slider -> bool

  end (* SLIDER *)

structure Slider : SLIDER = 
  struct

    structure CML = CML
    structure W = Widget
    structure SV = SliderView

    fun error (f,msg) = LibBase.failure{module="Slider",func=f,msg=msg}

    type range = {from : int, to : int}

    open Geometry

    datatype mse_msg = 
      Grab of point | Move of point | Ungrab of point | MseIn of bool

    type slider_rep = {
      curx : int,
      curv : int
    }

            (* mouse reader *)
(*
    fun mseP (mseChan,m) = let
          open Interact
          fun downLoop () =
                case msgBodyOf (CML.sync m) of 
                  MOUSE_LastUp {pt,...} => CML.send (mseChan, Ungrab pt)
                | MOUSE_Motion {pt,...} => 
                    (CML.send(mseChan, Move pt);downLoop ())
                | MOUSE_Leave _ => (CML.send (mseChan, MseIn false);downLoop ())
                | MOUSE_Enter _ => (CML.send (mseChan, MseIn true);downLoop ())
                | _ => downLoop () 

          fun loop () = (
                case msgBodyOf (CML.sync m) of 
                  MOUSE_FirstDown {pt,...} => (
                    CML.send (mseChan, Grab pt);
                    downLoop ()
                  )
                | MOUSE_Enter _ => CML.send(mseChan, MseIn true)
                | MOUSE_Leave _ => CML.send(mseChan, MseIn false)
                | _ => ();
                loop ())
          in
            loop ()
          end
*)
    fun mseP (mseChan,m) = let
          open Interact CML
          val timeOut = timeOutEvt(Time.fromMilliseconds 30)
          val filterCnt = 5

          fun motionLoop (pt,0) = (send (mseChan,Move pt);downLoop())
            | motionLoop (pt,cnt) =
                select [
                  wrap(timeOut,fn () => (send(mseChan,Move pt);downLoop())),
                  wrap(m, fn evt =>
                    case msgBodyOf evt of 
                      MOUSE_LastUp {pt,...} => send (mseChan, Ungrab pt)
                    | MOUSE_Motion {pt,...} => motionLoop (pt,cnt-1)
                    | MOUSE_Leave _ => 
                        (send (mseChan, MseIn false);motionLoop (pt,cnt))
                    | MOUSE_Enter _ => 
                        (send (mseChan, MseIn true);motionLoop (pt,cnt))
                    | _ => motionLoop (pt,cnt))
                ]
          and downLoop () =
                case msgBodyOf (sync m) of 
                  MOUSE_LastUp {pt,...} => send (mseChan, Ungrab pt)
                | MOUSE_Motion {pt,...} => motionLoop (pt,filterCnt)
                | MOUSE_Leave _ => (send (mseChan, MseIn false);downLoop ())
                | MOUSE_Enter _ => (send (mseChan, MseIn true);downLoop ())
                | _ => downLoop () 

          fun loop () = (
                case msgBodyOf (sync m) of 
                  MOUSE_FirstDown {pt,...} => (
                    send (mseChan, Grab pt);
                    downLoop ()
                  )
                | MOUSE_Enter _ => send(mseChan, MseIn true)
                | MOUSE_Leave _ => send(mseChan, MseIn false)
                | _ => ();
                loop ())
          in
            loop ()
          end

    datatype reply = Okay | Error

    datatype rqst = 
      SetValue of (int * reply SyncVar.ivar)
    | GetValue of int SyncVar.ivar
    | GetRange of range SyncVar.ivar
    | GetActive of bool SyncVar.ivar
    | SetActive of bool
    | DoRealize of { env : Interact.in_env, win : W.EXB.window, sz : size }

    datatype slider = Slider of {
        widget : W.widget,
        rqstChan : rqst CML.chan,
        evt : int CML.event
      }

    fun buffer (inevt, outchan) = let
          fun loop ([],[]) = loop([],[CML.sync inevt])
            | loop (q,[]) = loop([],rev q)
            | loop (q,q' as (m::rest)) = 
                CML.select [
                  CML.wrap(inevt, fn msg => loop(msg::q,q')),
                  CML.wrap(CML.sendEvt(outchan,m), fn () => loop(q,rest))
                ]
          in loop ([],[]) end

    fun okayVal({fromV,toV,...} : SV.res, v) =
          if fromV <= toV then (fromV <= v) andalso (v <= toV)
          else (fromV >= v) andalso (v >= toV)

    fun realize ({env,win,sz},res,active,v,rqstChan,valChan) = let
          open CML Interact
          val mseChan = channel ()
          val rqstChan' = channel ()
          val rqstevt = recvEvt rqstChan'
          val mevt = recvEvt mseChan
          val InEnv{m,ci,...} = ignoreKey env
          val state = (v,active,false,false)

          fun config (state,sz) = let
                val drawf = SV.drawf(win,sz,res)
                val ptToVal = SV.ptToVal(sz,res)

                fun handleCI (CI_Redraw _, me) =
                      (drawf (me,true); me)
                  | handleCI (CI_Resize (RECT{wid,ht,...}), me) = 
                      config(me,SIZE{wid=wid,ht=ht})
                  | handleCI (_,me) = me
  
                fun handleReq (SetValue (v',ans),state as (v,a,r,d)) =
                      if okayVal(res,v') then (
                        SyncVar.iPut(ans,Okay);
                        if v = v' then NONE
                        else (send(valChan,v'); SOME(v',a,r,d)))
                      else (SyncVar.iPut(ans,Error);NONE)
                  | handleReq (GetValue ans,state) = (SyncVar.iPut(ans,#1 state); NONE)
                  | handleReq (GetRange ans,_) =
                      (SyncVar.iPut(ans,{from= #fromV res,to= #toV res});NONE)
                  | handleReq (GetActive ans,state) = (SyncVar.iPut(ans,#2 state);NONE)
                  | handleReq (SetActive b',(v,b,r,d)) = 
                      if b = b' then NONE else SOME(v,b',r,d)
                  | handleReq (_,_) = NONE

                fun handleM (Grab pt, (v,_,r,_)) = let
                      val v' = (ptToVal pt) handle _ => v
                      val state = (v',true,r,true)
                      in 
                        drawf(state,false); 
                        if v <> v' then send(valChan, v') else ();
                        state 
                      end
                  | handleM (Move pt, (v,_,r,_)) = let
                      val v' = (ptToVal pt) handle _ => v
                      val state = (v',true,r,true)
                      in 
                        if v = v' then ()
                        else (drawf(state,false);send(valChan, v'));
                        state 
                      end
                  | handleM (Ungrab pt,(v,_,r,_)) = let
                      val v' = (ptToVal pt) handle _ => v
                      val state = (v',true,r,false)
                      in 
                        drawf(state,false);
                        if v <> v' then send(valChan, v') else ();
                        state 
                      end
                  | handleM (MseIn r', me as (v,_,r,d)) =
                      if r' = r then me
                      else let
                        val state' = (v,true,r',d)
                        in drawf(state',false); state' end
      
                fun activeP (me as (v,a,r,d)) =
                      select [
                        wrap(mevt, fn m => activeP(handleM(m,me))),
                        wrap(rqstevt, fn evt =>
                           case handleReq(evt,me) of
                             NONE => activeP me
                           | SOME me' => 
                               if #2 me' then (
                                 drawf (me',false); 
                                 activeP me'
                               )
                               else (
                                 drawf (me',true); 
                                 inactiveP me'
                               )),
                        wrap(ci,fn evt => activeP(handleCI(msgBodyOf evt,me)))
                      ]
                and inactiveP (me as (v,a,r,d)) =
                      select [
                        wrap(mevt, fn (MseIn r') => inactiveP(v,a,r',d)
                                    | _ => inactiveP me),
                        wrap(rqstevt, fn evt =>
                           case handleReq(evt,me) of
                             NONE => inactiveP me
                           | SOME me' => 
                               if #2 me' then (
                                 drawf (me',true); 
                                 activeP me'
                               )
                               else (
                                 drawf (me',false); 
                                 inactiveP me'
                               )),
                        wrap(ci,fn evt => inactiveP(handleCI(msgBodyOf evt,me)))
                      ]
                in
                  if #2 state then activeP state else inactiveP state
                end
          in
            spawn (fn () => buffer (recvEvt rqstChan,rqstChan'));
            spawn (fn () => mseP (mseChan,m));
            config (state,sz)
          end

    fun init (res,isActive,v,rqstChan,valChan) = let
          fun handleReq (SetValue (v,ans),state as (active,_)) =
                if okayVal(res,v) then (SyncVar.iPut(ans,Okay);(active,v))
                else (SyncVar.iPut(ans,Error);state)
            | handleReq (GetValue ans,state as (_,v)) =
                (SyncVar.iPut(ans,v); state)
            | handleReq (GetRange ans,state) =
                (SyncVar.iPut(ans,{from= #fromV res,to= #toV res});state)
            | handleReq (GetActive ans,state) =
                (SyncVar.iPut(ans,#1 state);state)
            | handleReq (SetActive b,(_,v)) = (b,v)
            | handleReq (DoRealize arg,(active,v)) = 
                (realize (arg,res,active,v,rqstChan,valChan);(active,v))
          fun loop state = loop(handleReq(CML.recv rqstChan,state))
          in loop (isActive,v) end

    fun getCurrent (NONE,res) = #fromV res
      | getCurrent (SOME v,res) =
          if okayVal(res,v) then v
          else error ("slider","current value out of range")

    val attrs = [
          (Attrs.attr_isActive,    Attrs.AT_Bool,    Attrs.AV_Bool true),
          (Attrs.attr_current,     Attrs.AT_Int,     Attrs.AV_NoValue)
        ]

    fun slider (root,view,args) = let
          open Attrs
          val attrs = W.findAttr (W.attrs(view, attrs@SV.attrs, args))
          val res = SV.getResources (root,attrs)
          val isActive = getBool (attrs attr_isActive)
          val v = getCurrent(getIntOpt(attrs attr_current),res)
          val valChan = CML.channel ()
          val rqstChan = CML.channel ()
          in 
            CML.spawn(fn () => init (res,isActive,v,rqstChan,valChan));
            Slider {
              widget = W.mkWidget {
                root=root,
                args= fn () => {background = SOME (#bg res)},
                boundsOf = SV.boundsOf res,
                realize = fn arg => CML.send(rqstChan,DoRealize arg)
              }, 
              rqstChan = rqstChan,
              evt = CML.recvEvt valChan
            }
          end

    fun widgetOf (Slider{widget,...}) = widget
    fun evtOf (Slider{evt,...}) = evt
    fun setValue (Slider{rqstChan,...}) v = let
          val ans = SyncVar.iVar ()
          in
            CML.send(rqstChan, SetValue (v,ans));
            case SyncVar.iGet ans of
              Okay => ()
            | Error => error("setValue","improper value")
          end
    fun get msg (Slider{rqstChan,...}) = let
          val v = SyncVar.iVar ()
          in
            CML.send(rqstChan, msg v);
            SyncVar.iGet v
          end
    val getRange = get GetRange
    val getValue = get GetValue
    val getActive = get GetActive
    fun setActive (Slider{rqstChan,...},b) = CML.send(rqstChan, SetActive b)

  end (* Slider *)
