(* toplevel-win.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This is the code for the root thread of a window heirarchy.  A top-level
 * window thread gets a stream of X-events from the window registry.
 *)

structure TopLevelWin : sig

    val mkTopLevelWinEnv :
      (Display.screen * Display.scr_depth * XProtTypes.win_id)
      (* modified ddeboer, Jul 2004; original: 
        -> (WindowEnv.in_env * DrawTypes.window) *)
        -> (WindowEnv.in_env * DrawTypes.window * unit CML.chan)

  end = struct

    open CML XEventTypes XDisplay DrawTypes WindowEnv Display

  (* The top-level window (usually a shell widget) should never pass on CO message *)
    fun mkCOThread coEvt = (
      spawn (fn () => (
        sync coEvt; MLXError.impossible("[TopLevelWin: unexpected CO message]"))))

    fun mkRouter (DPY{keymap, ...}, xevtEvt, dmConfigCh, topWin) = let
      val mkDescendantWin = let
        val WIN{scr, scr_depth, draw_cmd, ...} = topWin
        in
          fn id => WIN{id=id, scr=scr, scr_depth=scr_depth, draw_cmd=draw_cmd}
        end
      val (inEnv, outEnv) = WindowEnv.createWinEnv()
      val (routeCI, routeK, routeM) = let
        val OutEnv{ci, k, m, co} = outEnv
        in
          mkCOThread co;
          (ci, k, m)
        end
      val lookupKey = Keymap.lookupKeysym keymap
      local val seqn = ref 0 in
      fun wrapMsg (path, msg) = let val n = !seqn
        in
          seqn := n+1;
          AMSG{path=path, seqn=n, msg=msg}
        end
      end
      (* added ddeboer: add a channel by which client message events may be communicated
       * to the application. *)
      val delCh = CML.channel()
      (* end added ddeboer *)
      fun doKey (mkMsg, keyEvt) = routeK (mkMsg (lookupKey keyEvt))
      fun doDownBut (path, info : button_xevtinfo) = let
        val {button, event_pt, root_pt, time, mbut_state, ...} = info
        val msg = if (KeyBut.mbutAllClr mbut_state)
              then MOUSE_FirstDown{
              but = button,
              pt = event_pt,
              scr_pt = root_pt,
              time = time
            }
              else MOUSE_Down{
              but = button,
              pt = event_pt,
              scr_pt = root_pt,
                (* invert button so that the state is post-transition *)
              state = KeyBut.invertMBut(mbut_state, button),
              time = time
            }
        in
          routeM (wrapMsg (path, msg))
        end
      fun doUpBut (path, info : button_xevtinfo) = let
        val {button, event_pt, root_pt, time, mbut_state, ...} = info
        val state = KeyBut.invertMBut(mbut_state, button)
        val msg = if (KeyBut.mbutAllClr state)
              then MOUSE_LastUp{
              but = button,
              pt = event_pt,
              scr_pt = root_pt,
              time = time
            }
              else MOUSE_Up{
              but = button,
              pt = event_pt,
              scr_pt = root_pt,
              state = state,
              time = time
            }
        in
          routeM (wrapMsg (path, msg))
        end
      val alwaysUnit = alwaysEvt()
      fun doConfigSync (path, configMsg) = wrap (alwaysUnit, fn () => (
        sync (routeM (wrapMsg (path, MOUSE_ConfigSync)));
        sync (routeK (wrapMsg (path, KEY_ConfigSync)));
        sync (routeCI (wrapMsg (path, configMsg)))))
      fun routeXEvt (path, KeyPressXEvt arg) =
        doKey (fn x => wrapMsg (path, KEY_Press x), arg)
        | routeXEvt (path, KeyReleaseXEvt arg) =
        doKey (fn x => wrapMsg (path, KEY_Release x), arg)
        | routeXEvt (path, ButtonPressXEvt arg) = doDownBut (path, arg)
        | routeXEvt (path, ButtonReleaseXEvt arg) = doUpBut (path, arg)
        | routeXEvt (path, MotionNotifyXEvt{event_pt, root_pt, time, ...}) =
        routeM (wrapMsg (path,
          MOUSE_Motion{pt=event_pt, scr_pt=root_pt, time=time}))
        | routeXEvt (path, EnterNotifyXEvt{event_pt, root_pt, time, ...}) =
        routeM (wrapMsg (path,
          MOUSE_Enter{pt=event_pt, scr_pt=root_pt, time=time}))
        | routeXEvt (path, LeaveNotifyXEvt{event_pt, root_pt, time, ...}) =
        routeM (wrapMsg (path,
          MOUSE_Leave{pt=event_pt, scr_pt=root_pt, time=time}))
(*******
        | routeXEvt (_, FocusInXEvt{...}) = ()
        | routeXEvt (_, FocusOutXEvt{...}) = ()
        | routeXEvt (_, KeymapNotifyXEvt{...}) = ()
******)
        | routeXEvt (path, ExposeXEvt{rects, ...}) =
        routeCI(wrapMsg (path, CI_Redraw rects))
(*******
        | routeXEvt (_, GraphicsExposeXEvt{...}) = ()
        | routeXEvt (_, NoExposeXEvt{...}) = ()
        | routeXEvt (_, VisibilityNotifyXEvt _) = ()
******)
        | routeXEvt (path, CreateNotifyXEvt{parent, window, ...}) =
        doConfigSync (path, CI_ChildBirth(mkDescendantWin window))
        | routeXEvt (path, DestroyNotifyXEvt{window, event, ...}) =
        if (window = event)
          then routeCI (wrapMsg (path, CI_OwnDeath))
          else doConfigSync (path, CI_ChildDeath(mkDescendantWin window))
        | routeXEvt (WinRegistry.PathDst _, UnmapNotifyXEvt _) =
        wrap (alwaysUnit, fn () => send (dmConfigCh, DrawMaster.DM_Unmap))
        | routeXEvt (_, UnmapNotifyXEvt _) = alwaysUnit
        | routeXEvt (WinRegistry.PathDst _, MapNotifyXEvt _) =
        wrap (alwaysUnit, fn () => send (dmConfigCh, DrawMaster.DM_Map))
        | routeXEvt (_, MapNotifyXEvt _) = alwaysUnit
(*******
        | routeXEvt (_, MapRequestXEvt{...}) = ()
        | routeXEvt (_, ReparentNotifyXEvt{...}) = ()
******)
        | routeXEvt (path, ConfigureNotifyXEvt{rect, ...}) =
        routeCI (wrapMsg (path, CI_Resize rect))
(*******
        | routeXEvt (_, ConfigureRequestXEvt{...}) = ()
        | routeXEvt (_, GravityNotifyXEvt{...}) = ()
        | routeXEvt (_, ResizeRequestXEvt{...}) = ()
        | routeXEvt (_, CirculateNotifyXEvt{...}) = ()
        | routeXEvt (_, CirculateRequestXEvt{...}) = ()
        | routeXEvt (_, PropertyNotifyXEvt{...}) = ()
        | routeXEvt (_, SelectionClearXEvt{...}) = ()
        | routeXEvt (_, SelectionRequestXEvt{...}) = ()
        | routeXEvt (_, SelectionNotifyXEvt{...}) = ()
        | routeXEvt (_, ColormapNotifyXEvt{...}) = ()
******)
(****** modification, ddeboer, Jul 2004: route this event when delete. 
from ..protocol/xevttypes.sml:
... ClientMessageXEvt of {
        window : win_id,        
        typ : atom,         the type of the message
        value : raw_data        the message value
      }
*)
        | routeXEvt (_, ClientMessageXEvt{window,typ,...}) = 
            wrap (alwaysUnit, fn () => 
                (* (if typ=(ICCC.internAtom dpy "WM_DELETE_WINDOW") 
                 then *) (CML.send (delCh, ())) (* else () )*) )
(** end mod ****)
        | routeXEvt (_, evt) = wrap (alwaysUnit, fn () => (
        XDebug.trace(XDebug.topTM, fn () => [
            "[TopLvl.routeXEvt: unexpected event ", (XPrint.xevtName evt), "]\n"
          ])))
(* +DEBUG *)
      fun debugRouter (res as (_, xevt)) = (
        XDebug.trace(XDebug.topTM, fn () => [
            "TopLevelWin.router: get ", XPrint.xevtName xevt, "\n"
          ]);
        res)
(* -DEBUG *)
      fun router ([], []) = router ([debugRouter(sync xevtEvt)], [])
        | router ([], l) = router (rev l, [])
        | router (front as (msgOut::r), rear) = select [
          wrap (xevtEvt, fn res => router(front, (debugRouter res)::rear)),
          wrap (routeXEvt msgOut, fn () => router(r, rear))
        ]
      in
      (* modified ddeboer; Jul 2004: original: *
        (inEnv, fn pending => router (pending, [])) *)
        (inEnv, (fn pending => router (pending, [])), delCh)
      end (* mkRouter *)


  (* create the X-event-router and draw-master servers for a top-level window,
   * returning the input environment and top-level window.
   *)
    fun mkTopLevelWinEnv (
          scr as SCREEN{dpy, ...}, scr_depth as SCRDEPTH{gc_server, ...}, winId
    ) = let
      val DPY{xdpy as XDPY{conn, ...}, registry, ...} = dpy
      val dmConfigCh = channel()
      val dm = DrawMaster.mkDM (recvEvt dmConfigCh, gc_server, registry, conn)
      val xevtEvt = WinRegistry.logTopWin(registry, winId)
      val topWin = WIN{id=winId, scr=scr, scr_depth=scr_depth, draw_cmd=dm}
      (* modified ddeboer, Jul 2004; original: *
      val (inEnv, router) = mkRouter (dpy, xevtEvt, dmConfigCh, topWin) *)
      val (inEnv, router, delCh) = mkRouter (dpy, xevtEvt, dmConfigCh, topWin)
      fun initRouter () = let
        fun loop buf = (case sync xevtEvt
             of (arg as (_, ExposeXEvt _)) => (
(* DEBUG *) XDebug.trace(XDebug.topTM, fn () => ["initRouter: ExposeEvt\n"]);
              send(dmConfigCh, DrawMaster.DM_FirstExpose);
(* DEBUG *) XDebug.trace(XDebug.topTM, fn () => ["initRouter: DM_FirstExpose sent\n"]);
              (arg :: buf))
              | arg => loop(arg::buf))
        in
(* DEBUG *) XDebug.trace(XDebug.topTM, fn () => ["initRouter: winId = ", XPrint.xidToString winId, "\n"]);
          router (rev (loop []));
(* DEBUG *) XDebug.trace(XDebug.topTM, fn () => ["initRouter: go\n"])
        end
      in
        XDebug.xspawn("TopLevelWin.router", initRouter);
        (* modified ddeboer; Jul 2004: original: *
        (inEnv, topWin) *)
        (inEnv, topWin, delCh)
      end (* mkTopLevelWinEnv *)

  end (* TopLevelWin *)
