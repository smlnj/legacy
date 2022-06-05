(* window.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure Window : WINDOW =
  struct

    structure A = StdAtoms

    open Geometry XProtTypes XWin Display DrawTypes

  (* set the value of a property *)
    fun setProperty (dpy, winId, name, value) =
      dpyRequest dpy (XRequest.encodeChangeProperty {
                win = winId, name = name, prop = value, mode = ReplaceProp
              });

  (* user-level window attributes *)
    datatype window_attr
      = WA_Background_None
      | WA_Background_ParentRelative
      | WA_Background_Pixmap of DrawTypes.pixmap
      | WA_Background_Tile of DrawTypes.tile
      | WA_Background_Color of ColorServer.color
      | WA_Border_CopyFromParent
      | WA_Border_Pixmap of DrawTypes.pixmap
      | WA_Border_Tile of DrawTypes.tile
      | WA_Border_Color of ColorServer.color
      | WA_BitGravity of XProtTypes.gravity
      | WA_WinGravity of XProtTypes.gravity
      | WA_Cursor_None
      | WA_Cursor of Cursor.cursor

  (* window configuration values *)
    datatype window_config
      = WC_Origin of point
      | WC_Size of size
      | WC_BorderWid of int
      | WC_StackMode of XProtTypes.stack_mode
      | WC_RelStackMode of (window * XProtTypes.stack_mode)

  (* extract the pixel from a color *)
    fun pixelOf (ColorServer.COLOR{pixel, ...}) = pixel

  (* map user-level window attributes to internal x-window attributes *)
    fun winAttrToXWinAttr (WA_Background_None) =
      XWV_BackgroundPixmap_None
      | winAttrToXWinAttr (WA_Background_ParentRelative) =
      XWV_BackgroundPixmap_ParentRelative
      | winAttrToXWinAttr (WA_Background_Pixmap(PM{id, ...})) =
      XWV_BackgroundPixmap id
      | winAttrToXWinAttr (WA_Background_Tile(TILE(PM{id, ...}))) = 
      XWV_BackgroundPixmap id
      | winAttrToXWinAttr (WA_Background_Color color) =
      XWV_BackgroundPixel(pixelOf color)
      | winAttrToXWinAttr (WA_Border_CopyFromParent) =
      XWV_BorderPixmap_CopyFromParent
      | winAttrToXWinAttr (WA_Border_Pixmap(PM{id, ...})) =
      XWV_BorderPixmap id
      | winAttrToXWinAttr (WA_Border_Tile(TILE(PM{id, ...}))) =
      XWV_BorderPixmap id
      | winAttrToXWinAttr (WA_Border_Color color) =
      XWV_BorderPixel(pixelOf color)
      | winAttrToXWinAttr (WA_BitGravity g) =
      XWV_BitGravity g
      | winAttrToXWinAttr (WA_WinGravity g) =
      XWV_WinGravity g
      | winAttrToXWinAttr (WA_Cursor_None) =
      XWV_Cursor_None
      | winAttrToXWinAttr (WA_Cursor(Cursor.CURSOR{id, ...})) =
      XWV_Cursor id

    val mapAttrs = List.map winAttrToXWinAttr

    val stdXEventMask = XEventTypes.maskOfXEvtList [
        XEventTypes.XEVT_KeyPress,
        XEventTypes.XEVT_KeyRelease,
        XEventTypes.XEVT_ButtonPress,
        XEventTypes.XEVT_ButtonRelease,
        XEventTypes.XEVT_PointerMotion,
            XEventTypes.XEVT_EnterWindow,
            XEventTypes.XEVT_LeaveWindow,
        XEventTypes.XEVT_Exposure,
        XEventTypes.XEVT_StructureNotify,
        XEventTypes.XEVT_SubstructureNotify,
        XEventTypes.XEVT_PropertyChange
      ]

    val popupXEventMask = XEventTypes.maskOfXEvtList [
        XEventTypes.XEVT_Exposure,
        XEventTypes.XEVT_StructureNotify,
        XEventTypes.XEVT_SubstructureNotify
      ]

    exception BadWindowGeometry

    fun chkGeom g = if Geometry.validGeom g then g else raise BadWindowGeometry

    fun createSimpleTopWin (scr as SCREEN{scr=scrinfo, dpy}) = let
      val SCR{xscr=XDisplay.XSCR{root, ...}, root_servers, ...} = scrinfo
      val SCRDEPTH{depth, ...} = root_servers
      val DPY{xdpy = XDisplay.XDPY{conn, nextXId, ...}, ...} = dpy
      val winId = nextXId()
      (* modified ddeboer Jul 2004: original: 
      val (inEnv, win) = TopLevelWin.mkTopLevelWinEnv (scr, root_servers, winId) *)
      val (inEnv, win, delCh) = TopLevelWin.mkTopLevelWinEnv (scr, root_servers, winId)
      fun createFn {geom, border, backgrnd} = (
        XWin.newXWin conn {
            id = winId,
            parent = root,
            in_only = SOME false,
            depth = depth,
            visual = NONE,
            geom = chkGeom geom,
            attrs = [
            XWin.XWV_BorderPixel(pixelOf border),
            XWin.XWV_BackgroundPixel(pixelOf backgrnd),
            XWin.XWV_EventMask stdXEventMask
              ]
          };
        (* modified ddeboer Jul 2004; original: *
        (win, inEnv)) *)
        (win, inEnv, delCh))
      in
        createFn
      end (* createSimpleTopWin *)

    fun createSimpleSubwin (WIN{id=parentId, scr, draw_cmd, scr_depth, ...}) = let
      val SCREEN{dpy=DPY{xdpy=XDisplay.XDPY{conn, nextXId, ...}, ...}, ...} = scr
      val winId = nextXId()
      val win = WIN{
          id = winId,
          scr = scr,
          draw_cmd = draw_cmd,
          scr_depth = scr_depth
        }
      val SCRDEPTH{depth, ...} = scr_depth
      fun createFn {geom, border, backgrnd} = let
        val borderPixel = (case border
               of NONE => XWin.XWV_BorderPixmap_CopyFromParent
            | (SOME c) => XWin.XWV_BorderPixel(pixelOf c)
              (* end case *))
        val backgroundPixel = (case backgrnd
               of NONE => XWin.XWV_BackgroundPixmap_ParentRelative
            | (SOME c) => XWin.XWV_BackgroundPixel(pixelOf c)
              (* end case *))
        in
          XWin.newXWin conn {
              id = winId,
              parent = parentId,
              in_only = SOME false,
              depth = depth,
              visual = NONE,
              geom = chkGeom geom,
              attrs = [
              borderPixel,
              backgroundPixel,
              XWin.XWV_EventMask stdXEventMask
            ]
            };
          win
        end
      in
        createFn
      end


  (* create a simple popup window  *)
    fun createSimplePopupWin (scrn as SCREEN{scr, dpy}) {geom, border, backgrnd} = let
      val SCR{xscr=XDisplay.XSCR{root, ...}, root_servers, ...} = scr
      val SCRDEPTH{depth, ...} = root_servers
      val DPY{xdpy = XDisplay.XDPY{conn, nextXId, ...}, ...} = dpy
      val winId = nextXId()
      (* modified ddeboer, Jul 2004; original: 
      val (inEnv, win) = TopLevelWin.mkTopLevelWinEnv(scrn, root_servers, winId) *)
      val (inEnv, win, delCh) = TopLevelWin.mkTopLevelWinEnv(scrn, root_servers, winId)
      in
        XWin.newXWin conn {
        id = winId,
        parent = root,
        in_only = SOME false,
        depth = depth,
        visual = NONE,
        geom = chkGeom geom,
        attrs = [
            XWin.XWV_OverrideRedirect true,
            XWin.XWV_SaveUnder true,
            XWin.XWV_BorderPixel(pixelOf border),
            XWin.XWV_BackgroundPixel(pixelOf backgrnd),
            XWin.XWV_EventMask popupXEventMask
          ]
          };
        (win, inEnv)
      end

  (* create a simple transient window *)
    fun createTransientWin propWin {geom, border, backgrnd} = let
      open XProps
          val WIN{id, scr=scrn as SCREEN{scr, dpy},...} = propWin
      val SCR{xscr=XDisplay.XSCR{root, ...}, root_servers, ...} = scr
      val SCRDEPTH{depth, ...} = root_servers
      val DPY{xdpy = XDisplay.XDPY{conn, nextXId, ...}, ...} = dpy
      val winId = nextXId()
      (* modified ddeboer, Jul 2004; original: 
      val (inEnv, win) = TopLevelWin.mkTopLevelWinEnv(scrn, root_servers, winId) *)
      val (inEnv, win, delCh) = TopLevelWin.mkTopLevelWinEnv(scrn, root_servers, winId)
      in
        XWin.newXWin conn {
        id = winId,
        parent = root,
        in_only = SOME false,
        depth = depth,
        visual = NONE,
        geom = chkGeom geom,
        attrs = [
            XWin.XWV_BorderPixel(pixelOf border),
            XWin.XWV_BackgroundPixel(pixelOf backgrnd),
            XWin.XWV_EventMask stdXEventMask
          ]
          };
        setProperty (
          dpy, winId, A.atom_WM_TRANSIENT_FOR, makeTransientHint propWin);
        (win, inEnv)
      end

    exception InputOnly

    fun createInputOnlyWin win (RECT{x, y, wid, ht}) = let
      val WIN{id=parentId, scr, scr_depth, draw_cmd, ...} = win
      val SCREEN{dpy=DPY{xdpy=XDisplay.XDPY{conn, nextXId, ...}, ...}, ...} = scr
      val winId = nextXId()
      fun drawCmd (arg as (DrawMaster.DMSG_Destroy _)) = draw_cmd arg
        | drawCmd _ = raise InputOnly
      val win = WIN{
          id = winId,
          scr = scr,
          draw_cmd = drawCmd,
          scr_depth = scr_depth
        }
      in
        XWin.newXWin conn {
        id = winId,
        parent = parentId,
        in_only = SOME true,
        depth = 0,
        visual = NONE,
        geom = chkGeom(WGEOM{pos=PT{x=x, y=y}, sz=SIZE{wid=wid, ht=ht}, border=0}),
        attrs = [XWin.XWV_EventMask stdXEventMask]
          };
        win
      end

  (* Set the standard window-manager properties of a top-level window *)
    fun setWMProperties win {
      win_name, icon_name, argv, size_hints, wm_hints, class_hints
    } = let
      open XProtTypes XAtoms XProps
      val WIN{id, scr=SCREEN{dpy, ...}, ...} = win
      fun putProp (name, value) = setProperty (dpy, id, name, value)
      fun putStrProp (_, NONE) = ()
        | putStrProp (atom, SOME s) = putProp (atom, makeStringProp s)
      in
        putStrProp (A.atom_WM_NAME, win_name);
        putStrProp (A.atom_WM_ICON_NAME, icon_name);
        putProp (A.atom_WM_NORMAL_HINTS, makeWMSizeHints size_hints);
        putProp (A.atom_WM_HINTS, makeWMHints wm_hints);
        case class_hints
         of SOME{res_name, res_class} =>
          putProp (A.atom_WM_CLASS,
            makeStringProp (String.concat[res_name, "\000", res_class]))
          | NONE => ()
        (* end case *);
        case argv
         of [] => ()
          | _ => putProp (A.atom_WM_COMMAND, makeCommandHints(argv))
        (* end case *)
      end

  (* Set the window-manager protocols for a window *)
    fun setWMProtocols win atoml = let
      open XProtTypes XProps
      val WIN{id, scr=SCREEN{dpy, ...}, ...} = win
      fun putProp n a = setProperty (dpy, id, n, makeAtomProp a)
          in
            case (XAtoms.lookupAtom dpy "WM_PROTOCOLS")
         of NONE => false
              | (SOME protocols_atom) => (app (putProp protocols_atom) atoml; true)
        (* end case *)
          end

  (* Map window configuration values to a value list *)
    fun doConfigVal arr = let
      fun upd (i, v) = Array.update(arr, i, SOME v)
      in
        fn (WC_Origin(PT{x, y})) => (
          upd(0, Word.fromInt x); upd(1, Word.fromInt y))
         | (WC_Size(SIZE{wid, ht})) => (
          upd(2, Word.fromInt wid); upd(3, Word.fromInt ht))
         | (WC_BorderWid wid) => upd(4, Word.fromInt wid)
         | (WC_StackMode mode) => (
          Array.update(arr, 5, NONE);
          upd(6, XCvtFuns.stackModeToWire mode))
         | (WC_RelStackMode(WIN{id=(XID x), ...}, mode)) => (
          upd(5, x); upd(6, XCvtFuns.stackModeToWire mode))
      end
    val doConfigVals = XCvtFuns.doValList 7 doConfigVal

    fun configureWin (WIN{id, scr=SCREEN{dpy, ...}, ...}) vals =
      dpyRequest dpy (XRequest.encodeConfigureWindow{
          win = id, vals = doConfigVals vals
        })

    fun moveWin win pt = configureWin win [WC_Origin pt]

    fun resizeWin win sz = configureWin win [WC_Size sz]

    fun moveAndResizeWin win (RECT{x, y, wid, ht}) = configureWin win [
        WC_Origin(PT{x=x, y=y}), WC_Size(SIZE{wid=wid, ht=ht})
      ]

  (* Map a window *)
    fun mapWin (WIN{id, scr=SCREEN{dpy, ...}, ...}) = (
      dpyRequest dpy (XRequest.encodeMapWindow{win=id});
      dpyFlushOut dpy)

  (* Unmap a window *)
    fun unmapWin (WIN{id, scr=SCREEN{dpy, ...}, ...}) = (
      dpyRequest dpy (XRequest.encodeUnmapWindow{win=id});
      dpyFlushOut dpy)

  (* Withdraw (unmap and notify window manager) a top-level window *)
    local 
      open XEventTypes
      val mask = maskOfXEvtList[XEVT_SubstructureNotify,
                                XEVT_SubstructureRedirect]
    in
    fun withdrawWin (WIN{id, scr=SCREEN{scr=SCR{xscr,...}, dpy}, ...}) = let
          val XDisplay.XSCR{root,...} = xscr
          in
        dpyRequest dpy (XSendEvent.encodeSendUnmapNotify
          {dst=SendEvtTo_Window root, propagate=false, evt_mask=mask, 
               event=root, window=id, from_configure=false});
        dpyFlushOut dpy
          end
    end (* local *)

  (* Destroy a window.  We do this via the draw-master, to avoid a race with any
   * pending draw requests on the window.
   *)
    fun destroyWin (WIN{id, draw_cmd, ...}) = 
      draw_cmd(DrawMaster.DMSG_Destroy(DrawMaster.DSTRY_Win id))

  (* map a point in the window's coordinate system to the screen's
   * coordinate system *)
    fun winPtToScrPt (WIN{id, scr, ...}) pt = let
      val SCREEN{dpy, scr=SCR{xscr=XDisplay.XSCR{root, ...}, ...}, ...} = scr
      val {dst_pt, ...} = XReply.decodeTranslateCoordsReply (
        CML.sync (dpyRequestReply dpy
          (XRequest.encodeTranslateCoords{
            src_win=id, dst_win=root, src_pt=pt
          })))
      in
        dst_pt
      end

  (* set the cursor of the window *)
    fun setCursor (WIN{id, scr, ...}) c = let
      val SCREEN{dpy=DPY{xdpy=XDisplay.XDPY{conn, ...}, ...}, ...} = scr
      val cur = (case c
         of NONE => XWV_Cursor_None
          | (SOME(Cursor.CURSOR{id, ...})) => XWV_Cursor id
        (* end case *))
      in
        XWin.changeXWinAttrs conn (id, [cur])
      end

  (* set the background color attribute of the window.  Note that this does
   * not have an immediate affect on the window's contents, but if it is done
   * before the window is mapped, the window will come up with the right color.
   *)
    fun setBackground (WIN{id, scr, ...}) color = let
      val SCREEN{dpy=DPY{xdpy=XDisplay.XDPY{conn, ...}, ...}, ...} = scr
      val color = (case color
         of NONE => XWV_BackgroundPixmap_ParentRelative
          | (SOME c) => XWV_BackgroundPixel(pixelOf c)
        (* end case *))
      in
        XWin.changeXWinAttrs conn (id, [color])
      end (* setBackground *)

  (* Set various window attributes *)
    fun changeWinAttrs (WIN{id, scr, ...}) = let
      val SCREEN{dpy=DPY{xdpy=XDisplay.XDPY{conn, ...}, ...}, ...} = scr
      val change = XWin.changeXWinAttrs conn
      in
        fn attrs => change (id, map winAttrToXWinAttr attrs)
      end (* changeWinAttrs *)

    fun screenOfWin (WIN{scr, ...}) = scr
    fun displayOfWin (WIN{scr=SCREEN{dpy, ...}, ...}) = dpy

    (* added ddeboer Jan 2005 *)
    (* grabKeyboard: we would like a reply of XProtTypes.GrabSuccess *)
    fun grabKeyboard (WIN{id,scr=SCREEN{dpy, ...}, ...}) = 0
        (* commented out, ddeboer, mar 2005 - this needs reworked.
        let val ans = 
            (XReply.decodeGrabKeyboardReply (CML.sync (Display.dpyRequestReply dpy 
                        (XRequest.encodeGrabKeyboard { 
                            win=id, * type XTy.XID *
                            owner_evts=false, 
                            ptr_mode=XProtTypes.AsynchronousGrab, 
                            kbd_mode=XProtTypes.AsynchronousGrab, 
                            time=XProtTypes.CurrentTime}))))
                handle XIo.LostReply => raise (MLXError.XERROR "[reply lost]")
                     | (XIo.ErrorReply err) =>
                        raise (MLXError.XERROR(XPrint.xerrorToString err))
        in (case (ans) of
            XProtTypes.GrabSuccess => 0
          | XProtTypes.AlreadyGrabbed => 1
          | XProtTypes.GrabInvalidTime => 2
          | XProtTypes.GrabNotViewable => 3
          | XProtTypes.GrabFrozen => 4)
        end *)
    fun ungrabKeyboard (WIN{id,scr=SCREEN{dpy, ...}, ...}) =
        let val ans = 
            ( (* XReply.decodeGrabKeyboardReply *) (CML.sync (Display.dpyRequestReply dpy 
                        (XRequest.encodeUngrabKeyboard { 
                            time=XProtTypes.CurrentTime}))))
                handle XIo.LostReply => raise (MLXError.XERROR "[reply lost]")
                     | (XIo.ErrorReply err) =>
                        raise (MLXError.XERROR(XPrint.xerrorToString err))
        in (* TODO: figure out what type of reply comes from an ungrab request, and decode it *)
            0
        end
    (* end added ddeboer *)
  end (* Window *)
