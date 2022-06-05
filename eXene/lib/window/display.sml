(* display.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure Display =
  struct
    local
      structure G = Geometry
      open XDisplay PenRep Keymap WinRegistry
      open GCServer FontServer ColorServer DrawMaster
    in

    datatype display = DPY of {
    xdpy : xdisplay,          (* *)
    screens : scr list,
    default_scr : scr,
    registry : registry,          (* to register top-level windows *)
    font_server : font_server,
    atom_server : AtomServer.atom_server,
    property_server : PropertyServer.property_server,
    selection_server : SelectionServer.selection_server,
    keymap : keymap               (* the keymap server *)
      }

    and scr_depth = SCRDEPTH of {   (* the GC-server and draw-master for a given *)
                        (* depth of a screen *)
    depth : int,    
    gc_server : gc_server,        (* the GC server for this screen *)
    draw_cmd : draw_msg -> unit   (* the draw-master connection for the root *)
                      (* window of the screen *)
      }

    and scr = SCR of {
    xscr : xscreen,
    color_server : color_server,      (* The color map server for this screen *)
    servers : scr_depth list,     (* the GC-servers for the allowed depths *)
                      (* of this screen *)
    root_servers : scr_depth
      }

    and screen = SCREEN of {      (* A screen handle for users *)
    dpy : display,
    scr : scr
      } 

  (* identity tests *)
    fun sameDisplay (
      DPY{xdpy=XDPY{conn=c1, ...}, ...}, DPY{xdpy=XDPY{conn=c2, ...}, ...}
    ) =
      XIo.sameConn(c1, c2)
    fun sameScreen (
      SCREEN{dpy=dpy1, scr=SCR{xscr=XSCR{id=id1, ...}, ...}},
      SCREEN{dpy=dpy2, scr=SCR{xscr=XSCR{id=id2, ...}, ...}}
    ) = ((id1 = id2) andalso sameDisplay(dpy1, dpy2))

    fun openDisplay (dpyName, auth) = let
      val (xdpy as XDPY{default_scr, screens, conn, nextXId, ...}) =
        openXDisplay {dpyName=dpyName, auth=auth}
      val keymap = Keymap.createKeymap xdpy
      val atomServer = AtomServer.mkServer xdpy
      val (propCh, propServer) = PropertyServer.mkServer (xdpy, atomServer)
      val (selCh, selServer) = SelectionServer.mkServer xdpy
      val registry = createRegistry {
          dpy= xdpy, keymap= keymap, propEvtCh= propCh, selEvtCh= selCh
        }
      fun mkScrInfo (xscr as XSCR{root, root_visual, visualdepths, ...}) = let
        fun mkScrDepth (depth, gcServer) = let
              val configCh = CML.channel()
              val _ = CML.spawn(fn () => CML.send(configCh, DM_FirstExpose))
              in
            SCRDEPTH{
                depth = depth,
                gc_server = gcServer,
                draw_cmd =
                  mkDM (CML.recvEvt configCh, gcServer, registry, conn)
              }
              end
        fun mkGCServers ([], l) = l
          | mkGCServers (vd :: r, l) = let
              val d = depthOfVisual vd
              fun mkServer () = let
                val xid = nextXId()
                in
                (* make a pixmap to serve as the witness drawable for
                 * the GC server *)
                  XIo.request conn (XRequest.encodeCreatePixmap{
                  pixmap = xid, drawable = root,
                  size = G.SIZE{wid=1, ht=1}, depth = d
                });
                  mkScrDepth (d, mkGCServer (xdpy, xid))
                end
              fun look [] = mkServer() :: l
            | look (SCRDEPTH{depth, ...} :: r) =
                if (d = depth) then l else look r
              in
            mkGCServers (r, look l)
              end
        val rootServers = mkScrDepth(
            depthOfVisual root_visual, mkGCServer(xdpy, root))
        val servers = mkGCServers(visualdepths, [rootServers])
        val servers = mkGCServers([XProtTypes.Depth 1], servers)
        in
          SCR{
              xscr = xscr,
              color_server = ColorServer.mkColorServer (xdpy, xscr),
              servers = servers,
              root_servers = rootServers
            }
        end
      val scrInfoList = map mkScrInfo screens
      in
        DPY{
        xdpy = xdpy,
        screens = scrInfoList,
        default_scr = List.nth(scrInfoList, default_scr),
        registry = registry,
        atom_server = atomServer,
        font_server = mkFontServer xdpy,
        property_server = propServer,
        selection_server = selServer,
        keymap = keymap
          }
      end

  (* X-server I/O *)
    local
      fun dpyIO f (DPY{xdpy=XDPY{conn, ...}, ...}) = f conn
    in
    val dpyRequest = dpyIO XIo.request
    val dpyRequestAndChk = dpyIO XIo.requestAndChk
    val dpyRequestReply = dpyIO XIo.requestReply
    val dpyRequestMultiReply = dpyIO XIo.requestMultiReply
    val dpyFlushOut = dpyIO XIo.flushOut
    end (* local *)

  (* close the display.
   * NOTE: there are probably other things that should go on here, such as notifying
   * the window registry.
   *)
    fun closeDisplay (DPY{xdpy, ...}) = XDisplay.closeDisplay xdpy

  (* return the maximum request size supported by the display *)
    fun maxReqLen (DPY{xdpy=XDPY{max_req_len, ...}, ...}) = max_req_len

  (* atom operations *)
    local
      fun wrapAtomOp f (DPY{atom_server, ...}) = f atom_server
    in
    val internAtom = wrapAtomOp AtomServer.internAtom
    val lookupAtom = wrapAtomOp AtomServer.lookupAtom
    val nameOfAtom = wrapAtomOp AtomServer.nameOfAtom
    end

  (* font operations *)
    fun openFont (DPY{font_server, ...}) = FontServer.openFont font_server

    fun defaultScreenOf (dpy as DPY{default_scr, ...}) =
      SCREEN{dpy = dpy, scr = default_scr}

    fun screensOf (dpy as DPY{screens, ...}) =
      map (fn s => SCREEN{dpy = dpy, scr = s}) screens

    fun ringBell dpy percent =
      dpyRequest dpy (XRequest.encodeBell{
          percent = Int.min(100, Int.max(~100, percent))
        })


  (** Screen functions **)

    fun colorOfScr (SCREEN{scr=SCR{color_server, ...}, ...}) =
      ColorServer.getColor color_server
    fun blackOfScr (SCREEN{scr=SCR{color_server, ...}, ...}) =
      ColorServer.blackOf color_server
    fun whiteOfScr (SCREEN{scr=SCR{color_server, ...}, ...}) =
      ColorServer.whiteOf color_server

    fun displayOfScr (SCREEN{dpy, ...}) = dpy

    (* Additions by ddeboer, May 2004. 
     * Dusty deBoer, KSU CIS 705, Spring 2004. *)
     
    (* return the root window of a screen.
     * needed in obtaining strings from xrdb, as they are stored in a
     * property of the root window.
     *)
    fun rootWinOfScr (SCREEN{scr=SCR{xscr=XSCR{root, ... } , ...}, ...}) = root
    
    (* end additions by ddeboer *)
    
    fun sizeOfScr (SCREEN{scr=SCR{xscr=XSCR{sz_in_pixels, ...}, ...}, ...}) =
      sz_in_pixels
    fun sizeMMOfScr (SCREEN{scr=SCR{xscr=XSCR{sz_in_mm, ...}, ...}, ...}) = sz_in_mm

    fun depthOfScr (SCREEN{scr=SCR{xscr=XSCR{root_visual, ...}, ...}, ...}) =
      depthOfVisual root_visual
    fun displayClassOfScr (SCREEN{scr=SCR{xscr=XSCR{root_visual, ...}, ...}, ...}) = (
      case (displayClassOfVisual root_visual)
       of (SOME c) => c
        | _ => MLXError.impossible "[Display.displayClassOfScr: bogus root visual]")

  (* extract the GC server for the given depth *)
    fun serversOfScrDepth (SCREEN{scr=SCR{servers, ...}, ...}, d) = let
      fun f [] = MLXError.xerror "invalid depth for screen"
        | f ((sd as SCRDEPTH{depth, ...}) :: r) =
          if (depth = d) then sd else (f r)
      in
        f servers
      end

    end (* local open ... *)
  end (* Display *)
