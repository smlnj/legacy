(* xdisplay.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure XDisplay : sig

    exception BadAddr of string

    datatype xdisplay = XDPY of {
    conn : XIo.connection,      (* the connection to the server *)
    name : string,          (* "host:display.scr" *)
    vendor : string,        (* the name of the server's vendor *)
    default_scr : int,      (* the number of the default screen *)
    screens : xscreen list,     (* the screens attached to this display. *)
    pixmap_formats : XProtTypes.pixmap_format list,
    max_req_len : int,
    image_byte_order : XProtTypes.order,
    bitmap_bit_order : XProtTypes.order,
    bitmap_scanline_unit : XProtTypes.raw_format,
    bitmap_scanline_pad : XProtTypes.raw_format,
    min_keycode : XProtTypes.keycode,
    max_keycode : XProtTypes.keycode,
    nextXId : unit -> XProtTypes.xid (* resource id allocator *)
      }

    and xscreen = XSCR of {
    id : int,           (* the number of this screen *)
    root : XProtTypes.win_id,   (* the root window id of this screen *)
    cmap : XProtTypes.colormap_id,  (* the default colormap *)
    white : XProtTypes.pixel,   (* White and Black pixel values *)
    black : XProtTypes.pixel,
    root_input_mask : XProtTypes.event_mask,
                    (* initial root input mask *)
    sz_in_pixels : Geometry.size,   (* the width and height in pixels *)
    sz_in_mm : Geometry.size,   (* the width and height in millimeters *)
    min_installed_cmaps : int,
    max_installed_cmaps : int,
    root_visual : XProtTypes.visual_depth,
    backing_store : XProtTypes.backing_store,
    save_unders : bool,
    visualdepths : XProtTypes.visual_depth list
      }

    val openXDisplay : {
        dpyName : string,
        auth : XProtTypes.authentication option
      } -> xdisplay

    val closeDisplay : xdisplay -> unit
    val depthOfVisual : XProtTypes.visual_depth -> int
    val displayClassOfVisual : XProtTypes.visual_depth
      -> XProtTypes.display_class option

  end = struct

    exception BadAddr = XServerAddr.BadAddr

    structure NDB = NetHostDB
    structure W8V = Word8Vector
    structure G = Geometry
    structure XTy = XProtTypes

    structure XD = XDebug

    datatype xdisplay = XDPY of {
    conn : XIo.connection,      (* the connection to the server *)
    name : string,          (* "host:display.scr" *)
    vendor : string,        (* the name of the server's vendor *)
    default_scr : int,      (* the number of the default screen *)
    screens : xscreen list,     (* the screens attached to this display. *)
    pixmap_formats : XTy.pixmap_format list,
    max_req_len : int,
    image_byte_order : XTy.order,
    bitmap_bit_order : XTy.order,
    bitmap_scanline_unit : XTy.raw_format,
    bitmap_scanline_pad : XTy.raw_format,
    min_keycode : XTy.keycode,
    max_keycode : XTy.keycode,
    nextXId : unit -> XTy.xid   (* resource id allocator *)
      }

    and xscreen = XSCR of {
    id : int,           (* the number of this screen *)
    root : XTy.win_id,      (* the root window id of this screen *)
    cmap : XTy.colormap_id,     (* the default colormap *)
    white : XTy.pixel,      (* White and Black pixel values *)
    black : XTy.pixel,
    root_input_mask : XTy.event_mask, (* initial root input mask *)
    sz_in_pixels : G.size,      (* the width and height in pixels *)
    sz_in_mm : G.size,      (* the width and height in millimeters *)
    min_installed_cmaps : int,
    max_installed_cmaps : int,
    root_visual : XTy.visual_depth,
    backing_store : XTy.backing_store,
    save_unders : bool,
    visualdepths : XTy.visual_depth list
      }

  (* return index of first bit set (starting at 1), return 0 if n = 0, and
   * assume that n > 0.
   *)
      fun ffs 0w0 = MLXError.xerror "bogus resource mask"
    | ffs w = let
        fun lp (w, i) =
          if (Word.andb(w, 0w1) = 0w0)
            then lp(Word.>>(w, 0w1), i+0w1) else i
        in
          lp (w, 0w1)
        end

    (* initialize a connection by sending a connection request *)
      fun initConnection (sock, auth, name, scrNum) = let
(*+DEBUG*)
val _ = XDebug.trace(XDebug.ioTM, fn () => [
    "initializing connection to \"", name, "\"\n"
  ])
(*-DEBUG*)
        val connectMsg = XRequest.encodeConnectionReq {
            minorVersion = 0,
            auth = auth
          }
        val _ = SockUtil.sendVec (sock, connectMsg)
(*+DEBUG*)
val _ = XDebug.trace(XDebug.ioTM, fn () => ["reading connection reply header\n"])
(*-DEBUG*)
        (* ddeboer, fall 2004: error in ssh tunneling happens in following line 
            modified to retry on exception  *)
        fun sleep n = CML.sync(CML.timeOutEvt(Time.fromSeconds n))
        fun retry 0 ts = SockUtil.recvVec (sock,8)
          | retry n ts = SockUtil.recvVec (sock,8) handle _ => (sleep ts; (retry (n-1) (ts*2)))
        val hdr = retry 2 1
        (* original: val hdr = SockUtil.recvVec (sock,8) *)
        val len = 4 * LargeWord.toIntX(PackWord16Big.subVec(hdr, 3))
(*+DEBUG*)
val _ = XDebug.trace(XDebug.ioTM, fn () => [
    "reading connection reply body (", Int.toString len, " bytes)\n"
  ])
(*-DEBUG*)
        (* following lines rewritten, ddeboer, Jan 2005:
         * I'm not sure that this is what is causing the problems I'm trying to fix,
         * but if the reply status (W8V.sub(hdr,0)) is Authenticate (0w2),
         * the following previous code appears to block waiting for 
         * len bytes to be received into reply:
        val reply = SockUtil.recvVec (sock, len)
        fun getMsg () = Byte.unpackStringVec(Word8VectorSlice.slice(
          reply,
          0,
          SOME(Word8.toIntX(W8V.sub(hdr, 1)))))
        fun error msg = (Socket.close sock; MLXError.xerror msg)
        in
          case W8V.sub(hdr, 0)
           of 0w0 => error ("connection refused: " ^ getMsg())
        | 0w1 => let
            val info = XReply.decodeConnectReqReply (hdr, reply)
            val conn = XIo.openConn sock
            in
              (conn, info, name, scrNum)
            end
        | 0w2 => error "connection requires more authentication"
        | _ => error "unknown connection reply" *)
        fun getReply (len) = SockUtil.recvVec (sock, len)
        fun getMsg (reply) = Byte.unpackStringVec(Word8VectorSlice.slice(
          reply,
          0,
          SOME(Word8.toIntX(W8V.sub(hdr, 1)))))
        fun error msg = (Socket.close sock; MLXError.xerror msg)
        in
          case W8V.sub(hdr, 0)
           of 0w0 => error ("connection refused: " ^ getMsg(getReply(len)))
            | 0w1 => let
                val info = XReply.decodeConnectReqReply (hdr,getReply(len))
                val conn = XIo.openConn sock
                in
                  (conn, info, name, scrNum)
                end
            | 0w2 => error "connection requires more authentication"
            | _ => error "unknown connection reply" 
          (* end case *)
        end

    (* Parse the address and open the appropriate kind of connection *)
      fun connect (s, auth) = let
        val {addr, dpy_name, screen} = XServerAddr.getServerAddr s
        fun repeat connFn = let
          fun loop 0 = connFn()
            | loop n = (connFn() handle _ => loop(n-1))
          in
            loop 4  (* try upto five times *)
          end
            handle (OS.SysErr(s, _)) => raise (BadAddr s)
        fun inetConn (addr, port) = let
val _ = TraceCML.trace (XD.ioTM, fn () => [
      "inetConn: addr = \"", NDB.toString addr, "\", port = ",
      Int.toString port, "\n"
    ])
          val sock = INetSock.TCP.socket ()
          in
            repeat (fn () =>
              Socket.connect (sock, INetSock.toAddr(addr, port)));
            initConnection (sock, auth, dpy_name, screen)
          end
        in
          case addr
           of XServerAddr.UNIX path => let
          val sock = UnixSock.Strm.socket ()
          in
            repeat (fn () => Socket.connect (sock, UnixSock.toAddr path));
            initConnection (sock, auth, dpy_name, screen)
          end
        | XServerAddr.INET_Addr(host, port) => (
            case NDB.fromString host
             of (SOME addr) => inetConn (addr, port)
              | NONE => raise BadAddr "bad IP address format"
            (* end case *))
        | XServerAddr.INET_Hostname(host, port) => (
            case NDB.getByName host
             of (SOME entry) => inetConn (NDB.addr entry, port)
              | NONE => raise BadAddr "host not found"
            (* end case *))
          (* end case *)
        end

    (* build a resource-id allocation function *)
      fun mkResourceFn (base, mask) = let
        val resCh = CML.channel()
        val incr = ffs(mask)
        fun loop i = (CML.send(resCh, XTy.XID i); loop(i+incr))
        in
          (* CML.spawn (fn () => (loop base)); *)
          XDebug.xspawn ("ResourceIdAlloc", fn () => (loop base));
          fn () => (CML.recv resCh)
        end

    fun mkScreen (scr_num) {
        root_win, cmap, white, black, input_masks, pixel_wid, pixel_ht,
        mm_wid, mm_ht, installed_maps = {min, max}, root_visualid,
        backing_store, save_unders, root_depth, visualdepths
      } = let
      fun getRootVisual [] = (MLXError.xerror "cannot find root visual")
        | getRootVisual ((XTy.Depth _) :: r) = getRootVisual r
        | getRootVisual ((v as XTy.VisualDepth{id, depth, ...}) :: r) =
        if ((id = root_visualid) andalso (depth = root_depth))
          then v
          else (getRootVisual r)
      in
        XSCR{
        id = scr_num,
        root = root_win,
        cmap = cmap,
        white = white,
        black = black,
        root_input_mask = input_masks,
        sz_in_pixels = G.SIZE{wid = pixel_wid, ht = pixel_ht},
        sz_in_mm = G.SIZE{wid = mm_wid, ht = mm_ht},
        min_installed_cmaps = min,
        max_installed_cmaps = max,
        root_visual = getRootVisual visualdepths,
        backing_store = backing_store,
        save_unders = save_unders,
        visualdepths = visualdepths
          }
      end (* mkScreen *)

    fun mkScreens info_list = let
      fun mkS (i, []) = []
        | mkS (i, info::r) = (mkScreen i info) :: mkS(i+1, r)
      in
        mkS (0, info_list)
      end

    fun openXDisplay {dpyName, auth} = let
      val (conn, info, name, scrNum) = connect (dpyName, auth)
      val _ = XShutdown.logConnection conn
      val screens = mkScreens (#roots info)
      val (dpy as (XDPY dpyrec)) = XDPY{
          conn = conn,
          name = name,
          vendor = #vendor info,
          screens = screens,
          default_scr = scrNum,
          pixmap_formats = #formats info,
          max_req_len = #max_req_len info,
          image_byte_order = #im_byte_order info,
          bitmap_bit_order = #bitmap_order info,
          bitmap_scanline_unit = #bitmap_scanline_unit info,
          bitmap_scanline_pad = #bitmap_scanline_pad info,
          min_keycode = #min_keycode info,
          max_keycode = #max_keycode info,
          nextXId = mkResourceFn (#rsrc_id_base info, #rsrc_id_mask info)
        }
      fun errHandler () = let
        val (seqn, errMsg) = XIo.readXError conn
        in
          TraceCML.trace (XD.errorTM, fn () => [
              "Error on request #", Word.fmt StringCvt.DEC seqn,
              ": ", XPrint.xerrorToString(XReply.decodeError errMsg),
              "\n"
            ]);
          errHandler ()
        end
      in
        XDebug.xspawn("errHandler", errHandler);
        dpy
      end

  (* closeDisplay : xdisplay -> unit *)
    fun closeDisplay (XDPY{conn, ...}) = (
let val tid = CML.getTid() in TraceCML.trace(XD.ioTM, fn () => [
    CML.tidToString tid, " ***** closeDisplay *****\n"
  ])
end;
      XIo.closeConn conn;
      XShutdown.unlogConnection conn)

    fun depthOfVisual (XTy.Depth d) = d
      | depthOfVisual (XTy.VisualDepth{depth, ...}) = depth

    fun displayClassOfVisual (XTy.Depth _) = NONE
      | displayClassOfVisual (XTy.VisualDepth{class, ...}) = SOME class

  end (* XDisplay *)
