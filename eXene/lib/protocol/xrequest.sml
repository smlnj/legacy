(* xrequest.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Functions for encoding X11 protocol request messages.
 *
 * TODO
 *   - encodeAllocColorCells
 *   - encodeAllocColorPlanes
 *   - encodeChangeKeyboardMapping
 *   - encodeSetPointerMapping
 *   - encodeGetPointerMapping
 *   - encodeSetModifierMapping
 *)

structure XRequest =
  struct
    local
      structure G = Geometry
      structure XTy = XProtTypes

      structure W8A = Word8Array
      structure W8V = Word8Vector

    (* we need to treat requests as arrays for initialization purposes, but
     * we don't want them to be modifiable afterwords.
     *)
      val v2a : Word8Vector.vector -> Word8Array.array = Unsafe.cast

      fun pad n = if (Word.andb(Word.fromInt n, 0w3) <> 0w0)
	    then pad(n+1)
	    else n

      fun mkReqBuf sz = Unsafe.Word8Vector.create sz

      fun put8 (buf, i, w)= W8A.update(v2a buf, i, w)
      fun putWord8 (buf, i, x) =
	    put8(buf, i, Word8.fromLargeWord(Word.toLargeWord x))
      fun putSigned8 (buf, i, x) = put8(buf, i, Word8.fromInt x)

      fun put16 (buf, i, x) = PackWord16Big.update(v2a buf, i div 2, x)
      fun putWord16 (buf, i, x) = put16(buf, i, Word.toLargeWord x)
      fun putSigned16 (buf, i, x) = put16(buf, i, LargeWord.fromInt x)

      fun put32 (buf, i, x) = PackWord32Big.update(v2a buf, i div 4, x)
      fun putWord32 (buf, i, x) = put32(buf, i, Word.toLargeWord x)
      fun putSigned32 (buf, i, x) = put32(buf, i, LargeWord.fromInt x)

      fun putString (buf, i, s) = Byte.packString(v2a buf, i, Substring.full s)
      fun putData (buf, i, bv) = W8A.copyVec{
	      dst=v2a buf, di=i, src=bv
	    }

      fun putBool (buf, i, false) = put8 (buf, i, 0w0)
	| putBool (buf, i, true) = put8 (buf, i, 0w1)

      fun putXId (buf, i, XTy.XID n) = putWord32 (buf, i, n)
      fun putXIdOption (buf, i, NONE) = putWord32 (buf, i, 0w0)
	| putXIdOption (buf, i, SOME(XTy.XID n)) =  putWord32 (buf, i, n)

      fun putAtom (buf, i, XTy.XAtom n) = putWord32 (buf, i, n)
      fun putAtomOption (buf, i, NONE) = putWord32 (buf, i, 0w0)
	| putAtomOption (buf, i, SOME(XTy.XAtom n)) = putWord32 (buf, i, n)

      fun putPixel (buf, i, XTy.PIXEL n) = putSigned32(buf, i, n)
      fun putPlaneMask (buf, i, XTy.PLANEMASK n) = putWord32(buf, i, n)
      fun putEventMask (buf, i, XTy.XEVTMASK m) = putWord32(buf, i, m)
      fun putPtrEventMask (buf, i, XTy.XEVTMASK m) = putWord16(buf, i, m)

      fun putPt (buf, i, G.PT{x, y}) = (
	    putSigned16(buf, i, x); putSigned16(buf, i+2, y))
      fun putSize (buf, i, G.SIZE{wid, ht}) = (
	    putSigned16(buf, i, wid); putSigned16(buf, i+2, ht))
      fun putRect (buf, i, G.RECT{x, y, wid, ht}) = (
	    putSigned16(buf, i, x); putSigned16(buf, i+2, y);
	    putSigned16(buf, i+4, wid); putSigned16(buf, i+6, ht))
      fun putArc (buf, i, G.ARC{x, y, wid, ht, angle1, angle2}) = (
	    putSigned16(buf, i, x); putSigned16(buf, i+2, y);
	    putSigned16(buf, i+4, wid); putSigned16(buf, i+6, ht);
	    putSigned16(buf, i+8, angle1); putSigned16(buf, i+10, angle2))
      fun putWGeom (buf, i, G.WGEOM{pos, sz, border}) = (
	    putPt(buf, i, pos);
	    putSize(buf, i+4, sz);
	    putSigned16(buf, i+8, border))

      fun putTS (buf, i, XTy.CurrentTime) = put32(buf, i, 0w0)
	| putTS (buf, i, XTy.TimeStamp(XTime.XT t)) = put32(buf, i, Word32.toLarge t)

      fun putRGB (buf, i, XTy.RGB{red, green, blue}) = (
	    putWord16(buf, i, red);
	    putWord16(buf, i+2, green);
	    putWord16(buf, i+4, blue))

      fun putGrabMode(buf, i, XTy.SynchronousGrab) = put8(buf, i, 0w0)
	| putGrabMode(buf, i, XTy.AsynchronousGrab) = put8(buf, i, 0w1)

      fun putList (f, sz : int) (buf, base, list) = let
	    fun put (_, []) = ()
	      | put (i, x::r) = (f(buf, i, x); put(i+sz, r))
	    in
	      put (base, list)
	    end

      val putPts = putList (putPt, 4)
      val putRects = putList (putRect, 8)
      val putPixels = putList (putPixel, 4)

    (* build a value list and mask from a value option array *)
      fun mkValList (XTy.VALS arr) = let
	    fun f (~1, n, m, l) = (n, XTy.VALMASK m, l)
	      | f (i, n, m, l) = (case Array.sub(arr, i)
		   of (SOME x) => f(i-1, n+1, Word.orb(m, Word.<<(0w1, Word.fromInt i)), x::l)
		    | NONE => f(i-1, n, m, l)
		  (* end case *))
	    in
	      f ((Array.length arr)-1, 0, 0w0, [])
	    end

    (* Put value masks and lists *)
      local
	val putVals = putList (putWord32, 4)
      in
      fun putValList (buf, i, XTy.VALMASK m, vals) = (
	    putWord32(buf, i, m);
	    putVals(buf, i+4, vals))
      fun putValList16 (buf, i, XTy.VALMASK m, vals) = (
	    putWord16(buf, i, m);
	    putVals(buf, i+4, vals))
      end (* local *)

    (** X11 protocol request codes and sizes (from "Xproto.h") **)
      type reqinfo = {code : Word8.word, size : int}
      val reqCreateWindow		= {code = 0w1, size = 8} : reqinfo
      val reqChangeWindowAttributes	= {code = 0w2, size = 3} : reqinfo
      val reqGetWindowAttributes	= {code = 0w3, size = 2} : reqinfo
      val reqDestroyWindow		= {code = 0w4, size = 2} : reqinfo
      val reqDestroySubwindows		= {code = 0w5, size = 2} : reqinfo
      val reqChangeSaveSet		= {code = 0w6, size = 2} : reqinfo
      val reqReparentWindow		= {code = 0w7, size = 4} : reqinfo
      val reqMapWindow			= {code = 0w8, size = 2} : reqinfo
      val reqMapSubwindows		= {code = 0w9, size = 2} : reqinfo
      val reqUnmapWindow		= {code = 0w10, size = 2} : reqinfo
      val reqUnmapSubwindows		= {code = 0w11, size = 2} : reqinfo
      val reqConfigureWindow		= {code = 0w12, size = 3} : reqinfo
      val reqCirculateWindow		= {code = 0w13, size = 2} : reqinfo
      val reqGetGeometry		= {code = 0w14, size = 2} : reqinfo
      val reqQueryTree			= {code = 0w15, size = 2} : reqinfo
      val reqInternAtom			= {code = 0w16, size = 2} : reqinfo
      val reqGetAtomName		= {code = 0w17, size = 2} : reqinfo
      val reqChangeProperty		= {code = 0w18, size = 6} : reqinfo
      val reqDeleteProperty		= {code = 0w19, size = 3} : reqinfo
      val reqGetProperty		= {code = 0w20, size = 6} : reqinfo
      val reqListProperties		= {code = 0w21, size = 2} : reqinfo
      val reqSetSelectionOwner		= {code = 0w22, size = 4} : reqinfo
      val reqGetSelectionOwner		= {code = 0w23, size = 2} : reqinfo
      val reqConvertSelection		= {code = 0w24, size = 6} : reqinfo
      val reqSendEvent			= {code = 0w25, size = 11} : reqinfo
      val reqGrabPointer		= {code = 0w26, size = 6} : reqinfo
      val reqUngrabPointer		= {code = 0w27, size = 2} : reqinfo
      val reqGrabButton			= {code = 0w28, size = 6} : reqinfo
      val reqUngrabButton		= {code = 0w29, size = 3} : reqinfo
      val reqChangeActivePointerGrab	= {code = 0w30, size = 4} : reqinfo
      val reqGrabKeyboard		= {code = 0w31, size = 4} : reqinfo
      val reqUngrabKeyboard		= {code = 0w32, size = 2} : reqinfo
      val reqGrabKey			= {code = 0w33, size = 4} : reqinfo
      val reqUngrabKey			= {code = 0w34, size = 3} : reqinfo
      val reqAllowEvents		= {code = 0w35, size = 2} : reqinfo
      val reqGrabServer			= {code = 0w36, size = 1} : reqinfo
      val reqUngrabServer		= {code = 0w37, size = 1} : reqinfo
      val reqQueryPointer		= {code = 0w38, size = 2} : reqinfo
      val reqGetMotionEvents		= {code = 0w39, size = 4} : reqinfo
      val reqTranslateCoords		= {code = 0w40, size = 4} : reqinfo
      val reqWarpPointer		= {code = 0w41, size = 6} : reqinfo
      val reqSetInputFocus		= {code = 0w42, size = 3} : reqinfo
      val reqGetInputFocus		= {code = 0w43, size = 1} : reqinfo
      val reqQueryKeymap		= {code = 0w44, size = 1} : reqinfo
      val reqOpenFont			= {code = 0w45, size = 3} : reqinfo
      val reqCloseFont			= {code = 0w46, size = 2} : reqinfo
      val reqQueryFont			= {code = 0w47, size = 2} : reqinfo
      val reqQueryTextExtents		= {code = 0w48, size = 2} : reqinfo
      val reqListFonts			= {code = 0w49, size = 2} : reqinfo
      val reqListFontsWithInfo		= {code = 0w50, size = 2} : reqinfo
      val reqSetFontPath		= {code = 0w51, size = 2} : reqinfo
      val reqGetFontPath		= {code = 0w52, size = 1} : reqinfo
      val reqCreatePixmap		= {code = 0w53, size = 4} : reqinfo
      val reqFreePixmap			= {code = 0w54, size = 2} : reqinfo
      val reqCreateGC			= {code = 0w55, size = 4} : reqinfo
      val reqChangeGC			= {code = 0w56, size = 3} : reqinfo
      val reqCopyGC			= {code = 0w57, size = 4} : reqinfo
      val reqSetDashes			= {code = 0w58, size = 3} : reqinfo
      val reqSetClipRectangles		= {code = 0w59, size = 3} : reqinfo
      val reqFreeGC			= {code = 0w60, size = 2} : reqinfo
      val reqClearArea			= {code = 0w61, size = 4} : reqinfo
      val reqCopyArea			= {code = 0w62, size = 7} : reqinfo
      val reqCopyPlane			= {code = 0w63, size = 8} : reqinfo
      val reqPolyPoint			= {code = 0w64, size = 3} : reqinfo
      val reqPolyLine			= {code = 0w65, size = 3} : reqinfo
      val reqPolySegment		= {code = 0w66, size = 3} : reqinfo
      val reqPolyRectangle		= {code = 0w67, size = 3} : reqinfo
      val reqPolyArc			= {code = 0w68, size = 3} : reqinfo
      val reqFillPoly			= {code = 0w69, size = 4} : reqinfo
      val reqPolyFillRectangle		= {code = 0w70, size = 3} : reqinfo
      val reqPolyFillArc		= {code = 0w71, size = 3} : reqinfo
      val reqPutImage			= {code = 0w72, size = 6} : reqinfo
      val reqGetImage			= {code = 0w73, size = 5} : reqinfo
      val reqPolyText8			= {code = 0w74, size = 4} : reqinfo
      val reqPolyText16			= {code = 0w75, size = 4} : reqinfo
      val reqImageText8			= {code = 0w76, size = 4} : reqinfo
      val reqImageText16		= {code = 0w77, size = 4} : reqinfo
      val reqCreateColormap		= {code = 0w78, size = 4} : reqinfo
      val reqFreeColormap		= {code = 0w79, size = 2} : reqinfo
      val reqCopyColormapAndFree	= {code = 0w80, size = 3} : reqinfo
      val reqInstallColormap		= {code = 0w81, size = 2} : reqinfo
      val reqUninstallColormap		= {code = 0w82, size = 2} : reqinfo
      val reqListInstalledColormaps	= {code = 0w83, size = 2} : reqinfo
      val reqAllocColor			= {code = 0w84, size = 4} : reqinfo
      val reqAllocNamedColor		= {code = 0w85, size = 3} : reqinfo
      val reqAllocColorCells		= {code = 0w86, size = 3} : reqinfo
      val reqAllocColorPlanes		= {code = 0w87, size = 4} : reqinfo
      val reqFreeColors			= {code = 0w88, size = 3} : reqinfo
      val reqStoreColors		= {code = 0w89, size = 2} : reqinfo
      val reqStoreNamedColor		= {code = 0w90, size = 4} : reqinfo
      val reqQueryColors		= {code = 0w91, size = 2} : reqinfo
      val reqLookupColor		= {code = 0w92, size = 3} : reqinfo
      val reqCreateCursor		= {code = 0w93, size = 8} : reqinfo
      val reqCreateGlyphCursor		= {code = 0w94, size = 8} : reqinfo
      val reqFreeCursor			= {code = 0w95, size = 2} : reqinfo
      val reqRecolorCursor		= {code = 0w96, size = 5} : reqinfo
      val reqQueryBestSize		= {code = 0w97, size = 3} : reqinfo
      val reqQueryExtension		= {code = 0w98, size = 2} : reqinfo
      val reqListExtensions		= {code = 0w99, size = 1} : reqinfo
      val reqChangeKeyboardMapping	= {code = 0w100, size = 2} : reqinfo
      val reqGetKeyboardMapping		= {code = 0w101, size = 2} : reqinfo
      val reqChangeKeyboardControl	= {code = 0w102, size = 2} : reqinfo
      val reqGetKeyboardControl		= {code = 0w103, size = 1} : reqinfo
      val reqBell			= {code = 0w104, size = 1} : reqinfo
      val reqChangePointerControl	= {code = 0w105, size = 3} : reqinfo
      val reqGetPointerControl		= {code = 0w106, size = 1} : reqinfo
      val reqSetScreenSaver		= {code = 0w107, size = 3} : reqinfo
      val reqGetScreenSaver		= {code = 0w108, size = 1} : reqinfo
      val reqChangeHosts		= {code = 0w109, size = 2} : reqinfo
      val reqListHosts			= {code = 0w110, size = 1} : reqinfo
      val reqSetAccessControl		= {code = 0w111, size = 1} : reqinfo
      val reqSetCloseDownMode		= {code = 0w112, size = 1} : reqinfo
      val reqKillClient			= {code = 0w113, size = 2} : reqinfo
      val reqRotateProperties		= {code = 0w114, size = 3} : reqinfo
      val reqForceScreenSaver		= {code = 0w115, size = 1} : reqinfo
      val reqSetPointerMapping		= {code = 0w116, size = 1} : reqinfo
      val reqGetPointerMapping		= {code = 0w117, size = 1} : reqinfo
      val reqSetModifierMapping		= {code = 0w118, size = 1} : reqinfo
      val reqGetModifierMapping		= {code = 0w119, size = 1} : reqinfo
      val reqNoOperation		= {code = 0w127, size = 1} : reqinfo

    (* Allocate a buffer for a fixed-sized message and initialize the
      * code and size fields.  Return the buffer.
     *)
      fun mkReq ({code, size} : reqinfo) = let
	    val buf = mkReqBuf(4*size)
	    in
	      put8 (buf, 0, code);		(* request opcode *)
	      putSigned16 (buf, 2, size);	(* request size (in words) *)
	      buf
	    end

    (* Allocate a buffer for a fixed-sized message that contains an xid
     * in its first field, and initialize the code and size fields.  Return
     * the buffer.
     *)
      fun mkResourceReq (info, xid) = let
	    val buf = mkReq info
	    in
	      putXId (buf, 4, xid);  (* resource id *)
	      buf
	    end

    (* Allocate and initialize a buffer for a variable-sized request.
     * Return the new buffer.
     *)
      fun mkExtraReq ({code, size}, extra) = let
	    val sz = size+extra
	    val buf = mkReqBuf (4*sz)
	    in
	      put8 (buf, 0, code);	(* request opcode *)
	      putSigned16 (buf, 2, sz);	(* request size (in words) *)
	      buf
	    end

    (* Allocate and initialize a buffer for a variable-sized request.  Only allocate
     * space for the header.  Return the new buffer.
     *)
      fun mkVarReq ({code, size}, extra) = let
	    val sz = size+extra
	    val buf = mkReqBuf (4*size)
	    in
	      put8 (buf, 0, code);		(* request opcode *)
	      putSigned16 (buf, 2, size+extra);	(* request size (in words) *)
	      buf
	    end

    in

  (* encode the connection request message.  This consists of the byte-order,
   * protocol version, and optional authentication data.
   *)
    fun encodeConnectionReq { minorVersion, auth } = let
	  fun setPrefix sz = let
		val buf = W8V.tabulate(sz, fn _ => 0w0)
		in
		  put8(buf, 0, Byte.charToByte #"B");	(* byte order: MSB *)
		  put8(buf, 3, 0w11);			(* major version: 11 *)
		  put8(buf, 5, Word8.fromInt minorVersion);
		  buf
		end
	  in
	    case auth
	     of NONE => setPrefix 12
	      | (SOME(XTy.AUTH{name, data, ...})) => let
		  val authNameLen = pad(size name)
		  val authDataLen = pad(Word8Vector.length data)
		  val prefix = setPrefix (12 + authNameLen + authDataLen)
		  in
		    putSigned16 (prefix, 6, size name);
		    putSigned16 (prefix, 8, Word8Vector.length data);
		    putString (prefix, 12, name);
		    putData (prefix, 12 + authNameLen, data);
		    prefix
		  end
	    (* end case *)
	  end

    fun encodeCreateWindow { win, parent, input_only, depth, visual, geom, vals } = let
	  val (nvals, mask, vals) = mkValList vals
	  val msg = mkExtraReq (reqCreateWindow, nvals)
	  in
	    putSigned8(msg, 1, depth);
	    putXId(msg, 4, win);
	    putXId(msg, 8, parent);
	    putWGeom(msg, 12, geom);
	    put16(msg, 22, case input_only
	       of NONE => 0w0
		| (SOME false) => 0w1
		| (SOME true) => 0w2);
	    putWord32(msg, 24, case visual
	       of NONE => 0w0
		| SOME(XTy.VISUALID id) => id
	      (* end case *));
	    putValList (msg, 28, mask, vals);
	    msg
	  end

    fun encodeChangeWindowAttributes { win, vals } = let
	  val (nvals, mask, vals) = mkValList vals
	  val msg = mkExtraReq (reqChangeWindowAttributes, nvals)
	  in
	    putXId(msg, 4, win);
	    putValList (msg, 8, mask, vals);
	    msg
	  end

    fun encodeGetWindowAttributes { win } = mkResourceReq (reqGetWindowAttributes, win)

    fun encodeDestroyWindow { win } = mkResourceReq (reqDestroyWindow, win)
    fun encodeDestroySubwindows { win } = mkResourceReq (reqDestroySubwindows, win)

    fun encodeChangeSaveSet { insert, win } = let
	  val msg = mkReq (reqChangeSaveSet)
	  in
	    putBool(msg, 1, insert);
	    putXId(msg, 4, win);
	    msg
	  end

    fun encodeReparentWindow { win, parent, pos } = let
	  val msg = mkResourceReq (reqReparentWindow, win)
	  in
	    putXId (msg, 8, parent);
	    putPt (msg, 12, pos);
	    msg
	  end

    fun encodeMapWindow { win } = mkResourceReq (reqMapWindow, win)
    fun encodeMapSubwindows { win } = mkResourceReq (reqMapSubwindows, win)
    fun encodeUnmapWindow { win } = mkResourceReq (reqUnmapWindow, win)
    fun encodeUnmapSubwindows { win } = mkResourceReq (reqUnmapSubwindows, win)

    fun encodeConfigureWindow { win, vals } = let
	  val (nvals, mask, vals) = mkValList vals
	  val msg = mkExtraReq (reqConfigureWindow, nvals)
	  in
	    putXId(msg, 4, win);
	    putValList16 (msg, 8, mask, vals);
	    msg
	  end

    fun encodeCirculateWindow { parent, win, place } = let
	  val msg = mkReq (reqCirculateWindow)
	  in
	    putXId(msg, 4, parent);
	    putXId(msg, 8, win);
	    put8(msg, 12, case place
	       of XTy.PlaceOnTop => 0w0
		| XTy.PlaceOnBottom => 0w1
	      (* end case *));
	    msg
	  end

    fun encodeGetGeometry { drawable } = mkResourceReq (reqGetGeometry, drawable)

    fun encodeQueryTree { win } = mkResourceReq (reqQueryTree, win)

    fun encodeInternAtom { name, only_if_exists } = let
	  val n = String.size name
	  val msg = mkExtraReq (reqInternAtom, (pad n) div 4)
	  in
	    putBool (msg, 1, only_if_exists);
	    putSigned16 (msg, 4, n);
	    putString (msg, 8, name);
	    msg
	  end

    fun encodeGetAtomName { atom = (XTy.XAtom id) } =
	  mkResourceReq (reqGetAtomName, XTy.XID id)

    fun encodeChangeProperty {win, name, prop, mode} = let
	  val XTy.PROP_VAL{typ, value = XTy.RAW_DATA{format, data}} = prop
	  val nbytes = Word8Vector.length data
	  val msg = mkExtraReq (reqChangeProperty, (pad nbytes) div 4)
	  val (nitems, fmt) = case format
	       of XTy.Raw8 => (nbytes, 0w8)
		| XTy.Raw16 => (nbytes div 2, 0w16)
		| XTy.Raw32 => (nbytes div 4, 0w32)
	  in
	    put8(msg, 1, case mode
	       of XTy.ReplaceProp => 0w0
		| XTy.PrependProp => 0w1
		| XTy.AppendProp => 0w2
	      (* end case *));
	    putXId(msg, 4, win);
	    putAtom(msg, 8, name);
	    putAtom(msg, 12, typ);
	    put8(msg, 16, fmt);
	    putSigned32(msg, 20, nitems);
	    putData(msg, 24, data);
	    msg
	  end

    fun encodeDeleteProperty { win, prop } = let
	  val msg = mkReq reqDeleteProperty
	  in
	    putXId(msg, 4, win);
	    putAtom(msg, 8, prop);
	    msg
	  end

    fun encodeGetProperty { win, prop, typ, offset, len, delete } = let
	  val msg = mkReq (reqGetProperty)
	  in
	    putBool(msg, 1, delete);
	    putXId(msg, 4, win);
	    putAtom(msg, 8, prop);
	    putAtomOption(msg, 12, typ);
	    putSigned32(msg, 16, offset);
	    putSigned32(msg, 20, len);
	    msg
	  end

    fun encodeListProperties { win } = mkResourceReq (reqListProperties, win)

    fun encodeSetSelectionOwner { win, selection, timestamp } = let
	  val msg = mkReq reqSetSelectionOwner
	  in
	    putXIdOption(msg, 4, win);
	    putAtom(msg, 8, selection);
	    putTS(msg, 12, timestamp);
	    msg
	  end

    fun encodeGetSelectionOwner { selection = (XTy.XAtom x) } =
	  mkResourceReq (reqGetSelectionOwner, XTy.XID x)

    fun encodeConvertSelection
	{ selection, target, property, requestor, timestamp } = let
	  val msg = mkReq reqConvertSelection
	  in
	    putXId(msg, 4, requestor);
	    putAtom(msg, 8, selection);
	    putAtom(msg, 12, target);
	    putAtomOption(msg, 16, property);
	    putTS(msg, 20, timestamp);
	    msg
	  end

  (* NOTE: this just encodes the header info; the encoding of the event
   * message is handled by the routines in XSendEvent.
   *)
    fun encodeSendEvent { dst, propagate, evt_mask } = let
	  val msg = mkReq (reqSendEvent)
	  in
	    putBool (msg, 1, propagate);
	    case dst
	     of XTy.SendEvtTo_PointerWindow => put32(msg, 4, 0w0)
	      | XTy.SendEvtTo_InputFocus => put32(msg, 4, 0w1)
	      | (XTy.SendEvtTo_Window wid) => putXId(msg, 4, wid)
	    (* end case *);
	    putEventMask (msg, 8, evt_mask);
	    msg
	  end

    fun encodeGrabPointer
	{ win, owner_evts, evt_mask, ptr_mode, kbd_mode, confine_to, cursor, time } = let
	  val msg = mkReq (reqGrabPointer)
	  in
	    putBool (msg, 1, owner_evts);
	    putXId (msg, 4, win);
	    putPtrEventMask (msg, 8, evt_mask);
	    putGrabMode (msg, 10, ptr_mode);
	    putGrabMode (msg, 11, kbd_mode);
	    putXIdOption (msg, 12, confine_to);
	    putXIdOption (msg, 16, cursor);
	    putTS (msg, 20, time);
	    msg
	  end

    fun encodeGrabKeyboard
	{ win, owner_evts, ptr_mode, kbd_mode, time } = let
	  val msg = mkReq (reqGrabKeyboard)
	  in
	    putBool(msg, 1, owner_evts);
	    putXId(msg, 4, win);
	    putTS(msg, 8, time);
	    putGrabMode(msg, 12, ptr_mode);
	    putGrabMode(msg, 13, kbd_mode);
	    msg
	  end

    local
      fun ungrab info { time } = let
	    val msg = mkReq (info)
	    in
	      putTS(msg, 4, time);
	      msg
	    end
    in
    val encodeUngrabPointer = ungrab reqUngrabPointer
    val encodeUngrabKeyboard = ungrab reqUngrabKeyboard
    end

    fun encodeChangeActivePointerGrab { evt_mask, cursor, time } = let
	  val msg = mkReq (reqChangeActivePointerGrab)
	  in
	    putXIdOption(msg, 4, cursor);
	    putTS(msg, 8, time);
	    putPtrEventMask(msg, 12, evt_mask);
	    msg
	  end

    local
      fun putModifiers(buf, i, mset) = let
	    val m = case (KeyBut.mkModState mset)
		 of XTy.AnyModKey => 0wx8000
		  | (XTy.MKState m) => m
	    in
	      putWord16(buf, i, m)
	    end
      fun putButton(buf, i, SOME(XTy.MButton b)) = putSigned8(buf, i, b)
	| putButton(buf, i, NONE) = put8(buf, i, 0w0)
      fun putKeyCode(buf, i, XTy.KEYCODE k) = putSigned8(buf, i, k)
    in
    fun encodeGrabButton
	{ button, modifiers, win, owner_evts, evt_mask, ptr_mode, kbd_mode,
	  confine_to, cursor } = let
	  val msg = mkReq reqGrabButton
	  in
	    putBool(msg, 1, owner_evts);
	    putXId(msg, 4, win);
	    putPtrEventMask(msg, 8, evt_mask);
	    putGrabMode(msg, 10, ptr_mode);
	    putGrabMode(msg, 11, kbd_mode);
	    putXIdOption(msg, 12, confine_to);
	    putXIdOption(msg, 16, cursor);
	    putButton(msg, 18, button);
	    putModifiers(msg, 20, modifiers);
	    msg
	  end

    fun encodeGrabKey { key, modifiers, win, owner_evts, ptr_mode, kbd_mode } = let
	  val msg = mkReq reqGrabKey
	  in
	    putBool(msg, 1, owner_evts);
	    putXId(msg, 4, win);
	    putModifiers(msg, 8, modifiers);
	    putKeyCode(msg, 10, key);
	    putGrabMode(msg, 11, ptr_mode);
	    putGrabMode(msg, 12, kbd_mode);
	    msg
	  end

    fun encodeUngrabButton { button, modifiers, win } = let
	  val msg = mkReq (reqUngrabButton)
	  in
	    putButton(msg, 1, button);
	    putXId(msg, 4, win);
	    putModifiers(msg, 8, modifiers);
	    msg
	  end

    fun encodeUngrabKey { key, modifiers, win } = let
	  val msg = mkReq (reqUngrabKey)
	  in
	    putKeyCode(msg, 1, key);
	    putXId(msg, 4, win);
	    putModifiers(msg, 8, modifiers);
	    msg
	  end
    end (* local *)

    fun encodeAllowEvents { mode, time } = let
	  val msg = mkReq (reqAllowEvents)
	  in
	    put8(msg, 1, case mode
	       of XTy.AsyncPointer => 0w0 | XTy.SyncPointer => 0w1
		| XTy.ReplayPointer => 0w2 | XTy.AsyncKeyboard => 0w3
		| XTy.SyncKeyboard => 0w4 | XTy.ReplayKeyboard => 0w5
		| XTy.AsyncBoth => 0w6 | XTy.SyncBoth => 0w7
	      (* end case *));
	    putTS(msg, 4, time);
	    msg
	  end

    fun encodeQueryPointer { win } = mkResourceReq (reqQueryPointer, win)

    fun encodeGetMotionEvents { win, start, stop } = let
	  val msg = mkReq (reqGetMotionEvents)
	  in
	    putXId(msg, 4, win);
	    putTS(msg, 8, start);
	    putTS(msg, 12, stop);
	    msg
	  end

    fun encodeTranslateCoords { src_win, dst_win, src_pt } = let
	  val msg = mkResourceReq (reqTranslateCoords, src_win)
	  in
	    putXId (msg, 8, dst_win);
	    putPt (msg, 12, src_pt);
	    msg
	  end

    fun encodeWarpPointer { src, dst, src_rect, dst_pt } = let
	  val msg = mkReq reqWarpPointer
	  in
	    putXIdOption(msg, 4, src);
	    putXIdOption(msg, 8, dst);
	    putRect(msg, 12, src_rect);
	    putPt(msg, 20, dst_pt);
	    msg
	  end

    fun encodeSetInputFocus { focus, revert_to, timestamp } = let
	  val msg = mkReq reqSetInputFocus
	  in
	    put8(msg, 1, case revert_to
	       of XTy.RevertToNone => 0w0
		| XTy.RevertToPointerRoot => 0w1
		| XTy.RevertToParent => 0w2
	      (* end case *));
	    putXId(msg, 4, case focus
	       of XTy.InputFocus_None => (XTy.XID 0w0)
		| XTy.InputFocus_PointerRoot => (XTy.XID 0w1)
		| (XTy.InputFocus_Window w) => w
	      (* end case *));
	    putTS (msg, 8, timestamp);
	    msg
	  end

    fun encodeOpenFont { font, name } = let
	  val n = String.size name
	  val msg = mkExtraReq (reqOpenFont, (pad n) div 4)
	  in
	    putXId (msg, 4, font);
	    putSigned16 (msg, 8, n);
	    putString (msg, 12, name);
	    msg
	  end

    fun encodeCloseFont { font } = mkResourceReq (reqCloseFont, font)

    fun encodeQueryFont { font } = mkResourceReq (reqQueryFont, font)

    fun encodeQueryTextExtents { font, str } = let
	  val len = String.size str
	  val p = pad len
	  val msg = mkExtraReq(reqQueryTextExtents, p div 4)
	  in
	    putBool(msg, 1, ((len - p) = 2));
	    putXId(msg, 4, font);
	    putString(msg, 8, str);
	    msg
	  end

    local
      fun encode info { pattern, max } = let
	    val len = String.size pattern
	    val msg = mkExtraReq (info, (pad len) div 4)
	    in
	      putSigned16(msg, 4, max);
	      putSigned16(msg, 6, len);
	      putString(msg, 8, pattern);
	      msg
	    end
    in
    val encodeListFonts = encode reqListFonts
    val encodeListFontsWithInfo = encode reqListFontsWithInfo
    end (* local *)

    fun encodeSetFontPath { path } = let
	  fun f ([], n, l) = (n, String.concat(List.rev l))
	    | f (s::r, n, l) = let val len = String.size s
		in
(** should check that len <= 255 **)
		  f(r, n+1, s :: String.str(Char.chr len) :: l)
		end
	  val (nstrs, data) = f(path, 0, [])
	  val len = String.size data
	  val msg = mkExtraReq (reqSetFontPath, (pad len) div 4)
	  in
	    putSigned16(msg, 4, nstrs);
	    putString(msg, 8, data);
	    msg
	  end

    fun encodeCreatePixmap { pixmap, drawable, depth, size } = let
	  val msg = mkResourceReq (reqCreatePixmap, pixmap)
	  in
	    putSigned8 (msg, 1, depth);
	    putXId (msg, 8, drawable);
	    putSize (msg, 12, size);
	    msg
	  end

    fun encodeFreePixmap { pixmap } = mkResourceReq (reqFreePixmap, pixmap)

    fun encodeCreateGC { gc, drawable, vals } = let
	  val (nvals, mask, vals) = mkValList vals
	  val msg = mkExtraReq (reqCreateGC, nvals)
	  in
	    putXId(msg, 4, gc);
	    putXId(msg, 8, drawable);
	    putValList (msg, 12, mask, vals);
	    msg
	  end

    fun encodeChangeGC { gc, vals } = let
	  val (nvals, mask, vals) = mkValList vals
	  val msg = mkExtraReq (reqChangeGC, nvals)
	  in
	    putXId(msg, 4, gc);
	    putValList (msg, 8, mask, vals);
	    msg
	  end

    fun encodeCopyGC { src, dst, mask = XTy.VALMASK m } = let
	  val msg = mkReq (reqCopyGC)
	  in
	    putXId(msg, 4, src);
	    putXId(msg, 8, dst);
	    putWord32(msg, 12, m);
	    msg
	  end

    fun encodeSetDashes { gc, dash_offset, dashes } = let
	  val n = List.length dashes
	  val msg = mkExtraReq (reqSetDashes, (pad n) div 4)
	  in
	    putXId(msg, 4, gc);
	    putSigned16(msg, 8, dash_offset);
	    putSigned16(msg, 10, n);
	    putList (putSigned8, 1) (msg, 12, dashes);
	    msg
	  end

    fun encodeSetClipRectangles { gc, clip_origin, ordering, rects } = let
	  val msg = mkExtraReq (reqSetClipRectangles, 2 * (List.length rects))
	  in
	    put8(msg, 1, case ordering
	       of XTy.UnsortedOrder => 0w0 | XTy.YSortedOrder => 0w1
		| XTy.YXSortedOrder => 0w2 | XTy.YXBandedOrder => 0w3
	      (* end case *));
	    putXId(msg, 4, gc);
	    putPt(msg, 8, clip_origin);
	    putRects(msg, 12, rects);
	    msg
	  end

    fun encodeFreeGC { gc } = mkResourceReq (reqFreeGC, gc)

    fun encodeClearArea { win, rect, exposures } = let
	  val msg = mkResourceReq (reqClearArea, win)
	  in
	    putBool (msg, 1, exposures);
	    putRect (msg, 8, rect);
	    msg
	  end

    fun encodeCopyArea { gc, src, dst, src_pt, size, dst_pt } = let
	  val msg = mkResourceReq (reqCopyArea, src)
	  in
	    putXId (msg, 8, dst);
	    putXId (msg, 12, gc);
	    putPt (msg, 16, src_pt);
	    putPt (msg, 20, dst_pt);
	    putSize (msg, 24, size);
	    msg
	  end

    fun encodeCopyPlane { gc, src, dst, src_pt, size, dst_pt, plane } = let
	  val msg = mkResourceReq (reqCopyPlane, src)
	  in
	    putXId (msg, 8, dst);
	    putXId (msg, 12, gc);
	    putPt (msg, 16, src_pt);
	    putPt (msg, 20, dst_pt);
	    putSize (msg, 24, size);
	    put32 (msg, 28, LargeWord.<<(0w1, Word.fromInt plane));
	    msg
	  end


    local
      fun encodePoly req_info { drawable, gc, relative, items } = let
	    val msg = mkExtraReq (req_info, List.length items)
	    in
	      putBool(msg, 1, relative);
	      putXId(msg, 4, drawable);
	      putXId(msg, 8, gc);
	      putPts (msg, 12, items);
	      msg
	    end
    in
    val encodePolyPoint = encodePoly reqPolyPoint
    val encodePolyLine = encodePoly reqPolyLine
    end


    local
      fun encode (info, putItems, sz) { drawable, gc, items } = let
	    val msg = mkExtraReq (info, sz*(List.length items))
	    in
	      putXId(msg, 4, drawable);
	      putXId(msg, 8, gc);
	      putItems (msg, 12, items);
	      msg
	    end
      val putSegs = putList
	      (fn (buf, i, G.LINE(p1, p2)) => (putPt(buf, i, p1); putPt(buf, i+4, p2)), 8)
      val putArcs = putList (putArc, 12)
    in
    val encodePolySegment = encode (reqPolySegment, putSegs, 2)
    val encodePolyRectangle = encode (reqPolyRectangle, putRects, 2)
    val encodePolyFillRectangle = encode (reqPolyFillRectangle, putRects, 2)
    val encodePolyArc = encode (reqPolyArc, putArcs, 3)
    val encodePolyFillArc = encode (reqPolyFillArc, putArcs, 3)
    end (* local *)

    fun encodeFillPoly { drawable, gc, shape, relative, pts } = let
	  val msg = mkExtraReq (reqFillPoly, List.length pts)
	  in
	    putXId(msg, 4, drawable);
	    putXId(msg, 8, gc);
	    put8(msg, 12, case shape
	       of XTy.ComplexShape => 0w0
		| XTy.NonconvexShape => 0w1
		| XTy.ConvexShape => 0w2
	      (* end case *));
	    putBool(msg, 13, relative);
	    putPts (msg, 16, pts);
	    msg
	  end

    local
      fun putImageFormat (buf, i, XTy.XYBitmap) = put8(buf, i, 0w0)
	| putImageFormat (buf, i, XTy.XYPixmap) = put8(buf, i, 0w1)
	| putImageFormat (buf, i, XTy.ZPixmap) = put8(buf, i, 0w2)
    in
    fun encodePutImage { drawable, gc, depth, size, dst, lpad, format, data } = let
	  val n = W8V.length data
	  val msg = mkExtraReq (reqPutImage, (pad n) div 4)
	  in
	    putImageFormat(msg, 1, format);
	    putXId(msg, 4, drawable);
	    putXId(msg, 8, gc);
	    putSize(msg, 12, size);
	    putPt(msg, 16, dst);
	    putSigned8(msg, 20, lpad);
	    putSigned8(msg, 21, depth);
	    putData(msg, 24, data);
	    msg
	  end
    fun encodeGetImage { drawable, rect, plane_mask, format } = let
	  val msg = mkResourceReq (reqGetImage, drawable)
	  in
	    putImageFormat(msg, 1, format);
	    putRect(msg, 8, rect);
	    putPlaneMask(msg, 16, plane_mask);
	    msg
	  end
    end (* local *)

    local
      fun textlen (nil, n) = n
	| textlen ((XTy.FontItem _)::r, n) = textlen(r, n+5)
	| textlen ((XTy.TextItem(_, s))::r, n) = textlen(r, n+2+(String.size s))
      fun encode (itemlen, req_info) { drawable, gc, pt, items } = let
	    fun put (msg, i, []) = ()
	      | put (msg, i, (XTy.FontItem(XTy.XID fid)) :: r) = (
		  put8(msg, i, 0w255);
		(* NOTE: this is unaligned, so we have to do it byte-by-byte *)
		  putWord8 (msg, i+1, Word.>>(fid, 0w24));
		  putWord8 (msg, i+2, Word.>>(fid, 0w16));
		  putWord8 (msg, i+3, Word.>>(fid, 0w8));
		  putWord8 (msg, i+4, fid);
		  put (msg, i+5, r))
	      | put (msg, i, (XTy.TextItem(delta, s)) :: r) = let
		  val n = itemlen s
		  in
		    if (n > 254)
		      then MLXError.impossible "excessive string in PolyText"
		      else ();
		    putSigned8(msg, i, n);
		    putSigned8(msg, i+1, delta);
		    putString(msg, i+2, s);
		    put (msg, i+2+(String.size s), r)
		  end
	    val l = textlen (items, 0)
	    val p = pad l
	    val msg = mkExtraReq (req_info, p div 4)
	    in
	      if (p = l) then () else put8(msg, 16+l, 0w0);  (* Xlib does this *)
	      putXId(msg, 4, drawable);
	      putXId(msg, 8, gc);
	      putPt(msg, 12, pt);
	      put(msg, 16, items);
	      msg
	    end
    in
    val encodePolyText8 = encode (String.size, reqPolyText8)
    val encodePolyText16 = encode (fn s => ((String.size s) div 2), reqPolyText16)
    end (* local *)

    local
      fun encode (textlen, req_info) { drawable, gc, pt, str } = let
	    val len = String.size str
	    val msg = mkExtraReq (req_info, (pad len) div 4)
	    in
	      putSigned8(msg, 1, textlen str);
	      putXId(msg, 4, drawable);
	      putXId(msg, 8, gc);
	      putPt(msg, 12, pt);
	      putString(msg, 16, str);
	    msg
	    end
    in
    val encodeImageText8 = encode (String.size, reqImageText8)
    val encodeImageText16 = encode (fn s => ((String.size s) div 2), reqImageText16)
    end (* local *)

    fun encodeCreateColormap { cmap, win, visual, all_writable } = let
	  val msg = mkReq reqCreateColormap
	  in
	    putBool(msg, 1, all_writable);
	    putXId(msg, 4, cmap);
	    putXId(msg, 8, win);
	    putXId(msg, 12, visual);
	    msg
	  end

    fun encodeFreeColormap { cmap } = mkResourceReq (reqFreeColormap, cmap)

    fun encodeCopyColormapAndFree { src, dst } = let
	  val msg = mkReq reqCopyColormapAndFree
	  in
	    putXId(msg, 4, dst);
	    putXId(msg, 8, src);
	    msg
	  end

    fun encodeInstallColormap { cmap } = mkResourceReq (reqInstallColormap, cmap)
    fun encodeUninstallColormap { cmap } = mkResourceReq (reqUninstallColormap, cmap)

    fun encodeListInstalledColormaps { win } =
	  mkResourceReq (reqListInstalledColormaps, win)

    fun encodeAllocColor { cmap, color } = let
	  val msg = mkReq (reqAllocColor)
	  in
	    putXId(msg, 4, cmap);
	    putRGB(msg, 8, color);
	    msg
	  end

    fun encodeAllocNamedColor { cmap, name } = let
	  val n = String.size name
	  val msg = mkExtraReq (reqAllocNamedColor, (pad n) div 4)
	  in
	    putXId(msg, 4, cmap);
	    putSigned16(msg, 8, n);
	    putString (msg, 12, name);
	    msg
	  end

(**************************************************************************************
    fun encodeAllocColorCells = let
	  val msg = mkReq (reqAllocColorCells)
	  in
	    raise XERROR "unimplemented" (*** FIX ***)
	  end
    fun encodeAllocColorPlanes = let
	  val msg = mkReq (reqAllocColorPlanes)
	  in
	    raise XERROR "unimplemented" (*** FIX ***)
	  end
**************************************************************************************)

    fun encodeFreeColors { cmap, plane_mask, pixels } = let
	  val msg = mkExtraReq (reqFreeColors, List.length pixels)
	  in
	    putXId(msg, 4, cmap);
	    putPlaneMask(msg, 8, plane_mask);
	    putPixels (msg, 12, pixels);
	    msg
	  end

    local
      fun putColorItem (buf, i, XTy.COLORITEM{pixel, red, green, blue}) = let
	    val rmask = (case red
		   of NONE => 0w0
		    | (SOME x) => (putWord16(buf, i+4, x); 0w1)
		  (* end case *))
	    val gmask = (case green
		   of NONE => 0w0
		    | (SOME x) => (putWord16(buf, i+6, x); 0w2)
		  (* end case *))
	    val bmask = (case blue
		   of NONE => 0w0
		    | (SOME x) => (putWord16(buf, i+8, x); 0w4)
		  (* end case *))
	    in
	      putPixel(buf, i, pixel);
	      put8(buf, i+10, Word8.orb(rmask, Word8.orb(gmask, bmask)))
	    end
      val putColorItemList = putList (putColorItem, 12)
    in
    fun encodeStoreColors { cmap, items } = let
	  val msg = mkExtraReq (reqStoreColors, 3*(List.length items))
	  in
	    putXId(msg, 4, cmap);
	    putColorItemList(msg, 8, items);
	    msg
	  end
    end (* local *)

    fun encodeStoreNamedColor
	{ cmap, name, pixel, do_red, do_green, do_blue } = let
	  val n = String.size name
	  val msg = mkExtraReq (reqStoreNamedColor, (pad n) div 4)
	  val mask = Word8.orb(
		if do_red then 0w1 else 0w0,
		Word8.orb(
		  if do_green then 0w2 else 0w0,
		  if do_blue then 0w4 else 0w0))
	  in
	    put8(msg, 1, mask);
	    putXId(msg, 4, cmap);
	    putPixel(msg, 8, pixel);
	    putString (msg, 12, name);
	    msg
	  end

    fun encodeQueryColors { cmap, pixels } = let
	  val msg = mkExtraReq (reqQueryColors, List.length pixels)
	  in
	    putXId(msg, 4, cmap);
	    putPixels (msg, 8, pixels);
	    msg
	  end

    fun encodeLookupColor { cmap, name } = let
	  val n = String.size name
	  val msg = mkExtraReq (reqLookupColor, (pad n) div 4)
	  in
	    putXId(msg, 4, cmap);
	    putSigned16(msg, 8, n);
	    putString(msg, 12, name);
	    msg
	  end

    fun encodeCreateCursor { cursor, src, mask, fore_rgb, back_rgb, hot_spot} = let
	  val msg = mkReq (reqCreateCursor)
	  in
	    putXId(msg, 4, cursor);
	    putXId(msg, 8, src);
	    putXIdOption(msg, 12, mask);
	    putRGB(msg, 16, fore_rgb);
	    putRGB(msg, 22, back_rgb);
	    putPt(msg, 24, hot_spot);
	    msg
	  end

    fun encodeCreateGlyphCursor
	{ cursor, src_font, mask_font, src_chr, mask_chr, fore_rgb, back_rgb } = let
	  val msg = mkReq (reqCreateGlyphCursor)
	  in
	    putXId(msg, 4, cursor);
	    putXId(msg, 8, src_font);
	    putXIdOption(msg, 12, mask_font);
	    putSigned16(msg, 16, src_chr);
	    putSigned16(msg, 18, mask_chr);
	    putRGB(msg, 20, fore_rgb);
	    putRGB(msg, 26, back_rgb);
	    msg
	  end

    fun encodeFreeCursor { cursor } = mkResourceReq (reqFreeCursor, cursor)

    fun encodeRecolorCursor { cursor, fore_color, back_color } = let
	  val msg = mkReq reqRecolorCursor
	  in
	    putXId(msg, 4, cursor);
	    putRGB(msg, 8, fore_color);
	    putRGB(msg, 14, back_color);
	    msg
	  end

    fun encodeQueryBestSize { class, drawable, size } = let
	  val msg = mkReq reqQueryBestSize
	  in
	    put8(msg, 1, case class
	       of XTy.CursorShape => 0w0
		| XTy.TileShape => 0w1
		| XTy.StippleShape => 0w2
	      (* end case *));
	    putXId(msg, 4, drawable);
	    putSize(msg, 8, size);
	    msg
	  end

    fun encodeQueryExtension name = let
	  val n = String.size name
	  val msg = mkExtraReq (reqQueryExtension, (pad n) div 4)
	  in
	    putSigned16(msg, 4, n);
	    putString(msg, 8, name);
	    msg
	  end

(**************************************************************************************
    fun encodeChangeKeyboardMapping = let
	  val msg = mkReq (reqChangeKeyboardMapping)
	  in
	    raise XERROR "unimplemented" (*** FIX ***)
	  end
**************************************************************************************)

    fun encodeGetKeyboardMapping {first=(XTy.KEYCODE k), count} = let
	  val msg = mkReq reqGetKeyboardMapping
	  in
	    putSigned8(msg, 4, k);
	    putSigned8(msg, 5, count);
	    msg
	  end

    fun encodeChangeKeyboardControl { vals } = let
	  val (nvals, mask, vals) = mkValList vals
	  val msg = mkExtraReq (reqChangeKeyboardControl, nvals)
	  in
	    putValList(msg, 4, mask, vals);
	    msg
	  end

    fun encodeBell { percent } = let
	  val msg = mkReq reqBell
	  in
	    putSigned8(msg, 1, percent);
	    msg
	  end

    fun encodeChangePointerControl { acceleration, threshold } = let
	  val msg = mkReq reqChangePointerControl
	  in
	    case acceleration
	     of NONE => putBool(msg, 10, false)
	      | (SOME{numerator, denominator}) => (
		  putBool(msg, 10, true);
		  putSigned16(msg, 4, numerator);
		  putSigned16(msg, 6, denominator));
	    case threshold
	     of NONE => putBool(msg, 11, false)
	      | (SOME threshold) => (
		  putBool(msg, 11, false);
		  putSigned16(msg, 8, threshold));
	    msg
	  end

    fun encodeSetScreenSaver
	{ timeout, interval, prefer_blanking, allow_exposures } = let
	  val msg = mkReq reqSetScreenSaver
	  fun put (i, NONE) = put8(msg, i, 0w2)
	    | put (i, SOME b) = putBool(msg, i, b)
	  in
	    putSigned16(msg, 4, timeout);
	    putSigned16(msg, 6, interval);
	    put(8, prefer_blanking);
	    put(9, allow_exposures);
	    msg
	  end

    fun encodeChangeHosts { host, remove } = let
	  val (family, addr) = case host
		 of (XTy.InternetHost s) => (0w0, s)
		  | (XTy.DECnetHost s) => (0w1, s)
		  | (XTy.ChaosHost s) => (0w2, s)
	  val len = String.size addr
	  val msg = mkExtraReq (reqChangeHosts, (pad len) div 4)
	  in
	    putBool(msg, 1, remove);
	    put8(msg, 4, family);
	    putSigned16(msg, 6, len);
	    putString(msg, 8, addr);
	    msg
	  end

    fun encodeSetAccessControl { enable } = let
	  val msg = mkReq (reqSetAccessControl)
	  in
	    putBool(msg, 1, enable);
	    msg
	  end

    fun encodeSetCloseDownMode { mode } = let
	  val msg = mkReq (reqSetCloseDownMode)
	  in
	    put8(msg, 1, case mode
	       of XTy.DestroyAll => 0w0
		| XTy.RetainPermanent => 0w1
		| XTy.RetainTemporary => 0w2
	      (* end case *));
	    msg
	  end

    fun encodeKillClient { resource } = let
	  val rid = case resource of NONE => (XTy.XID 0w0) | (SOME x) => x
	  in
	    mkResourceReq (reqKillClient, rid)
	  end

    fun encodeRotateProperties { win, delta, properties } = let
	  val n = List.length properties
	  val msg = mkExtraReq (reqRotateProperties, n)
	  in
	    putXId(msg, 4, win);
	    putSigned16(msg, 8, n);
	    putSigned16(msg, 10, delta);
	    putList (putAtom, 4) (msg, 12, properties);
	    msg
	  end

    fun encodeForceScreenSaver { activate } = let
	  val msg = mkReq (reqForceScreenSaver)
	  in
	    putBool(msg, 1, activate);
	    msg
	  end

(**************************************************************************************
    fun encodeSetPointerMapping = let
	  val msg = mkReq (reqSetPointerMapping)
	  in
	    raise XERROR "unimplemented" (*** FIX ***)
	  end
    fun encodeGetPointerMapping = let
	  val msg = mkReq (reqGetPointerMapping)
	  in
	    raise XERROR "unimplemented" (*** FIX ***)
	  end
    fun encodeSetModifierMapping = let
	  val msg = mkExtraReq (reqSetModifierMapping, ?)
	  in
	    raise XERROR "unimplemented" (*** FIX ***)
	  end
**************************************************************************************)

  (* Fixed requests *)
    val requestNoOperation = mkReq reqNoOperation
    val requestGetInputFocus = mkReq reqGetInputFocus
    val requestQueryKeymap = mkReq reqQueryKeymap
    val requestGrabServer = mkReq reqGrabServer
    val requestUngrabServer = mkReq reqUngrabServer
    val requestGetFontPath = mkReq reqGetFontPath
    val requestListExtensions = mkReq reqListExtensions
    val requestGetKeyboardControl = mkReq reqGetKeyboardControl
    val requestGetPointerControl = mkReq reqGetPointerControl
    val requestGetScreenSaver = mkReq reqGetScreenSaver
    val requestListHosts = mkReq reqListHosts
    val requestGetModifierMapping = mkReq reqGetModifierMapping

    end (* local open ... *)

  end (* XRequests *)
