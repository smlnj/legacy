(* xreply.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Routines to decode reply, error and event messages received from the server.
 *
 * TODO
 *   events
 *     decodeKeymapNotify
 *   replies
 *     decodeAllocColorCellsReply
 *     decodeAllocColorPlanesReply
 *     decodeGetPointerMappingReply
 *     decodeListExtensionsReply
 *     decodeQueryExtensionReply
 *     decodeQueryKeymapReply
 *)

structure XReply =
  struct
    local
      structure W8 = Word8 and W8V = Word8Vector
      structure G = Geometry
      structure XTy = XProtTypes
      structure XEvt = XEventTypes

      val & = LargeWord.andb
      val ++ = LargeWord.orb
      infix & ++

      fun isSet(x, i) = ((x & LargeWord.<<(0w1, i)) <> 0w0)

      fun pad n = (case Word.andb(Word.fromInt n, 0w3)
         of 0w0 => n
          | r => (n + (4 - Word.toIntX r))
        (* end case *))

      fun getString (bv, i, n) =
      Byte.unpackStringVec (Word8VectorSlice.slice (bv, i, SOME n))

      val get8 = W8.toLargeWord o W8V.sub
      fun getWord8 arg = Word.fromLargeWord(W8.toLargeWord(W8V.sub arg))
      fun getInt8 arg = W8.toInt(W8V.sub arg)
      fun getSigned8 arg = W8.toIntX(W8V.sub arg)

      fun get16 (s, i) = PackWord16Big.subVec(s, i div 2)
      fun getWord16 (s, i) = Word.fromLargeWord(get16(s, i))
      fun getInt16 (s, i) = LargeWord.toInt(get16(s, i))
      fun getSigned16 (s, i) = LargeWord.toIntX(PackWord16Big.subVecX(s, i div 2))

      fun get32 (s, i) = Word32.fromLargeWord(PackWord32Big.subVec(s, i div 4))
      fun getSigned32 (s, i) =
        Int32.fromLarge(LargeWord.toLargeInt(PackWord32Big.subVecX(s, i div 4)))
      fun getWord (s, i) = Word.fromLargeWord(PackWord32Big.subVec(s, i div 4))
      fun getInt (s, i) = LargeWord.toIntX(PackWord32Big.subVecX(s, i div 4))

      val w8vextract = Word8VectorSlice.vector o Word8VectorSlice.slice

fun wrapFn name f (s, i) = (f(s, i) handle ex => (
XDebug.errTrace(fn () => ["**** ", name, "(", Int.toString(W8V.length s), ",",
Int.toString i, ")\n"]); raise ex))
val get8 = wrapFn "get8" get8
val getWord8 = wrapFn "getWord8" getWord8
val getInt8 = wrapFn "getInt8" getInt8
val getSigned8 = wrapFn "getSigned8" getSigned8
val get16 = wrapFn "get16" get16
val getWord16 = wrapFn "getWord16" getWord16
val getInt16 = wrapFn "getInt16" getInt16
val getSigned16 = wrapFn "getSigned16" getSigned16
val get32 = wrapFn "get32" get32
val getSigned32 = wrapFn "getSigned32" getSigned32
val getWord = wrapFn "getWord" getWord
val getInt = wrapFn "getInt" getInt

      fun getList (f, sz : int) (buf, i, n) = let
        fun get (_, 0, l) = List.rev l
          | get (i, n, l) = get (i+sz, n-1, f(buf, i)::l)
        in
          get (i, n, [])
        end

    (* get a list of strings, where each string is preceded by a one-byte length
     * field.
     *)
      fun getStringList (buf, i, n) = let
        fun get (_, 0, l) = List.rev l
          | get (i, n, l) = let
          val len = getInt8(buf, i) and j = i+1
          in
            get (j+len, n-1, getString(buf, j, len) :: l)
          end
        in
          get (i, n, [])
        end

      val getXAtom = (XTy.XAtom o getWord)
      fun getXAtomOption arg = (case getWord arg
         of 0w0 => NONE
          | x => SOME(XTy.XAtom x)
        (* end case *))
      val getXId = (XTy.XID o getWord)
      fun getXIdOption arg = (case getWord arg
         of 0w0 => NONE
          | x => SOME(XTy.XID x)
        (* end case *))

      val getEventMask = (XTy.XEVTMASK o getWord)
      val getVisualId = (XTy.VISUALID o getWord)
      fun getVisualIdOption arg = (case getWord arg
         of 0w0 => NONE
          | x => SOME(XTy.VISUALID x)
        (* end case *))
      val getPixel = XTy.PIXEL o getInt

(* Are time values signed??? *)
      fun getTime (s, i) = XTime.XT(get32(s, i))
      fun getTS (s, i) = (case get32(s, i)
         of 0w0 => XTy.CurrentTime
          | t => XTy.TimeStamp(XTime.XT t)
        (* end case *))

      fun getBool arg = (case (W8V.sub arg) of 0w0 => false | _ => true)
      fun getPt (s, i) = G.PT{ x = getSigned16(s, i), y = getSigned16(s, i+2) }
      fun getSize (s, i) = G.SIZE{ wid = getInt16(s, i), ht = getInt16(s, i+2) }
      fun getRect (s, i) = G.RECT{
          x = getSigned16(s, i), y = getSigned16(s, i+2),
          wid = getInt16(s, i+4), ht = getInt16(s, i+6)
        }
      fun getWGeom (s, i) = G.WGEOM{
          pos = getPt(s, i), sz = getSize(s, i+4), border = getInt16(s, i+8)
        }

      val getKeyCode = XTy.KEYCODE o getInt8

      fun getStkPos arg = (case (W8V.sub arg)
         of 0w0 => XTy.PlaceOnTop
          | _ => XTy.PlaceOnBottom
        (* end case *))

      fun getFocusMode (s, i) = (case W8V.sub(s, i)
         of 0w0 => XTy.FocusNormal | 0w1 => XTy.FocusGrab
          | 0w2 => XTy.FocusUngrab | 0w3 => XTy.FocusWhileGrabbed
          | _ => MLXError.impossible "bad focus mode"
        (* end case *))
      fun getFocusDetail (s, i) = (case W8V.sub(s, i)
         of 0w0 => XTy.FocusAncestor | 0w1 => XTy.FocusVirtual
          | 0w2 => XTy.FocusInferior | 0w3 => XTy.FocusNonlinear
          | 0w4 => XTy.FocusNonlinearVirtual | 0w5 => XTy.FocusPointer
          | 0w6 => XTy.FocusPointerRoot | 0w7 => XTy.FocusNone
          | _ => MLXError.impossible "bad focus detail"
        (* end case *))

      fun getKeyButSet (s, i) = let val m = getWord16(s, i)
        in
          ( XTy.MKState(Word.andb(m, 0wxFF)),
        XTy.MBState(Word.andb(m, 0wx1F00)))
        end

      fun getRGB (buf, i) = XTy.RGB{
          red = getWord16(buf, i),
          green = getWord16(buf, i+2),
          blue = getWord16(buf, i+4)
        }

      fun getBS (buf, i) = (case W8V.sub(buf, i)
         of 0w0 => XTy.BS_NotUseful
          | 0w1 => XTy.BS_WhenMapped
          | _ => XTy.BS_Always
        (* end case *))

      fun getFontDir (buf, i) = (case W8V.sub(buf, i)
         of 0w0 => XTy.FontLeftToRight | 0w1 => XTy.FontRightToLeft
          | _ => MLXError.impossible "bad font direction"
        (* end case *))

      val getXIdList = getList (getXId, 4)
      val getXAtomList = getList (getXAtom, 4)

      local
        fun toGravity (0w1 : Word8.word) = SOME XTy.NorthWestGravity
      | toGravity 0w2 = SOME XTy.NorthGravity
      | toGravity 0w3 = SOME XTy.NorthEastGravity
      | toGravity 0w4 = SOME XTy.WestGravity
      | toGravity 0w5 = SOME XTy.CenterGravity
      | toGravity 0w6 = SOME XTy.EastGravity
      | toGravity 0w7 = SOME XTy.SouthWestGravity
      | toGravity 0w8 = SOME XTy.SouthGravity
      | toGravity 0w9 = SOME XTy.SouthEastGravity
      | toGravity 0w10 = SOME XTy.StaticGravity
      | toGravity _ = NONE
      in
      fun getBitGravity arg = (case toGravity(W8V.sub arg)
         of NONE => XTy.ForgetGravity
          | SOME g => g
        (* end case *))
      fun getWinGravity arg = (case toGravity(W8V.sub arg)
         of NONE => XTy.UnmapGravity
          | SOME g => g
        (* end case *))
      end (* local *)

      fun getRawFormat arg = (case (W8V.sub arg)
       of 0w8 => XTy.Raw8 | 0w16 => XTy.Raw16 | 0w32 => XTy.Raw32
        | _ => MLXError.impossible "[getRawFormat: bad ClientMessage]")

    in

  (** Get the reply from a connection request **)
    local
      val prefix_sz = 8
      fun getOrder(buf, i) = (case get8(buf, i)
         of 0w0 => XTy.LSBFirst
          | _ => XTy.MSBFirst)
      fun getFormat (buf, i) = XTy.FORMAT {
          depth = getInt8(buf, i),
          bits_per_pixel = getInt8(buf, i+1),
          scanline_pad = getRawFormat(buf, i+2)
        }
      fun getVisualDepth (buf, i, depth) = XTy.VisualDepth{
          id = getVisualId(buf, i),
          depth = depth,
          class = (case W8V.sub(buf, i+4)
         of 0w0 => XTy.StaticGray | 0w1 => XTy.GrayScale
          | 0w2 => XTy.StaticColor | 0w3 => XTy.PseudoColor
          | 0w4 => XTy.TrueColor | 0w5 => XTy.DirectColor
          | _ => MLXError.impossible "bad visual depth"
        (* end case *)),
          bits_per_rgb = getInt8(buf, i+5),
          cmap_entries = getInt16(buf, i+6),
          red_mask = getWord(buf, i+8),
          green_mask = getWord(buf, i+12),
          blue_mask = getWord(buf, i+16)
        }
      fun getVisualDepthList (buf, i, ndepths) = let
        fun getDepths (0, i, l) = (List.rev l, i)
          | getDepths (ndepths, i, l) = let
          val depth = getInt8(buf, i)
          in
            case (getInt16(buf, i+2))
             of 0 => getDepths (ndepths-1, i+8, (XTy.Depth depth)::l)
              | nVisuals => getVisuals (ndepths-1, depth, nVisuals, i+8, l)
          end
        and getVisuals (ndepths, _, 0, i, l) = getDepths (ndepths, i, l)
          | getVisuals (ndepths, depth, k, i, l) =
          getVisuals (ndepths, depth, k-1, i+24,
            getVisualDepth(buf, i, depth)::l)
        in
          getDepths (ndepths, i, [])
        end
      fun getScreen (buf, i) = let
        val (vdepths, next) = getVisualDepthList(buf, i+40, getInt8(buf, i+39))
        in (
          {
        root_win = getXId(buf, i),
        cmap = getXId(buf, i+4),
        white = getPixel(buf, i+8),
        black = getPixel(buf, i+12),
        input_masks = getEventMask(buf, i+16),
        pixel_wid = getInt16(buf, i+20),
        pixel_ht = getInt16(buf, i+22),
        mm_wid = getInt16(buf, i+24),
        mm_ht = getInt16(buf, i+26),
        installed_maps = {min = getInt16(buf, i+28), max = getInt16(buf, i+30)},
        root_visualid = getVisualId(buf, i+32),
        backing_store = getBS(buf, i+36),
        save_unders = getBool(buf, i+37),
        root_depth = getInt8(buf, i+38),
        visualdepths = vdepths
          }, next)
        end
      val getFormats = getList (getFormat, 8)
      fun getScreens (buf, i, nscreens) = let
        fun get (0, _, l) = List.rev l
          | get (n, i, l) = let
          val (scr, next) = getScreen(buf, i)
          in
            get(n-1, next, scr::l)
          end
        in
          get (nscreens, i, [])
        end
    in
    fun decodeConnectReqReply (prefix, msg) = let
      val vendor_len = getInt16(msg, 16)
      val nscreens = getInt8(msg, 20)
      val nformats = getInt8(msg, 21)
      val format_offset = pad (32 + vendor_len)
      val screen_offset = format_offset + 8*nformats
      in {
        protocol_version = {
        major = getInt16(prefix, 2),
        minor = getInt16(prefix, 4)
          },
        release_num = getInt(msg, 0),
        rsrc_id_base = getWord(msg, 4),
        rsrc_id_mask = getWord(msg, 8),
        motion_buf_sz = getInt(msg, 12),
        max_req_len = getInt16(msg, 18),
        im_byte_order = getOrder(msg, 22),
        bitmap_order = getOrder(msg, 23),
        bitmap_scanline_unit = getRawFormat(msg, 24),
        bitmap_scanline_pad = getRawFormat(msg, 25),
        min_keycode = getKeyCode(msg, 26),
        max_keycode = getKeyCode(msg, 27),
        vendor = getString(msg, 32, vendor_len),
        formats = getFormats(msg, format_offset, nformats),
        roots = getScreens(msg, screen_offset, nscreens)
      } end
    end (* local *)


  (** decode event messages **)

    local
      fun getKeyXEvt buf = let val (mks, mbs) = getKeyButSet(buf, 28)
        in {
          keycode = getKeyCode(buf, 1),
          time = getTime(buf, 4),
          root = getXId(buf, 8),
          event = getXId(buf, 12),
          child = getXIdOption(buf, 16),
          root_pt = getPt(buf, 20),
          event_pt = getPt(buf, 24),
          mod_state = mks,
          mbut_state = mbs,
          same_screen = getBool(buf, 30)
        } end
      fun getButtonXEvt buf = let val (mks, mbs) = getKeyButSet(buf, 28)
        in {
          button = XTy.MButton(getInt8(buf, 1)),
          time = getTime(buf, 4),
          root = getXId(buf, 8),
          event = getXId(buf, 12),
          child = getXIdOption(buf, 16),
          root_pt = getPt(buf, 20),
          event_pt = getPt(buf, 24),
          mod_state = mks,
          mbut_state = mbs,
          same_screen = getBool(buf, 30)
        } end
      fun decodeMotionNotify buf = let
        val (mks, mbs) = getKeyButSet(buf, 28)
        in
          XEvt.MotionNotifyXEvt {
          hint = getBool(buf, 1),
          time = getTime(buf, 4),
          root = getXId(buf, 8),
          event = getXId(buf, 12),
          child = getXIdOption(buf, 16),
          root_pt = getPt(buf, 20),
          event_pt = getPt(buf, 24),
          mod_state = mks,
          mbut_state = mbs,
          same_screen = getBool(buf, 30)
        }
        end
      fun getEnterLeaveXEvt buf = let
        val (mks, mbs) = getKeyButSet(buf, 28)
        val flags = get8(buf, 31)
        in {
          detail = getFocusDetail(buf, 1),
          time = getTime(buf, 4),
          root = getXId(buf, 8),
          event = getXId(buf, 12),
          child = getXIdOption(buf, 16),
          root_pt = getPt(buf, 20),
          event_pt = getPt(buf, 24),
          mod_state = mks,
          mbut_state = mbs,
          mode = getFocusMode(buf, 30),
          focus = isSet(flags, 0w0),
          same_screen = isSet(flags, 0w1)
        } end
      fun getFocusXEvt buf = {
          detail = getFocusDetail(buf, 1),
          event = getXId(buf, 4),
          mode = getFocusMode(buf, 8)
        }
      fun decodeKeymapNotify buf =
        XEvt.KeymapNotifyXEvt {}(** NOTE: no seqn # **) (** FIX **)
      fun decodeExpose buf = XEvt.ExposeXEvt {
          window = getXId(buf, 4),
          rects = [getRect(buf, 8)],
          count = getInt16(buf, 16)
        }
      fun decodeGraphicsExpose buf = XEvt.GraphicsExposeXEvt {
          drawable = getXId(buf, 4),
          rect = getRect(buf, 8),
          minor_opcode = getWord16(buf, 16),
          count = getInt16(buf, 18),
          major_opcode = getWord16(buf, 20)
        }
      fun decodeNoExpose buf = XEvt.NoExposeXEvt {
          drawable = getXId(buf, 4),
          minor_opcode = getWord16(buf, 8),
          major_opcode = getWord16(buf, 10)
        }
      fun decodeVisibilityNotify buf = XEvt.VisibilityNotifyXEvt {
          window = getXId(buf, 4),
          state = (case W8V.sub(buf, 8)
         of 0w0 => XTy.VisibilityUnobscured
          | 0w1 => XTy.VisibilityPartiallyObscured
          | 0w2 => XTy.VisibilityFullyObscured
          | _ => MLXError.impossible "bad VisibilityNotify"
        (* end case *))
        }
      fun decodeCreateNotify buf = XEvt.CreateNotifyXEvt {
          parent = getXId(buf, 4),
          window = getXId(buf, 8),
          rect = getRect(buf, 12),
          border_wid = getInt16(buf, 20),
          override_redirect = getBool(buf, 21)
        }
      fun decodeDestroyNotify buf = XEvt.DestroyNotifyXEvt {
          event = getXId(buf, 4),
          window = getXId(buf, 8)
        }
      fun decodeUnmapNotify buf = XEvt.UnmapNotifyXEvt {
          event = getXId(buf, 4),
          window = getXId(buf, 8),
          from_config = getBool(buf, 12)
        }
      fun decodeMapNotify buf = XEvt.MapNotifyXEvt {
          event = getXId(buf, 4),
          window = getXId(buf, 8),
          override_redirect = getBool(buf, 12)
        }
      fun decodeMapRequest buf = XEvt.MapRequestXEvt {
          parent = getXId(buf, 4),
          window = getXId(buf, 8)
        }
      fun decodeReparentNotify buf = XEvt.ReparentNotifyXEvt {
          event = getXId(buf, 4),
          parent = getXId(buf, 8),
          window = getXId(buf, 12),
          corner = getPt(buf, 16),
          override_redirect = getBool(buf, 20)
        }
      fun decodeConfigureNotify buf = XEvt.ConfigureNotifyXEvt {
          event = getXId(buf, 4),
          window = getXId(buf, 8),
          sibling = getXIdOption(buf, 12),
          rect = getRect(buf, 16),
          border_wid = getInt16(buf, 20),
          override_redirect = getBool(buf, 22)
        }
      fun decodeConfigureRequest buf = let
        val mask = get16(buf, 26)
        fun getOpt getFn (i, j) = if isSet(mask, i)
          then SOME(getFn(buf, j))
          else NONE
        in
          XEvt.ConfigureRequestXEvt{
          stack_mode = if isSet(mask, 0w6)
            then (case W8V.sub(buf, 1)
               of 0w0 => SOME XTy.Above
                | 0w1 => SOME XTy.Below
                | 0w2 => SOME XTy.TopIf
                | 0w3 => SOME XTy.BottomIf
                | 0w4 => SOME XTy.Opposite
                | _ => MLXError.impossible "bad ConfigureRequest"
              (* end case *))
            else NONE,
        parent = getXId(buf, 4),
        window = getXId(buf, 8),
        sibling = getXIdOption(buf, 12),
        x = getOpt getSigned16 (0w0, 16),
        y = getOpt getSigned16 (0w1, 18),
        wid = getOpt getInt16 (0w2, 20),
        ht = getOpt getInt16 (0w3, 22),
        border_wid = getOpt getInt16 (0w4, 24)
          }
        end
      fun decodeGravityNotify buf = XEvt.GravityNotifyXEvt {
          event = getXId(buf, 4),
          window = getXId(buf, 8),
          corner = getPt(buf, 12)
        }
      fun decodeResizeRequest buf = XEvt.ResizeRequestXEvt {
          window = getXId(buf, 4),
          req_sz = getSize(buf, 8)
        }
      fun decodeCirculateNotify buf = XEvt.CirculateNotifyXEvt {
          event = getXId(buf, 4),
          window = getXId(buf, 8),
          parent = getXId(buf, 12),
          place = getStkPos(buf, 16)
        }
      fun decodeCirculateRequest buf = XEvt.CirculateRequestXEvt {
          parent = getXId(buf, 4),
          window = getXId(buf, 8),
          place = getStkPos(buf, 12)
        }
      fun decodePropertyNotify buf = XEvt.PropertyNotifyXEvt {
          window = getXId(buf, 4),
          atom = getXAtom(buf, 8),
          time = getTime(buf, 12),
          deleted = getBool(buf, 16)
        }
      fun decodeSelectionClear buf = XEvt.SelectionClearXEvt {
          time = getTime(buf, 4),
          owner = getXId(buf, 8),
          selection = getXAtom(buf, 12)
        }
      fun decodeSelectionRequest buf = XEvt.SelectionRequestXEvt {
          time = getTS(buf, 4),
          owner = getXId(buf, 8),
          requestor = getXId(buf, 12),
          selection = getXAtom(buf, 16),
          target = getXAtom(buf, 20),
          property = getXAtomOption(buf, 24)
        }
      fun decodeSelectionNotify buf = XEvt.SelectionNotifyXEvt {
          time = getTS(buf, 4),
          requestor = getXId(buf, 8),
          selection = getXAtom(buf, 12),
          target = getXAtom(buf, 16),
          property = getXAtomOption(buf, 20)
        }
      fun decodeColormapNotify buf = XEvt.ColormapNotifyXEvt {
          window = getXId(buf, 4),
          cmap = getXIdOption(buf, 8),
          new = getBool(buf, 12),
          installed = getBool(buf, 13)
        }
      fun decodeClientMessage buf =
      (XEvt.ClientMessageXEvt {
          window = getXId(buf, 4),
          typ = getXAtom(buf, 8),
          value = XTy.RAW_DATA {
          format = getRawFormat(buf, 1),
          data = w8vextract(buf, 12, SOME 20)
        }
        })
      fun decodeMappingNotify buf = (case W8V.sub(buf, 4)
       of 0w0 => XEvt.ModifierMappingNotifyXEvt
        | 0w1 => XEvt.KeyboardMappingNotifyXEvt {
              first_keycode = getKeyCode(buf, 5),
              count = getInt8(buf, 6)
            }
        | 0w2 => XEvt.PointerMappingNotifyXEvt
        | _ => MLXError.impossible "bad MappingNotify")
    in
    fun decodeXEvent (code : Word8.word, buf) = let
      val n = W8.andb(code, 0wx7f)
      val xevt = case n
           of 0w2 => XEvt.KeyPressXEvt (getKeyXEvt buf)
        | 0w3 => XEvt.KeyReleaseXEvt (getKeyXEvt buf)
        | 0w4 => XEvt.ButtonPressXEvt (getButtonXEvt buf)
        | 0w5 => XEvt.ButtonReleaseXEvt (getButtonXEvt buf)
        | 0w6 => decodeMotionNotify buf
        | 0w7 => XEvt.EnterNotifyXEvt (getEnterLeaveXEvt buf)
        | 0w8 => XEvt.LeaveNotifyXEvt (getEnterLeaveXEvt buf)
        | 0w9 => XEvt.FocusInXEvt (getFocusXEvt buf)
        | 0w10 => XEvt.FocusOutXEvt (getFocusXEvt buf)
        | 0w11 => decodeKeymapNotify buf
        | 0w12 => decodeExpose buf
        | 0w13 => decodeGraphicsExpose buf
        | 0w14 => decodeNoExpose buf
        | 0w15 => decodeVisibilityNotify buf
        | 0w16 => decodeCreateNotify buf
        | 0w17 => decodeDestroyNotify buf
        | 0w18 => decodeUnmapNotify buf
        | 0w19 => decodeMapNotify buf
        | 0w20 => decodeMapRequest buf
        | 0w21 => decodeReparentNotify buf
        | 0w22 => decodeConfigureNotify buf
        | 0w23 => decodeConfigureRequest buf
        | 0w24 => decodeGravityNotify buf
        | 0w25 => decodeResizeRequest buf
        | 0w26 => decodeCirculateNotify buf
        | 0w27 => decodeCirculateRequest buf
        | 0w28 => decodePropertyNotify buf
        | 0w29 => decodeSelectionClear buf
        | 0w30 => decodeSelectionRequest buf
        | 0w31 => decodeSelectionNotify buf
        | 0w32 => decodeColormapNotify buf
        | 0w33 => decodeClientMessage buf
        | 0w34 => decodeMappingNotify buf
        | _ => MLXError.impossible "bad event code"
      in
        (code = n, xevt)
      end
  (* we export the decode functions for reporting graphics exposures *)
    val decodeGraphicsExpose = decodeGraphicsExpose
    val decodeNoExpose = decodeNoExpose
    end (* local *)


  (** decode error messages **)
      local
    structure XErr = XErrors
    fun get_err (kind, buf) = XErr.XErr{
        kind = kind,
        minor_op = getWord16(buf, 8),
        major_op = W8V.sub(buf, 10)
          }
      in
      fun decodeError buf = (case (W8V.sub (buf, 1))
       of 0w1 => get_err (XErr.BadRequest, buf)
        | 0w2 => get_err (XErr.BadValue(getString(buf, 4, 4)), buf)
        | 0w3 => get_err (XErr.BadWindow(getXId(buf, 4)), buf)
        | 0w4 => get_err (XErr.BadPixmap(getXId(buf, 4)), buf)
        | 0w5 => get_err (XErr.BadAtom(getXId(buf, 4)), buf)
        | 0w6 => get_err (XErr.BadCursor(getXId(buf, 4)), buf)
        | 0w7 => get_err (XErr.BadFont(getXId(buf, 4)), buf)
        | 0w8 => get_err (XErr.BadMatch, buf)
        | 0w9 => get_err (XErr.BadDrawable(getXId(buf, 4)), buf)
        | 0w10 => get_err (XErr.BadAccess, buf)
        | 0w11 => get_err (XErr.BadAlloc, buf)
        | 0w12 => get_err (XErr.BadColor(getXId(buf, 4)), buf)
        | 0w13 => get_err (XErr.BadGC(getXId(buf, 4)), buf)
        | 0w14 => get_err (XErr.BadIDChoice(getXId(buf, 4)), buf)
        | 0w15 => get_err (XErr.BadName, buf)
        | 0w16 => get_err (XErr.BadLength, buf)
        | 0w17 => get_err (XErr.BadImplementation, buf)
        | _ => MLXError.impossible "bad error number")
      end (* local *)


  (** decode reply messages **)

    fun decodeGetWindowAttributesReply msg = {
        backing_store = getBS(msg, 1),
        visual = getVisualId(msg, 8),
        input_only = (case get16(msg, 12)
           of 0w1 => false | 0w2 => true
        | _ => MLXError.impossible "bad GetWindowAttributes reply"
          (* end case *)),
        bit_gravity = getBitGravity(msg, 14),
        win_gravity = getWinGravity(msg, 15),
        backing_planes = XTy.PLANEMASK(getWord(msg, 16)),
        backing_pixel = getPixel(msg,20),
        save_under = getBool(msg, 24),
        map_is_installed = getBool(msg, 25),
        map_state = (case W8V.sub(msg, 26)
           of 0w0 => XTy.WinIsUnmapped
        | 0w1 => XTy.WinIsUnviewable
        | 0w2 => XTy.WinIsViewable
        | _ => MLXError.impossible "bad GetWindowAttributes reply"
          (* end case *)),
        override_redirect = getBool(msg, 27),
        colormap = getXIdOption(msg, 28),
        all_event_mask = getEventMask(msg, 32),
        event_mask = getEventMask(msg, 36),
        do_not_propagate = getEventMask(msg, 40)
      }

    fun decodeAllocColorCellsReply msg = {
        err = MLXError.impossible "unimplemented" (*** FIX ***)
      }
    fun decodeAllocColorPlanesReply msg = {
        err = MLXError.impossible "unimplemented" (*** FIX ***)
      }

    fun decodeAllocColorReply msg = {
        visual_rgb = getRGB(msg, 8),
        pixel = getPixel(msg, 16)
      }

    fun decodeAllocNamedColorReply msg = {
        pixel = getPixel(msg, 8),
        exact_rgb = getRGB(msg, 12),
        visual_rgb = getRGB(msg, 18)
      }

    fun decodeGetAtomNameReply msg = getString(msg, 32, getInt16(msg, 8))

    fun decodeGetFontPathReply msg = getStringList (msg, 32, getInt16(msg, 8))

    fun decodeGetGeometryReply msg = {
        depth = getInt8(msg, 1),
        root = getXId(msg, 8),
        geom = getWGeom(msg, 12)
      }

    fun decodeGetImageReply msg = {
        depth = getInt8(msg, 1),
        visualid = getVisualIdOption(msg, 8),
        data = w8vextract(msg, 32, SOME(4*getInt(msg, 4)))
      }

    fun decodeGetInputFocusReply msg = {
        revert_to = (case W8V.sub(msg, 1)
           of 0w0 => XTy.RevertToNone
        | 0w1 => XTy.RevertToPointerRoot
        | _ => XTy.RevertToParent
          (* end case *)),
        focus = (case getWord(msg, 8)
           of 0w0 => XTy.InputFocus_None
        | 0w1 => XTy.InputFocus_PointerRoot
        | w => XTy.InputFocus_Window(XTy.XID w)
          (* end case *))
      }

    fun decodeGetKeyboardControlReply msg = {
        glob_auto_repeat = getBool(msg, 1),
        led_mask = get32(msg, 8),
        key_click_pct = getInt8(msg, 12),
        bell_pct = getInt8(msg, 13),
        bell_pitch = getInt16(msg, 14),
        bell_duration = getInt16(msg, 16),
        auto_repeats = w8vextract(msg, 20, SOME 32)
      }

    fun decodeGetKeyboardMappingReply msg = let
      val symsPerCode = getInt8(msg, 1)
      val nKeyCodes = getInt(msg, 4) div symsPerCode
    (* get the keysyms bound to a given keycode;  Discard trailing NoSymbols,
     * but include intermediate ones. *)
      fun cleanTl (XTy.NoSymbol :: r) = cleanTl r
        | cleanTl l = rev l
      fun getSyms (i, 0, l) = cleanTl l
        | getSyms (i, j, l) = (case getInt(msg, i)
           of 0 => getSyms(i+4, j-1, XTy.NoSymbol :: l)
        | k => getSyms(i+4, j-1, (XTy.KEYSYM k) :: l))
      in
        getList (fn (_, i) => getSyms(i, symsPerCode, []), symsPerCode*4)
          (msg, 32, nKeyCodes)
          end

    fun decodeGetModifierMappingReply msg = let
      val codesPerMod = getInt8(msg, 1)
      fun getSyms k = let
        fun get (i, 0) = []
          | get (i, j) = (case getInt8(msg, i)
               of 0 => get(i+1, j-1)    (* 0 == unused *)
                | k => (XTy.KEYCODE k) :: get(i+1, j-1)
              (* end case *))
        in
          get (32 + codesPerMod*k, codesPerMod)
        end
      in {
        shift_keycodes = getSyms 0,
        lock_keycodes = getSyms 1,
        cntl_keycodes = getSyms 2,
        mod1_keycodes = getSyms 3,
        mod2_keycodes = getSyms 4,
        mod3_keycodes = getSyms 5,
        mod4_keycodes = getSyms 6,
        mod5_keycodes = getSyms 7
          } end

    local
      val getEvts = getList
        ((fn (buf, i) => { time = getTime(buf, i), coord = getPt(buf, i+4) }), 8)
    in
    fun decodeGetMotionEventsReply msg = getEvts (msg, 32, getInt16(msg, 8))
    end (* local *)

    fun decodeGetPointerControlReply msg = {
        acceleration_numerator = get16(msg, 8),
        acceleration_denominator = get16(msg, 10),
        threshold = get16(msg, 12)
      }

    fun decodeGetPointerMappingReply msg = {
        err = MLXError.impossible "unimplemented" (*** FIX ***)
      }

    fun decodeGetPropertyReply msg = (case getWord(msg, 8)
       of 0w0 => NONE
        | t => let
        val nitems = getInt(msg, 16)
        val (fmt, nbytes) = (case W8V.sub(msg, 1)
             of 0w8 => (XTy.Raw8, nitems)
              | 0w16 => (XTy.Raw16, 2*nitems)
              | 0w32 => (XTy.Raw32, 4*nitems)
              | _ => MLXError.impossible "bad GetProperty reply"
            (* end case *))
        in
          SOME {
              typ = XTy.XAtom t,
              bytes_after = getInt(msg, 12),
              value = XTy.RAW_DATA {
              format = fmt,
              data = w8vextract(msg, 32, SOME nbytes)
            }
            }
        end)

    fun decodeGetScreenSaverReply msg = {
        timeout = get16(msg, 8),
        interval = get16(msg, 10),
        prefer_blanking = getBool(msg, 12),
        allow_exposures = getBool(msg, 13)
      }

    fun decodeGetSelectionOwnerReply msg = getXIdOption(msg, 8)

    local
      fun decodeGrabReply msg = (case W8V.sub(msg, 1)
         of 0w0 => XTy.GrabSuccess | 0w1 => XTy.AlreadyGrabbed
          | 0w2 => XTy.GrabInvalidTime | 0w3 => XTy.GrabNotViewable
          | _ => XTy.GrabFrozen
        (* end case *))
    in
    val decodeGrabKeyboardReply = decodeGrabReply
    val decodeGrabPointerReply = decodeGrabReply
    end (* local *)

    fun decodeInternAtomReply msg = getXAtom(msg, 8)

    fun decodeListExtensionsReply msg = {
        err = MLXError.impossible "unimplemented" (*** FIX ***)
      }

    fun decodeListFontsReply msg = getStringList (msg, 32, getInt16(msg, 8))

    local
      fun getHostList (buf, n) = let
        fun get (_, 0, l) = l
          | get (i, n, l) = let
          val addrLen = getInt16(buf, i+2)
          val addr = getString(buf, i+4, addrLen)
          val host = case W8V.sub(buf, i)
             of 0w0 => XTy.InternetHost addr
              | 0w1 => XTy.DECnetHost addr
              | 0w2 => XTy.ChaosHost addr
              | _ => raise (MLXError.xerror "unknown host family"
            (* end case *))
          in
            get (i+(pad addrLen)+4, n-1, host::l)
          end
        in
          get (32, n, [])
        end
    in
    fun decodeListHostsReply msg = {
        enabled = getBool(msg, 1),
        hosts = getHostList(msg, getInt16(msg, 8))
      }
    end (* local *)

    fun decodeListInstalledColormapsReply msg = getXIdList(msg, 32, getInt16(msg, 8))

    fun decodeListPropertiesReply msg = getXAtomList(msg, 32, getInt16(msg, 8))

    fun decodeLookupColorReply msg = {
        exact_rgb = getRGB(msg, 8),
        visual_rgb = getRGB(msg, 14)
      }

    fun decodeQueryBestSizeReply msg = {
        wid = getInt16(msg, 8),
        ht = getInt16(msg, 10)
      }

    local
      val getRGBList = getList (getRGB, 8)
    in
    fun decodeQueryColorsReply msg = getRGBList(msg, 32, getInt16(msg, 8))
    end (* local *)

    fun decodeQueryExtensionReply msg = {
        err = MLXError.impossible "unimplemented" (*** FIX ***)
      }

    local
      val getProps = getList ((fn (buf, i) => XTy.FontProp{
        name = getXAtom(buf, i),
        value = get32(buf, i+4)
          }), 8)
      fun getCharInfo (buf, i) = XTy.CharInfo{
        left_bearing = getSigned16(buf, i),
        right_bearing = getSigned16(buf, i+2),
        char_wid = getSigned16(buf, i+4),
        ascent = getSigned16(buf, i+6),
        descent = getSigned16(buf, i+8),
        attributes = getWord16(buf, i+10)
          }
      val getCharInfoList = getList (getCharInfo, 12)
      fun getInfo buf = let
        val n_props = getInt16(buf, 46)
        in {
          min_bounds = getCharInfo(buf, 8),
          max_bounds = getCharInfo(buf, 24),
          min_char = getInt16(buf, 40),
          max_char = getInt16(buf, 42),
          default_char = getInt16(buf, 44),
          draw_dir = getFontDir(buf, 48),
          min_byte1 = getInt8(buf, 49),
          max_byte1 = getInt8(buf, 50),
          all_chars_exist = getBool(buf, 51),
          font_ascent = getInt16(buf, 52),
          font_descent = getInt16(buf, 54),
          n_props = n_props,
          properties = getProps(buf, 60, n_props)
        } end
    in
(****** THIS GENERATES MULTIPLE REPLYS ****
  (* this gets a list of font name/info replies *)
    fun decodeListFontsWithInfoReply msg = let
      fun getList l = let
        val (msg, extra) = getReply (conn, sizeOfListFontsWithInfoReply)
        val name_len = get8(msg, 1)
        in
          if (name_len = 0)
        then (* this is the last in a series of replies *)
          (rev l)
        else let
          val info = getInfo(msg, extra)
          val reply = {
              min_bounds = #min_bounds info,
              max_bounds = #max_bounds info,
              min_char = #min_char info,
              max_char = #max_char info,
              default_char = #default_char info,
              draw_dir = #draw_dir info,
              min_byte1 = #min_byte1 info,
              max_byte1 = #max_byte1 info,
              all_chars_exist = #all_chars_exist info,
              font_ascent = #font_ascent info,
              font_descent = #font_descent info,
              replies_hint = get32(msg, 56),
              properties = #properties info,
              name = getString(extra, 8*(#n_props info), name_len)
            }
          in
            getList (reply :: l)
          end
        end (* getList *)
      in
        getList []
      end (* getListFontsWithInfoReply *)
*********)

    fun decodeQueryFontReply msg = let
      val info = getInfo msg
      in {
        min_bounds = #min_bounds info,
        max_bounds = #max_bounds info,
        min_char = #min_char info,
        max_char = #max_char info,
        default_char = #default_char info,
        draw_dir = #draw_dir info,
        min_byte1 = #min_byte1 info,
        max_byte1 = #max_byte1 info,
        all_chars_exist = #all_chars_exist info,
        font_ascent = #font_ascent info,
        font_descent = #font_descent info,
        properties = #properties info,
        char_infos = getCharInfoList(msg, 60+8*(#n_props info), getInt(msg, 56))
      } end
    end (* local *)

    fun decodeQueryKeymapReply msg = {
        err = MLXError.impossible "unimplemented" (*** FIX ***)
      }

    fun decodeQueryPointerReply msg = let val (mks, mbs) = getKeyButSet(msg, 24)
      in {
        same_scr = getBool(msg, 1),
        root = getXId(msg, 8),
        child = getXIdOption(msg, 12),
        root_pt = getPt(msg, 16),
        win_pt = getPt(msg, 20),
        mod_state = mks,
        mbut_state = mbs
      } end

    fun decodeQueryTextExtentsReply msg = {
        draw_dir = getFontDir(msg, 1),
        font_ascent = get16(msg, 8),
        font_descent = get16(msg, 10),
        overall_ascent = get16(msg, 12),
        overall_descent = get16(msg, 14),
        overall_wid = get16(msg, 16),
        overall_left = get16(msg, 18),
        overall_right = get16(msg, 20)
      }

    fun decodeQueryTreeReply msg = {
        root = getXId(msg, 8),
        parent = getXIdOption(msg, 12),
        children = getXIdList (msg, 32, getInt16(msg, 16))
      }

    local
      fun getSetMappingReply msg = (case get8(msg, 1)
         of 0w0 => XTy.MappingSuccess
          | 0w1 => XTy.MappingBusy
          | _ => XTy.MappingFailed
        (* end case *))
    in
    val decodeSetModifierMappingReply = getSetMappingReply
    val decodeSetPointerMappingReply = getSetMappingReply
    end (* local *)

    fun decodeTranslateCoordsReply msg = {
        child = getXIdOption(msg, 8),
        dst_pt = getPt(msg, 12)
      }

    end (* local open XTypes *)

  end (* XReply *)
