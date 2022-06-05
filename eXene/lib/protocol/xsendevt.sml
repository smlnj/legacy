(* xsendevt.sml
 *
 * COPYRIGHT (c) 1992 by AT&T.  See COPYRIGHT file for details.
 *
 * Functions to encode SendEvent messages.
 *)

structure XSendEvent =
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

      val encodeSendEvent = XRequest.encodeSendEvent
      val eventOffset = 12

      fun put8 (buf, i, w)= W8A.update(v2a buf, i+eventOffset, w)
      fun putSigned8 (buf, i, x) = put8(buf, i, Word8.fromInt x)

      fun put16 (buf, i, x) =
	    PackWord16Big.update(v2a buf, i div 2 + eventOffset div 2, x)
      fun putSigned16 (buf, i, x) = put16(buf, i, LargeWord.fromInt x)

      fun put32 (buf, i, x) =
	    PackWord32Big.update(v2a buf, i div 4 + eventOffset div 4, x)
      fun putWord32 (buf, i, x) = put32(buf, i, Word.toLargeWord x)
      fun putSigned32 (buf, i, x) = put32(buf, i, LargeWord.fromInt x)

      fun putBool (buf, i, false) = put8 (buf, i, 0w0)
	| putBool (buf, i, true) = put8 (buf, i, 0w1)

      fun putXId (buf, i, XTy.XID n) = putWord32 (buf, i, n)
      fun putXIdOption (buf, i, NONE) = putWord32 (buf, i, 0w0)
	| putXIdOption (buf, i, SOME(XTy.XID n)) =  putWord32 (buf, i, n)

      fun putAtom (buf, i, XTy.XAtom n) = putWord32 (buf, i, n)
      fun putAtomOption (buf, i, NONE) = putWord32 (buf, i, 0w0)
	| putAtomOption (buf, i, SOME(XTy.XAtom n)) = putWord32 (buf, i, n)

      fun putTS (buf, i, XTy.CurrentTime) = put32(buf, i, 0w0)
	| putTS (buf, i, XTy.TimeStamp(XTime.XT t)) = put32(buf, i, Word32.toLarge t)

    (* event codes *)
      val evtKeyPressXEvt	= 0w2 : Word8.word
      val evtKeyReleaseXEvt	= 0w3 : Word8.word
      val evtButtonPressXEvt	= 0w4 : Word8.word
      val evtButtonReleaseXEvt	= 0w5 : Word8.word
      val evtdecodeMotionNotify	= 0w6 : Word8.word
      val evtEnterNotifyXEvt	= 0w7 : Word8.word
      val evtLeaveNotifyXEvt	= 0w8 : Word8.word
      val evtFocusInXEvt	= 0w9 : Word8.word
      val evtFocusOutXEvt	= 0w10 : Word8.word
      val evtKeymapNotify	= 0w11 : Word8.word
      val evtExpose		= 0w12 : Word8.word
      val evtGraphicsExpose	= 0w13 : Word8.word
      val evtNoExpose		= 0w14 : Word8.word
      val evtVisibilityNotify	= 0w15 : Word8.word
      val evtCreateNotify	= 0w16 : Word8.word
      val evtDestroyNotify	= 0w17 : Word8.word
      val evtUnmapNotify	= 0w18 : Word8.word
      val evtMapNotify		= 0w19 : Word8.word
      val evtMapRequest		= 0w20 : Word8.word
      val evtReparentNotify	= 0w21 : Word8.word
      val evtConfigureNotify	= 0w22 : Word8.word
      val evtConfigureRequest	= 0w23 : Word8.word
      val evtGravityNotify	= 0w24 : Word8.word
      val evtResizeRequest	= 0w25 : Word8.word
      val evtCirculateNotify	= 0w26 : Word8.word
      val evtCirculateRequest	= 0w27 : Word8.word
      val evtPropertyNotify	= 0w28 : Word8.word
      val evtSelectionClear	= 0w29 : Word8.word
      val evtSelectionRequest	= 0w30 : Word8.word
      val evtSelectionNotify	= 0w31 : Word8.word
      val evtColormapNotify	= 0w32 : Word8.word
      val evtClientMessage	= 0w33 : Word8.word
      val evtMappingNotify	= 0w34 : Word8.word

      fun putEventCode (msg, code) = put8(msg, 0, code)

    in

    fun encodeSendSelectionNotify
	{ dst, propagate, evt_mask, requestor, selection, target, property, time } = let
	  val msg = encodeSendEvent {
			dst = dst, propagate = propagate, evt_mask = evt_mask
		      }
	  in
	    putEventCode (msg, evtSelectionNotify);
	    putTS (msg, 4, time);
	    putXId (msg, 8, requestor);
	    putAtom (msg, 12, selection);
	    putAtom (msg, 16, target);
	    putAtomOption (msg, 20, property);
	    msg
	  end

    fun encodeSendUnmapNotify
	{ dst, propagate, evt_mask, event, window, from_configure } = let
	  val msg = encodeSendEvent {
			dst = dst, propagate = propagate, evt_mask = evt_mask
		      }
	  in
	    putEventCode (msg, evtUnmapNotify);
	    putXId (msg, 4, event);
	    putXId (msg, 8, window);
	    putBool (msg, 12, from_configure);
	    msg
	  end

    end (* local *)
  end (* XSendEvent *)
