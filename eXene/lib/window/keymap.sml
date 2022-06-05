(* keymap.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
 * and the Massachusetts Institute of Technology, Cambridge, Massachusetts.
 *
 * This mystery code was derived from the MIT Xlib implementation.  The following
 * description of the keycode to keysym translation is lifted from the X11 protocol
 * definition:
 *
 * A KEYCODE represents a physical (or logical) key.  Keycodes lie in the
 * inclusive range [8,255].  A keycode value carries no intrinsic information,
 * although server implementors may attempt to encode geometry information
 * (for example, matrix) to be interpreted in a server-dependent fashion.  The
 * mapping between keys and keycodes cannot be changed using the protocol.
 * 
 * A KEYSYM is an encoding of a symbol on the cap of a key.  The set of defined
 * KEYSYMs include the character sets Latin 1, Latin 2, Latin 3, Latin 4, Kana,
 * Arabic, Cryllic, Greek, Tech, Special, Publish, APL, and Hebrew as well as a
 * set of symbols common on keyboards (Return, Help, Tab, and so on).  KEYSYMs
 * with the most-significant bit (of the 29 bits) set are reserved as
 * vendor-specific.
 * 
 * A list of KEYSYMs is associated with each KEYCODE.  The list is intended to
 * convey the set of symbols on the corresponding key.  If the list (ignoring
 * trailing NoSymbol entries) is a single KEYSYM ``[K],'' then the list is
 * treated as if it were the list ``[K, NoSymbol, K, NoSymbol].''  If the list
 * (ignoring trailing NoSymbol entries) is a pair of KEYSYMs ``[K1, K2]'',
 * then the list is treated as if it were the list ``[K1, K2, K1, K2]''.  If
 * the list (ignoring trailing NoSymbol entries) is a triple of KEYSYMs
 * ``[K1, K2, K3]'', then the list is treated as if it were the list
 * ``[K1, K2, K3, NoSymbol]''.  When an explicit ``void'' element is desired
 * in the list, the value VoidSymbol can be used.
 * 
 * The first four elements of the list are split into two groups of KEYSYMs.
 * Group 1 contains the first and second KEYSYMs, Group 2 contains third and
 * fourth KEYSYMs.  Within each group, if the second element of the group is
 * NoSymbol, then the group should be treated as if the second element were the
 * same as the first element, except when the first element is an alphabetic
 * KEYSYM ``K'' for which both lowercase and uppercase forms are defined. In
 * that case, the group should be treated as if the first element were the
 * lowercase form of ``K'' and the second element were the uppercase form
 * of ``K''.
 * 
 * The standard rules for obtaining a KEYSYM from a KeyPress event make use of
 * only the Group 1 and Group 2 KEYSYMs; no interpretation of other KEYSYMs in
 * the list is given here.  Which group to use is determined by modifier state.
 * Switching between groups is controlled by the KEYSYM named MODE SWITCH, by
 * attaching that KEYSYM to some KEYCODE and attaching that KEYCODE to any one
 * of the modifiers Mod1 through Mod5.  This modifier is called the ``group
 * modifier''.  For any KEYCODE, Group 1 is used when the group modifier is
 * off, and Group 2 is used when the group modifier is on.
 * 
 * Within a group, which KEYSYM to use is also determined by modifier state.  The
 * first KEYSYM is used when the Shift and Lock modifiers are off.  The second
 * KEYSYM is used when the Shift modifier is on, or when the Lock modifier is on
 * and the second KEYSYM is uppercase alphabetic, or when the Lock modifier is on
 * and is interpreted as ShiftLock.  Otherwise, when the Lock modifier is on and
 * is interpreted as CapsLock, the state of the Shift modifier is applied first
 * to select a KEYSYM, but if that KEYSYM is lowercase alphabetic, then the
 * corresponding uppercase KEYSYM is used instead.
 * 
 * The KEYMASK modifier named Lock is intended to be mapped to either a CapsLock
 * or a ShiftLock key, but which one is left as application-specific and/or
 * user-specific.  However, it is suggested that the determination be made
 * according to the associated KEYSYM(s) of the corresponding KEYCODE.
 *
 * NOTE: XReply.decodeGetKeyboardMappingReply removes trailing NoSymbol entries.
 *)

signature KEYMAP =
  sig
    type keymap
    val createKeymap : XDisplay.xdisplay -> keymap
    val refreshKeymap : keymap -> unit
    val lookupKeysym : keymap -> XEventTypes.key_xevtinfo
	  -> (Keysym.keysym * KeyBut.modkey_state)
  end (* KEYMAP *)

structure Keymap : KEYMAP =
  struct

    structure XTy = XProtTypes

    val & = Word.andb and ++ = Word.orb
    infix & ++

    fun query (encode, decode) conn = let
	  val requestReply = XIo.requestReply conn
	  in
	    fn req => decode (CML.sync (requestReply (encode req)))
	  end
    val getKeyboardMapping = query
	  (XRequest.encodeGetKeyboardMapping, XReply.decodeGetKeyboardMappingReply)
    val getModifierMapping = query
	  (fn () => XRequest.requestGetModifierMapping,
	    XReply.decodeGetModifierMappingReply)

    (* Keycode to keysym map *)
      abstype keycode_map = KM of (int * XTy.keysym list array)
      with
	fun newKCMap (XDisplay.XDPY info) = let
	      val (minKeycode as (XTy.KEYCODE minKC)) = #min_keycode info
	      val (XTy.KEYCODE maxKC) = #max_keycode info
	      val kbdMap = getKeyboardMapping (#conn info)
		    {first=minKeycode, count=((maxKC - minKC) + 1)}
	      in
		KM(minKC, Array.fromList kbdMap)
	      end
      (* NOTE: some X servers generate bogus keycodes on occasion *)
	fun lookupKC (KM(minKC, a)) (XTy.KEYCODE kc) =
	      (Array.sub(a, kc - minKC) handle Subscript => [])
      end

    (* the meaning of the Lock modifier key *)
      datatype lock_meaning = NoLock | LockShift | LockCaps

    (* the shifting mode of a key-button state *)
      datatype shift_mode = Unshifted | Shifted | CapsLocked of bool

      datatype mapping = MAP of {
	  lookup	    : XTy.keycode -> XTy.keysym list,
	  is_mode_switched  : XTy.modkey_state -> bool,
	  shift_mode	    : XTy.modkey_state -> shift_mode
	}
	  
    (* Return the upper-case and lower-case keysyms for the given keysym *)
      fun convertCase (XTy.KEYSYM sym) = let
	    in
	      case (Word.fromInt sym & 0wxFF00)
	       of 0w0 => ( (* Latin1 *)
		  if ((0x41 <= sym) andalso (sym <= 0x5A))	(* A..Z *)
		    then (XTy.KEYSYM(sym + (0x61-0x41)), XTy.KEYSYM sym)
		  else if ((0x61 <= sym) andalso (sym <= 0x7a))	(* a..z *)
		    then (XTy.KEYSYM sym, XTy.KEYSYM(sym - (0x61-0x41)))
		  else if ((0xC0 <= sym) andalso (sym <= 0xD6))	(* Agrave..Odiaeresis*)
		    then (XTy.KEYSYM(sym + (0xE0-0xC0)), XTy.KEYSYM sym)
		  else if ((0xE0 <= sym) andalso (sym <= 0xF6))	(* agrave..odiaeresis*)
		    then (XTy.KEYSYM sym, XTy.KEYSYM(sym - (0xE0-0xC0)))
		  else if ((0xD8 <= sym) andalso (sym <= 0xDE))	(* Ooblique..Thorn*)
		    then (XTy.KEYSYM(sym + (0xD8-0xF8)), XTy.KEYSYM sym)
		  else if ((0xF8 <= sym) andalso (sym <= 0xFE))	(* oslash..thorn*)
		    then (XTy.KEYSYM sym, XTy.KEYSYM(sym - (0xD8-0xF8)))
		    else (XTy.KEYSYM sym, XTy.KEYSYM sym))
		| _ => (XTy.KEYSYM sym, XTy.KEYSYM sym)
	    end
      val lowerCase = #1 o convertCase
      val upperCase = #2 o convertCase

    (* Return the shift-mode defined by a list of modifiers with respect to the
     * given lock meaning *)
      fun shiftMode lockMeaning state = (
	    case (KeyBut.shiftIsSet state, KeyBut.lockIsSet state, lockMeaning)
	     of (false, false, _) => Unshifted
	      | (false, true, NoLock) => Unshifted
	      | (false, true, LockShift) => Shifted
	      | (true, true, NoLock) => Shifted
	      | (true, false, _) => Shifted
	      | (shift, _, _) => (CapsLocked shift))

    (* translate a keycode plus modifier-state to a keysym *)
      fun keycodeToKeysym (MAP{lookup, is_mode_switched, shift_mode}) (kc, state) = let
	  (* if there are more than two keysyms for the keycode and the shift mode
	   * is switched, then discard the first two keysyms *)
	    val syms = (case (lookup kc, is_mode_switched state)
		  of (_::_::(r as _::_), true) => r
		   | (l, _) => l)
	    val sym = (case (syms, shift_mode state)
		 of ([], _) => XTy.NoSymbol
		  | ([ks], Unshifted) => lowerCase ks
		  | (ks::_, Unshifted) => ks
		  | ([ks], Shifted) => upperCase ks
		  | (_::ks::_, Shifted) => ks
		  | ([ks], CapsLocked _) => upperCase ks
		  | (lks::uks::_, CapsLocked shift) => let
		      val (lsym, usym) = convertCase uks
		      in
			if (shift orelse ((uks = usym) andalso (lsym <> usym)))
			  then usym
			  else upperCase lks
		      end)
	    in
	      if (sym = Keysym.voidSymbol) then XTy.NoSymbol else sym
	    end (* keycodeToKeysym *)

    (* Get the display's modifier mapping, and analyze it to set
     * the lock semantics and which modes translate into switched mode.
     *)
      fun createMap (dpy as XDisplay.XDPY{conn, ...}) = let
	    val modMap = getModifierMapping conn ()
	    val lookup = lookupKC (newKCMap dpy)
	  (* get the lock meaning, which will be LockCaps, if any lock key contains
	   * the CAPS_LOCK keysym (KEYSYM 0xFFE5); otherwise it will be LockShift,
	   * if any lock key contains the SHIFT_LOCK keysym (KEYSYM 0xFFE6); otherwise
	   * it will be NoLock.
	   *)
	    val lockMeaning = let
		  fun find ([], [], meaning) = meaning
		    | find (kc :: r, [], meaning) = find (r, lookup kc, meaning)
		    | find (kcl, (XTy.KEYSYM 0xFFE5)::_, _) = LockCaps
		    | find (kcl, (XTy.KEYSYM 0xFFE6)::r, _) = find(kcl, r, LockShift)
		    | find (kcl, _::r, meaning) = find(kcl, r, meaning)
		  in
		    find (#lock_keycodes modMap, [], NoLock)
		  end
	  (* compute a bit-vector with a 1 in bit-i if one of ModKey[i+1] keycodes
	   * has the Mode_switch keysym (KEYSYM 0xFF7E) in its keysym list.
	   *)
	    val switchMode = let
		  fun isModeSwitch [] = false
		    | isModeSwitch ((XTy.KEYSYM 0xFF7E) :: _) = true
		    | isModeSwitch (_::r) = isModeSwitch r
		  val chkKC = List.exists (fn kc => isModeSwitch(lookup kc))
		  val keys = if chkKC(#mod1_keycodes modMap)
			then [XTy.Mod1Key] else []
		  val keys = if chkKC(#mod2_keycodes modMap)
			then (XTy.Mod2Key::keys) else keys
		  val keys = if chkKC(#mod3_keycodes modMap)
			then (XTy.Mod3Key::keys) else keys
		  val keys = if chkKC(#mod4_keycodes modMap)
			then (XTy.Mod4Key::keys) else keys
		  val keys = if chkKC(#mod5_keycodes modMap)
			then (XTy.Mod5Key::keys) else keys
		  in
		    KeyBut.mkModState keys
		  end
	    fun switchFn state =
		  not (KeyBut.emptyMod (KeyBut.intersectMod (state, switchMode)))
	    in
	      MAP{
		  lookup = lookup,
		  shift_mode = shiftMode lockMeaning,
		  is_mode_switched = switchFn
		}
	    end (* createMap *)

    datatype req = Refresh | Lookup of (XTy.keycode * XTy.modkey_state)
    datatype keymap = KM of {
	req_ch : req CML.chan,
	reply_ch : XTy.keysym CML.chan
      }

  (* create the keymap server for the display connection *)
    fun createKeymap (dpy as XDisplay.XDPY{conn, ...}) = let
	  val reqCh = CML.channel() and replyCh = CML.channel()
	  fun server () = let
		fun loop codeToSym = loop (
			case (CML.recv reqCh)
			 of Refresh => (keycodeToKeysym (createMap dpy))
			  | Lookup arg => (CML.send(replyCh, codeToSym arg); codeToSym)
		      )
		in
		  loop (keycodeToKeysym (createMap dpy))
		end
	  in
	    XDebug.xspawn ("KeymapServer", server);
	    KM {req_ch = reqCh, reply_ch = replyCh}
	  end (* createKeymap *)

    fun refreshKeymap (KM{req_ch, ...}) = CML.send(req_ch, Refresh)

    fun lookupKeysym (KM{req_ch, reply_ch}) ({
	  keycode, mod_state, ...
	} : XEventTypes.key_xevtinfo) = (
	  CML.send(req_ch, Lookup(keycode, mod_state));
	  (CML.recv reply_ch, mod_state))

  end (* Keymap *)
