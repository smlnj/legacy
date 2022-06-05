(* xprops.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Support for the standard X properties and types as defined in
 * version 1.0 of the ICCCM.  These routines can be used to build
 * various property values (including the standard ones).
 *)

structure XProps : sig

  (* Hints about the window size *)
    datatype size_hints
      = HINT_USPosition
      | HINT_PPosition
      | HINT_USSize
      | HINT_PSize
      | HINT_PMinSize of Geometry.size
      | HINT_PMaxSize of Geometry.size
      | HINT_PResizeInc of Geometry.size
      | HINT_PAspect of { min : (int * int), max : (int * int) }
      | HINT_PBaseSize of Geometry.size
      | HINT_PWinGravity of XProtTypes.gravity

  (* Window manager hints *)
    datatype wm_hints
      = HINT_Input of bool		    (* does this application rely on the *)
					    (* window manager to get keyboard input? *)
					    (* Initial window state (choose one) *)
      | HINT_WithdrawnState		      (* for windows that are not mapped *)
      | HINT_NormalState		      (* most applications want to start *)
					      (* this way *)
      | HINT_IconicState		      (* application wants to start as an *)
					      (* icon *)
      | HINT_IconTile of DrawTypes.tile	    (* tile to be used as icon *)
      | HINT_IconPixmap of DrawTypes.pixmap (* pixmap to be used as icon *)
      | HINT_IconWindow of DrawTypes.window (* window to be used as icon *)
      | HINT_IconMask of DrawTypes.pixmap   (* icon mask bitmap *)
      | HINT_IconPosition of Geometry.point (* initial position of icon *)
      | HINT_WindowGroup of DrawTypes.window(* the group leader *)

    val makeStringProp : string -> XProtTypes.prop_val
	(* Build a property value of type STRING *)

    val makeAtomProp : XProtTypes.atom -> XProtTypes.prop_val
	(* Build a property value of type ATOM *)

    val makeWMSizeHints : size_hints list -> XProtTypes.prop_val
    val makeWMHints : wm_hints list -> XProtTypes.prop_val

    val makeCommandHints : string list -> XProtTypes.prop_val
	(* Build a command-line argument property *)

    val makeTransientHint : DrawTypes.window -> XProtTypes.prop_val

  end = struct

    structure G = Geometry
    structure XA = XAtoms
    structure A = StdAtoms
    structure XTy = XProtTypes
    structure D = DrawTypes

    structure W8V = Word8Vector

    val ++ = Word.orb
    infix ++

    fun wordToVec x = let
	  val w = Word.toLargeWord x
	  fun get8 n = Word8.fromLargeWord(LargeWord.>>(w, n))
	  in
	    W8V.fromList [get8 0w24, get8 0w16, get8 0w8, get8 0w0]
	  end

  (* convert an array of words to a Word8Vector.vector. *)
    fun arrToVec arr = let
	  fun f (0, l) = W8V.fromList l
	    | f (i, l) = let
		val i = i-1
		val w = Word.toLargeWord(Array.sub(arr, i))
		fun get8 n = Word8.fromLargeWord(LargeWord.>>(w, n))
		val b0 = get8 0w0
		val b1 = get8 0w8
		val b2 = get8 0w16
		val b3 = get8 0w24
		  in
		    f (i, b3::b2::b1::b0::l)
		  end
	    in
	      f (Array.length arr, [])
	    end

  (* map a list of hints to a word array, with position 0 containing
   * the field mask, and the other positions containing the field values.
   *)
    fun mkHintData (sz, putHint) lst = let
	  val data = Array.array(sz, 0w0)
	  val put1 = putHint (fn (i, x) => Array.update(data, i, x))
	  fun put ([], m) = m
	    | put (x::r, m) = put(r, put1(x, m))
	  val mask = put (lst, 0w0)
	  in
	    Array.update(data, 0, mask);
	    arrToVec data
	  end

  (* Hints about the window size *)
    datatype size_hints
      = HINT_USPosition
      | HINT_PPosition
      | HINT_USSize
      | HINT_PSize
      | HINT_PMinSize of G.size
      | HINT_PMaxSize of G.size
      | HINT_PResizeInc of G.size
      | HINT_PAspect of { min : (int * int), max : (int * int) }
      | HINT_PBaseSize of G.size
      | HINT_PWinGravity of XTy.gravity

  (* Window manager hints *)
    datatype wm_hints
      = HINT_Input of bool		(* does this application rely on the window *)
					(* manager to get keyboard input? *)
					(* Initial window state (choose one) *)
      | HINT_WithdrawnState		  (* for windows that are not mapped *)
      | HINT_NormalState		  (* most applications want to start this way *)
      | HINT_IconicState		  (* application wants to start as an icon *)
      | HINT_IconTile of D.tile		(* tile to be used as icon *)
      | HINT_IconPixmap of D.pixmap	(* pixmap to be used as icon *)
      | HINT_IconWindow of D.window	(* window to be used as icon *)
      | HINT_IconMask of D.pixmap	(* icon mask bitmap *)
      | HINT_IconPosition of G.point	(* initial position of icon *)
      | HINT_WindowGroup of D.window	(* the group leader *)

  (* Build a property value of type STRING *)
    fun makeStringProp data = XTy.PROP_VAL {
	    typ = A.atom_STRING,
	    value = XTy.RAW_DATA{format = XTy.Raw8, data = Byte.stringToBytes data}
	  }

  (* Build a property value of type ATOM *)
    fun makeAtomProp (XTy.XAtom v) = XTy.PROP_VAL {
	    typ = A.atom_ATOM,
	    value = XTy.RAW_DATA{format = XTy.Raw32, data = wordToVec v}
	  }

    local
      val sizeHintsData = let
	    fun putHint upd = let
		  fun putSz (i, G.SIZE{wid, ht}) = (
			upd(i, Word.fromInt wid); upd(i+1, Word.fromInt ht))
		  fun put1 (HINT_USPosition, m) = (m ++ 0w1)
		    | put1 (HINT_PPosition, m) = (m ++ 0w2)
		    | put1 (HINT_USSize, m) = (m ++ 0w4)
                    | put1 (HINT_PSize, m) = (m ++ 0w8)
		    | put1 (HINT_PMinSize sz, m) = (putSz(5, sz); m ++ 0w16)
		    | put1 (HINT_PMaxSize sz, m) = (putSz(7, sz); m ++ 0w32)
		    | put1 (HINT_PResizeInc sz, m) = (putSz(9, sz); m ++ 0w64)
		    | put1 (HINT_PAspect{min=(x1, y1), max=(x2, y2)}, m) = (
			upd(11, Word.fromInt x1); upd(12, Word.fromInt y1);
			upd(13, Word.fromInt x2); upd(14, Word.fromInt y2);
			m ++ 0w128)
	 	    | put1 (HINT_PBaseSize sz, m) = (putSz(15, sz); m ++ 0w256)
	 	    | put1 (HINT_PWinGravity g, m) = (
			upd(17, XCvtFuns.gravityToWire g); m ++ 0w512)
		  in
		    put1
		  end
	    in
	      mkHintData (18, putHint)
	    end
    in
    fun makeWMSizeHints lst = XTy.PROP_VAL {
	    typ = A.atom_WM_SIZE_HINTS,
	    value = XTy.RAW_DATA{format = XTy.Raw32, data = sizeHintsData lst}
	  }
    end (* local *)

    local
      val wmHintsData = let
	    fun putHint upd (hint, m) = (case hint
		 of (HINT_Input true) => (upd(1, 0w1); m ++ 0w1)
		  | HINT_WithdrawnState => (upd(2, 0w0); m ++ 0w2)
		  | HINT_NormalState => (upd(2, 0w1); m ++ 0w2)
		  | HINT_IconicState => (upd(2, 0w3); m ++ 0w2)
		  | (HINT_IconTile(D.TILE(D.PM{id=XTy.XID pix, ...}))) => (
		      upd(3, pix); m ++ 0w4)
		  | (HINT_IconPixmap(D.PM{id=XTy.XID pix, ...})) => (
		      upd(3, pix); m ++ 0w4)
		  | (HINT_IconWindow(D.WIN{id=XTy.XID win, ...})) => (
		      upd(4, win); m ++ 0w8)
		  | (HINT_IconPosition(G.PT{x, y})) => (
		      upd(5, Word.fromInt x); upd(6, Word.fromInt y); m ++ 0w16)
		  | (HINT_IconMask(D.PM{id=XTy.XID pix, ...})) => (
		      upd(7, pix); m ++ 0w32)
		  | (HINT_WindowGroup(D.WIN{id=XTy.XID win, ...})) => (
		      upd(8, win); m ++ 0w64)
		  | _ => raise (MLXError.XERROR "Bad WM Hint"))
	    in
	      mkHintData (9, putHint)
	    end
    in
    fun makeWMHints lst = XTy.PROP_VAL {
	    typ = A.atom_WM_HINTS,
	    value = XTy.RAW_DATA{format = XTy.Raw32, data = wmHintsData lst}
	  }
    end (* local *)

  (* Build a command-line argument property *)
    fun makeCommandHints args =
	  makeStringProp (String.concat (map (fn s => s^"\000") args))

    fun makeTransientHint (D.WIN{id=XTy.XID win, ...}) = XTy.PROP_VAL {
	    typ = A.atom_WINDOW,
	    value = XTy.RAW_DATA{format = XTy.Raw32, data = wordToVec win}
	  }

  end (* XProps *)
