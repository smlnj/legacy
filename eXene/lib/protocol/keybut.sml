(* keybut.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Support for modifier key sets and mouse button sets.  The modifier key value
 * AnyModifier is the power-set of modifier keys.
 *)

signature KEY_BUT =
  sig

  (* modifier buttons *)
    datatype modkey
      = ShiftKey | LockKey | ControlKey
      | Mod1Key | Mod2Key | Mod3Key | Mod4Key | Mod5Key
      | AnyModifier
  (* mouse buttons *)
    datatype mbutton = MButton of int

  (* modifier key states *)
    eqtype modkey_state

    val mkModState : modkey list -> modkey_state

    val unionMod : (modkey_state * modkey_state) -> modkey_state
    val intersectMod : (modkey_state * modkey_state) -> modkey_state
  (* matchMod(a, b) returns true, if a=b or if b=AnyModifier. *)
    val matchMod : (modkey_state * modkey_state) -> bool
    val emptyMod : modkey_state -> bool

    val shiftIsSet : modkey_state -> bool
    val lockIsSet : modkey_state -> bool
    val cntrlIsSet : modkey_state -> bool
    val modIsSet : (modkey_state * int) -> bool

  (* Mouse button states *)
    eqtype mbutton_state

    val mkButState : mbutton list -> mbutton_state

    val unionMBut : (mbutton_state * mbutton_state) -> mbutton_state
    val intersectMBut : (mbutton_state * mbutton_state) -> mbutton_state

    val invertMBut : (mbutton_state * mbutton) -> mbutton_state

    val mbutAllClr : mbutton_state -> bool
    val mbutSomeSet : mbutton_state -> bool
    val mbut1IsSet : mbutton_state -> bool
    val mbut2IsSet : mbutton_state -> bool
    val mbut3IsSet : mbutton_state -> bool
    val mbut4IsSet : mbutton_state -> bool
    val mbut5IsSet : mbutton_state -> bool
    val mbutIsSet : (mbutton_state * mbutton) -> bool
 
  end (* KEY_BUT *)

structure KeyBut : KEY_BUT =
  struct
    open XProtTypes

    val & = Word.andb and ++ = Word.orb
    val << = Word.<<
    infix & ++ <<

  (** Modifier key states **)

    val shiftMask	= 0wx0001
    val lockMask	= 0wx0002
    val cntlMask	= 0wx0004
    val mod1Mask	= 0wx0008
    val mod2Mask	= 0wx0010
    val mod3Mask	= 0wx0020
    val mod4Mask	= 0wx0040
    val mod5Mask	= 0wx0080

    fun unionMod (MKState m1, MKState m2) = MKState(m1 ++ m2)
      | unionMod _ = AnyModKey
    fun intersectMod (MKState m1, MKState m2) = MKState(m1 & m2)
      | intersectMod (AnyModKey, m) = m
      | intersectMod (m, AnyModKey) = m
    fun matchMod (MKState m1, MKState m2) = (m1 = m2)
      | matchMod (_, AnyModKey) = true
      | matchMod _ = false
    fun emptyMod AnyModKey = true
      | emptyMod (MKState 0w0) = true
      | emptyMod _ = false

    fun mkModState l = let
	  exception Any
	  fun f ([], m) = MKState m
	    | f (k::r, m) = let
		  val mask = (case k
		       of AnyModifier => raise Any
			| ShiftKey => shiftMask
			| LockKey => lockMask
			| ControlKey => cntlMask
			| Mod1Key => mod1Mask
			| Mod2Key => mod2Mask
			| Mod3Key => mod3Mask
			| Mod4Key => mod4Mask
			| Mod5Key => mod5Mask)
		  in
		    f (r, m ++ mask)
		  end
	  in
	    (f (l, 0w0)) handle Any => AnyModKey
	  end

    fun shiftIsSet AnyModKey = true
      | shiftIsSet (MKState s) = ((s & shiftMask) <> 0w0)
    fun lockIsSet AnyModKey = true
      | lockIsSet (MKState s) = ((s & lockMask) <> 0w0)
    fun cntrlIsSet AnyModKey = true
      | cntrlIsSet (MKState s) = ((s & cntlMask) <> 0w0)
    fun modIsSet (AnyModKey, _) = true
      | modIsSet (MKState s, i) = ((s & (mod1Mask << Word.fromInt(i-1))) <> 0w0)


  (** Mouse button states **)

    val but1Mask	= 0wx0100
    val but2Mask	= 0wx0200
    val but3Mask	= 0wx0400
    val but4Mask	= 0wx0800
    val but5Mask	= 0wx1000
    val allButMask	= 0wx1f00

    fun unionMBut (MBState m1, MBState m2) = MBState(m1 ++ m2)
    fun intersectMBut (MBState m1, MBState m2) = MBState(m1 & m2)

    fun invertMBut (MBState s, MButton b) =
	  MBState(Word.xorb(s, but1Mask << (Word.fromInt(b-1))))

    fun mkButState l = let
	  fun f ([], m) = MBState m
	    | f ((MButton i)::r, m) = f (r, m ++ (but1Mask << Word.fromInt(i-1)))
	  in
	    f(l, 0w0)
	  end

    fun mbutAllClr (MBState s)  = ((s & allButMask) = 0w0)
    fun mbutSomeSet (MBState s) = ((s & allButMask) <> 0w0)
    fun mbut1IsSet (MBState s)  = ((s & but1Mask) <> 0w0)
    fun mbut2IsSet (MBState s)  = ((s & but2Mask) <> 0w0)
    fun mbut3IsSet (MBState s)  = ((s & but3Mask) <> 0w0)
    fun mbut4IsSet (MBState s)  = ((s & but4Mask) <> 0w0)
    fun mbut5IsSet (MBState s)  = ((s & but5Mask) <> 0w0)
    fun mbutIsSet (MBState s, MButton i) =
	  ((s & (but1Mask << Word.fromInt(i-1))) <> 0w0)

  end (* KeyBut *)
