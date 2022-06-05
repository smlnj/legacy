(* interact.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Types and operations to support user interaction and other external
 * X-events.
 *)

signature INTERACT =
  sig

    structure G : GEOMETRY
    structure EXB : EXENE_BASE

  (* modifier buttons *)
    datatype modkey
      = ShiftKey | LockKey | ControlKey
      | Mod1Key | Mod2Key | Mod3Key | Mod4Key | Mod5Key
      | AnyModifier

  (* modifier key states *)
    eqtype modkey_state

    val mkModState : modkey list -> modkey_state

    val unionMod : (modkey_state * modkey_state) -> modkey_state
    val intersectMod : (modkey_state * modkey_state) -> modkey_state

    val emptyMod : modkey_state -> bool
    val shiftIsSet : modkey_state -> bool
    val lockIsSet : modkey_state -> bool
    val cntrlIsSet : modkey_state -> bool
    val modIsSet : (modkey_state * int) -> bool

  (* keysyms and translations *)
    datatype keysym = KEYSYM of int | NoSymbol

    type translation

    val defaultTranslation : translation

    val rebind : translation -> (keysym * modkey list * string)
	  -> translation

    exception KeysymNotFound
    val lookupString : translation -> (keysym * modkey_state)
	  -> string

  (* mouse buttons *)
    datatype mbutton = MButton of int

  (* Mouse button states *)
    eqtype mbutton_state

    val mkButState : mbutton list -> mbutton_state

    val unionMBut : (mbutton_state * mbutton_state) -> mbutton_state
    val intersectMBut : (mbutton_state * mbutton_state) -> mbutton_state

    val invertMBut : (mbutton_state * mbutton) -> mbutton_state

    val mbutAllClr  : mbutton_state -> bool
    val mbutSomeSet : mbutton_state -> bool
    val mbut1IsSet  : mbutton_state -> bool
    val mbut2IsSet  : mbutton_state -> bool
    val mbut3IsSet  : mbutton_state -> bool
    val mbut4IsSet  : mbutton_state -> bool
    val mbut5IsSet  : mbutton_state -> bool
    val mbutIsSet   : (mbutton_state * mbutton) -> bool


  (* keyboard messages *)
    datatype kbd_msg
      = KEY_Press of (keysym * modkey_state)
      | KEY_Release of (keysym * modkey_state)
      | KEY_ConfigSync

  (* Mouse messages *)
    datatype mouse_msg
      = MOUSE_Motion of {
	    pt : G.point,		(* the mouse position in window coords *)
	    scr_pt : G.point,		(* the mouse position in screen coords *)
	    time : EXB.XTime.time
	  }
      | MOUSE_FirstDown of {
	    but : mbutton,		(* the button that is in transition *)
	    pt : G.point,		(* the mouse position in window coords *)
	    scr_pt : G.point,		(* the mouse position in screen coords *)
	    time : EXB.XTime.time
	  }
      | MOUSE_LastUp of {
	    but : mbutton,		(* the button that is in transition *)
	    pt : G.point,		(* the mouse position in window coords *)
	    scr_pt : G.point,		(* the mouse position in screen coords *)
	    time : EXB.XTime.time
	  }
      | MOUSE_Down of {
	    but : mbutton,		(* the button that is in transition *)
	    pt : G.point,		(* the mouse position in window coords *)
	    scr_pt : G.point,		(* the mouse position in screen coords *)
	    state :  mbutton_state,	(* the state of the mouse buttons *)
	    time : EXB.XTime.time
	  }
      | MOUSE_Up of {
	    but : mbutton,		(* the button that is in transition *)
	    pt : G.point,		(* the mouse position in window coords *)
	    scr_pt : G.point,		(* the mouse position in screen coords *)
	    state :  mbutton_state,	(* the state of the mouse buttons *)
	    time : EXB.XTime.time
	  }
      | MOUSE_Enter of {
	    pt : G.point,		(* the mouse position in window coords *)
	    scr_pt : G.point,		(* the mouse position in screen coords *)
	    time : EXB.XTime.time
	  }
      | MOUSE_Leave of {
	    pt : G.point,		(* the mouse position in window coords *)
	    scr_pt : G.point,		(* the mouse position in screen coords *)
	    time : EXB.XTime.time
	  }
      | MOUSE_ConfigSync


  (* Command/control messages from parent *)
    datatype cmd_in
      = CI_Redraw of G.rect list
      | CI_Resize of G.rect
      | CI_ChildBirth of EXB.window
      | CI_ChildDeath of EXB.window
      | CI_OwnDeath

  (* Command/control messages to parent (really requests) *)
    datatype cmd_out
      = CO_ResizeReq
      | CO_KillReq


  (* addressed messages (with sequence numbers) *)
    type 'a addr_msg

    datatype 'a next_win = Here of 'a | ToChild of 'a addr_msg
    val stripMsg : 'a addr_msg -> 'a next_win

    val toWindow : ('a addr_msg * EXB.window) -> bool
    val addrLookup : 'a EXB.window_map -> 'b addr_msg -> 'a

    exception NoMatchWin
    val whichWindow : (EXB.window * 'a) list -> 'b addr_msg -> 'a

    val beforeMsg : ('a addr_msg * 'a addr_msg) -> bool

    val msgBodyOf : 'a addr_msg -> 'a


  (* window environments *)
    datatype in_env = InEnv of {	(* this is the window's view of its  *)
					(* environment *)
	k : kbd_msg addr_msg CML.event,
	m : mouse_msg addr_msg CML.event,
	ci : cmd_in addr_msg CML.event,
	co : cmd_out -> unit CML.event
      }
    datatype out_env = OutEnv of {	(* this is the paren't view of one of its *)
					(* children's environment. *)
	k : kbd_msg addr_msg -> unit CML.event,
	m : mouse_msg addr_msg -> unit CML.event,
	ci : cmd_in addr_msg -> unit CML.event,
	co : cmd_out CML.event
      }

    val createWinEnv : unit -> (in_env * out_env)

    val replaceMouse : (in_env * mouse_msg addr_msg CML.event) -> in_env
    val replaceKey   : (in_env * kbd_msg addr_msg CML.event) -> in_env
    val replaceCI    : (in_env * cmd_in addr_msg CML.event) -> in_env

    val ignoreMouse  : in_env -> in_env
    val ignoreKey    : in_env -> in_env
    val ignoreInput  : in_env -> in_env
    val ignoreAll    : in_env -> in_env

    val nullStream   : 'a addr_msg CML.event

    val whileMouseState : (mbutton_state -> bool)
	  -> (mbutton_state * mouse_msg CML.event) -> unit

  end (* INTERACT *)
