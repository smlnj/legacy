(* window-env.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A window environment is a collection of input and output streams used by the window
 * to communicate with its parent.  There are three input streams, mouse, keyboard
 * and command-in, and one output stream, command-out.
 *)

structure WindowEnv =
  struct
    local
      open CML Geometry XProtTypes DrawTypes WinRegistry
    in

    local
      type motion_trans = {
        pt : point,         (* the mouse position in window coords *)
        scr_pt : point,     (* the mouse position in screen coords *)
        time : XTime.time
      }
      type button_up_down = {
        but : mbutton,      (* the button that is in transition *)
        pt : point,         (* the mouse position in window coords *)
        scr_pt : point,     (* the mouse position in screen coords *)
(** NOTE: we may also want the modifier-key state **)
        time : XTime.time
      }
      type button_trans = {
        but : mbutton,      (* the button that is in transition *)
        pt : point,         (* the mouse position in window coords *)
        scr_pt : point,     (* the mouse position in screen coords *)
        state :  mbutton_state, (* list of buttons that are pressed *)
(** NOTE: we may also want the modifier-key state **)
        time : XTime.time
      }
    in
    datatype mouse_msg
      = MOUSE_Motion of motion_trans
      | MOUSE_FirstDown of button_up_down
      | MOUSE_LastUp of button_up_down
      | MOUSE_Down of button_trans
      | MOUSE_Up of button_trans
      | MOUSE_Enter of motion_trans
      | MOUSE_Leave of motion_trans
      | MOUSE_ConfigSync
    end (* local *)

    datatype kbd_msg
      = KEY_Press of (Keysym.keysym * modkey_state)
      | KEY_Release of (Keysym.keysym * modkey_state)
      | KEY_ConfigSync

    datatype cmd_in
      = CI_Redraw of rect list
      | CI_Resize of rect
      | CI_ChildBirth of window
      | CI_ChildDeath of window
      | CI_OwnDeath

    datatype cmd_out
      = CO_ResizeReq
      | CO_KillReq

  (* An addressed message (with sequence number) *)
    datatype 'a addr_msg = AMSG of {
    path : path,
    seqn : int,
    msg : 'a
      }

    datatype in_env = InEnv of {    (* this is the window's view of its  *)
                    (* environment *)
    m : mouse_msg addr_msg event,
    k : kbd_msg addr_msg event,
    ci : cmd_in addr_msg event,
    co : cmd_out -> unit event
      }
    datatype out_env = OutEnv of {  (* this is the paren't view of one of its *)
                    (* children's environment. *)
    m : mouse_msg addr_msg -> unit event,
    k : kbd_msg addr_msg -> unit event,
    ci : cmd_in addr_msg -> unit event,
    co : cmd_out event
      }

  (* createWinEnv : unit -> (in_env * out_env) *)
    fun createWinEnv () = let
      val mCh = channel() and kCh = channel()
      val ciCh = channel() and coCh = channel()
      fun outEvt ch x = sendEvt(ch, x)
      in
        (InEnv{
        m = recvEvt mCh, k = recvEvt kCh, ci = recvEvt ciCh, co = outEvt coCh
          },
         OutEnv{
        m = outEvt mCh, k = outEvt kCh, ci = outEvt ciCh, co = recvEvt coCh
          })
      end

    datatype 'a next_win = Here of 'a | ToChild of 'a addr_msg
    fun stripMsg (AMSG{path=PathDst _, msg, ...}) = Here msg
      | stripMsg (AMSG{path=Path(_, p), seqn, msg}) =
      ToChild(AMSG{path=p, seqn=seqn, msg=msg})

    local
      fun nextWin (AMSG{path=PathDst dst, ...}) = dst
    | nextWin (AMSG{path=Path(w, _), ...}) = w
    in

    fun toWindow (amsg, WIN{id, ...}) = ((nextWin amsg) = id)
    fun addrLookup map = let
      val lookup = HashWindow.lookupWinId map
      in
        fn amsg => lookup (nextWin amsg)
      end

    exception NoMatchWin
    fun whichWindow wins amsg = let
      val w = nextWin amsg
      fun find [] = raise NoMatchWin
        | find ((WIN{id, ...}, x) :: r) = if (id = w) then x else (find r)
      in
        find wins
      end

    fun beforeMsg (AMSG{seqn=a, ...}, AMSG{seqn=b, ...}) = (a < b)

    fun msgBodyOf (AMSG{msg, ...}) = msg
    end (* local fun nextWin ... *)

  (* Replace the given input stream with another *)
    fun replaceMouse (InEnv{k, ci, co, ...}, m) = InEnv{m=m, k=k, ci=ci, co=co}
    fun replaceKey (InEnv{m, ci, co, ...}, k)   = InEnv{m=m, k=k, ci=ci, co=co}
    fun replaceCI (InEnv{m, k, co, ...}, ci) = InEnv{m=m, k=k, ci=ci, co=co}

    exception SyncOnIgnoredStream

  (* Create new input environment that ignores the given stream.  Using (i.e.
   * synchronizing on) an ignored stream will raise an exception, but ignoring
   * a stream twice will work.
   *)
    local
      fun ignore evt = let
        val ignoreEvt = wrap (alwaysEvt (), fn () => raise SyncOnIgnoredStream)
        fun loop () = (sync evt; loop())
        in
          spawn (fn () => ((loop ()) handle _ => ()));
          ignoreEvt
        end
    in
    fun ignoreMouse (InEnv{m, k, ci, co}) = InEnv{m=ignore m, k=k, ci=ci, co=co}
    fun ignoreKey (InEnv{m, k, ci, co}) = InEnv{m=m, k=ignore k, ci=ci, co=co}
    fun ignoreInput (InEnv{m, k, ci, co}) = InEnv{m=ignore m, k=ignore k, ci=ci, co=co}
    fun ignoreAll (InEnv{m, k, ci, co}) =
      InEnv{m=ignore m, k=ignore k, ci=ignore ci, co=co}
    end (* local *)

  (* an input stream that never produces messages *)
    val nullStream : 'a addr_msg event = CML.never

  (* Eat mouse events while the given mouse-button state predicate is satisfied.
   * Note that the mouse stream may need to be wrapped by "msgBodyOf"
   *)
    fun whileMouseState pred (initState, m) = let
      fun loop state = (
        if (pred state)
          then (case (sync m)
           of (MOUSE_FirstDown{but, ...}) => loop (KeyBut.mkButState [but])
            | (MOUSE_LastUp _) => loop (MBState 0w0)
            | (MOUSE_Down{state, ...}) => loop state
            | (MOUSE_Up{state, ...}) => loop state
            | _ => loop state)
          else ())
      in
        loop initState
      end

    end (* local *)
  end (* WindowEnv *)
