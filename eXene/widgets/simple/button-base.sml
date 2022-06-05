(* button-base.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Base types and values for buttons, etc.
 *)

structure ButtonBase = 
  struct

    datatype mouse_evt = 
      MseDown of Interact.mbutton 
    | MseUp of Interact.mbutton
    | MseIn of bool

    type button_state = (Widget.wstate * bool * bool)

    fun mkWState (true,v) = Widget.Active v
      | mkWState (false,v) = Widget.Inactive v

    fun flip (Widget.Active s) = Widget.Active(not s)
      | flip (Widget.Inactive s) = Widget.Inactive(not s)
    
    fun getState (Widget.Active set,_,_) = set
      | getState (Widget.Inactive set,_,_) = set
  
    fun setState (true, (Widget.Inactive _,r,d)) = (Widget.Inactive true,r,d)
      | setState (true, (Widget.Active _,r,d)) = (Widget.Active true,r,d)
      | setState (false, (Widget.Inactive _,r,d)) = (Widget.Inactive false,r,d)
      | setState (false, (Widget.Active _,r,d)) = (Widget.Active false,r,d)

    fun getActive (Widget.Active _,_,_) = true
      | getActive _ = false

    fun setActive (true, (Widget.Inactive v,r,d)) = (Widget.Active v,r,d)
      | setActive (false, (Widget.Active v,r,d)) = (Widget.Inactive v,r,d)
      | setActive (_,s) = s

    datatype req_msg = 
      GetActive of bool SyncVar.ivar
    | SetActive of bool
    | GetState of bool SyncVar.ivar
    | SetState of bool
    | DoRealize of {
        env : Interact.in_env,
        win : Widget.EXB.window,
        sz : Geometry.size
      }
    | GetBounds of Widget.bounds SyncVar.ivar
    | GetArgs of Widget.win_args SyncVar.ivar
  
    fun mseP (m, mchan) = let
          open CML Interact
          fun downLoop btn = 
                case msgBodyOf (sync m) of 
                  MOUSE_LastUp _ => send (mchan, MseUp btn)
                | MOUSE_Leave _ => (send (mchan, MseIn false); downLoop btn)
                | MOUSE_Enter _ => (send (mchan, MseIn true); downLoop btn)
                | _ => downLoop btn 

          fun loop () =
                (case msgBodyOf (sync m) of 
                  MOUSE_FirstDown {but,...} => (
                    send (mchan, MseDown but);
                    downLoop but
                  )
                | MOUSE_Enter _ => send(mchan, MseIn true)
                | MOUSE_Leave _ => send(mchan, MseIn false)
                | _ => ();
                loop ())
          in loop () end

  
  end (* ButtonBase *)
