(* toggle-type.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Base types for toggles
 *)

structure ToggleType = 
  struct

    datatype toggle_act = 
      Toggle of bool
    | ToggleReady
    | ToggleNormal
  
    datatype toggle = TOGGLE of {
      widget : Widget.widget,
      evt : toggle_act CML.event,
      rqst : ButtonBase.req_msg CML.chan
    }

    fun widgetOf (TOGGLE{widget,...}) = widget
    fun evtOf (TOGGLE{evt,...}) = evt
    fun setState (TOGGLE{rqst,...}, arg) = 
          CML.send(rqst, ButtonBase.SetState arg)
    fun getState (TOGGLE{rqst,...}) = let
          val v = SyncVar.iVar ()
          in CML.send(rqst, ButtonBase.GetState v); SyncVar.iGet v end
    fun setActive (TOGGLE{rqst,...}, arg) = 
          CML.send(rqst, ButtonBase.SetActive arg)
    fun getActive (TOGGLE{rqst,...}) = let
          val v = SyncVar.iVar ()
          in CML.send(rqst, ButtonBase.GetActive v); SyncVar.iGet v end
  
  end (* ToggleType *)
