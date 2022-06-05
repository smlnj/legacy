(* button-type.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Base types for buttons.
 *)

structure ButtonType = 
  struct

    datatype button_act = 
      BtnDown of Interact.mbutton 
    | BtnUp of Interact.mbutton
    | BtnReady
    | BtnNormal
  
    datatype button = Button of {
      widget : Widget.widget,
      rqst : ButtonBase.req_msg CML.chan,
      evt : button_act CML.event
    }
  
    fun widgetOf (Button{widget,...}) = widget
    fun evtOf (Button{evt,...}) = evt
    fun setActive (Button{rqst,...}, arg) = 
          CML.send(rqst, ButtonBase.SetActive arg)
    fun getActive (Button{rqst,...}) = let
          val v = SyncVar.iVar ()
          in CML.send(rqst, ButtonBase.GetActive v); SyncVar.iGet v end

  end (* ButtonType *)
