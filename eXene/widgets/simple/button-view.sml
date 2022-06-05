(* button-view.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

signature BUTTON_VIEW =
  sig
    type button_view

    val buttonView : Widget.root * Widget.view * Widget.arg list -> button_view
    val bounds : button_view -> Widget.bounds
    val win_args : button_view -> Widget.win_args
    val config : (button_view * Widget.EXB.window * Widget.G.size) ->
          ButtonBase.button_state -> unit   
  end
