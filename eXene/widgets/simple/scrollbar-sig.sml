(* scrollbar-sig.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Scrollbar widget.
 *)

signature SCROLLBAR =
  sig
    structure CML : CML
    structure W : WIDGET

    datatype scroll_evt
      = ScrUp of real
      | ScrDown of real
      | ScrStart of real
      | ScrMove of real
      | ScrEnd of real
  
    type scrollbar

    val hScrollbar : (W.root * W.view * W.arg list) -> scrollbar
    val vScrollbar : (W.root * W.view * W.arg list) -> scrollbar

    val mkHScrollbar : W.root -> {color : W.EXB.color option, sz : int} -> scrollbar
    val mkVScrollbar : W.root -> {color : W.EXB.color option, sz : int} -> scrollbar

    val evtOf : scrollbar -> scroll_evt CML.event
    val widgetOf : scrollbar -> W.widget
    val setVals : scrollbar -> {sz : real option, top : real option} -> unit

  end (* SCROLLBAR *)
