(* scrollport-sig.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * ScrollPort widget, for panning over a child widget using scrollbars.
 *
 * TODO:
 *   granularity
 *)

signature SCROLL_PORT =
  sig

    structure W : WIDGET

    type scroll_port

    val scrollPort : (W.root * W.view * W.arg list) -> W.widget -> scroll_port

    val mkScrollPort : {
	    widget : W.widget,
	    continuous : bool,
	    color : W.EXB.color option,
	    hsb : {top : bool} option,
	    vsb : {left : bool} option
	  } -> scroll_port

    val widgetOf : scroll_port -> W.widget

  end (* SCROLL_PORT *)
