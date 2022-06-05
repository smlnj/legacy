(* canvas-sig.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *)

signature CANVAS =
  sig

    structure W : WIDGET
    structure D : DRAWING

    type canvas

    val canvas : (W.root * W.view * W.arg list) -> W.bounds
          -> (canvas * W.G.size * W.Interact.in_env)

    val mkCanvas : W.root -> W.bounds
          -> (canvas * W.G.size * W.Interact.in_env)

    val widgetOf : canvas -> W.widget
    val sizeOf : canvas -> W.G.size
    val drawableOfCanvas : canvas -> D.drawable

  (* modify the attributes of the canvas *)
    val setBackground : canvas -> W.EXB.color option -> unit
	(* set the background color attribute of the window.  Note that this does
	 * not have an immediate affect on the window's contents, but if it is done
	 * before the window is mapped, the window will come up with the right color.
	 *)
    val setCursor : canvas -> W.EXB.cursor option -> unit

  end (* CANVAS *)
