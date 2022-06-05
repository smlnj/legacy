(* viewport-sig.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Viewport widget, for panning over a child widget.
 *
 * TODO:
 *   Allow child window to vary within bounds.
 *   Parameterize by child (granularity, specific scroll function)
 *)

signature VIEWPORT =
  sig
    structure W : WIDGET
    structure CML : CML

    type viewport

    val viewport : (W.root * W.view * W.arg list) -> W.widget -> viewport
    val mkViewport : W.widget -> viewport
	(* Create a viewport (a classical window) on the virtual graphical
	 * space provides by the widget.  In the current model, the viewport
	 * cannot extend beyond the underlying widget boundaries.  In
	 * particular, it can be no larger than the widget.  The viewport
	 * determines a rectangle in the underlying widgets coordinate system.
         *)

    val getGeometry : viewport -> { rect : W.G.rect, childSz : W.G.size }
	(* Return the underlying widget's current size, and the position
	 * of the viewport rectangle in the widget's coordinates.
	 *)

    val widgetOf : viewport -> W.widget
	(* Convert a viewport into a widget *)

    val setOrig : viewport -> W.G.point -> unit
	(* Set the position of the view by specifying the view's origin.
	 * This raises LibBase.BadArg, if the new rectangle is illegal.
	 *)
    val setHorz : viewport -> int -> unit
	(* Set the horizontal position of the view (the x-coord of the origin) 
	 * This raises LibBase.BadArg, if the new rectangle is illegal.
         *)
    val setVert : viewport -> int -> unit
	(* Set the vertical position of the view (the y-coord of the origin) 
	 * This raises LibBase.BadArg, if the new rectangle is illegal.
         *)

    val evtOf : viewport -> { rect : W.G.rect, childSz : W.G.size } CML.event
	(* Return an event that fires whenever the viewport
	 * configuration changes.
	 *)

  end (* VIEWPORT *)
