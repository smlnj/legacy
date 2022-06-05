(* rounded-rect-sig.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 *
 * Routines to draw/fill rectangles with rounded corners.
 *
 *)

signature ROUNDED_RECT =
  sig
    structure G : GEOMETRY

    val drawRoundedRect : Drawing.drawable -> Drawing.pen
	  -> {rect : G.rect, c_wid : int, c_ht : int} -> unit

    val fillRoundedRect : Drawing.drawable -> Drawing.pen
	  -> {rect : G.rect, c_wid : int, c_ht : int} -> unit

  end (* ROUNDED_RECT *)

