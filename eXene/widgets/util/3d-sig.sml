(* 3d-sig.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

signature THREE_D =
  sig
    structure G : GEOMETRY
    structure D : DRAWING

    datatype relief = Flat | Raised | Sunken | Groove | Ridge
        
    val drawRect : D.drawable ->
          {rect : G.rect, width : int, relief : relief} ->
          WidgetBase.shades -> unit

    val drawFilledRect : D.drawable ->
          {rect : G.rect, width : int, relief : relief} ->
          WidgetBase.shades -> unit

    val drawRoundRect : D.drawable ->
          {rect : G.rect, width : int, c_wid : int, c_ht : int, relief : relief} ->
          WidgetBase.shades  -> unit

    val drawPoly : D.drawable ->
          {pts : G.point list, width : int, relief : relief} ->
          WidgetBase.shades  -> unit

    val draw3DRect : D.drawable -> (G.rect * int) -> 
          {top : D.pen, bottom : D.pen} -> unit

    val draw3DRect2 : D.drawable -> (G.rect * int) -> 
          {top : D.pen, bottom : D.pen} -> unit

    val draw3DPoly : D.drawable -> (G.point list * int) -> 
          {top : D.pen, bottom : D.pen} -> unit

    val draw3DPoly2 : D.drawable -> (G.point list * int) -> 
          {top : D.pen, bottom : D.pen} -> unit

    val draw3DRoundRect : D.drawable ->
          {rect : G.rect, width : int, c_wid : int, c_ht : int} ->
          {top : D.pen, bottom : D.pen} -> unit

    val draw3DRoundRect2 : D.drawable ->
          {rect : G.rect, width : int, c_wid : int, c_ht : int} ->
          {top : D.pen, bottom : D.pen} -> unit

  end

