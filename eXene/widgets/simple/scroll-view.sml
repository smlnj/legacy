(* scroll-view.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Scrollbar views.
 *)

signature SCROLL_VIEW =
  sig
    type scroll_view

    val horzScrollbar : scroll_view
    val vertScrollbar : scroll_view

  end (* SCROLL_VIEW *)

structure ScrollView : SCROLL_VIEW = 
  struct

    structure W = Widget
    structure EXB = W.EXB

    type scroll_state = {
      size : int,
      coord : Geometry.point -> int,
      draw : int * int -> unit,
      move : int * int * int * int -> unit
    }

    type scroll_view = {
      bounds_of : int -> unit -> W.bounds,
      realize : (W.root * EXeneBase.color) -> Drawing.drawable
        -> Geometry.size -> scroll_state
    }

    open Geometry EXeneBase Drawing

    val inset = 1
    val bwidth = 2

    fun move_fn (clear, fill) (x:int,w:int,x',w') = let
          val e = x+w
          val e' = x'+w'
          in
            if x < x' then
              if e <= x' then clear(x,w)
              else if e < e' then clear (x,x'-x)
              else (clear (x,x'-x); clear (e',e-e'))
            else if x = x' then
              if e <= e' then () else clear (e',e-e')
            else if x < e' then
              if e > e' then clear(e',e-e') else ()
            else clear (x,w);
            fill (x',w')
          end

    fun vertRealize (root, color) = let
          val shades as {base,...} = W.shades root color
          in
            fn dr => let
              val dr = Drawing.feedback dr
              val clear = clearArea dr
              fun config (SIZE{wid,ht}) = let
                    val rwid = wid - 2*inset
                    fun drawFn (x,w) = let
                          val r = RECT{x=inset,y=x,wid=rwid,ht=w}
                          in
                            clearDrawable dr;
                            fillRect dr base r;
                            ThreeD.drawRect dr {rect=r,width=bwidth,relief=ThreeD.Raised} shades
                          end
                    fun clr (x,w) = clear (RECT{x=inset,y=x,ht=w,wid=rwid})
                    val moveFn = move_fn (clr,drawFn)
                    in
                      {
                        size = ht,
                        coord = fn PT{x,y} => y,
                        draw = drawFn,
                        move = moveFn
                      }
                    end
              in
                config
              end
          end

    fun vertBounds dim = let
          val swid = dim div 3
          val bnds = { 
              x_dim = W.fixDim dim,
              y_dim = W.DIM{base=swid,incr=1,min=0,nat=0,max=NONE}
            }
          in fn () => bnds end

    fun horzRealize (root, color) = let
          val shades as {base,...} = W.shades root color
          in
            fn dr => let
              val dr = Drawing.feedback dr
              val clear = clearArea dr
              fun config (SIZE{wid,ht}) = let
                    val rht = ht - 2*inset
                    fun drawFn (x,w) = let
                          val r = RECT{x=x,y=inset,wid=w,ht=rht}
                          in
                            clearDrawable dr;
                            fillRect dr base r;
                            ThreeD.drawRect dr {rect=r,width=bwidth,relief=ThreeD.Raised} shades
                          end
                    fun clr (x,w) = clear (RECT{x=x,y=inset,wid=w,ht=rht})
                    val moveFn = move_fn (clr,drawFn)
                    in
                      {
                        size = wid,
                        coord = fn PT{x,y} => x,
                        draw = drawFn,
                        move = moveFn
                      }
                    end
              in
                config
              end
          end

    fun horzBounds dim = let
          val swid = dim div 3
          val bnds = { 
              y_dim = W.fixDim dim,
              x_dim = W.DIM{base=swid,incr=1,min=0,nat=0,max=NONE}
            }
          in fn () => bnds end

    val horzScrollbar = {bounds_of=horzBounds, realize=horzRealize}
    val vertScrollbar = {bounds_of=vertBounds, realize=vertRealize}

end (* ScrollView *)
