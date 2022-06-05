(* shape.sml
 *
 * COPYRIGHT (c) 1991, 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Widget wrappers to constrain widget's shape.
 *)

signature SHAPE = 
  sig

    structure W : WIDGET

    val mkShape : {
          widget : W.widget,
          bounds_fn : ((unit -> W.bounds) -> W.bounds),
          resize_fn : ((unit -> W.bounds) -> bool)
        } -> W.widget

    val mkRigid : W.widget -> W.widget
    val mkFlex : W.widget -> W.widget
    val fixSize : (W.widget * W.G.size) -> W.widget
    val freeSize : (W.widget * W.G.size) -> W.widget

  end (* SHAPE *)

structure Shape : SHAPE = 
  struct

    structure W = Widget

    local 
      open Geometry Interact 
      fun doShape shapeFn widget = shapeFn (widget,W.natSize widget)
      fun dummy x = x
    in

    fun mkShape wrapfn {widget, bounds_fn = bnds, resize_fn = resize} = let
          val bounds_of = W.boundsFn widget
          fun realize {env=InEnv{m,k,ci,co}, win, sz} = let
                val ochan = CML.channel ()
                fun outEvt ch x = CML.sendEvt(ch, x)
                val cinenv = InEnv{k=k,m=m,ci=ci,co=outEvt ochan}
                val childco = wrapfn(CML.recvEvt ochan)

                fun loop () =
                      loop (case CML.sync childco of
                        CO_KillReq => CML.sync (co CO_KillReq)
                      | CO_ResizeReq => 
                          if resize bounds_of then CML.sync (co CO_ResizeReq)
                          else ()
                      )
                in
                  CML.spawn loop;
                  W.realizeFn widget {env=cinenv,win=win,sz=sz}
                end
          in
            W.mkWidget {
              root = W.rootOf widget,
              args = W.argsFn widget,
              boundsOf = fn () => bnds bounds_of,
              realize = realize
            }
          end

    fun fixSize (w, SIZE{wid,ht}) = let
          val bounds = W.fixBounds (wid,ht)
          in
            mkShape dummy {
              widget=w, 
              bounds_fn = fn _ => bounds, 
              resize_fn = fn _ => false
            }
          end

    fun freeSize (w, SIZE{wid,ht}) = let
          val x_dim = W.DIM {base = 0, incr = 1, min = 1, nat = wid, max = NONE}
          val y_dim = W.DIM {base = 0, incr = 1, min = 1, nat = ht, max = NONE}
          val bounds = {x_dim = x_dim, y_dim = y_dim}
          in
            mkShape dummy {
              widget=w, 
              bounds_fn = fn _ => bounds, 
              resize_fn = fn _ => true
            }
          end

    val mkRigid = doShape fixSize
    val mkFlex = doShape freeSize
    val mkShape = mkShape W.wrapQueue

    end (* local *)
  end (* Shape *)
