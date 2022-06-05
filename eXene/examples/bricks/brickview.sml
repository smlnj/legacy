(* brickview.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

signature BRICKVIEW =
  sig
    structure W : WIDGET
    structure U : BRICK_UTIL

    type brick_view

    val mkBrickView : W.root -> (U.Position * U.MseEvt CML.chan * U.Palette) -> brick_view
    val widgetOf : brick_view -> W.widget

    val showView : brick_view -> string -> unit
    val endView : brick_view -> string -> unit
    val markView : brick_view -> unit
    val normView : brick_view -> unit

    val setText : brick_view -> string -> unit
    val highlightOn : brick_view -> unit
    val highlightOff : brick_view -> unit
  end

structure BrickView :  BRICKVIEW =
  struct
    structure W = Widget
    structure U = BrickUtil

    open Geometry Interact Widget U

    datatype brick_view = BV of {
        widget : widget,
        highlight : bool -> unit,
        setTextFn : string -> unit,
        showViewFn : string -> unit,
        endViewFn : string -> unit,
        markViewFn : unit -> unit,
        normViewFn : unit -> unit
      }

    fun mkBrickView root (pt, brickCh, palette : Palette) = let
      val label = Label.mkLabel root {
            label = "",
            font = SOME BrickFont,
            foregrnd = NONE,
            backgrnd = SOME (#brick palette),
            align = HCenter}
            
      val widget' = Shape.fixSize (Label.widgetOf label,SIZE{wid=BrickSizeH,ht=BrickSizeV})
      val frame = Frame.mkFrame {
            color = SOME (#darkLines palette),
            width = 1,
            widget = widget'}
      val (widget, revt) = filterMouse (Frame.widgetOf frame)

      val reqChan = CML.channel ()

      fun setText txt me = (Label.setLabel label (Label.Text txt); me)

      fun showText (backc, framec) txt _ = (
        Label.setBackground label backc;
        Label.setLabel label (Label.Text txt);
        Frame.setColor frame framec;
        framec)

      val showView = showText (#concrete palette, SOME(#lightLines palette))
      val endView = showText (#brick palette, SOME(#lightLines palette))
      val markView = showText (#mark palette, SOME(#darkLines palette)) "ok"
      val normView = showText (#brick palette, SOME(#darkLines palette)) ""

      val hilite = SOME(#highlightLines palette)
      fun highlight true me = (Frame.setColor frame hilite; me)
        | highlight false me = (Frame.setColor frame me; me)

      fun handleMouse (MOUSE_FirstDown{but,...}, _) = 
            (CML.send(brickCh,Down(but,pt)); true)
        | handleMouse (MOUSE_LastUp{but,...}, true) = 
            (CML.send(brickCh,Up(but,pt)); false)
        | handleMouse (MOUSE_Leave _, true) = 
            (CML.send(brickCh,Cancel pt); false)
        | handleMouse (_,me) = me

      fun main ((m,_),me) = let
        fun loop (updown,border) =
          loop(CML.select [
            CML.wrap(m, fn evt => (handleMouse (msgBodyOf evt, updown),border)),
            CML.wrap(CML.recvEvt reqChan, fn f => (updown, f border))
          ])
        in
          loop me
        end

      fun initLoop () = let
        fun loop (me as (updown,border)) = CML.select [
            CML.wrap(revt, fn evt => main(evt,me)),
            CML.wrap(CML.recvEvt reqChan, fn f => loop(updown, f border))
          ]
        in
          loop (false, SOME (#darkLines palette))
        end
      in
        CML.spawn initLoop;
        BV {
          widget = widget,
          highlight = fn b => CML.send(reqChan,highlight b),
          setTextFn = fn t => CML.send(reqChan,setText t),
          showViewFn = fn t => CML.send(reqChan,showView t),
          endViewFn = fn t => CML.send(reqChan,endView t),
          normViewFn = fn () => CML.send(reqChan,normView),
          markViewFn = fn () => CML.send(reqChan,markView)
        }
      end

    fun widgetOf (BV{widget,...}) = widget

    fun showView (BV{showViewFn,...}) txt = showViewFn txt
    fun endView (BV{endViewFn,...}) txt = endViewFn txt
    fun markView (BV{markViewFn,...}) = markViewFn ()
    fun normView (BV{normViewFn,...}) = normViewFn ()

    fun setText (BV{setTextFn,...}) txt = setTextFn txt
    fun highlightOn (BV{highlight,...}) = highlight true
    fun highlightOff (BV{highlight,...}) = highlight false

  end


