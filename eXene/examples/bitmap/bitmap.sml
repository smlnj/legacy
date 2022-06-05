(* bitmap.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

signature BITMAP =
  sig
    structure W : WIDGET
    structure CML : CONCUR_ML

    exception BadParam

    type bitmap

    val mkBitmap : W.root -> {
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      horzCells : int,
      vertCells : int
    } -> bitmap

    val widgetOf : bitmap -> W.widget
    val setPixel : bitmap -> (bool * W.G.point) -> unit
    val imageOf : bitmap -> W.EXB.image

  end

structure Bitmap : BITMAP =
  struct
    structure W = Widget
    structure CML = CML
    structure EXB = EXeneBase

    open CML Geometry EXeneBase EXeneWin Interact Drawing Widget

    exception BadParam

    type pixchange = bool * point

    datatype rqst = 
      DoRealize of {
        env : in_env,
        win : window,
        sz : size
      }
    | Set of pixchange
    | ImageOf of image chan

    datatype bitmap = BM of {widget : widget, setChan : rqst chan}

    fun setColor scr (SOME c, _) = c
      | setColor scr (NONE, dflt) = dflt

    fun mkBitmap root {horzCells, vertCells, foregrnd, backgrnd} = let
      val scr = screenOf root
      val setChan = channel ()
      val psize = SIZE{wid=horzCells,ht=vertCells}
      val pixMap = createPixmap scr (psize,1)
      val pm = drawableOfPM pixMap
      val prect = mkRect(originPt,psize)
      val _ = clearDrawable pm
      val size = fixBounds (horzCells, vertCells)
      val maxX = horzCells-1
      val maxY = vertCells-1
      val forec = setColor scr (foregrnd, blackOfScr scr)
      val backc = setColor scr (backgrnd, whiteOfScr scr)

      val onPen = newPen [PV_Foreground color1]
      val offPen = newPen [PV_Foreground color0]
      val copyPen = newPen [PV_Foreground forec, PV_Background backc]

      fun set (true, pt) = drawPt pm onPen pt
        | set (false, pt) = drawPt pm offPen pt

      fun blt dw r = bitBlt dw copyPen {src=PMSRC pixMap, src_rect=r, dst_pos=originOfRect r}
   
      fun redraw(dw,rlist) = app (blt dw) rlist

      fun sendImage rchan = send(rchan, createImageFromPixmap pixMap)

      fun realize {env, win, sz} = let
        val InEnv{ci,...} = ignoreInput env
        val mChan = channel ()
        val dw = drawableOfWin win
  
        fun handleCI (CI_Resize (RECT{x,y,wid,ht})) = ()
          | handleCI (CI_Redraw rlist) = (redraw (dw,rlist); ())
          | handleCI CI_OwnDeath = ()
          | handleCI _ = ()
  
        fun handleSet (DoRealize _) = ()
          | handleSet (ImageOf arg) = (sendImage arg; ())
          | handleSet (Set arg) = (set arg; redraw(dw,[prect]); ())

        fun loop () =
          loop(select [
            wrap (ci, fn evt => (handleCI (msgBodyOf evt))),
            wrap (receive setChan, fn evt => (handleSet evt))
          ])
      in
        loop ()
      end

      fun initLoop () =
        case (accept setChan) of
          DoRealize arg => realize arg
        | Set arg => (set arg; initLoop ())
        | ImageOf arg => (sendImage arg; initLoop ())
    in
      spawn initLoop;
      BM {
        widget = mkWidget{
          root=root, 
          boundsOf = fn () => size, 
          realize= fn arg => send(setChan, DoRealize arg)
        },
        setChan = setChan
      }
    end

    fun widgetOf (BM{widget,...}) = widget
    fun setPixel (BM{setChan,...}) arg = send(setChan,Set arg)
    fun imageOf (BM{setChan,...}) = let
      val retChan = channel ()
    in
      send(setChan,ImageOf retChan);
      accept retChan
    end
  end
