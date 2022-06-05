(* bitmap-edit.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

signature BITMAP_EDIT =
  sig
    structure W : WIDGET

    exception BadParam

    type bitmapedit

    val mkBitmapEdit : W.root -> {
      horzCells : int,
      vertCells : int,
      cellSize : int
    } -> bitmapedit

    val widgetOf : bitmapedit -> W.widget
    val evtOf : bitmapedit -> (bool * W.G.point) CML.event

  end

structure BitmapEdit : BITMAP_EDIT =
  struct
    structure W = Widget

    open CML Geometry EXeneBase EXeneWin Interact Drawing Widget

    exception BadParam

    type pixchange = bool * point

    datatype bitmapedit = BME of {widget : widget, evt : pixchange event}
    datatype cellChange = Draw of point | Undraw of point | Flip of point

    fun mseReader (m, transFn, mChan) = let
      fun track msg pt = let
        val p = transFn pt

        fun next () = 
          case msgBodyOf (sync m) of
            MOUSE_Motion {pt,...} => (transFn pt, false)
          | MOUSE_LastUp {pt,...} => (transFn pt, true)
          | MOUSE_Down {pt,...} => (transFn pt, false)
          | MOUSE_Up {pt,...} => (transFn pt, false)
          | _ => next ()

        fun loop p = let
          val (p',done) = next ()
        in
          if p <> p' then send(mChan, msg p') else ();
          if done then () else loop p'
        end
          
      in
        send(mChan, msg p);
        loop p
      end

      fun handleMse(MOUSE_FirstDown{but,pt,...}) =
          (case but of
            MButton 1 => track Draw pt
          | MButton 2 => track Undraw pt
          | _ => track Flip pt
          )
        | handleMse(_) = ()
 
      fun loop () = loop(handleMse(msgBodyOf (sync m)))

    in
      loop () 
    end

    fun mkBitmapEdit root {horzCells, vertCells, cellSize} = let
      val scr = screenOf root
      val evtChan = channel ()
      val pixWid = horzCells*cellSize + 1
      val pixHt = vertCells*cellSize + 1
      val pixMap = createPixmap scr (SIZE{wid=pixWid,ht=pixHt},1)
      val pm = drawableOfPM pixMap
      val cellMap = Array.array(horzCells*vertCells,false)
      val size = {
          x_dim=DIM{base=1,incr=cellSize,min=2,nat=horzCells,max=SOME horzCells},
          y_dim=DIM{base=1,incr=cellSize,min=2,nat=vertCells,max=SOME vertCells}
        }
      val maxX = horzCells-1
      val maxY = vertCells-1
      val drawSz = cellSize - 3

      val onPen = newPen [PV_Foreground color1, PV_LineStyle_OnOffDash,
        PV_DashOffset 0, PV_Dash_Fixed 1]
      val offPen = newPen [PV_Foreground color0]
      val copyPen = newPen [PV_Foreground (blackOfScr scr), PV_Background (whiteOfScr scr)]

      fun fillPixMap pm = let
        val pwid = pixWid-1
        val pht = pixHt-1
        fun mkHzSeg i = let val y = i*cellSize in LINE(PT{x=0,y=y},PT{x=pwid,y=y}) end
        fun mkVtSeg i = let val x = i*cellSize in LINE(PT{x=x,y=0},PT{x=x,y=pht}) end
        fun mkSegs segFn (i,bnd,l) =
          if i = bnd then (segFn i)::l
          else mkSegs segFn (i+1,bnd,(segFn i)::l) 
      in
        clearDrawable pm;
        drawSegs pm onPen (mkSegs mkVtSeg (0,horzCells+1,(mkSegs mkHzSeg (0,vertCells+1,[]))))
      end
 
      fun transFn (PT{x,y}) = PT{
	      x=Int.max(0,Int.min(x div cellSize, maxX)),
	      y=Int.max(0,Int.min(y div cellSize, maxY))
	    }
      fun index (PT{x,y}) = x + y*horzCells
      fun ptToRect (PT{x,y}) = RECT{
	      x=2+x*cellSize,
	      y=2+y*cellSize,
	      wid=drawSz,
	      ht=drawSz
	    }

      fun blt dw r = bitBlt dw copyPen {src=PMSRC pixMap, src_rect=r, dst_pos=originOfRect r}
   
      fun redraw(dw, rlist) = app (ignore o (blt dw)) rlist

      fun drawCell (dw,pt,turnOn,pen) = let
        val indx = index pt
      in
        if turnOn = Array.sub(cellMap,indx) then NONE
        else (
          Array.update(cellMap,indx,turnOn);
          let 
            val r = ptToRect pt
          in
            fillRect pm pen r;
            redraw (dw,[r])
          end;
          SOME(turnOn,pt)
        )
      end

      fun realize {env, win, sz} = let
        val InEnv{ci,m,...} = Interact.ignoreKey env
        val mChan = channel ()
        val dw = drawableOfWin win
  
        fun handleCI (CI_Resize (RECT{x,y,wid,ht})) = ()
          | handleCI (CI_Redraw rlist) = (redraw (dw,rlist); ())
          | handleCI CI_OwnDeath = ()
          | handleCI _ = ()
  
        fun handleMse (Draw pt) = drawCell (dw,pt,true,onPen)
          | handleMse (Undraw pt) = drawCell (dw,pt,false,offPen)
          | handleMse (Flip pt) = 
              (case Array.sub(cellMap,index pt) of
                true => drawCell (dw,pt,false,offPen)
              | _ => drawCell (dw,pt,true,onPen))

        fun loop () = let
          fun ifChange NONE = ()
            | ifChange (SOME v) = send(evtChan, v)
        in
          loop(select [
            wrap (ci, fn evt => (handleCI (msgBodyOf evt))),
            wrap (CML.recvEvt mChan, fn evt => ifChange(handleMse evt))
          ])
        end
      in
        spawn (fn () => mseReader(m,transFn,mChan));
        spawn loop;
        ()
      end
    in
      fillPixMap pm;
      BME {
        widget = mkWidget{
          root=root, 
          boundsOf = fn () =>size, 
          realize=realize
        },
        evt = CML.recvEvt evtChan
      }
    end

    fun widgetOf (BME{widget,...}) = widget
    fun evtOf (BME{evt,...}) = evt
  end
