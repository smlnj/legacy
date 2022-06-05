(*
 * Various geometric utility routines.
 * This assumes a mechanism for allowing only
 * one thread at a time to grab the server.
 *)

signature GET = sig
  
  structure EXB : EXENE_BASE
  structure I : INTERACT

  val getPt : (EXB.window * I.mouse_msg I.addr_msg CML.event)
    -> (I.mbutton * I.mbutton_state)
    -> EXB.G.point option CML.event

  val getClickPt : (EXB.window * I.mouse_msg I.addr_msg CML.event)
    -> (I.mbutton * I.mbutton_state)
    -> EXB.G.point option CML.event

  val getRect : (EXB.window * I.mouse_msg I.addr_msg CML.event)
    -> I.mbutton
    -> EXB.G.rect option CML.event
(*
  val moveRect : (EXB.window * I.mouse_msg I.addr_msg CML.event)
    -> (I.mbutton * I.mbutton_state * EXB.G.rect)
    -> EXB.G.rect option CML.event
*)
end (* GET *)

structure Get : GET = struct
  
  structure EXB = EXeneBase
  structure I = Interact
  open CML Geometry EXeneBase Interact Drawing

  fun ptsToRect (PT{x,y}, PT{x=x',y=y'}) = let
    fun minmax (a : int,b) = if a <= b then (a,b-a) else (b,a-b)
    val (ox, sx) = minmax(x,x')
    val (oy, sy) = minmax(y,y')
  in
    RECT{x=ox,y=oy,wid=sx,ht=sy}
  end

  fun waitMouse mevt = 
    case sync mevt of
      MOUSE_FirstDown {but, pt, ...} => (but, pt)
    | _ => waitMouse mevt

  fun waitUp (dpy, mevt, cursor) = let
        fun loop () =
              case sync mevt of
                MOUSE_LastUp _ => ()
              | _ => loop ()
        in
          changeActiveGrabCursor dpy cursor;
          loop ()
        end

  fun get_pt waitup (win, m) (mbut, state) = let
    val dpy = EXeneWin.displayOfWin win
    val retc = channel ()
    val mevt = wrap(m, msgBodyOf) 
    fun isSet s = mbutIsSet (s, mbut)

    fun doPt () = let
      val ptCursor = EXB.stdCursor dpy StdCursor.tcross

      (* Need to block output to win subtree *)

      (* Create overlay window and set its cursor *)
      val {sz, ...} = geomOfWin win
      val overwin = EXeneWin.createInputOnlyWin win (mkRect (originPt, sz))
      val _ = EXeneWin.setCursor overwin (SOME ptCursor)
      val _ = EXeneWin.mapWin overwin

        (* make sure button is up *)
      val _ = whileMouseState isSet (state, mevt)

        (* wait for mouse hit *)
      val (bttn, pt) = waitMouse mevt
    in
      if waitup then whileMouseState mbutSomeSet (state, mevt) else ();

      EXeneWin.destroyWin overwin;
      (* Unblock output to win subtree *)
      
      if bttn = mbut 
        then send (retc, SOME pt)
        else send (retc, NONE)
    end

  in
    spawn doPt;
    receive retc
  end

  val getPt = get_pt false
  val getClickPt = get_pt true

  fun getRect (win, m) mbut = let
    val dpy = EXeneWin.displayOfWin win
    val black = blackOfScr (EXeneWin.screenOfWin win)
    val pen = newPen [PV_Function OP_Xor, PV_Foreground color1]
    val retc = channel ()
    val mevt = wrap(m, msgBodyOf) 
    fun isSet s = mbutIsSet (s, mbut)
    val draw = drawRect (feedback(drawableOfWin win)) pen

    fun do_rect (pos, clip_fn) = let
      val cursor = EXB.stdCursor dpy StdCursor.tcross
      val _ = changeActiveGrabCursor dpy cursor
      val initr = ptsToRect (pos, pos)
      val _ = draw initr
  
      fun loopRect (r, p) =
        case sync mevt of
          MOUSE_Motion {pt,...} => update(r,p,clip_fn pt)
        | MOUSE_LastUp {but,pt,...} => (draw r; r)
        | MOUSE_Up {but,pt,...} => update(r,p,clip_fn pt)
        | MOUSE_Down {pt,...} => update(r,p,clip_fn pt)
        | _ => loopRect (r,p)
      and update (oldr, oldp, newp) =
        if newp = oldp then loopRect (oldr, oldp)
        else let
          val newr = ptsToRect (pos, newp)
        in
          draw oldr;
          draw newr;
          loopRect (newr, newp)
        end
    in
      loopRect (initr, pos)
    end
  
    fun doRect () = let
      val rectCursor = EXB.stdCursor dpy StdCursor.sizing
      val xCursor = EXB.stdCursor dpy StdCursor.x_cursor
      
      (* Need to block output to win subtree *)

      (* Create overlay window and set its cursor *)
      val {sz=sz as SIZE{wid,ht}, ...} = geomOfWin win
      val overwin = EXeneWin.createInputOnlyWin win (mkRect (originPt, sz))
      val _ = EXeneWin.setCursor overwin (SOME rectCursor)
      val _ = EXeneWin.mapWin overwin

      fun extRect (RECT{x,y,wid,ht}) = RECT{x=x,y=y,wid=wid+1,ht=ht+1}
      fun clip (PT{x,y}) = PT{
        x= if x < 0 then 0 else if x >= wid then (wid-1) else x,
        y= if y < 0 then 0 else if y >= ht then (ht-1) else y
      }

        (* wait for mouse hit *)
      val (bttn, p) = waitMouse mevt
      val rect = if mbut = bttn then do_rect (clip p,clip)
                 else (waitUp(dpy,mevt,xCursor);RECT{x=0,y=0,wid=0,ht=0})
    in
      EXeneWin.destroyWin overwin;
      (* Unblock output to win subtree *)
      
      if mbut = bttn 
        then send(retc, SOME (extRect rect))
        else send (retc, NONE)
    end
  in
    spawn doRect;
    receive retc
  end
  
  (* moveRect:
   * Move outline of given rectangle on screen.
   * First window argument specifies window making the grab
   * of resources.
   * Rectangle is in coordinates of second window;
   * return final rectangle in coordinates of second window.
   * We assume argument bttn is down; we wait until that
   * button is up to record final rectangle; we return
   * when all buttons are up.
   *)
(*
  fun moveRect (ownwin : Pwin, win : Pwin, bttn : ButtonState, r : Rect) = let
  
    val winrect = inqRect win
    val winorigin = origin winrect
    val (winox, winoy) = coords winorigin
    val (wincx, wincy) = coords (winorigin + size winrect)
    val rsize = size r
    val (width, height) = coords rsize
    val bttnum = buttonNum bttn
  
      /* make overlay */
    val overlay = mkOverlayWin ownwin
  
      /* change cursor */
    val _ = setPointer (overlay, SOME moveCursor)
  
      /* get current mouse position */
    val (bttn0,pos0) = inqPointer overlay
  
      /* translate initial r to screen coordinates */
    val r0 = translate (r, winorigin)
  
      /* doRect assumes bttn is down, and loops until button is up. */
    fun doRect (p: Point, r : Rect) = let
      val Mouse(bttns, newp) = PW.readMouse overlay
    in
      if isUp (bttnum, bttns) then
          /* erase rectangle */
        drawRect (overlay, r, PN.XOrPen);
        r
      else let
        val (delx,dely) = coords (newp - p)           
        val (ox, oy) = coords (origin r)
          /* set new x values */
        val newox =
          if ox + delx < winox then winox
          elseif (ox + width + delx > wincx) then wincx - width
          else ox + delx fi
  
          /* set new y values */
        val newoy =
          if oy + dely < winoy then winoy
          elseif (oy + height + dely > wincy) then wincy - height
          else oy + dely fi
  
        val newr = mkRect(mkPoint(newox, newoy), rsize)
      in
          /* redraw only if new rectangle */
        if (ox != newox) orelse (oy != newoy) then
          drawRect (overlay, r, PN.XOrPen);
          drawRect (overlay, newr, PN.XOrPen)
        else () fi;
        doRect (newp, newr)
      end fi
    end
  
    val finalr = 
      if isUp (bttnum, bttn0) then
        r0
      else
        /* draw original rectangle */
        drawRect (overlay, r0, PN.XOrPen);
        doRect (pos0, r0)
      fi
  in
      /* reset cursor */
    resetPointer overlay;
  
      /* wait for buttons up */
    bttnsUp overlay;
  
      /* release overlay */
    delOverlayWin overlay;
  
    SOME (rtranslate (finalr, winorigin))
  end
*)

end (* Get *)
