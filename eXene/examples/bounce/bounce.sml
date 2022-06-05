(* bounce.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure Bounce =
  struct
    local
      open CML Geometry EXeneBase Ball

    (* create and map the bounce window *)
      fun initBounce dpyName = let
	    val dpy = openDisplay (dpyName,NONE)
	    val scr = defaultScreenOf dpy
	    val (win, inEnv) =
		  EXeneWin.createSimpleTopWin scr {
		      geom = WGEOM{pos=PT{x=0, y=0}, sz=SIZE{wid=400, ht=400}, border=1},
		      border = blackOfScr scr,
		      backgrnd = color0 (* to get XOR to work *)
		    }
	    val (mouseEvt, keyEvt, cmdEvt) = let
		  open Interact
		  val InEnv{m, k, ci, ...} = ignoreKey inEnv
		  in
		    (wrap(m, msgBodyOf), wrap(k, msgBodyOf), wrap(ci, msgBodyOf))
		  end
	    val icon = createTileFromImage scr Heads.att_data
	    in
	      EXeneWin.setWMProperties win {
		  argv = SMLofNJ.getArgs(),
		  win_name = SOME "Bounce",
		  icon_name = SOME "bounce",
		  size_hints = [
		      ICCC.HINT_PPosition,
		      ICCC.HINT_PSize,
		      ICCC.HINT_PMinSize(SIZE{wid = 200, ht = 200})
		    ],
		  wm_hints = [ICCC.HINT_IconTile icon],
		  class_hints = NONE
		};
	      EXeneWin.mapWin win;
(** HOW DO WE SYNC ON THE MAPPING?? DO WE NEED TO?? **)
	      (dpy, win, mouseEvt, cmdEvt)
	    end (* initBounce *)

      fun runBounce dpyName = let
	    val (dpy, win, mouseEvt, cmdEvt) = initBounce dpyName
	    val windowSz = #sz (geomOfWin win)
	    val drawCh = BounceDM.bounceDM win
	    val mchan = Multicast.mChannel()
	    fun redraw (seqn, sz) = (
		  send(drawCh, BounceDM.Redraw seqn);
		  Multicast.multicast(mchan, REDRAW(seqn, sz)))
	    fun kill pt = Multicast.multicast(mchan, KILL pt)
	    fun killAll() = Multicast.multicast(mchan, KILL_ALL)
	    val newBall = mkBallFn (win, mchan, drawCh)
	    fun mkCursor c = let
		  val cursor = stdCursor dpy c
		  in
		    recolorCursor{
			cursor = cursor,
			fore_rgb = RGB{red=0w65535, green=0w65535, blue=0w65535},
			back_rgb = RGB{red=0w0, green=0w0, blue=0w0}

		      };
		    cursor
		  end
	    val normalCursor = mkCursor StdCursor.crosshair
	    val ballCursor = mkCursor StdCursor.dot
	    fun setCursor c = EXeneWin.setCursor win (SOME c)
	    fun quit () = (
		  closeDisplay dpy;
		  RunCML.shutdown OS.Process.success)
	    val popupMenu = Menu.popupMenu win
	    open Interact
	    fun waitLoop (seqn, sz) = let
		  fun handleM (MOUSE_FirstDown{but=MButton 1, pt, time, ...}) = (
			setCursor ballCursor; downLoop(seqn, sz, pt, time))
		    | handleM (MOUSE_FirstDown{but=MButton 2, pt, time, ...}) = (
			kill pt; waitLoop(seqn, sz))
		    | handleM (MOUSE_FirstDown{but as MButton 3, pt, time, ...}) = (
			case (sync (popupMenu (but, pt, time, mouseEvt)))
			 of NONE => waitLoop(seqn, sz)
			  | (SOME "Refresh") => (
			      redraw(seqn+1, sz); waitLoop(seqn+1, sz))
			  | (SOME "Kill All") => (killAll(); waitLoop(seqn, sz))
			  | (SOME "Quit") => quit()
			  | _ => raise LibBase.Impossible "Bounce: menu")
		    | handleM _ = waitLoop(seqn, sz)
		  fun handleCmd (CI_Redraw _) = (
			redraw(seqn+1, sz); waitLoop(seqn+1, sz))
		    | handleCmd (CI_Resize(RECT{wid, ht, ...})) = let
			val sz = SIZE{wid=wid, ht=ht}
			in
			  redraw(seqn, sz); waitLoop(seqn, sz)
			end
		    | handleCmd (CI_OwnDeath) = quit()
		    | handleCmd _ = ()
		  in
		    sync (choose [
			wrap(mouseEvt, handleM),
			wrap(cmdEvt, handleCmd)
		      ])
		  end
	    and downLoop (seqn, sz, p0, t0) = let
		  fun handleM (MOUSE_LastUp{but=MButton 1, pt, time, ...}) = let
			val sec = XTime.toReal(XTime.- (time, t0))
			val PT{x, y} = subPt(pt, p0)
			val dt = sec * (real updatesPerSec)
			fun limit a = let
			      val r = (real a) / dt
			      val da = Real.trunc r
			      val (abs, sign) =
				    if Real.!= (r, 0.0) then (~da, ~1) else (da, 1)
			      in
				if (da = 0)
				  then if Real.!= (r, 0.0)
				    then sign
				    else 0
				else if (abs*updatesPerSec > 1000)
				  then Int.quot(sign*200, updatesPerSec)
				  else da
			      end
			in
			  newBall(seqn, pt, PT{x = limit x, y = limit y}, sz);
			  backUp(seqn, sz)
			end
		    | handleM (MOUSE_LastUp _) = backUp(seqn, sz)
		    | handleM (MOUSE_Leave _) = backUp(seqn, sz)
		    | handleM _ = downLoop(seqn, sz, p0, t0)
		  fun handleCmd (CI_Redraw _) = (
			redraw(seqn+1, sz); backUp(seqn+1, sz))
		    | handleCmd (CI_Resize(RECT{wid, ht, ...})) = let
			val sz = SIZE{wid=wid, ht=ht}
			in
			  redraw(seqn, sz); backUp(seqn, sz)
			end
		    | handleCmd (CI_OwnDeath) = quit()
		    | handleCmd _ = ()
		  in
		    sync (choose [
			wrap(mouseEvt, handleM),
			wrap(cmdEvt, handleCmd)
		      ])
		  end
	    and backUp(seqn, sz) = (setCursor normalCursor; waitLoop(seqn, sz))
	    in
	      setCursor normalCursor;
	      waitLoop(0, windowSz)
	    end (* runBounce *)

    in

    fun doit' (flgs, dpy) = (
	  XDebug.init flgs;
	  RunCML.doit (
	    fn () => (XDebug.xspawn("bounce", fn () => runBounce dpy); ()),
	    SOME(Time.fromMilliseconds 10));
	  OS.Process.success)

    fun doit s = doit' ([],s)

    fun main (prog,server::_) = doit server
      | main _ = doit ""

    end (* local *)
  end (* Bounce *)
