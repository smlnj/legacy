(* tri.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

structure Main : sig

    val doit' : string list * string -> OS.Process.status
    val doit : string -> OS.Process.status
    val main : (string * string list) -> OS.Process.status

  end = struct

  open CML Geometry EXeneBase
  structure I = Interact
  structure D = Drawing

  val minWid = 300 and minHt = 300
  val minSz = SIZE{wid = minWid, ht = minHt}

  val buttonWid = 100 and buttonHt = 30
  val buttonCornerRad = 8
  fun buttonWinGeom (wid, ht) = WGEOM{
	  pos = PT{x = (wid - buttonWid) div 2, y = ht-(buttonHt+10)},
	  sz = SIZE{wid=buttonWid, ht=buttonHt},
	  border = 0
	}

  fun drawWinGeom (wid, ht) = WGEOM{
	  pos = PT{x = 5, y = 5},
	  sz = SIZE{wid = wid - 10, ht = ht - (buttonHt+25)},
	  border = 1
	}

  fun init dpyName = let
	val dpy = openDisplay (dpyName,NONE)
	val scr = defaultScreenOf dpy
	val winSz = SIZE{wid = 450, ht = 400}
	val (win, inEnv) =
	      EXeneWin.createSimpleTopWin scr {
		  geom = WGEOM{pos=PT{x=0, y=0}, sz=winSz, border=1},
		  border = blackOfScr scr,
		  backgrnd = whiteOfScr scr
		}
	val iconTile = createTileFromImage scr IconBitmap.icon_bitmap
	in
	  EXeneWin.setWMProperties win {
	      argv = SMLofNJ.getArgs(),
	      win_name = SOME "Triangle",
	      icon_name = SOME "triangle",
	      size_hints = [
		  ICCC.HINT_PPosition,
		  ICCC.HINT_PSize,
		  ICCC.HINT_PMinSize minSz
		],
	      wm_hints = [ICCC.HINT_IconTile iconTile],
	      class_hints = SOME{res_name="triangle", res_class="Triangle"}
	    };
	  EXeneWin.mapWin win;
	  (scr, win, inEnv)
	end

  fun allocWindows dpy = let
	val (scr, topWin, topEnv) = init dpy
	val (SIZE{wid, ht}) = sizeOfWin topWin
	val drawWin = EXeneWin.createSimpleSubwin topWin {
		geom = drawWinGeom(wid, ht),
		border = SOME(blackOfScr scr),
		backgrnd = SOME(whiteOfScr scr)		
	      }
	val buttonWin = EXeneWin.createSimpleSubwin topWin {
		geom = buttonWinGeom(wid, ht),
		border = NONE,
		backgrnd = SOME(whiteOfScr scr)		
	      }
	in
	  EXeneWin.mapWin drawWin;
	  EXeneWin.mapWin buttonWin;
	  {top_win=topWin, top_env=topEnv, draw_win=drawWin, but_win=buttonWin}
	end

  fun mkButtonThreads (win, env) = let
	val I.InEnv{m, ci, ...} = I.ignoreKey env
	val mouseEvt = wrap (m, I.msgBodyOf)
	val cmdEvt = wrap (ci, I.msgBodyOf)
	val drawable = D.drawableOfWin win
	val pen = D.newPen[
		D.PV_Function D.OP_Copy,
		D.PV_Foreground(blackOfScr(EXeneWin.screenOfWin win))
	      ]
	val drawRect = RoundedRect.drawRoundedRect drawable pen
	val text = "RESET"
	val font = Font.openFont (EXeneWin.displayOfWin win) "9x15"
	val textPt = let
	      val textWidth = Font.textWidth font text
	      val {ascent, descent} = Font.fontHt font
	      in
		PT{
		    x = (buttonWid - textWidth) div 2,
		    y = ((buttonHt - (ascent + descent)) div 2) + ascent
		  }
	      end
	val drawText = D.drawString drawable pen font
	fun redraw () = (
	      drawRect {
		  rect = RECT{x = 0, y = 0, ht = buttonHt-1, wid = buttonWid-1},
		  c_wid = buttonCornerRad,
		  c_ht = buttonCornerRad
		};
	      drawText (textPt, text))
	val resetCh = channel()
	fun loop () = let
	      fun mouseFn (I.MOUSE_FirstDown _) = send(resetCh, ())
		| mouseFn _ = ()
	      fun cmdFn (I.CI_Redraw _) = redraw()
		| cmdFn I.CI_OwnDeath = ()
		| cmdFn _ = ()
	      in
		select [
		    wrap (mouseEvt, mouseFn),
		    wrap (cmdEvt, cmdFn)
		  ];
		loop ()
	      end
	in
	  spawn loop;
	  recvEvt resetCh
	end (* mkButtonThreads *)

  fun mkDrawThreads (win, resetEvt, env) = let
	val I.InEnv{m, ci, ...} = I.ignoreKey env
	val mouseEvt = wrap (m, I.msgBodyOf)
	val cmdEvt = wrap (ci, I.msgBodyOf)
	val drawCh = channel()
	fun mouseThread () = (case (sync mouseEvt)
	       of I.MOUSE_FirstDown{pt, ...} => send(drawCh, pt)
		| _ => ()
	      (* end case *);
	      mouseThread())
	val drawEvt = recvEvt drawCh
	val drawable = D.drawableOfWin win
	val pen = D.newPen [
		D.PV_Function D.OP_Copy,
		D.PV_Foreground(blackOfScr(EXeneWin.screenOfWin win))
	      ]
	val draw = D.fillPolygon drawable pen
	fun drawTriangle (PT{x, y}) = draw {
		verts = [PT{x=x, y=y-10}, PT{x=x-8, y=y+6}, PT{x=x+8, y=y+6}],
		shape = D.ConvexShape
	      }
	fun loop state = let
		fun reset () = (D.clearDrawable drawable; loop[])
		fun handleCmd (I.CI_Redraw _) = (
		      D.clearDrawable drawable;
		      app drawTriangle state;
		      loop state)
		  | handleCmd I.CI_OwnDeath = ()
		  | handleCmd _ = (loop state)
		fun draw pt = (drawTriangle pt; loop(pt::state))
		in
		  select [
		      wrap (resetEvt, reset),
		      wrap (cmdEvt, handleCmd),
		      wrap (drawEvt, draw)
		    ]
		end
	  in
	    spawn mouseThread;
	    spawn (fn () => loop []);
	    ()
	  end (* mkDrawThreads *)

  fun mkTopLevelThreads {top_win, top_env=I.InEnv{k, m, ci, ...}, but_win, draw_win} = let
	val (butInEnv, butOutEnv) = I.createWinEnv()
	val (drawInEnv, drawOutEnv) = I.createWinEnv()
	val (inEnv, outEnv) = I.createWinEnv()
	val inEnv = I.ignoreAll inEnv
	fun findEnv msg = (case (I.stripMsg msg)
	       of (I.Here _) => outEnv
		| (I.ToChild msg') => (
		    if (I.toWindow(msg', draw_win))
		      then drawOutEnv
		    else if (I.toWindow(msg', but_win))
		      then butOutEnv
		      else raise (Fail "findEnv"))
	      (* end case *))
	val kbdEvt = (wrap(k, fn msg => let
		val I.OutEnv{k, ...} = findEnv msg
		in
		  sync (k msg)
		end))
	val mouseEvt = (wrap(m, fn msg => let
		val I.OutEnv{m, ...} = findEnv msg
		in
		  sync (m msg)
		end))
	val cmdEvt = (wrap(ci, fn msg => let
		val I.OutEnv{ci, ...} = findEnv msg
		in
		  sync (ci msg)
		end))
	fun router () = (
	      select [kbdEvt, mouseEvt, cmdEvt];
	      router())
	in
	  spawn router;
	  {but_env = butInEnv, draw_env = drawInEnv}
	end

  fun triangle dpy = let
	open CML Interact
	val (x as {but_win, draw_win, ...}) = allocWindows dpy
	val {but_env, draw_env} = mkTopLevelThreads x
	in
	  mkDrawThreads (draw_win, mkButtonThreads (but_win, but_env), draw_env)
	end

    fun doit' (flgs, dpy) = (
	  XDebug.init flgs;
	  RunCML.doit (
	    fn () => (XDebug.xspawn("triangle", fn () => triangle dpy); ()),
	    SOME(Time.fromMilliseconds 20)))
    fun doit s = doit' ([], s)

    fun main (prog, server::_) = doit server
      | main _ = doit ""

  end (* Main *)

