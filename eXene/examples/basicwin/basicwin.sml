(* basicwin.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This code was transcribed from a C program that is under the following copyright:
 *
 * Copyright 1989 O'Reilly and Associates, Inc.
 *)

structure BasicWin : sig

    val doit' : (string list * string * Int32.int) -> OS.Process.status
    val doit  : string -> OS.Process.status
    val main  : (string * string list) -> OS.Process.status

  end = struct

    structure G = Geometry
    structure EXB = EXeneBase

    val minWid = 300 and minHt = 200
    val minSz = G.SIZE{wid = minWid, ht = minHt}

  (* a trace module for debugging output (see CML manual) *)
    val basicWTM = TraceCML.traceModule(XDebug.eXeneTM, "basicWin")
    fun trace f = TraceCML.trace (basicWTM, f)

  fun init dpyName = let
val _ = trace(fn () => ["open display ", dpyName, "\n"])
	val dpy = (EXB.openDisplay (dpyName, NONE))
		handle EXB.BadAddr s => (
		  TextIO.print s; TextIO.print "\n";
		  RunCML.shutdown OS.Process.failure)
	val scr = EXB.defaultScreenOf dpy
	val winSz = let val G.SIZE{wid, ht} = EXB.sizeOfScr scr
	      in
		G.SIZE{wid = wid div 3, ht = ht div 4}
	      end
	val (win, inEnv) =
	      EXeneWin.createSimpleTopWin scr {
		  geom = G.WGEOM{pos=G.PT{x=0, y=0}, sz=winSz, border=1},
		  border = EXB.blackOfScr scr,
		  backgrnd = EXB.whiteOfScr scr
		}
(** The real basicwin gets the list of icon sizes for the display here **)
	val iconTile = EXB.createTileFromImage scr IconBitmap.iconBitmap
	in
trace(fn () => ["set props\n"]);
	  EXeneWin.setWMProperties win {
	      argv = SMLofNJ.getArgs(),
	      win_name = SOME "Basic Window Program",
	      icon_name = SOME "basicwin",
	      size_hints = [
		  ICCC.HINT_PPosition,
		  ICCC.HINT_PSize,
		  ICCC.HINT_PMinSize minSz
		],
	      wm_hints = [ICCC.HINT_IconTile iconTile],
	      class_hints = SOME{res_name="basicwin", res_class="Basicwin"}
	    };
	  EXeneWin.mapWin win;
	  (dpy, scr, inEnv, win)
	end

  fun mkPen scr = Drawing.newPen [
	  Drawing.PV_Foreground(EXB.blackOfScr scr),
	  Drawing.PV_LineWidth 6,
	  Drawing.PV_LineStyle_OnOffDash,
	  Drawing.PV_CapStyle_Round,
	  Drawing.PV_JoinStyle_Round,
	  Drawing.PV_DashOffset 0,
	  Drawing.PV_Dash_List [12, 24]
	]

  fun loadFont dpy = Font.openFont dpy "9x15"

  fun placeText (win, pen, font, G.SIZE{wid, ht}) = let
val _ = trace(fn () => ["placeText:\n"])
	val drawString = Drawing.drawString (Drawing.drawableOfWin win) pen font
	val textWidth = Font.textWidth font
	val (fontHt, fontDescent) = let val {ascent, descent} = Font.fontHt font
	      in
		(ascent + descent, descent)
	      end
	fun draw (yPos, s) = let
	      val w = textWidth s
	      in
		drawString(G.PT{x = ((wid - w) div 2), y = yPos}, s)
	      end
	val yOffset = (ht div 2) - fontHt - fontDescent
	val G.SIZE{wid=scrWid, ht=scrHt} = EXB.sizeOfScr(EXeneWin.screenOfWin win)
	val depth = EXB.depthOfScr(EXeneWin.screenOfWin win)
	in
trace(fn () => ["placeText: draw text\n"]);
	  app draw [
	      (fontHt,			"Hi! I'm a window, who are you?"),
	      (ht - (2*fontHt),		"To terminate program: press any key"),
	      (yOffset,			"Screen Dimensions:"),
	      (yOffset + fontHt,	" Height - "^(Int.toString scrHt)^" pixels"),
	      (yOffset + (2*fontHt),	" Width  - "^(Int.toString scrWid)^" pixels"),
	      (yOffset + (3*fontHt),	" Depth  - "^(Int.toString depth)^" plane(s)"),
	      (ht - fontHt,		"or button while in this window")
	    ]
	end

  fun placeGraphics (win, pen, G.SIZE{wid=winWid, ht=winHt}) = let
val _ = trace(fn () => ["placeGraphics:\n"])
	val wid = (3 * winWid) div 4
	val ht = winHt div 2
	in
	  Drawing.drawRect (Drawing.drawableOfWin win) pen
	    (G.RECT{
		x = (winWid div 2) - (wid div 2),
		y = (winHt div 2) - (ht div 2),
		wid = wid, ht = ht
	      })
	end

  fun tooSmall (win, pen, font) = let
	val {ascent, ...} = Font.fontHt font
	in
	  Drawing.drawString (Drawing.drawableOfWin win) pen font
	    (G.PT{x=2, y=ascent+2}, "Too Small")
	end

  fun basicwin dpy = let
	open Interact
val _ = trace(fn () => ["init\n"]);
	val (dpy, scr, InEnv{m, k, ci, ...}, win) = init dpy
	val m = CML.wrap(m, msgBodyOf)
	val k = CML.wrap(k, msgBodyOf)
	val ci = CML.wrap(ci, msgBodyOf)
val _ = trace(fn () => ["mkPen\n"]);
	val pen = mkPen scr
val _ = trace(fn () => ["load\n"]);
	val font = loadFont dpy
	fun quit _ = (
	      trace(fn () => ["QUIT\n"]);
	      EXB.closeDisplay dpy;
	      RunCML.shutdown OS.Process.success)
	fun sizeTooSmall (G.SIZE{wid, ht}) = (wid < minWid) orelse (ht < minHt)
	fun loop (sz) = let
	      fun handleM (MOUSE_FirstDown _) = quit()
		| handleM (MOUSE_LastUp _) = quit()
		| handleM _ = loop (sz)
	      fun handleCI (CI_Resize(G.RECT{wid, ht, ...})) =
		    loop (G.SIZE{wid=wid, ht=ht})
		| handleCI (CI_Redraw _) = (
		    if (sizeTooSmall sz)
		      then tooSmall(win, pen, font)
		      else (
			placeText(win, pen, font, sz);
			placeGraphics (win, pen, sz));
		    loop sz)
		| handleCI (CI_Die) = quit()
	      in
		CML.select [
		    CML.wrap(m, handleM),
		    CML.wrap(k, quit),
		    CML.wrap(ci, handleCI)
		  ]
	      end
	in
trace(fn () => ["go\n"]);
	  loop(minSz)
	end

  fun doit' (flgs, dpy, tq) = (
        XDebug.init flgs;
        RunCML.doit (
	  fn () => (XDebug.xspawn("basicwin", fn () => basicwin dpy); ()),
	  SOME(Time.fromMilliseconds tq)))

  fun doit s = doit' ([], s, 20)

  fun main (prog, "-display" :: server :: _) = basicwin server
    | main _ = basicwin ""

end
