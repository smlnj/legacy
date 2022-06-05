functor AnimateSimFun
    (structure Sim: SIM
     val bodyData: (Sim.vect * Sim.vect * real * int * string option) list) =
struct

    structure V = Sim.V
    structure W = Widget
    structure D = Drawing
    structure G = Geometry
    structure I = Interact
    structure S = Styles
    structure A = Attrs

    val contr_size = 12

    val G = 6.67e~8
    val dt = 500.0 (* 2000.0 *)
    val max = 7.80e13 * 2.1

    val N = 30 (* 15 *)

    fun spacer n = Box.Glue { nat = n, min = n, max = SOME n }
    fun rubber n = Box.Glue { nat = n, min = 1, max = NONE }

    val sp5 = spacer 5

    fun mkSimDisplay (root, view) = let

	fun quit () = let
	    fun q () =
		(CML.sync (CML.timeOutEvt (Time.fromMilliseconds 20));
		 Root.delRoot root;
		 RunCML.shutdown OS.Process.success)
	in
	    CML.spawn q; ()
	end

	val scr = W.screenOf root
	val display = W.displayOf root
	val black = W.EXB.blackOfScr scr
	val white = W.EXB.whiteOfScr scr
	val colorByName = (W.EXB.colorOfScr scr) o W.EXB.CMS_Name

	local
	    val recalCh = CML.channel ()
	in
	    val recalEvt = CML.recvEvt recalCh
	    fun recal () = CML.send (recalCh, ())
	end

	val s_args = [(A.attr_isVertical, A.AV_Bool false),
		      (A.attr_background, A.AV_Str "gray"),
		      (A.attr_width, A.AV_Int contr_size),
		      (A.attr_fromValue, A.AV_Int 0),
		      (A.attr_toValue, A.AV_Int 100)]
	val q_args = [(A.attr_label, A.AV_Str "Q")]
	val r_args = [(A.attr_label, A.AV_Str "R")]
	val slider = Slider.slider (root, view, s_args)
	fun centerSlider () = Slider.setValue slider 50
	val (qbuttonW, rbuttonW) = let
	    val s = 2 * contr_size
	    val qb = Button.textCmd (root, view, q_args) quit
	    val rb = Button.textCmd (root, view, r_args) recal
	in
	    (Shape.fixSize (Button.widgetOf qb, G.SIZE { wid = s, ht = s }),
	     Shape.fixSize (Button.widgetOf rb, G.SIZE { wid = s, ht = s }))
	end
	val controls_line = Box.HzCenter
	    [sp5, Box.WBox rbuttonW,
	     sp5, Box.WBox (Slider.widgetOf slider),
	     sp5, Box.WBox qbuttonW, sp5]

	local
	    val sliderEvt = Slider.evtOf slider
	    val zoomCh = CML.channel ()
	in
	    val zoomEvt = CML.recvEvt zoomCh
	    fun sliderThread base = let
		fun loop (base, cur) =
		    CML.select
		      [CML.wrap (sliderEvt,
				 fn sp => handleSlider (base, sp)),
		       CML.wrap (recalEvt, fn () => handleRecal cur)]
		and handleSlider (base, sp) = let
		    val fact = Math.pow (2.0, Real.fromInt (sp - 50) / 50.0)
		    val cur = base * fact
		in
		    CML.send (zoomCh, cur);
		    loop (base, cur)
		end
		and handleRecal cur = (centerSlider (); loop (cur, cur))
	    in
		centerSlider ();
		loop (base, base)
	    end
	end

	val drawPen = D.newPen [D.PV_Foreground white]
	val erasePen = D.newPen [D.PV_Foreground black]
	val timer = CML.timeOutEvt (Time.fromMilliseconds 20)

	fun mkBody (p, v, m, r, cs) = let
	    val color = getOpt (Option.map colorByName cs, white)
	    val pen = D.newPen [D.PV_Foreground color]
	in
	    { p = p, v = v, m = m, data = { pen = pen, radius = r }}
	end
	val bodies = map mkBody bodyData

	val reqCh = CML.channel ()
	val simT =
	    Sim.run { G = G, bodies = bodies, dt = dt, msgchan = reqCh, n = N }
	val simDeathEvt = CML.joinEvt simT

	fun realize { win, sz = G.SIZE { wid, ht }, env } = let

	    val depth = W.EXB.depthOfWin win
	    val drawwin = D.drawableOfWin win
	    val drawwin = D.feedback drawwin
	    val drawCircle = D.fillCircle drawwin
	    val I.InEnv { ci, m, ... } = I.ignoreKey env

	    datatype panCmd = PAN of { horiz: int, vert: int }

	    val panCh = CML.channel ()
	    val panEvt = CML.recvEvt panCh

	    fun mouseThread () = let
		fun idle () =
		    case I.msgBodyOf (CML.sync m) of
			I.MOUSE_FirstDown { but = I.MButton 1, pt, ... } =>
			    pan pt
		      | I.MOUSE_FirstDown { but = I.MButton 3, ... } => 
			    (quit (); idle ())
		      | _ => idle ()

		and pan (pt' as G.PT { x = x', y = y' }) =
		    case I.msgBodyOf (CML.sync m) of
			I.MOUSE_Motion { pt = pt as G.PT { x, y }, ... } =>
			    (CML.send (panCh,
				       PAN { horiz = x - x', vert = y - y' });
			     pan pt)
		      | I.MOUSE_Up { but = I.MButton 1, ... } => idle ()
		      | I.MOUSE_LastUp _ => idle ()
		      | _ => pan pt'
	    in
		idle ()
	    end

	    fun newTranslation { ocl, wid, ht, WZx, WZy, zoom } = let

		fun winCircle { p, v, m, data = { pen, radius } } = let
		    val { x, y } = V.proj2d p
		    val scrx = Real.round ((x - WZx) * zoom) handle _ => 0
		    val scry = Real.round ((y - WZy) * zoom) handle _ => 0
		in
		    { center = G.PT { x = scrx, y = scry }, rad = radius }
		end

		fun drawBody (new as { data = { pen, ... }, ... }) = let
		    val nc = winCircle new
		in
		    drawCircle pen nc; nc
		end

		fun moveBody (oc, new) =
		    (drawCircle erasePen oc; drawBody new)

		fun update ol = let
		    val ch = CML.channel ()
		    val _ = CML.send (reqCh, Sim.QUERY ch)
		    val nl = CML.recv ch
		in
		    SOME (case ol of
			      SOME ol => ListPair.map moveBody (ol, nl)
			    | NONE => List.map drawBody nl)
		end

		fun death cl = (print "Simulation has died!\n"; quit ();
				loop cl)

		and loop cl = CML.select
		    [CML.wrap (simDeathEvt, fn () => death cl),
		     CML.wrap (timer, fn () => (loop (update cl))),
		     CML.wrap (ci, fn x => handleCI (cl, I.msgBodyOf x)),
		     CML.wrap (panEvt, fn p => handlePan (cl, p)),
		     CML.wrap (zoomEvt, fn z => handleZoom (cl, z))]

		and handleCI (cl, I.CI_Resize (G.RECT r)) =
		    let
			val { wid = nw, ht = nh, ... } = r
			val f = 0.5 / zoom
		    in
			D.clearDrawable drawwin;
			newTranslation
			  { ocl = cl, wid = nw, ht = nh,
			    WZx = WZx - Real.fromInt (nw - wid) * f,
			    WZy = WZy - Real.fromInt (nh - ht) * f,
			    zoom = zoom }
		    end
		  | handleCI (cl, _) = loop cl

		and handlePan (cl, PAN { horiz, vert }) =
		    newTranslation
		      { ocl = cl, wid = wid, ht = ht, zoom = zoom,
		        WZx = WZx - Real.fromInt horiz / zoom,
			WZy = WZy - Real.fromInt vert / zoom }

		and handleZoom (cl, z) = let
		    val f = 0.5 * (1.0 / zoom - 1.0 / z)
		in
		    newTranslation
		      { ocl = cl, wid = wid, ht = ht, zoom = z,
		        WZx = WZx + Real.fromInt wid * f,
			WZy = WZy + Real.fromInt ht * f }
		end
	    in
		loop ocl
	    end

	    fun threadBody () = let
		val zoom = Real.fromInt wid / max
		val f = ~0.5 / zoom;
		val WZx = Real.fromInt wid * f
		val WZy = Real.fromInt ht * f
	    in
		CML.spawn (fn () => sliderThread zoom);
		newTranslation { ocl = NONE,
				 wid = wid, ht = ht,
				 WZx = WZx, WZy = WZy, zoom = zoom }
	    end
	    (* val gcTimeOut = CML.timeOutEvt (Time.fromSeconds 10)
	    fun gcThread () =
		(CML.sync gcTimeOut; SMLofNJ.Internals.GC.doGC 5; gcThread ())
		*)
	in
	    (* CML.spawn gcThread; *)
	    CML.spawn mouseThread;
	    CML.spawn threadBody;
	    ()
	end
	val size = W.fixBounds (500, 500)
	val dispW =
	    Shape.mkFlex (W.mkWidget
		   { boundsOf = fn () => size,
		     args = fn () => { background = SOME black },
		     root = root,
		     realize = realize })
    in
	Box.widgetOf (Box.mkLayout root (Box.VtCenter
			 [sp5, controls_line, sp5, Box.WBox dispW, sp5]))
    end

    fun simdisplay root = let
	val style = W.styleFromStrings (root, [])
	val name = S.mkView { name = S.styleName [],
			      aliases = [S.styleName []] }
	val view = (name,style)
	val sd = mkSimDisplay (root, view)
	val args = [(A.attr_title, A.AV_Str "N-Body"),
		    (A.attr_iconName, A.AV_Str "n-body")]
	val shell = Shell.shell (root, view, args) sd
    in
	Shell.init shell
    end

    fun doit' (debugFlags, server) =
	(XDebug.init debugFlags;
	 RunEXene.runWArgs simdisplay { dpy = SOME server, timeq = SOME 20 })
  
    fun doit () = RunEXene.run simdisplay

    fun main (_: string, prog :: server :: _) = doit' ([], server)
      | main _ = doit ()

    val main = (fn () => OS.Process.success) o main
end
