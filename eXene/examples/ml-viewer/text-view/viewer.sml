(* viewer.sml
 *
 * This is a ML source code viewer, which is a test application for
 * the new text widget.
 *)


structure Viewer : sig

    type viewer

    datatype face = Face of {
	font : Font.font option,
	color : EXeneBase.color_spec option
      }

    val mkViewer : Widget.root -> {
	    src : {space:int, kind:ViewBuffer.token_kind, text:string} list list,
	    font : Font.font,
	    comm_face : face,
	    kw_face : face,
	    sym_face : face,
	    id_face : face,
	    backgrnd : EXeneBase.color_spec
	  } -> viewer

    val widgetOf : viewer -> Widget.widget

    val viewOf : viewer -> {view_start : int, view_ht : int, nlines : int}

    val scrollView : (viewer * int) -> unit

  end = struct

    structure G = Geometry
    structure W = Widget
    structure VB = ViewBuffer
    structure TD = TextDisplay (structure TextPool = VB)
    structure TC = TD.TextPool.TextCanvas

    datatype req_msg
      = ViewMsg of {view_start : int, view_ht : int, nlines : int} CML.cond_var
      | ScrollMsg of int

    datatype viewer = Viewer of {
	widget : W.widget,
	text_dpy : TD.text_display CML.cond_var,
	req_ch : req_msg CML.chan
      }

    datatype face = Face of {
	font : Font.font option,
	color : EXeneBase.color_spec option
      }

    fun mkViewer root {
	  src, font, comm_face, kw_face, sym_face, id_face, backgrnd
	} = let
	  val cv = CML.condVar()
	  val reqCh = CML.channel()
	  val {ascent, descent} = Font.fontHt font
	  val lineHt = (ascent+descent)
	  val charWid = Font.textWidth font "m"
	  fun realize {env, win, sz as G.SIZE{ht, ...}} = let
		val canvas = TC.mkTextCanvas {
			win = win,
			size = sz,
			font = font,
			foregrnd = NONE,
			backgrnd = (SOME backgrnd)
		      }
		fun mkTB (Face{font, color}) = let
		      fun mk attrs = TC.mkTypeBall(canvas, attrs)
		      in
			case (font, color)
			 of (NONE, NONE) => mk []
			  | (SOME f, NONE) => mk [TC.TBV_Font f]
			  | (NONE, SOME c) => mk [TC.TBV_Foregrnd c]
			  | (SOME f, SOME c) =>
			      mk [TC.TBV_Font f, TC.TBV_Foregrnd c]
			(* end case *)
		      end
		val pool = VB.mkViewBuffer {
			src = src,
			nrows = ht quot lineHt,
			font = font,
			char_wid = charWid,
			ascent = ascent,
			descent = descent,
			line_ht = lineHt,
			fill_tb = TC.mkTypeBall(canvas, [TC.TBV_Foregrnd backgrnd]),
			comment_tb = mkTB comm_face,
			keyword_tb = mkTB kw_face,
			symbol_tb = mkTB sym_face,
			ident_tb = mkTB id_face
		      }
		val textDpy = TD.mkTextDisplay{
			canvas = canvas, text = pool, size = sz
		      }
		val redraw = TD.redraw textDpy
		val scrollUp = TD.scrollUp textDpy
		val scrollDown = TD.scrollDown textDpy
	      (* clear and fill in the region vacated by a scroll operation *)
		fun fillIn {vacated, damage} = (
		      TD.clearRect textDpy vacated;
		      redraw (vacated :: (CML.sync damage)))
		val Interact.InEnv{ci, ...} = Interact.ignoreInput env
		fun handleCI msg = (case (Interact.msgBodyOf msg)
		       of (Interact.CI_Redraw damage) => redraw damage
			| (Interact.CI_Resize(G.RECT{wid, ht, ...})) =>
			    TD.resize (textDpy, G.SIZE{wid=wid, ht=ht})
			| Interact.CI_OwnDeath => (
			    VDebug.pr["Viewer.die\n"]; CML.exit())
			| _ => ()
		      (* end case *))
		fun handleReq (ViewMsg replyCV) =
		      CML.writeVar (replyCV, VB.getView pool)
		  | handleReq (ScrollMsg newTop) = let
		      val {view_start, view_ht, nlines} = VB.getView pool
		      val _ = VB.setViewTop (pool, newTop)
		      val {view_start=newTop, ...} = VB.getView pool
		      in
			if (newTop = view_start)
			  then () (* no scroll necessary *)
			  else let
			    val delta = (newTop - view_start)
			    val G.SIZE{wid, ht} = TD.sizeOf textDpy
			    in
			      if (abs delta >= view_ht)
				then (
				  TC.clear canvas;
				  redraw [G.RECT{x=0, y=0, wid=wid, ht=ht}])
			      else if (delta < 0)
				then fillIn (scrollDown (~delta))
				else fillIn (scrollUp delta)
			    end
		      end
		val evt = [
			CML.wrap (ci, handleCI),
			CML.wrap (CML.receive reqCh, handleReq)
		      ]
		fun server () = (CML.select evt; server())
		in
		  CML.writeVar(cv, textDpy);
		  XDebug.xspawn("Viewer.server", server);
		  ()
		end
	  in
	    Viewer{
		widget = W.mkWidget{
		    root = root,
		    boundsOf = fn () => {
			x_dim = W.DIM{
			    base=0, incr=1, min=10, nat=80*charWid, max=NONE
			  },
			y_dim = W.DIM{
			    base=0, incr=1, min=20, nat=24*lineHt, max=NONE
			  }
		      },
		    realize = realize
		  },
		text_dpy = cv,
		req_ch = reqCh
	      }
	  end

    fun widgetOf (Viewer{widget, ...}) = widget

    fun viewOf (Viewer{req_ch, ...}) = let
	  val cv = CML.condVar()
	  in
	    CML.send (req_ch, ViewMsg cv);
	    CML.readVar cv
	  end

    fun scrollView (Viewer{req_ch, ...}, newTop) =
	  CML.send (req_ch, ScrollMsg newTop)

  end; (* Viewer *)
