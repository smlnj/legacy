(* scroll-viewer.sml
 *
 * An ML viewer with scroll bars.
 *)

structure ScrollViewer =
  struct
    structure G = Geometry
    structure W = Widget
    structure V = Viewer
    structure SB = Scrollbar
    structure SL = ScrollLayout

    fun mkViewer root (view, initLoc) = let
	  val isBW = (case (EXeneBase.displayClassOfScr (W.screenOf root))
	         of EXeneBase.StaticGray => true
		  | EXeneBase.GrayScale => true
		  | _ => false
		(* end case *))
	  val vsb = SB.mkVScrollbar root {color=NONE, sz = 10}
	  val vsbWidget = Frame.widgetOf (Frame.mkFrame {
		  color=NONE, width=1, widget=SB.widgetOf vsb
		})
	  val viewWid = V.widgetOf view
	  fun initSB () = let
		val {view_start, view_ht, nlines} = V.viewOf view
		val r_ht = real view_ht
		val r_nlines = if (nlines = 0) then r_ht else (real nlines)
		in
		  SB.setVals vsb {
		      sz = SOME(r_ht / r_nlines),
		      top = SOME(real view_start / r_nlines)
		    }
		end
	  val hScrollEvt = SB.evtOf vsb
	  fun setTop (newTop, nlines) = (
		V.scrollView (view, newTop);
		let val {view_start, nlines, ...} = V.viewOf view
		in
		  SB.setVals vsb {
		      sz = NONE,
		      top = if (nlines = 0)
			then SOME 0.0
			else SOME(real view_start / real nlines)
		    }
		end)
          fun smooth r = let
                val timeout = CML.timeout(CML.TIME{sec=0,usec=50000})
		val {view_start, nlines,...} = V.viewOf view
                val r_nlines = real nlines
                val top = floor(r * real nlines)
                fun handleSB i = fn evt => case evt
                      of (SB.ScrMove r) => let
                         val top' = floor(r * r_nlines)
                         in
                           if top' <> top then sm(i+1,top') else sm(i,top)
                         end
		       | (SB.ScrEnd r) => let
                         val top' = floor(r * r_nlines)
                         in
                           V.scrollView (view, top')
                         end
		       | _ => sm(i,top)
                and sm (0,top) = handleSB 0 (CML.sync hScrollEvt)
                  | sm (7,top) = (V.scrollView (view, top); sm(0,top))
                  | sm (i,top) =
		      CML.select [
			  CML.wrap (hScrollEvt, handleSB i),
			  CML.wrap (timeout, fn () => (V.scrollView (view, top); sm(0,top)))
			]
		in
                  if top <> view_start 
 		    then (V.scrollView (view, top); sm(0,top))
 		    else sm(0,view_start)
		end
	  fun scroller () = (case (CML.sync hScrollEvt)
		 of (SB.ScrUp r) => let (* move selected line to top *)
		      val {view_start, view_ht, nlines} = V.viewOf view
		      in
			setTop (view_start + floor(real view_ht * r), nlines)
		      end
		  | (SB.ScrDown r) => let (* move top to selected line *)
		      val {view_start, view_ht, nlines} = V.viewOf view
		      in
			setTop (view_start - floor(real view_ht * r), nlines)
		      end
		  | (SB.ScrStart r) => smooth r
		  | (SB.ScrMove r) => 
                       raise LibBase.Impossible "scroller: move before start"
		  | (SB.ScrEnd r) =>
                       raise LibBase.Impossible "scroller: end before start"
		(* end case *);
		scroller ())
	  fun scrollServer () = (
		V.scrollView (view, initLoc);
		initSB();
		scroller())
	  val layout = if isBW
		then Box.mkLayout root (Box.HzCenter [
		    Box.WBox vsbWidget,
		    Box.WBox(Divider.mkVertDivider root {
                        color=NONE, width=1
		      }),
		    Box.WBox viewWid
		  ])
		else SL.mkSBLayout root {
		    widget = viewWid,
		    hsb = NONE,
		    vsb = SOME{sb = vsbWidget, pad = 0, left = true}
		  }
	  in
	    XDebug.xspawn ("ScrollViewer.scroller", scrollServer);
	    Box.widgetOf layout
	  end

  end; (* ScrollViewer *)
