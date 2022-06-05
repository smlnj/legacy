(* scrollport.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * ScrollPort widget, for panning over a child widget
 * using scrollbars.
 *
 * TODO:
 *   granularity
 *)

structure ScrollPort : SCROLL_PORT =
  struct

    structure W = Widget
    structure CML = CML

    open Geometry EXeneBase Interact Widget

    datatype scroll_port = SP of {widget : widget}
    fun monitor (continuous, scrollb, sw, setview, geomEvt) () = let
	  open Scrollbar

	  val set = setVals scrollb
	  val scrollEvt = evtOf scrollb

	  fun init isOn (origin, sz, total) = let
		val r_total = real total
		val r_sz = real sz
		val maxo = total-sz

	        fun shiftUp (r, y) = let
		      val y' = y + Int.min(maxo-y, trunc((1.0-r)*r_sz))
		      in
		  	if (y = y')
			  then y
			  else (
			    setview y';
			    set{top = SOME((real y')/r_total), sz = NONE};
			    y')
			      handle _ => y
		      end

		fun shiftDown (r, y) = let
		      val y' = Int.max(0,y-trunc(r*r_sz))
		      in
			if (y = y')
			  then y
			  else (
			    setview y';
			    set{top = SOME((real y')/r_total), sz = NONE};
			    y') 
			      handle _ => y
		    end

		fun adjust (r,y) = let
		      val y' = trunc(r*r_total)
		      in
			if (y = y') then y else ((setview y'; y') handle _ => y)
		      end

		fun handle_sb adjustfn arg = (case arg
		       of (ScrStart r,y) => adjustfn(r,y)
			| (ScrUp r,y) => shiftUp(r,y)
			| (ScrDown r,y) => shiftDown(r,y)
			| (ScrMove r,y) => adjustfn(r,y)
			| (ScrEnd r,y) => adjust(r,y)
		    (* end of case *))

		val handleSB = if continuous
		      then (handle_sb adjust)
		      else (handle_sb (fn (_, y) => y))

		fun onloop origin = CML.select [
		      CML.wrap (scrollEvt, fn evt => onloop (handleSB (evt, origin))),
		      CML.wrap (geomEvt, init true)
		    ]
                fun offloop () = init false (CML.sync geomEvt)
              
		in
                  if maxo <= 0
                    then (if isOn then sw false else (); offloop ())
                    else let
                      val sz = r_sz/r_total
                      val top = (real origin)/r_total
                      in
		        set {sz = SOME sz, top = SOME top};
                        if isOn then () else sw true;
		        onloop origin
                      end
		end (* init *)
	  in
	    init false (0,1,1)
	  end (* monitor *)

    fun main (vp,vf,hf) = let
	  val vpEvt = Viewport.evtOf vp
	  fun loop () = let
		val {rect=RECT{x,y,wid,ht},childSz=SIZE sz} = CML.sync vpEvt
		in
		  vf (y,ht, #ht sz);
		  hf (x,wid, #wid sz);
		  loop ()
		end
	  in
	    loop ()
	  end (* main *)

    val attr_continuous = Quark.quark "continuous"
    val attr_hsb = Quark.quark "hsb"
    val attr_vsb = Quark.quark "vsb"

    val attrs = [
        (Attrs.attr_background,     Attrs.AT_Color,    Attrs.AV_Str "white"),
        (attr_continuous,           Attrs.AT_Bool,     Attrs.AV_Bool false),
        (attr_hsb,                  Attrs.AT_Bool,     Attrs.AV_NoValue),
        (attr_vsb,                  Attrs.AT_Bool,     Attrs.AV_NoValue)
      ]

    fun doLayout (w,SOME top,SOME left,view as (name,style)) = let
          open Box
          val root = rootOf w
          val hview = (Styles.extendView (name,"hscrollbar"),style)
          val hsb = Scrollbar.hScrollbar (root,hview,[])
          val hfr = Frame.frame (root,hview,[]) (Scrollbar.widgetOf hsb)
          val b1 = if top 
                      then layout (root,view,[]) (VtCenter[WBox(Frame.widgetOf hfr),WBox w])
                      else layout (root,view,[]) (VtCenter[WBox w,WBox(Frame.widgetOf hfr)])
          val vview = (Styles.extendView (name,"vscrollbar"),style)
          val vsb = Scrollbar.vScrollbar (root,vview,[])
          val vfr = Frame.frame (root,vview,[]) (Scrollbar.widgetOf vsb)
          val {y_dim=DIM{base,incr,min,nat,max},...} = 
                 boundsOf (Frame.widgetOf hfr)
          val g = Glue{nat=base+incr*nat,min=base+incr*min,
                 max= case max of NONE => NONE | SOME mx => SOME(base+incr*mx)}
          val b2 = if top
                     then layout (root,view,[]) (VtCenter[g, WBox (Frame.widgetOf vfr)])
                     else layout (root,view,[]) (VtCenter[WBox (Frame.widgetOf vfr), g])
          val hnum = if top then 0 else 1
          val vnum = if left then 0 else 1
          val b = if left 
                    then layout (root,view,[])
                           (HzCenter[WBox (widgetOf b2), WBox (widgetOf b1)])
                    else layout (root,view,[])
                           (HzCenter[WBox (widgetOf b1), WBox (widgetOf b2)])
          fun vsw true = mapBox b [vnum] | vsw false = unmapBox b [vnum]
          fun hsw true = (mapBox b1 [hnum]; mapBox b2 [hnum])
            | hsw false = (unmapBox b1 [hnum]; unmapBox b2 [hnum])
          in hsw false; vsw false; (b,SOME (hsb,hsw), SOME (vsb,vsw)) end
      | doLayout (w,SOME top,NONE,view as (name,style)) = let
          open Box
          val root = rootOf w
          val hview = (Styles.extendView (name,"hscrollbar"),style)
          val hsb = Scrollbar.hScrollbar (root,hview,[])
          val fr = Frame.frame (root,hview,[]) (Scrollbar.widgetOf hsb)
          val box = if top 
                      then layout (root,view,[]) (VtCenter[WBox(Frame.widgetOf fr),WBox w])
                      else layout (root,view,[]) (VtCenter[WBox w,WBox(Frame.widgetOf fr)])
          val hnum = if top then 0 else 1
          fun hsw true = mapBox box [hnum] | hsw false = unmapBox box [hnum]
          in hsw false;(box,SOME (hsb,hsw),NONE) end
      | doLayout (w,NONE,SOME left,view as (name,style)) = let
          open Box
          val root = rootOf w
          val vview = (Styles.extendView (name,"vscrollbar"),style)
          val vsb = Scrollbar.vScrollbar (root,vview,[])
          val fr = Frame.frame (root,vview,[]) (Scrollbar.widgetOf vsb)
          val box = if left 
                      then layout (root,view,[]) (HzCenter[WBox(Frame.widgetOf fr),WBox w])
                      else layout (root,view,[]) (HzCenter[WBox w,WBox(Frame.widgetOf fr)])
          val vnum = if left then 0 else 1
          fun vsw true = mapBox box [vnum] | vsw false = unmapBox box [vnum]
          in vsw false; (box,NONE,SOME (vsb,vsw)) end
      | doLayout (w,NONE,NONE,view) = 
          (Box.layout (rootOf w, view,[]) (Box.WBox w),NONE,NONE)
 
 
    fun scrollPort (root, view as (name,style), args) widget = let
          val attrs = W.findAttr (W.attrs(view,attrs,args))
          val color = Attrs.getColor (attrs Attrs.attr_background)
          val continuous = Attrs.getBool (attrs attr_continuous)
          val hsb = Attrs.getBoolOpt (attrs attr_hsb)
          val vsb = Attrs.getBoolOpt (attrs attr_vsb)
          val vview = (Styles.extendView (name,"viewport"),style)
          val vp = Viewport.viewport (root,vview,args) widget
          val fr = Frame.frame (root,vview,args) (Viewport.widgetOf vp)
          val (box,hsb,vsb) = doLayout (Frame.widgetOf fr,hsb,vsb,view)
	  fun realize arg = let
                fun doMon (_,NONE) = (fn _ => ())
                  | doMon (sv,SOME (sb,sw)) = let
                      val ch = CML.channel ()
                      in
		        CML.spawn (monitor(continuous,sb,sw,sv,CML.recvEvt ch));
		        fn arg => CML.send (ch, arg)
                      end
                val vf = doMon(Viewport.setVert vp,vsb)
                val hf = doMon(Viewport.setHorz vp,hsb)
		in
		  CML.spawn (fn () => main (vp,vf,hf));
		  realizeFn (Box.widgetOf box) arg
		end
          in
	    SP {
		widget = mkWidget{
		    root = rootOf widget, 
		    args = fn () => {background = SOME color},
		    boundsOf = boundsFn (Box.widgetOf box),
		    realize = realize
		  }
	    }
          end

    fun mkScrollPort {widget, continuous, color, hsb, vsb} = let
          open Attrs
          val root = rootOf widget
          val name = Styles.mkView {name = Styles.styleName ["scrollport"],
                                    aliases = []}
          val hsb = case hsb of NONE => NONE | SOME {top} => SOME top
          val vsb = case vsb of NONE => NONE | SOME {left} => SOME left
          fun add(_,NONE,l) = l
            | add(label,SOME b,l) = (label, AV_Bool b)::l
          val args = 
              add(attr_hsb,hsb,add(attr_vsb,vsb,add(attr_continuous,SOME continuous,[])))
          val args = case color of
                            NONE => args
                          | SOME c => (attr_background, AV_Color c)::args
          in scrollPort (root,(name,styleOf root),args) widget end

    fun widgetOf (SP{widget,...}) = widget

  end (* ScrollPort *)

