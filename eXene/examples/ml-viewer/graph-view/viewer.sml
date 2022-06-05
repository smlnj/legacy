signature VIEWER = sig

  structure VG : ATTR_GRAPH
  structure W : WIDGET
  structure VF : VIEW_FONT

  type viewer

  val mkViewer : VF.font_server * W.root -> VG.graph -> viewer
  val widgetOf : viewer -> W.widget

  datatype viewdim = VDIM of {min : int, sz : int, total : int}
  val setHorzView : viewer -> int -> unit
  val setVertView : viewer -> int -> unit
  val evtOf : viewer -> {horz : viewdim, vert : viewdim} CML.event

end (* VIEWER *)

structure GrViewer : VIEWER = struct

  structure VG = VGraph
  structure W = Widget
  structure VF = ViewFont

  open VG RGeometry EXeneBase Drawing Widget Geometry Interact CML

  exception Error of string

  datatype viewdim = VDIM of {min : int, sz : int, total : int}

  type viewnode = {
    node : VG.node,
    bbox : rect,
    draw : (drawable * pen) -> rect -> unit,
    fill : (drawable * pen) -> rect -> unit,
    label : string
  }

  type uwprojfn = rpoint -> point
  type wuprojfn = point -> rpoint

  datatype request = SetV of int | SetH of int | Delete

  datatype viewer = Viewer of {
    widget : widget,
    graph : VG.graph,
    viewc : { horz : viewdim, vert : viewdim} chan,
    reqc : request chan
  }

  type view_data = {
    utowin : uwprojfn,              (* Universal -> Window *)
    wintou : wuprojfn,              (* Window -> Universal *)
    vlist :  viewnode list,         (* List of visible nodes *)
    font : font option,             (* Font scaled to view *)
    picked : VG.node option         (* Picked node *)
  }

  fun wuprojRect (projfn : wuprojfn) r = let
    val RPT{x=ox,y=oy} = projfn (Geometry.originOfRect r)
    val RPT{x=cx,y=cy} = projfn (Geometry.cornerOfRect r)
  in
    RRECT{x=ox,y=oy,wid=cx-ox,ht=cy-oy}
  end

  fun uwprojRect (projfn : uwprojfn) r = let
    val PT{x=ox,y=oy} = projfn (RGeometry.originOfRect r)
    val PT{x=cx,y=cy} = projfn (RGeometry.cornerOfRect r)
  in
    RECT{x=ox,y=oy,wid=cx-ox,ht=cy-oy}
  end

    (* putText:
     * write text in given window, centered in
     * the given rectangle, and using largest font
     * so that text fits in the rectangle.
     *)
  fun putText (_, NONE, _) = (fn _ => ())
    | putText (win, SOME font, drawp) = let
        val {ascent,descent} = Font.fontHt font
        val fontht = ascent + descent
        val drawTxt = drawString (drawableOfWin win) drawp font
        in
          fn (txt, RECT{x,y,wid,ht}) => let
            val slen = Font.textWidth font txt
            val x = x + (wid - slen) div 2
            val y = y + ascent + (ht - fontht) div 2
            in
              drawTxt (PT{x=x,y=y},txt)
            end
        end

  val fullAngle = 360 * 64
  fun diamondOfBox (RECT{x,y,wid,ht}) = let
        val midx = x + wid div 2
        val midy = y + ht div 2
        val startp = PT{x=x,y=midy}
        in
          [startp,PT{x=midx,y=y},PT{x=x+wid,y=midy},PT{x=midx,y=y+ht},startp]
        end
  fun doEllipse drawfn (draww,pen) (RECT{x,y,wid,ht}) =
        drawfn draww pen (ARC{x=x,y=y,wid=wid,ht=ht,angle1=0,angle2=fullAngle})
  fun drawDiamond (draww,pen) r = drawLines draww pen (diamondOfBox r)
  fun fillDiamond (draww,pen) r =
        fillPolygon draww pen {verts = diamondOfBox r, shape = ConvexShape}
  fun doBox drawfn (draww,pen) r = drawfn draww pen r

  fun getDrawFns Attr.Ellipse = (doEllipse drawArc, doEllipse fillArc)
    | getDrawFns Attr.Diamond = (drawDiamond, fillDiamond)
    | getDrawFns _ = (doBox drawRect, doBox fillRect)

    (* Return the smallest rectangle containing
     * r with the same origin similar to template.
     *)
  fun mkSimilar (RRECT{x,y,wid=rx,ht=ry}, RECT{wid=tx,ht=ty,...}) = let
    val tmpy = (rx * (real ty)) / (real tx)
  in
    if tmpy >= ry then RRECT{x=x,y=y,wid=rx,ht=tmpy}
    else RRECT{x=x,y=y, wid = (ry * (real tx)) / (real ty), ht = ry}
  end

  fun doResize (bbox, RRECT{x,y,wid=nwid,ht=nht}, RRECT{x=ox,y=oy,wid=owid,ht=oht}) = let
        val RRECT{x=bbx,y=bby,wid=bbwid,ht=bbht} = bbox
        val ny = if nht >= bbht then bby - (nht - bbht)/2.0
                 else let
                   val ybot = oy+oht
                   val ytop = ybot-nht
                   in
                     if ytop < bby
                       then if ybot <= bby then bby else ytop
                       else if ytop >= bby+bbht then bby+bbht-nht else ytop
                   end
        val nx = if nwid >= bbwid then bbx - (nwid - bbwid)/2.0
                 else let
                   val xl = ox
                   val xr = ox+nwid
                   in
                     if xl < bbx
                       then if xr <= bbx then bbx else xl
                       else if xl >= bbx+bbwid then bbx+bbwid-nwid else xl
                   end
        in
          RRECT{wid=nwid,ht=nht,x=nx,y=ny}
        end

  fun set_scroller (viewc, RRECT{x=minx,y=miny,wid,ht}) visrect = let
    fun max (a : real, b) = if a > b then a else b
    fun min (a : real, b) = if a < b then a else b
    val RPT{x=ox, y=oy} = RGeometry.originOfRect visrect
    val RPT{x=cx, y=cy} = RGeometry.cornerOfRect visrect
    val startx = max(ox,minx)
    val starty = max(oy,miny)
    val sizex = min(cx,minx+wid) - startx
    val sizey = min(cy,miny+ht) - starty
    val ht = truncate ht
    val wid = truncate wid
  in
    send (viewc, {
        horz = VDIM{min=truncate(startx-minx),sz=truncate sizex,total=wid},
        vert = VDIM{min=truncate(starty-miny),sz=truncate sizey,total=ht}
      })
  end

    (* setPerspective:
     * Set transformation functions between universal
     * and window spaces. We assume the two rectangles
     * are similar.
     *)
  fun setPerspective (wr,ur) = let
    val RRECT{x=uminX,y=uminY,wid=udeltaX,ht=udeltaY} = ur
    val RECT{x=wminX,y=wminY,wid=wdeltaX,ht=wdeltaY} = wr
    val wminX = real wminX
    val wminY = real wminY
    val wdeltaX = real wdeltaX
    val wdeltaY = real wdeltaY

      (* function from universal to window *)
    fun pf (RPT{x,y}) = 
      PT{x = floor((wdeltaX * (x - uminX)) / udeltaX + wminX),
         y = floor((wdeltaY * (y - uminY)) / udeltaY + wminY)}

      (* function from window to universal *)
    fun rpf (PT{x,y}) =
      RPT{x = (udeltaX*((real x) - wminX)) / wdeltaX + uminX,
          y = (udeltaY*((real y) - wminY)) / wdeltaY + uminY}
  in
    (pf, rpf)
  end

  fun visNodes G (sz, utowin) = let
    val winr = mkRect (originPt, sz)
    val projr = uwprojRect utowin
    fun accept n = intersect (winr, projr (#bbox (VG.infoOfNode n)))

    fun mkViewNode n = let
      val {bbox,label,shape,...} = VG.infoOfNode n
      val (draw, fill) = getDrawFns shape
    in
      {node=n, bbox = projr bbox, label= label, fill = fill, draw = draw}
    end
    fun chkVisNode (n,l) =
          if intersect (winr, projr (#bbox (VG.infoOfNode n)))
            then (mkViewNode n)::l
            else l
  in
    foldNodes chkVisNode G []
  end

    (* drawGraph:
     * Simple drawing routine.
     * draw edges of all visible nodes;
     * draw visible nodes.
     *)
  fun drawGraph (G,win,drawp,linep,pickp) 
      ({utowin, vlist, font, picked,...} : view_data) = let

    val draww = drawableOfWin win
    val drawPLine = (drawLines draww linep) o Spline.bSpline
    val putText = putText (win,font,linep)
    fun drawArrow pts =
          fillPolygon draww linep {verts = map utowin pts, shape = ConvexShape}

    fun drawEdges ({node,...} : viewnode) = let
      fun drawEdge e = let
           val {pts, arrows,...} = VG.infoOfEdge e
           in
             drawPLine (map utowin pts);
             drawArrow arrows
           end
    in
      appOutEdges drawEdge (G,node);
      appInEdges drawEdge (G,node)
    end

    fun drawNode ({bbox,label,draw,fill,...} : viewnode) = (
      (* clearArea (drawableOfWin win) bbox; *)
      fill (draww,drawp) bbox;
      putText (label,bbox);
      draw (draww,linep) bbox
    )

    fun drawXNode n ({node,bbox,label,draw,fill} : viewnode) = (
      if VG.eqNode (node,n) 
        then fill (draww,pickp) bbox 
        else fill (draww,drawp) bbox;
      (* else clearArea (drawableOfWin win) bbox; *)
      putText (label,bbox);
      draw (draww,linep) bbox
    )
  in
    clearDrawable (drawableOfWin win);
    app drawEdges vlist;
    case picked of
      NONE => app drawNode vlist
    | SOME n => app (drawXNode n) vlist
  end

  fun drawGraphR (G,win,drawp,linep,pickp) 
      ({utowin, wintou, vlist, font, picked,...} : view_data) r = let

    val draww = drawableOfWin win
    val drawPLine = (drawLines draww linep) o Spline.bSpline
    val putText = putText (win,font,linep)
    val ur = wuprojRect wintou r
    fun drawArrow pts =
          fillPolygon draww linep {verts = map utowin pts, shape = ConvexShape}

    fun drawEdges ({node,...} : viewnode) = let
      fun drawEdge e = let
           val {pts, arrows,bbox} = VG.infoOfEdge e
           in
             if RGeometry.intersect (bbox,ur) then (
               drawPLine (map utowin pts);
               drawArrow arrows
             ) else ()
           end
    in
      appOutEdges drawEdge (G,node);
      appInEdges drawEdge (G,node)
    end

    fun drawNode ({bbox,label,draw,fill,...} : viewnode) = 
          if intersect (bbox,r) then (
            (* clearArea (drawableOfWin win) bbox; *)
            fill (draww,drawp) bbox;
            putText (label,bbox);
            draw (draww,linep) bbox
          ) else ()

    fun drawXNode n ({node,bbox,label,draw,fill} : viewnode) =
          if intersect (bbox,r) then (
            if VG.eqNode (node,n) 
              then fill (draww,pickp) bbox 
              else fill (draww,drawp) bbox;
            (* else clearArea (drawableOfWin win) bbox; *)
            putText (label,bbox);
            draw (draww,linep) bbox
          ) else ()
  in
    app drawEdges vlist;
    case picked of
      NONE => app drawNode vlist
    | SOME n => app (drawXNode n) vlist
  end

  fun setSelect (win,linep,pickp) 
    ({vlist,utowin,wintou,font,...} : view_data, 
     {node,bbox,label,fill,draw} : viewnode) = 
  (
    fill (drawableOfWin win,pickp) bbox; 
    putText (win,font,linep) (label,bbox);
    draw (drawableOfWin win,linep) bbox; 
    {vlist=vlist,utowin=utowin,wintou=wintou,font=font,picked= SOME node}
  )

  fun unsetSelect (win,linep,drawp) 
      ({vlist,utowin,wintou,font,...} : view_data, node) = let
    val {bbox,label,shape,...} = VG.infoOfNode node
    val (draw,fill) = getDrawFns shape
    val wrect = uwprojRect utowin bbox
  in
    fill (drawableOfWin win,drawp) wrect; 
    putText (win,font,linep) (label,wrect);
    draw (drawableOfWin win,linep) wrect; 
    {vlist=vlist,font=font,utowin=utowin,wintou=wintou,picked=NONE}
  end

  datatype mitem
    = Pick of point
    | ZoomIn of rect
    | ZoomOut of rect
    | Reset
    | Block
    | Unblock
    | GetPick of (string * string * (int * int) option) option CML.cond_var

  fun mouser (root, win, m, mousec) = let
    fun getPick () = let
	  val cv = CML.condVar()
	  in
	    send (mousec, GetPick cv);
	    readVar cv
	  end
    val mevt = wrap (m, msgBodyOf)
    val getrect = Get.getRect (win, m)
    val noBttns = mkButState []

    fun getr msg = let
      val _ = send(mousec, Block)
      val opt_rect = sync(getrect(MButton 3))
    in
      send(mousec, Unblock);
      case opt_rect of
        NONE => ()
      | SOME r => send(mousec, msg r)
    end

    fun popup menu = SimpleMenu.popupMenu (root, menu, NONE)

    fun menu2 (mname, fname, range) = let
	  fun openView (loc, range) () = MLViewer.openViewer root {
		  file = fname, module = mname, loc = loc, range = range
		}
	  in
	    case range
	     of NONE => SimpleMenu.MENU [
		    SimpleMenu.MenuItem ("View "^mname, openView (1, NONE))
		  ]
	      | (SOME(first, last)) => SimpleMenu.MENU [
		    SimpleMenu.MenuItem ("View "^fname, openView (first, NONE)),
		    SimpleMenu.MenuItem ("View "^mname,
		      openView (first, SOME{first=first, last=last}))
		  ]
	    (* end case *)
	  end

   val popup3 = popup (SimpleMenu.MENU [
	    SimpleMenu.MenuItem ("Zoom in", fn () => getr ZoomIn),
	    SimpleMenu.MenuItem ("Zoom out", fn () => getr ZoomOut),
	    SimpleMenu.MenuItem ("Reset", fn () => send(mousec,Reset)),
	    SimpleMenu.MenuItem ("Quit",
	      fn () => (W.delRoot root; RunCML.shutdown()))
	  ])

    fun loop () = loop (case (sync mevt)
	   of MOUSE_FirstDown {but=btn,pt,scr_pt,...} => (case btn
		 of MButton 1 => send(mousec, Pick pt)
		  | MButton 2 => (case getPick()
		       of NONE => ()
			| (SOME info) => let
			    val popup2 = popup (menu2 info)
			    in
			      case (sync (popup2 (btn, SimpleMenu.Item 0, scr_pt, m)))
			       of NONE => ()
			        | SOME action => action ()
			      (* end case *)
			    end
		      (* end case *))
		  | MButton 3 => (
		      case (sync (popup3 (btn,SimpleMenu.Item 0,scr_pt,m)))
		       of NONE => ()
		        | SOME action => action ()
		      (* end case *))
		  | MButton _ => ()
		(* end case *))
	    | _ => ()
	  (* end case *))
  in
    loop ()
  end

  val ltGrayImage = imageFromAscii (16, [[
    "0x2222", "0x8888", "0x2222", "0x8888",
    "0x2222", "0x8888", "0x2222", "0x8888",
    "0x2222", "0x8888", "0x2222", "0x8888",
    "0x2222", "0x8888", "0x2222", "0x8888"
  ]])
  
  fun mkBWPen scr = let
        val graytile = createTileFromImage scr ltGrayImage
        in
          newPen [
            PV_Foreground (blackOfScr scr), 
            PV_Background (whiteOfScr scr), 
            PV_FillStyle_OpaqueStippled, 
            PV_Stipple graytile
          ]
        end handle _ => LibBase.badArg{
                          module="Scrollbar",
                          func="mkMonoPen",
                          msg="could not create stipple"
                        }
  fun mkPickPen scr =
        case displayClassOfScr scr of
          StaticGray => mkBWPen scr
        | GrayScale => mkBWPen scr
        | _ => let
            val red = colorOfScr scr (CMS_RGB{red=65535,green=0,blue=0})
            in
              newPen [PV_Foreground red]
            end

  fun mkViewer (fontserver,root) vg = let
    val {bbox,fontsize,...} = infoOfGraph vg
    val scr = screenOf root

        (* Drawing pens. *)
    val drawPen = newPen [PV_Foreground (whiteOfScr (screenOf root))]
    val linePen = newPen [PV_Foreground (blackOfScr (screenOf root))]
    val pickPen = mkPickPen scr

    val reqc = channel ()
    val mousec = channel ()
    val viewc = channel ()
    val minsz = 30
      (* the dimensions should be scaled to fit reasonably in the screen *)
    val RRECT{wid,ht,...} = bbox
    val bounds = {
        x_dim = DIM{base=0,incr=1,min=minsz,nat=max(minsz,floor wid),max=NONE},
        y_dim = DIM{base=0,incr=1,min=minsz,nat=max(minsz,floor ht),max=NONE}
      }

    val getVList = visNodes vg
    val setScroller = set_scroller (viewc, bbox)

    val pix2pts = let
          val SIZE{ht=htpx,...} = sizeOfScr scr
          val SIZE{ht=htmm,...} = sizeMMOfScr scr
          val fact = (((real htmm)/(real htpx))/25.4) * 72.0
          in
            fn px => floor((real px)*fact)
          end

    fun scaleFont proj = let
          val PT{y,...} = proj roriginPt
          val PT{y=y',...} = proj(RPT{x=0.0,y= real fontsize})
          in pix2pts (y' - y) end

    fun setState (sz, winrect, urect, picked) = let
          val (proj, rproj) = setPerspective(winrect, urect)
          in
            {
              utowin = proj,
              wintou = rproj,
              vlist = getVList (sz, proj),
              font = ViewFont.findFont fontserver (scaleFont proj),
              picked = picked
            }
          end

    fun setView (arg as (_, _, urect, _)) = (setScroller urect; setState arg)

    fun realize {env, win, sz} = let
      val InEnv{m,ci,co,...} = ignoreKey env
      val draw = drawGraph (vg,win,drawPen,linePen,pickPen)
      val drawr = drawGraphR (vg,win,drawPen,linePen,pickPen)
      val setSelect = setSelect (win,linePen,pickPen)
      val unsetSelect = unsetSelect (win,linePen,drawPen)

      val winrect = mkRect(originPt, sz)

      val initstate = setView (sz, winrect, mkSimilar (bbox, winrect), NONE)

      fun initLoop (sz, state) = let

        fun scrollEvt (horz, newst, {wintou, picked, ...} : view_data) = let
          val winr = mkRect(originPt, sz)
          val RRECT{x,y,wid,ht} = wuprojRect wintou winr
          val (x,y) = if horz then (real newst,y) else (x,real newst)
          val newstate = setState (sz, winr, RRECT{x=x,y=y,wid=wid,ht=ht}, picked)
        in
          draw newstate;
          newstate
        end

        fun handleCI (CI_Redraw rs, state) = (
              XDebug.trace(XDebug.topTM, fn () => ["redraw\n"]);
              app (drawr state) rs; state)
          | handleCI (CI_Resize(RECT{wid,ht,...}),{utowin,wintou,picked,...}) =
            let
              val newsz = SIZE{wid=wid,ht=ht}
              val winrect = mkRect(originPt, newsz)
              val newr = wuprojRect wintou winrect
              val oldr = wuprojRect wintou (mkRect(originPt,sz))
              val newstate = setView (newsz, winrect, doResize(bbox,newr,oldr), picked)
            in
              XDebug.trace(XDebug.topTM, fn () => ["resize\n"]);
              clearDrawable (drawableOfWin win);
              initLoop (newsz, newstate)
            end
          | handleCI (_ , state) = state

        fun zoomIn (r, {wintou, picked, ...} : view_data) = let
          val winr = mkRect(originPt, sz)
          val urect = wuprojRect wintou r
          fun chk (r as RRECT{x,y,wid,ht}) =
               if wid < 30.0 orelse ht < 30.0 then RRECT{x=x,y=y,wid=30.0,ht=30.0} else r
          val newstate = setView (sz, winr, mkSimilar (chk urect, winr), picked)
        in
          draw newstate;
          newstate
        end

        fun zoomOut (r, {wintou, picked, ...} : view_data) = let
          val winr = mkRect(originPt, sz)
          val urect = wuprojRect wintou winr
          val newstate = setState (sz, rect(mkSimilar(rrect r,winr)), urect, picked)
        in
             (* Note that the 2nd argument to setScroller is *)
             (* not urect, as we have changed the perspective. *)
          setScroller (wuprojRect (#wintou newstate) winr);
          draw newstate;
          newstate
        end

        fun doPick (p, state as {vlist,picked,...} : view_data) = let
          fun accept ({bbox,...} : viewnode) = within(p, bbox)
        in
          case ListUtil.findOne accept vlist of
            SOME (nvn as {node=newvn,...}) =>        (* New pick *)
              (case picked of
                SOME oldvn =>                        (* Old pick *)
                     (* If old == new, do nothing *)
                  if VG.eqNode (newvn,oldvn) then state
                  else (
                    unsetSelect (state, oldvn);
                    setSelect (state, nvn)
                  )
              | _ => setSelect (state, nvn)
              )
          | _ =>                                     (* No new pick *)
              (case picked of
                SOME oldvn => unsetSelect (state, oldvn)      (* Old pick *)
              |  _ => state
              )
        end

        fun reset ({picked,...}: view_data) = let
          val winrect = mkRect(originPt, sz)
          val newstate = setView (sz, winrect, mkSimilar (bbox, winrect), picked)
        in
          draw newstate;
          newstate
        end

        fun block (state, ci_list) =
          select [
            wrap (receive mousec, fn msg => 
              case msg of
                Unblock => fold (fn (m,s) => handleCI(m,s)) ci_list state
              | _ => block(state,ci_list)
            ),
            wrap (ci, fn msg => block(state,(msgBodyOf msg)::ci_list))
          ]

	fun getPick ({picked = NONE, ...} : view_data) = NONE
	  | getPick {picked = SOME node, ...} = let
              val info = infoOfNode node
	      val getAttr = ModGraph.getAttr (ModGraph.NODE(#base info))
	      fun error attr = (
		    CIO.print("missing "^attr^" attribute\n");
		    raise Fail(implode["missing ", attr, " attribute"]))
	      val fname = (case (getAttr "file")
		     of NONE => error "file"
		      | (SOME s) => s
		    (* end case *))
	      val range = (case (getAttr "range")
		     of NONE => NONE
		      | (SOME s) => (case (Format.scan "%d:%d" s)
			 of [Format.INT a, Format.INT b] => SOME(a, b)
			  | _ => error "range"
			(* end case *))
		    (* end case *))
	      in
		SOME(#label info, fname, range)
	      end

        fun doMouse (Pick pt, state : view_data) = doPick (pt,state)
          | doMouse (ZoomIn rect, state) = zoomIn (rect, state)
          | doMouse (ZoomOut rect, state) = zoomOut (rect, state)
          | doMouse (Reset, state)= reset state
          | doMouse (Block, state)= block (state,[])
	  | doMouse (GetPick cv, state) = (CML.writeVar(cv, getPick state); state)
          | doMouse (_, state) = state

        fun doReq (SetV v, state : view_data) = scrollEvt (false, v, state)
          | doReq (SetH v, state) = scrollEvt (true, v, state)
          | doReq (Delete, state) = state

        fun loop state =
          loop (select [
            wrap (receive mousec, fn msg => doMouse (msg, state)),
            wrap (ci, fn msg => handleCI (msgBodyOf msg, state)),
            wrap (receive reqc, fn msg => doReq (msg, state))
          ])
      in
        loop state
      end

    in
      spawn (fn () => mouser(root, win, m, mousec));
      spawn (fn () => (initLoop (sz, initstate);()));
      ()
    end

    val w = mkWidget {
        root = root,
        boundsOf = fn () => bounds,
        realize = realize
      }
  in
    Viewer {
      widget = w,
      reqc = reqc,
      viewc = viewc,
      graph = vg
    }
  end

  fun widgetOf (Viewer{widget,...}) = widget
  fun setHorzView (Viewer{reqc,...}) v = send(reqc, SetH v)
  fun setVertView (Viewer{reqc,...}) v = send(reqc, SetV v)
  fun evtOf (Viewer{viewc,...}) = receive viewc

end (* Viewer *)
