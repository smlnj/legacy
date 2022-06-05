(* menu.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This is a very simple menu for the bounce demo; it just returns the selected
 * string (or NONE).
 *)

structure Menu =
  struct
    local
      open CML Geometry Interact EXeneWin

      val menuFontName = "8x13"
      val xPadding = 4		(* pad four pixels horizontally *)

      val menuItems = [ "Refresh", "Kill All", "Quit" ]

    (* translate mouse events by delta *)
      fun translateMouse delta = let
	    fun trans (MOUSE_Motion{scr_pt, pt, time}) =
		  MOUSE_Motion{scr_pt = scr_pt, pt=addPt(pt, delta), time=time}
	      | trans (MOUSE_Up{but, scr_pt, pt, state, time}) =
		  MOUSE_Up{but=but, scr_pt = scr_pt, pt=addPt(pt, delta), state=state, time=time}
	      | trans (MOUSE_LastUp{but, scr_pt, pt, time}) =
		  MOUSE_LastUp{but=but, scr_pt = scr_pt, pt=addPt(pt, delta), time=time}
	      | trans (MOUSE_Down{but, scr_pt, pt, state, time}) =
		  MOUSE_Down{but=but, scr_pt = scr_pt, pt=addPt(pt, delta), state=state, time=time}
	      | trans (MOUSE_FirstDown{but, scr_pt, pt, time}) =
		  MOUSE_FirstDown{but=but, scr_pt = scr_pt, pt=addPt(pt, delta), time=time}
	      | trans m = m
	    in
	      trans
	    end

      fun placeItems (font, items) = let
	    val {ascent, descent} = Font.fontHt font
	    val txtWid = Font.textWidth font
	    val itemHt = ascent + descent
	    val pad = xPadding+xPadding
	    fun place ([], curY, wid, l) =
		  (rev l, wid, curY)
	      | place(s::r, curY, wid, l) =
		  place(r, curY+itemHt, Int.max(wid, (txtWid s) + pad),
		    (PT{x=xPadding, y=curY+ascent}, s)::l)
	    val (items, wid, ht) = place (items, 0, 0, [])
	    fun selectItem (pt as PT{y, ...}) =
		  if (within (pt, RECT{x = 0, y = 0, wid = wid, ht = ht}))
		    then let
		      val i = Int.quot(y, itemHt)
		      val (pt as PT{y, ...}, s) = List.nth(items, i)
		      in
			SOME{
			    item = i,
			    text_pos = pt,
			    rect = RECT{x=0, y=y-ascent, wid=wid, ht=itemHt},
			    text = s
			  }
		      end
		    else NONE
	    in
	      (items, SIZE{wid = wid, ht = ht}, selectItem)
	    end

    (* create and draw the menu window, returning the window and input environment *)
      fun createMenu scr (PT{x, y}, pen, font, items) = let
	    val (items, menuSz as SIZE{wid, ht}, selectItem) = placeItems (font, items)
	    val SIZE{wid=scrWid, ht=scrHt} = EXeneBase.sizeOfScr scr
	    val menuOrigin = PT{
		    x = Int.min (Int.max (x - (wid div 2), 0), scrWid - wid),
		    y = Int.min (Int.max (y - (ht div 2), 0), scrHt - ht)
		  }
	    val (menuWin, inEnv) = createSimplePopupWin scr {
		    backgrnd = EXeneBase.whiteOfScr scr,
		    border = EXeneBase.blackOfScr scr,
		    geom = WGEOM{
			pos = subPt(menuOrigin, PT{x=1, y=1}),
			sz = menuSz, border = 1
		      }
		  }
	    val _ = mapWin menuWin
	    val drawItem = Drawing.drawString (Drawing.drawableOfWin menuWin) pen font
	    in
	      map drawItem items;
	      (menuOrigin, menuWin, ignoreAll inEnv, selectItem)
	    end
    in

    fun popupMenu win = let
	  val scr = screenOfWin win
	  val dpy = displayOfWin win
	  val font = Font.openFont dpy menuFontName
	  val {ascent, ...} = Font.fontHt font
	  val forePen = Drawing.newPen [Drawing.PV_Foreground(EXeneBase.blackOfScr scr)]
	  val backPen = Drawing.newPen [Drawing.PV_Foreground(EXeneBase.whiteOfScr scr)]
	  val create = createMenu scr
	  val menuCursor = EXeneBase.stdCursor dpy StdCursor.sb_left_arrow
	  fun doMenu (menuBut, pt, time, mEvt) = let
		val winOrigin = winPtToScrPt win originPt
		val replyCh = channel()
		val (menuOrigin, menuWin, menuEnv, selectItem) =
		      create (addPt(winOrigin, pt), forePen, font, menuItems)
		val mEvt = wrap(mEvt, translateMouse (subPt(winOrigin, menuOrigin)))
		fun sendSelection NONE = send(replyCh, NONE)
		  | sendSelection (SOME{item, text_pos, rect, text}) =
		      send(replyCh, SOME text)
		val drawable = Drawing.drawableOfWin menuWin
		fun flipOn {item, text_pos, rect, text} = (
		      Drawing.fillRect drawable forePen rect;
		      Drawing.drawString drawable backPen font (text_pos, text))
		fun flipOff {item, text_pos, rect, text} = (
		      Drawing.fillRect drawable backPen rect;
		      Drawing.drawString drawable forePen font
			(text_pos, text))
		fun mouseLoop (curItem, pt) = let
		      val curItem = case (curItem, selectItem pt)
			   of (SOME a, SOME b) =>
				if ((#item a) = (#item b))
				  then curItem
				  else (flipOff a; flipOn b; SOME b)
			    | (SOME a, NONE) => (flipOff a; NONE)
			    | (NONE, SOME b) => (flipOn b; SOME b)
			    | (NONE, NONE) => NONE
		      in
			case (sync mEvt)
			 of (MOUSE_Motion{scr_pt, pt, ...}) =>
			      mouseLoop(curItem, pt)
			  | (MOUSE_LastUp{but, ...}) => (curItem, true)
			  | (MOUSE_Up{but, state, ...}) =>
			      if (but = menuBut)
				then (curItem, false)
				else mouseLoop (curItem, pt)
			  | _ => mouseLoop(curItem, pt)
		      end
		fun allUp () = (case (sync mEvt) of MOUSE_LastUp _ => () | _ => allUp())
		fun trackMouse () = let
		      val (selection, lastUp) =
			    mouseLoop(NONE, addPt(pt, subPt(winOrigin, menuOrigin)))
		      in
			destroyWin menuWin;
			if lastUp then () else allUp();
			sendSelection selection
		      end
		in
		  EXeneBase.changeActiveGrabCursor dpy menuCursor;
		  XDebug.xspawn ("Menu:trackMouse", trackMouse);
		  CML.recvEvt replyCh
		end
	  in
	    doMenu
	  end

    end (* local *)
  end
