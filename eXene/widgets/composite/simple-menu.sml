(* simple-menu.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * A simple menu package.
 *
 * TODO:
 *  defaults for submenus
 *)

signature SIMPLE_MENU =
  sig
    structure W : WIDGET
    structure I : INTERACT
    structure CML : CML

    datatype 'a menu = MENU of 'a menu_item list
    and 'a menu_item
      = MenuItem of (string * 'a)
      | Submenu of (string * 'a menu)

    datatype menu_pos = Absolute of W.G.point | Item of int

    val popupMenu : (W.root * '1a menu * string option)
          -> (I.mbutton * menu_pos * W.G.point * I.mouse_msg I.addr_msg CML.event)
          -> '1a option CML.event

    datatype where_info = 
      WI of {
          but : I.mbutton,
          pt : W.G.point,
          scr_pt : W.G.point,
          time : EXeneBase.XTime.time
        }

    val buttonMenu : 
          (W.widget * W.Interact.mbutton list * '1a menu * (where_info -> menu_pos))
	  -> (W.widget * '1a CML.event)

    val attachMenu : (W.widget * W.Interact.mbutton list * '1a menu)
	  -> (W.widget * '1a CML.event)

    val attachLabeledMenu : (W.widget * W.Interact.mbutton list * string * '1a menu)
	  -> (W.widget * '1a CML.event)

  end (* SIMPLE_MENU *)

structure SimpleMenu : SIMPLE_MENU =
  struct
    structure W = Widget
    structure I = Interact
    structure CML = CML

    open W Geometry Interact

    datatype 'a menu = MENU of 'a menu_item list
    and 'a menu_item
      = MenuItem of (string * 'a)
      | Submenu of (string * 'a menu)

    datatype menu_pos = Absolute of W.G.point | Item of int

    datatype where_info = WI of {
        but : mbutton,
        pt : point,
        scr_pt : point,
        time : EXeneBase.XTime.time
      }

    val menuFont = "8x13"
    val pad = 1       (* padding between window border and actual menu items *)
    val totPad = pad+pad
    val bwid = 1      (* border width *)
    val inset = 1     (* add'l x padding to ensure highlighting encloses text and icon. *)
    val vspace = 1    (* y padding per item, for same reason as above. *)
    val NoButtons = mkButState []

    datatype label = LABEL of {
	box : rect, textPos : point, text : string
      }

    datatype 'a menu_rep = MREP of {
	sz : size,
	item_ht : int,
	font : Font.font,
	label : label option,		(* note: only top-level menus have labels *)
	items : 'a item_rep list
      }
    and 'a item_rep
      = ItemRep of {
	    label : label,
	    item : 'a
	  }
      | SubMRep of {
	    label : label,
	    menu_pos : point,		(* obsolete - position relative to parent menu *)
	    menu : 'a menu_rep
	  }

    val iconHt = 12 and iconWid = 12
    val iconSp = 1    (* minimum space between text and icon *)
    val submenuImage = EXB.IMAGE{
            sz = SIZE{wid=iconWid, ht=iconHt},
            data = [map Byte.stringToBytes [
                "\127\192", "\064\064", "\064\096", "\078\096",
                "\064\096", "\078\096", "\064\096", "\078\096",
                "\064\096", "\064\096", "\127\224", "\031\224"
              ]]
          }

    fun layoutMenu (font, menu, label) = let
	  val {ascent, descent} = Font.fontHt font
	  val textWid = Font.textWidth font
	  fun menuGeom (MENU items, label) = let
		fun maxW (m, hasSubm, nitems, []) =
		      (totPad + 2*inset + m, nitems, hasSubm)
		  | maxW (m, hasSubm, nitems, MenuItem(s, _) :: r) =
		      maxW(Int.max(m, textWid s), hasSubm, nitems+1, r)
		  | maxW (m, hasSubm, nitems, Submenu(s, _) :: r) =
		      maxW(Int.max(m, (textWid s) + iconWid + iconSp), true, nitems+1, r)
		in
		  case label
		   of NONE => maxW(0, false, 0, items)
		    | (SOME s) => maxW(textWid s, false, 1, items)
		end
	  fun layout (menu as (MENU items), label) = let
		val (maxWid, itemHt, totHt) = let
		      val (maxWid, n, hasSubm) = menuGeom (menu, label)
		      val fonth = ascent+descent
		      val itemHt = 
                        if hasSubm then Int.max(fonth+vspace, iconHt) else fonth+vspace
		      in
			(maxWid, itemHt, n*itemHt+totPad)
		      end
		fun mkCenterLabel (yPos, itemLabel) = let
		      val wid = textWid itemLabel
		      in
			LABEL{
			    box = RECT{x=0, y=yPos, wid=maxWid, ht=itemHt},
			    textPos = PT{x=(maxWid - wid) div 2, y=yPos+ascent},
			    text = itemLabel
			  }
		      end
		fun mkLabel (yPos, itemLabel) = let
		      val wid = textWid itemLabel
		      in
			LABEL{
			    box = RECT{x=pad, y=yPos, wid=maxWid-totPad, ht=itemHt},
			    textPos = PT{x=pad+inset, y=yPos+ascent},
			    text = itemLabel
			  }
		      end
		fun doItems (_, []) = []
		  | doItems (yPos, item::r) = let
		      val itemRep = case item
			   of (MenuItem(s, v)) => ItemRep{
				  label = mkLabel(yPos, s),
				  item = v
				}
			    | (Submenu(s, m)) => 
                                  let
				    val menu as MREP{sz=SIZE{wid,...},...} = layout (m, NONE)
                                  in
                                    SubMRep{
				      label = mkLabel(yPos, s),
				      (* menu_pos = PT{x=maxWid-(wid div 3), y=yPos}, *)
				      menu_pos = originPt,
				      menu = menu
				    }
                                  end
		      in
			itemRep :: doItems(yPos+itemHt, r)
		      end (* doItems *)
		val (label, items) = case label
		     of NONE => (NONE, doItems(pad, items))
		      | (SOME s) => (SOME(mkCenterLabel(0, s)),doItems(pad+itemHt, items))
		in
		  MREP{
		      sz = SIZE{wid=maxWid, ht=totHt},
		      item_ht = itemHt,
		      font = font,
		      label = label,
		      items = items
		    }
		end (* layout *)
	  in
	    layout (menu, label)
	  end (* layoutMenu *)

    type 'a mitem = {
	id : int, draw_on : unit -> unit, draw_off : unit -> unit, rep : 'a item_rep
      }

  (* Create a menu window.  This involves creating and mapping the window, and
   * setting up the code for drawing the items.  "pos" gives the position to place
   * the menu in screen coordinates.
   *)
    fun createMenuWin (scr, submenuIcon, mrep, pos as PT{x=menuX, y=menuY}) = let
	  open Drawing
	  val MREP{sz, font, item_ht, label, items, ...} = mrep
	  val SIZE{wid=menuWid, ht=menuHt} = sz
	  val white = EXB.whiteOfScr scr and black = EXB.blackOfScr scr
	  val (win, inEnv) = EXW.createSimplePopupWin scr {
		  backgrnd = white,
		  border = black,
		  geom = WGEOM{pos = subPt(pos, PT{x=bwid, y=bwid}), sz = sz, border = bwid}
		}
	  val _ = (ignoreAll inEnv; EXW.mapWin win)
	  val (itemsRect as RECT{y=itemsY, ...}) = let
		val (x, y, w, h) = (case label
		     of NONE => (menuX, menuY, menuWid, menuHt)
		      | (SOME _) => (menuX, menuY+item_ht, menuWid, menuHt-item_ht))
		in
		  RECT{x=x+pad, y=y+pad, wid=w-totPad, ht=h-totPad}
		end
            (* Geometry of menu window *)
	  val menuRect = RECT{x=menuX, y=menuY, wid=menuWid, ht=menuHt}
            (* Geometry of menu window including border *)
	  val allRect = RECT{x=menuX-bwid, y=menuY-bwid, wid=menuWid+2*bwid, ht=menuHt+2*bwid}
          fun inMenu p = within (p, allRect)
	  fun close () = (EXW.destroyWin win)
	  val forePen = newPen[PV_Foreground black, PV_Background white]
	  val backPen = newPen[PV_Foreground white, PV_Background black]
	  fun drawItem (LABEL{textPos, text, ...}) pen =
		drawString (drawableOfWin win) pen font (textPos, text)
	  fun clearItem (LABEL{box, ...}) pen = fillRect (drawableOfWin win) pen box
	  fun drawSubm (LABEL{box, textPos, text}) pen = let
		val (SOME icon) = submenuIcon
		val RECT{x, y, wid, ...} = box
		in
		  drawString (drawableOfWin win) pen font (textPos, text);
		  textureBlt (drawableOfWin win) pen {
		      dst_pos = PT{x = (x + wid) - (iconWid+inset), y = y+1},
		      src = icon
		    }
		end
	  fun mkItems ([], _) = []
	    | mkItems (item::r, n) = let
		val (draw, label) = (case item
		       of (ItemRep{label, ...}) => (drawItem label, label)
			| (SubMRep{label, ...}) => (drawSubm label, label))
		val clear = clearItem label
		fun drawOn () = (clear forePen; draw backPen)
		fun drawOff () = (clear backPen; draw forePen)
		in
		  draw forePen;
		  {id = n, draw_on = drawOn, draw_off = drawOff, rep = item}
		    :: mkItems(r, n+1)
		end (* mkItems *)
	  val items = mkItems(items, 0)
	  fun selectItem (pt as PT{x, y}) = if within(pt, itemsRect)
		then SOME(List.nth(items, Int.quot(y - itemsY, item_ht)))
		else NONE
	  in
	    case label
	     of (SOME title) => (clearItem title forePen; drawItem title backPen)
	      | _ => ();
	    {rect = menuRect, inMenu = inMenu, select = selectItem, close = close}
	  end (* createMenu *)

(** NOTE: the "'a menu_rep" constraint is because of a bug in the typechecker **)
    fun popMenu (menuRep : 'a menu_rep, mbut, scr, icon, menupt, pos, mouse) = let
	  val SIZE{wid=scrWid, ht=scrHt} = EXB.sizeOfScr scr
	(* adjust the position of a menu, to insure that it will fit on the screen *)
	  fun clipMenu (PT{x, y}, MREP{sz=SIZE{wid, ht}, ...}) = PT {
		  x = Int.max(bwid, if (x+wid < scrWid-bwid) then x else (scrWid - (wid+bwid))),
		  y = Int.max(bwid, if (y+ht < scrHt-bwid) then y else (scrHt - (ht+bwid)))
		}
	  val mEvt = CML.wrap (mouse, msgBodyOf)
	  fun popup (menuRep, menupt, mousePos as PT{x=mx,y=my}, leavePred) = let
		val MREP{sz as SIZE{wid, ...}, item_ht, ...} = menuRep

                  (* Calculate menu origin based on mouse position *)
                val menuPt =
                  case menupt of
                    Absolute p => p
                  | Item 0 => PT{x=mx-(wid div 2),y=my-(item_ht div 2)}
                  | Item n => PT{x=mx-(wid div 2),y=my-(item_ht div 2)-(item_ht * n)}

                val menuPos = clipMenu (menuPt, menuRep)
		val {rect as RECT{x=menuX, ...}, inMenu, select, close} =
		      createMenuWin (scr, icon, menuRep, menuPos)
		fun flipOn ({draw_on, ...} : 'a mitem) = draw_on()
		fun flipOff ({draw_off, ...} : 'a mitem) = draw_off()
		fun sameItem ({id = a, ...} : 'a mitem, {id = b, ...} : 'a mitem) =
		      (a = b)
		fun trackMouse (curItem, pt) = let
		      val curItem = case (curItem, select pt)
			   of (NONE, NONE) => NONE
			    | (SOME a, NONE) => (flipOff a; NONE)
			    | (NONE, SOME b) => (flipOn b; SOME b)
			    | (SOME a, SOME b) => if (sameItem(a, b))
				then curItem
				else (flipOff a; flipOn b; SOME b)
		      fun nextMouseEvt (curItem, scrPt) = (case (CML.sync mEvt)
			   of (MOUSE_Motion{scr_pt, ...}) =>
				trackMouse (curItem, scr_pt)
			    | (MOUSE_LastUp{but, scr_pt, ...}) => (
				case (curItem : 'a mitem option)
				 of (SOME{rep=ItemRep{item, ...}, ...}) => (
				      close(); (SOME item, false, scr_pt))
				  | _ => (close(); (NONE, false, scr_pt))
                                )
			    | (MOUSE_Up{but, scr_pt, ...}) =>
				trackMouse (curItem, scr_pt)
			    | (MOUSE_Down{scr_pt, ...}) =>
				trackMouse (curItem, scr_pt)
			    | _ => trackMouse (curItem, scrPt))
		      in
			case curItem
			 of (SOME{rep=SubMRep{menu, ...}, ...}) => let
			      val PT{x,...} = pt
			      in
                                  (* If item has a submenu and mouse is on or to the
                                   * right of the icon, put up submenu.
                                   * If second field of answer is false, user is done,
                                   * so close up shop. Otherwise, some button is still
                                   * down, so continue to track the
                                   * mouse. If the mouse is really in one of our
                                   * ancestors, this will be caught in trackMouse.
                                   * This latter case could be short-circuited by
                                   * checking here that the mouse is in our rectangle,
                                   * and, if not, returning directly.
                                   *)
				if (x + (iconWid+pad+inset) >= menuX + wid)
				  then let
				    fun pred pt = (leavePred pt) orelse within(pt, rect)
				    val answer = popup(menu, Item 0, pt, pred)
				    in
                                      if #2 answer then trackMouse(curItem,#3 answer)
			              else (close(); answer)
				    end
				  else nextMouseEvt(curItem, pt)
			      end
                             (* If the mouse is not on a menu item, and is not
                              * even in the menu window (including border), and
                              * is in some ancestor menu, then close up and return.
                              *)
			  | NONE => if not (inMenu pt) andalso (leavePred pt)
			      then (close(); (NONE, true, pt))
			      else nextMouseEvt(curItem, pt)
			  | _ => nextMouseEvt(curItem, pt)
		      end
		in
		  trackMouse (NONE, mousePos)
		end (* popup *)
	  val dpy = EXB.displayOfScr scr
	  val menuCursor = EXB.stdCursor dpy StdCursor.sb_left_arrow
	  val _ = EXB.changeActiveGrabCursor dpy menuCursor
	  in
	    #1(popup (menuRep, menupt, pos, fn _ => false))
	  end (* popMenu *)

  (* return true if the menu has a sub-menu *)
    fun hasSubmenu (MENU items) = let
	  fun f [] = false
	    | f ((MenuItem _)::r) = f r
	    | f _ = true
	  in
	    f items
	  end

    fun attach (selCh, widget, mbuts, menu, label, pos) = let
	  open CML
          val root = rootOf widget
	  val dpy = displayOf root and scr = screenOf root
	  val font = Font.openFont dpy menuFont
	  val menuRep = layoutMenu(font, menu, label)
	  val icon = if (hasSubmenu menu)
		then SOME(EXB.createTileFromImage scr submenuImage)
		else NONE
	  fun realize {env as InEnv{m, ...}, win, sz} = let
		val mCh = channel()
		val menuMBS = mkButState mbuts
		fun loop () = let
		      val msg = sync m
		      in
			case (msgBodyOf msg)
			 of (MOUSE_FirstDown(arg as {scr_pt, but, ...})) => if mbutIsSet(menuMBS, but)
			      then (
				case (popMenu (menuRep, but, scr, icon, pos (WI arg), scr_pt, m))
				 of NONE => ()
				  | (SOME v) => (spawn(fn () => send(selCh, v)); ()))
			      else send(mCh, msg)
			  | _ => send(mCh, msg);
			loop()
		      end
		in
		  spawn loop;
		  W.realizeFn widget 
                    {env = replaceMouse(env, recvEvt mCh), win=win, sz=sz};
		  ()
		end (* realize *)
	  in
	    mkWidget{root=root, args = fn () => {background = NONE}, boundsOf=boundsFn widget, realize=realize}
	  end (* attach *)

    fun attachMenu (widget, mbuts, menu) = let
	  val selCh = CML.channel()
	  in
	    (attach(selCh, widget, mbuts, menu, NONE, fn _ => Item 0), CML.recvEvt selCh)
	  end

    fun attachLabeledMenu (widget, mbuts, label, menu) = let
	  val selCh = CML.channel()
	  in
	    (attach(selCh, widget, mbuts, menu, SOME label, fn _ => Item 0), CML.recvEvt selCh)
	  end

    fun buttonMenu (widget, mbuts, menu, pos) = let
	  val selCh = CML.channel()
	  in
	    (attach(selCh, widget, mbuts, menu, NONE, pos), CML.recvEvt selCh)
	  end

    fun popupMenu (root, menu, label) = let
	  val dpy = displayOf root and scr = screenOf root
	  val font = Font.openFont dpy menuFont
	  val menuRep = layoutMenu(font, menu, label)
	  val icon = if (hasSubmenu menu)
		then SOME(EXB.createTileFromImage scr submenuImage)
		else NONE
	  fun doPop (mbut, menupt, scr_pt, m) = let
		val ch = CML.channel ()
		fun doit () = CML.send (
		      ch, popMenu (menuRep, mbut, scr, icon, menupt, scr_pt, m))
		in
		  CML.spawn doit;
		  CML.recvEvt ch
		end
	  in
	    doPop
	  end

  end (* SimpleMenu *)

