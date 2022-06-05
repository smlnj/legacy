(* winreg.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * TODO
 *   - refresh the keymap on ModifierMappingNotifyXEvt and KeyboardMappingNotifyXEvt
 *     events.
 *   - think about the relation of locks and changes in the tree structure; also
 *     locking already locked windows.
 *)

signature WIN_REGISTRY =
  sig

    type registry

    datatype path
      = PathDst of XProtTypes.win_id
      | Path of (XProtTypes.win_id * path)

    val createRegistry : {
	      dpy : XDisplay.xdisplay,
	      keymap : Keymap.keymap,
	      propEvtCh : XEventTypes.xevent CML.chan,
	      selEvtCh : XEventTypes.xevent CML.chan
	    } -> registry

    val logTopWin : (registry * XProtTypes.win_id)
	  -> (path * XEventTypes.xevent) CML.event

  (* lock a window and all of its descendants; return the unlocking function *)
    val lockWinTree : (registry * XProtTypes.win_id) -> (unit -> unit)

  (* test to see if a window is locked *)
    val isLocked : (registry * XProtTypes.win_id) -> bool

  end (* WIN_REGISTRY *)

structure WinRegistry : WIN_REGISTRY =
  struct

    datatype path
      = PathDst of XProtTypes.win_id
      | Path of (XProtTypes.win_id * path)

    local
      structure XTy = XProtTypes
      structure XEvt = XEventTypes
      structure XDpy = XDisplay

      datatype req_msg
	= REQ_new of XTy.win_id
	| REQ_lock of XTy.win_id
	| REQ_unlock of XTy.win_id
	| REQ_islocked of XTy.win_id

(** NOTE: could bundle this all into a couple of functions **)
      datatype win_desc = WD of {
	  id : XTy.win_id,
	  path : path,
	  parent : win_desc option,
	  children : win_desc list ref,
	  lock : bool ref,
	  evt_strm : (path * XEvt.xevent) CML.chan
	}

      datatype xevt_dst
	= ToWindow of XTy.win_id
	| CreateWin of {parent : XTy.win_id, new_win : XTy.win_id}
	| DestroyWin of XTy.win_id
	| ToKeymapServer
	| ToPropertyServer
	| ToSelectionServer
	| ToAll
	| ToTrash

	(* ddeboer test, fall 2004. 
	val rcref = ref 0*)
	
    (* discard instances of an X-event that are the product of SubstructureNotify,
     * instead of StructureNotify. *)
      fun filterSubstructXEvt (w1, w2) = if (w1 = w2) then (ToWindow w1) else ToTrash

    (* extract the destination xid of an X-event *)
      fun extractDst (XEvt.KeyPressXEvt{event, ...}) = ToWindow event
	| extractDst (XEvt.KeyReleaseXEvt{event, ...}) = ToWindow event
	| extractDst (XEvt.ButtonPressXEvt{event, ...}) = ToWindow event
	| extractDst (XEvt.ButtonReleaseXEvt{event, ...}) = ToWindow event
	| extractDst (XEvt.MotionNotifyXEvt{event, ...}) = ToWindow event
	| extractDst (XEvt.EnterNotifyXEvt{event, ...}) = ToWindow event
	| extractDst (XEvt.LeaveNotifyXEvt{event, ...}) = ToWindow event
	| extractDst (XEvt.FocusInXEvt{event, ...}) = ToWindow event
	| extractDst (XEvt.FocusOutXEvt{event, ...}) = ToWindow event
	(*| extractDst (XEvt.KeymapNotifyXEvt{, ...}) = *)
	| extractDst (XEvt.ExposeXEvt{window, ...}) = ToWindow window
	(*| extractDst (XEvt.GraphicsExposeXEvt ?? *)
	(*| extractDst (XEvt.NoExposeXEvt{, ...}) =*)
	| extractDst (XEvt.VisibilityNotifyXEvt{window, ...}) = ToWindow window
	| extractDst (XEvt.CreateNotifyXEvt{parent, window, ...}) =
	    CreateWin{parent = parent, new_win = window}
	| extractDst (XEvt.DestroyNotifyXEvt{event, window, ...}) =
	    if (event = window)
	      then (DestroyWin event)	(* remove window from registry *)
	      else (ToWindow event)	(* report to parent that child is dead *)
	| extractDst (XEvt.UnmapNotifyXEvt{event, window, ...}) =
	    filterSubstructXEvt(event, window)
	| extractDst (XEvt.MapNotifyXEvt{event, window, ...}) =
	    filterSubstructXEvt(event, window)
	(*| extractDst (XEvt.MapRequestXEvt{, ...}) =*)
	| extractDst (XEvt.ReparentNotifyXEvt _) = ToTrash
	| extractDst (XEvt.ConfigureNotifyXEvt{event, window, ...}) =
	    filterSubstructXEvt(event, window)
	(*| extractDst (XEvt.ConfigureRequestXEvt{, ...}) =*)
	| extractDst (XEvt.GravityNotifyXEvt{event, window, ...}) =
	    filterSubstructXEvt(event, window)
	(*| extractDst (XEvt.ResizeRequestXEvt{, ...}) =*)
	| extractDst (XEvt.CirculateNotifyXEvt{event, window, ...}) =
	    filterSubstructXEvt(event, window)
	(*| extractDst (XEvt.CirculateRequestXEvt{, ...}) =*)
(** NOTE: we may have other uses of PropertyNotify someday **)
	| extractDst (XEvt.PropertyNotifyXEvt _) = ToPropertyServer
	| extractDst (XEvt.SelectionClearXEvt _) = ToSelectionServer
	| extractDst (XEvt.SelectionRequestXEvt _) = ToSelectionServer
	| extractDst (XEvt.SelectionNotifyXEvt _) = ToSelectionServer
	| extractDst (XEvt.ColormapNotifyXEvt{window, ...}) = ToWindow window
	| extractDst (XEvt.ClientMessageXEvt{window, ...}) = ToWindow window
	| extractDst XEvt.ModifierMappingNotifyXEvt = ToAll
	| extractDst (XEvt.KeyboardMappingNotifyXEvt _)  = ToAll
	| extractDst XEvt.PointerMappingNotifyXEvt = ToAll
	| extractDst e = (
	    MLXError.warning(String.concat[
	      "[WinReg: unexpected ", XPrint.xevtName e, " event]\n"]);
	    ToTrash)
(* +DEBUG *)
local
  open XPrint
  fun dst2s (ToWindow w) = ("ToWindow(" ^ xidToString w ^ ")")
    | dst2s (CreateWin{parent, new_win}) = String.concat[
	  "CreateWin{parent=", xidToString parent, ", new_win=",
	  xidToString new_win, "}"
	]
    | dst2s (DestroyWin w) = ("DestroyWin(" ^ xidToString w ^ ")")
    | dst2s ToKeymapServer = "ToKeymapServer"
    | dst2s ToPropertyServer = "ToPropertyServer"
    | dst2s ToSelectionServer = "ToSelectionServer"
    | dst2s ToAll = "ToAll"
    | dst2s ToTrash = "ToTrash"
in
val extractDst = fn evt => let val dst = extractDst evt
      in
        XDebug.trace(XDebug.winregTM, fn () => ["WinReg: ", xevtName evt, " => ", dst2s dst, "\n"]);
        dst
      end
end
(* -DEBUG *)

    in

    datatype registry = WinReg of {
	reqch : req_msg CML.chan,
	replych : (path * XEvt.xevent) CML.event CML.chan,
	lockch : bool CML.chan
      }

    fun setLock v = let
	  fun set (WD{lock, children, ...}) = (lock := v; setList (!children))
	  and setList [] = ()
	    | setList (wd::r) = (set wd; setList r)
	  in
	    set
	  end
    val lockTree = setLock true
    val unlockTree = setLock false

    fun createRegistry {dpy=XDpy.XDPY{conn, ...}, keymap, propEvtCh, selEvtCh} = let
    	  (* testing: ddeboer, fall 2004. *
    	  val rn = (!rcref)
    	  val _ = (TextIO.print ("Started registry " ^ (Int.toString rn) ^ "\n");
    	  		rcref:=(!rcref)+1)
    	  * end testing. *)
	  val xevtIn = XIo.waitForXEvent conn
	  val registerReqCh = CML.channel() and registerReplyCh = CML.channel()
	  val lockReplyCh = CML.channel()
	  val idMap = HashXId.newMap()
	  val find = HashXId.lookup idMap
	  val insert = HashXId.insert idMap
	  val remove = HashXId.remove idMap
	  fun handleReq (REQ_new win) = let (* log a new top-level window *)
		val evtCh = CML.channel()
		in
		  insert (win, WD{
		      id = win, path = PathDst win, parent = NONE,
		      children = ref[], lock = ref false, evt_strm = evtCh
		    });
		  CML.send(registerReplyCh, CML.recvEvt evtCh)
		end
	    | handleReq (REQ_lock win) = (lockTree (find win))
	    | handleReq (REQ_unlock win) = (unlockTree (find win))
	    | handleReq (REQ_islocked win) = let
		val WD{lock, ...} = find win
		in
		  CML.send (lockReplyCh, !lock)
		end
	  fun newSubwin (parent, childId) = let
		val parentDesc as WD{path, evt_strm, children, lock, ...} = find parent
		fun extendPath (PathDst w) = Path(w, PathDst childId)
		  | extendPath (Path(w, path)) = Path(w, extendPath path)
		val childPath = extendPath path
		val child = WD{
			id = childId, path = childPath, parent = SOME parentDesc,
			children = ref [], lock = ref(! lock), evt_strm = evt_strm
		      }
		in
		  children := child :: (! children);
		  insert (childId, child)
		end
	  (* ddeboer, test, fall 2004 
	  val cref = ref 0*)
	  fun sendEvt (e, WD{path, evt_strm, ...}) = 
	  	(* test, ddeboer, fall 2004: *)
	  	( (*TextIO.print ("reg sending "^(Int.toString rn)^" "^(Int.toString (!cref))^"\n");*)
	  	 CML.send (evt_strm, (path, e)))
	  	 (*TextIO.print ("reg sent    "^(Int.toString rn)^" "^(Int.toString (!cref))^"\n");
	  	 cref := ((!cref)+1))*)
	  fun sendEvtToWin (e, winId) = (sendEvt (e, find winId))
		handle HashXId.XIdNotFound => ()
	  fun handleEvt e = (case (extractDst e)
		 of (ToWindow winId) => (sendEvtToWin(e, winId))
		  | (CreateWin{parent, new_win}) => (
		      newSubwin(parent, new_win);
		      sendEvtToWin(e, parent))
		  | (DestroyWin winId) => (case (remove winId)
		       of (win as WD{parent=SOME(WD{children,...}), ...}) => let
			    fun removeChild [] = (
				  MLXError.warning "[WinReg: missing child]"; [])
			      | removeChild ((w as WD{id, ...}) :: r) =
				  if (id = winId) then r else (w :: (removeChild r))
			    in
			      children := removeChild (! children);
			      sendEvt (e, win)
			    end
			| win => sendEvt (e, win)
		      (* end case *))
		  | ToKeymapServer => (
		      MLXError.warning "[WinReg: unexpected ToKeymapServer]"; ())
		  | ToPropertyServer => CML.send (propEvtCh, e)
		  | ToSelectionServer => CML.send (selEvtCh, e)
		  | ToTrash => ()
		  | ToAll => app (fn (_, x) => sendEvt(e, x)) (HashXId.list idMap)
		(* end case *))
	  val evt = CML.choose [
		  CML.wrap (CML.recvEvt registerReqCh, handleReq),
		  CML.wrap (xevtIn, handleEvt)
		]
	  (*fun loop () = (CML.sync evt; loop())*)
(* DEBUG *) fun loop () = (XDebug.trace(XDebug.winregTM, fn () => ["Winreg.loop: waiting\n"]); CML.sync evt; loop())
	  in
	    XDebug.xspawn ("WinReg", loop);
	    WinReg{
		reqch = registerReqCh,
		replych = registerReplyCh,
		lockch = lockReplyCh
	      }
	  end (* createRegistry *)

      fun logTopWin (WinReg{reqch, replych, ...}, win) = (
	    CML.send(reqch, REQ_new win); CML.recv replych)

      fun lockWinTree (WinReg{reqch, replych, ...}, win) = (
	    CML.send (reqch, REQ_lock win);
	    fn () => CML.send (reqch, REQ_unlock win))

      fun isLocked (WinReg{reqch, lockch, ...}, win) = (
	    CML.send (reqch, REQ_islocked win);
	    CML.recv lockch)

    end (* local *)
  end (* WinRegistry *)
