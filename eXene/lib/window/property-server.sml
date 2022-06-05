(* property-server.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * The property server maps PropertyChange X-events to those processes that
 * are interested in them, and manages a collection of unique property names.
 * This could be done by two separate threads, but it simplifies things to
 * put all of the property stuff here.
 *)

structure PropertyServer : PROPERTY_SERVER =
  struct

    structure XTy = XProtTypes
    structure XE = XEventTypes
    structure A = AtomServer
    structure Tbl = XAtomTbl

    type atom = XTy.atom

  (* make unique property names *)
    val fmtPropName = Format.format "_EXENE_%d"
    fun mkPropName n = fmtPropName [Format.INT n]

  (* observed changes to property values *)
     datatype prop_change = NewValue | Deleted

  (* property server requests *)
    datatype request
      = WatchProp of {
	    name : atom,	    (* the watched property's name *)
	    window : XTy.win_id,    (* the watched property's window *)
	    is_unique : bool,	    (* true, if the property is an internally *)
				    (* allocated uniquely named property *)
	    notify_ch : (prop_change * XTime.time) CML.chan
				    (* the channel to send notification of *)
				    (* changes *)
	  }
      | AllocProp of (XTy.win_id * atom SyncVar.ivar)

  (* the representation of the selection server connection *)
    datatype property_server = PropServer of {
	conn : XIo.connection,
	reqCh : request CML.chan
      }

  (* watched property info *)
    type prop_info = {
	window : XTy.win_id,
	watchers : (prop_change * XTime.time) CML.chan list,
	is_unique : bool
      }

  (* operations on the property info tables; each item in the table is a list
   * of prop_info values, one for each window that has a property of the given
   * property name.
   *)
    fun mkPropTable () : prop_info list Tbl.hash_table =
	  (Tbl.mkTable (16, Fail "PropTbl"))

    fun findProp (tbl, win, name) = let
	  fun look [] = NONE
	    | look ((item : prop_info)::r) = if (#window item = win)
		then SOME item
		else look r
	  in
	    case (Tbl.find tbl name)
	     of (SOME l) => look l
	      | _ => NONE
	    (* end case *)
	  end (* findProp *)

  (* insert a watcher of a property into the table. *)
    fun insertWatcher (tbl, win, name, watcher, isUnique) = let
	  fun look [] = [{window=win, watchers=[watcher], is_unique=isUnique}]
	    | look ((item : prop_info)::r) = if (#window item = win)
		then {
		    window= win, watchers= watcher::(#watchers item),
		    is_unique= #is_unique item
		  } :: r
		else item :: (look r)
	  in
	    case (Tbl.find tbl name)
	     of NONE => Tbl.insert tbl
		    (name, [{window=win, watchers=[watcher], is_unique=isUnique}])
	      | (SOME l) => Tbl.insert tbl (name, look l)
	    (* end case *)
	  end

  (* insert a unique property into the table.  Since the property is unique,
   * it should not be in the table.
   * NOTE: this will changee if we do uniqueness by window.
   *)
    fun insertUnique (tbl : prop_info list Tbl.hash_table, win, name) =
	  Tbl.insert tbl (name, [{window=win, watchers=[], is_unique=true}])

    fun removeProp (tbl, win, name) = let
	  fun look [] = MLXError.impossible "PropertyServer.removeProp"
	    | look ((item : prop_info)::r) = if (#window item = win)
		then r
		else item::(look r)
	  in
	    case look(Tbl.lookup tbl name)
	     of [] => (Tbl.remove tbl name; ())
	      | l => Tbl.insert tbl (name, l)
	    (* end case *)
	  end

    fun mkServer (xdpy as XDisplay.XDPY{conn, ...}, atomServer) = let
	(* a table of watched properties *)
	  val propTbl = mkPropTable ()
	(* a list of unique property names *)
	  val uniqueProps = ref []
	  fun getProp () = let
		fun look (n, []) = let
		      val atom = A.internAtom atomServer (mkPropName n)
		      in
			uniqueProps := (atom, ref false) :: !uniqueProps;
			atom
		      end
		  | look (n, (atom, avail)::r) =
		      if (! avail) then (avail := false; atom) else look(n+1, r)
		in
		  look (0, ! uniqueProps)
		end
	  fun freeProp name = let
		fun look [] = MLXError.impossible "PropertyServer.freeProp"
		  | look ((atom, avail)::r) =
		      if (name = atom) then avail := true else look r
		in
		  look (! uniqueProps)
		end
	(* the X-event and request channels *)
	  val evtCh = CML.channel()
	  val reqCh = CML.channel()
	(* asynchronously send a message on a list of channels *)
	  fun broadcast ([], msg) = ()
	    | broadcast (ch::r, msg) = (
		CML.spawn(fn () => CML.send(ch, msg));
		broadcast (r, msg))
	(* handle a selection related X-event *)
	  fun handleEvt (XE.PropertyNotifyXEvt{window, atom, time, deleted}) = (
		case (findProp(propTbl, window, atom), deleted)
		 of (SOME{watchers, ...}, false) =>
		      broadcast (watchers, (NewValue, time))
		  | (SOME{watchers, is_unique, ...}, true) => (
		      broadcast (watchers, (Deleted, time));
		      removeProp (propTbl, window, atom);
		      if is_unique then freeProp atom else ())
		  | (NONE, _) => ()
		(* end case *))
	    | handleEvt xevt =
		MLXError.impossible "PropertyServer.mkServer.handleEvt"
	  val xeventEvt = CML.wrap(CML.recvEvt evtCh, handleEvt)
	(* handle a request *)
	  fun handleReq (WatchProp{name, window, is_unique, notify_ch}) =
		insertWatcher (propTbl, window, name, notify_ch, is_unique)
	    | handleReq (AllocProp(window, replyV)) = let
		val name = getProp()
		in
		  insertUnique (propTbl, window, name);
		  SyncVar.iPut(replyV, name)
		end
	  val reqEvt = CML.wrap(CML.recvEvt reqCh, handleReq)
	(* the server loop *)
	  fun loop () = (
		CML.select [xeventEvt, reqEvt];
		loop())
	  in
	    XDebug.xspawn("PropertyServer", loop);
	    (evtCh, PropServer{conn=conn, reqCh=reqCh})
	  end (* mkServer *)

    fun sendReq (PropServer{reqCh, ...}, req) = CML.send(reqCh, req)

  (* return an event for monitoring changes to a property's state *)
    fun watchProperty (server, name, window, is_unique) = let
	  val watchCh = CML.channel ()
	  in
	    sendReq (server, WatchProp{
		name=name, window=window, is_unique=is_unique, notify_ch=watchCh
	      });
	    CML.recvEvt watchCh
	  end

  (* generate a property on the specified window that is guaranteed to be
   * unique.
   *)
    fun unusedProperty (server, window) = let
	  val replyV = SyncVar.iVar()
	  in
	    sendReq (server, AllocProp(window, replyV));
	    SyncVar.iGet replyV
	  end

  end; (* PropertyServer *)

