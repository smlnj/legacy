(* selection-server.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * A per-display server to handle the ICCCM selection protocol.
 *
 * NOTES:
 *  - what about incremental transfers?
 *  - currently these operations take a window as an argument, since the
 *    protocol requires one.  The selection server could allocate an unmapped
 *    window to serve as the source of ids, which would make selections
 *    independent of specific windows.  Let's see how the higher-level interfaces
 *    work out first.
 *
 * This mechanism must deal with a complicated protocol, and a bunch of different
 * kinds of X events and requests.  Here is a summary:
 *
 * REQUESTS:
 *    GetSelectionOwner	-- used by owner after a SetSelectionOwner to test if the
 *			   selection was acquired.
 *    SetSelectionOwner -- used by owner to acquire the selection.
 *    ConvertSelection  -- used by requestor to request that the selection value
 *			   be put into some property.
 *    GetProperty	-- used by the requestor to get the selection value.
 *    ChangeProperty	-- used by the owner to put the requested selection in
 *			   the requested property.  And used by the requestor to
 *			   delete the property, once it gets the value.
 *    SendEvent		-- used by the owner send a SelectionNotify event to the
 *			   requester.
 *
 * EVENTS:
 *    SelectionRequest	-- received by the owner as a result of the requestor
 *			   sending a ConvertSelection request.
 *    SelectionNotify	-- sent by the owner to the requestor, once the selection
 *			   has been put into the requested property.
 *    SelectionClear	-- received by the owner, when it loses the selection.
 *    PropertyNotify	-- received by the owner, once the requestor has deleted
 *			   the property.
 *)

structure SelectionServer : SELECTION_SERVER =
  struct

    structure XTy = XProtTypes
    structure XE = XEventTypes
    structure A = AtomServer
    structure Tbl = XAtomTbl

    type atom = XTy.atom
    type time = XTime.time

(* +DEBUG *)
    fun trace f = XDebug.trace(XDebug.selTM, f)
(* -DEBUG *)

  (* given message encode and reply decode functions, send and receive a query *)
    fun query (encode, decode) conn = let
          val requestReply = XIo.requestReply conn
          fun ask msg = (decode (CML.sync (requestReply (encode msg))))
                handle XIo.LostReply => raise (MLXError.XERROR "[reply lost]")
                     | (XIo.ErrorReply err) =>
                        raise (MLXError.XERROR(XPrint.xerrorToString err))
          in
            ask
          end

 (* Various protocol requests that we need *)
    val getSelectionOwner = query (
	  XRequest.encodeGetSelectionOwner, XReply.decodeGetSelectionOwnerReply)
    fun setSelectionOwner conn arg =
	  XIo.request conn (XRequest.encodeSetSelectionOwner arg)
    fun convertSelection conn arg =
	  XIo.request conn (XRequest.encodeConvertSelection arg)
    fun selectionNotify conn {requestor, selection, target, prop, time} =
	  XIo.request conn (XSendEvent.encodeSendSelectionNotify {
	      dst = XTy.SendEvtTo_Window requestor,
	      propagate = false, evt_mask = XTy.XEVTMASK 0w0,
	      requestor = requestor, selection = selection, target = target,
	      property = prop, time = time
	    })
    val reqGetProperty = query (
	  XRequest.encodeGetProperty, XReply.decodeGetPropertyReply)
    fun changeProperty conn arg =
	  XIo.request conn (XRequest.encodeChangeProperty arg)

  (* get a property value, which may require several requests *)
    fun getProperty conn (win, prop) = let
	  fun sizeOf (XTy.RAW_DATA{data, ...}) = (Word8Vector.length data div 4)
	  fun getChunk wordsSoFar = reqGetProperty conn {
                  win = win, prop = prop,
                  typ = NONE, (* AnyPropertyType *)
                  offset = wordsSoFar, len = 1024,
		  delete = false
                }
	  fun deleteProp () = reqGetProperty conn {
		  win = win, prop = prop,
		  typ = NONE, (* AnyPropertyType *)
		  offset = 0, len = 0, delete = true
	        }
	  fun extendData (data', XTy.RAW_DATA{data, ...}) = data :: data'
	  fun flattenData (data', XTy.RAW_DATA{format, data}) =
		XTy.RAW_DATA{
		    format=format,
		    data=Word8Vector.concat(rev (data :: data'))
		  }
	  fun getProp () = (case (getChunk 0)
		 of NONE => NONE
		  | (SOME{typ, bytes_after, value as XTy.RAW_DATA{data, ...}}) =>
		      if (bytes_after = 0)
		        then (
			  deleteProp();
			  SOME(XTy.PROP_VAL{typ=typ, value=value}))
		        else getRest (sizeOf value, [data])
		(* end case *))
	  and getRest (wordsSoFar, data') = (case (getChunk wordsSoFar)
		 of NONE => NONE
		  | (SOME{typ, bytes_after, value}) => if (bytes_after = 0)
		      then (
			deleteProp();
			SOME(XTy.PROP_VAL{typ=typ, value=flattenData(data', value)}))
		      else getRest(
			wordsSoFar + sizeOf value,
			extendData (data', value))
		(* end case *))
	  in
	    getProp ()
	  end

  (* the return result of a REQ_RequestSel *)
    type request_res = XTy.prop_val option CML.event

    type sel_request = {	    (* the request for a selection that gets *)
				    (* sent to the owner *)
	target : atom,
	time : time option,
	reply : XTy.prop_val option -> unit
      }

  (* an abstract handle on a selection *)
    datatype selection_handle = SH of {
	  selection : atom,
	  time : time,
	  reqEvt : sel_request CML.event,
	  relEvt : unit CML.event,
	  release : unit -> unit
	}

    datatype request
      = REQ_AcquireSel of {		(* acquire a selection *)
	  win : XTy.win_id,
	  selection : atom,
	  time : time,
	  ack : selection_handle option SyncVar.ivar
	}
      | REQ_ReleaseSel of atom		(* release a selection *)
      | REQ_RequestSel of {		(* request the value of a selection *)
	  win : XTy.win_id,
	  selection : atom,
	  target : atom, 
	  property : atom,
	  time : time,
	  ack : request_res SyncVar.ivar
	}

  (* info about held selections *)
    type selection_info = {
	  owner : XTy.win_id,
	  reqCh : sel_request CML.chan,
	  relV : unit SyncVar.ivar,
	  time : time
	}

  (* info about outstanding selection requests *)
    type request_info = XTy.prop_val option SyncVar.ivar

  (* the representation of the selection server connection *)
    datatype selection_server = SelServer of request CML.chan

    fun mkServer (xdpy as XDisplay.XDPY{conn, ...}) = let
	(* table of held selections *)
	  val selectionTbl : selection_info Tbl.hash_table =
		Tbl.mkTable (32, Fail "SelectionTbl")
	  val insertSel = Tbl.insert selectionTbl
	  val findSel = Tbl.find selectionTbl
	  val removeSel = Tbl.remove selectionTbl
	(* the table of pending selection requests *)
	  val requestTbl : request_info Tbl.hash_table =
		Tbl.mkTable (32, Fail "RequestTbl")
	  val insertReq = Tbl.insert requestTbl
	  val findReq = Tbl.find requestTbl
	  val removeReq = Tbl.remove requestTbl
	(* the X-event and request channels *)
	  val evtCh = CML.channel() and reqCh = CML.channel()
	(* handle a selection related X-event *)
	  fun handleEvt (XE.SelectionRequestXEvt xevt) = let
		fun rejectReq () = selectionNotify conn {
			requestor = #requestor xevt,
			selection = #selection xevt,
			target = #target xevt,
			prop = NONE,
			time = #time xevt
		      }
		in
trace(fn() => ["SelectionRequestXEvt\n"]);
		  case (findSel (#selection xevt), #time xevt)
		   of (NONE, _) => (* we don't hold this selection, return NONE *)
(trace(fn () => ["  SelectionRequestXEvt rejected: no selection\n"]);
		        rejectReq ()
)
		    | (SOME{reqCh, ...}, timeStamp) => let
			val optTime = (case timeStamp
			       of XTy.CurrentTime => NONE
				| XTy.TimeStamp time => SOME time
			      (* end case *))
		      (* propagate the request to the holder of the selection. *)
		        val prop = (case (#property xevt)
			       of NONE => (#target xevt) (* obsolete client *)
			        | (SOME p) => p
			      (* end case *))
		        val cv = SyncVar.iVar()
		        fun replyThread () = (
			      CML.send (reqCh, {
				  target = #target xevt,
				  time = optTime,
				  reply = (fn x => SyncVar.iPut(cv, x))
			        });
			      case (SyncVar.iGet cv)
			       of NONE => rejectReq()
			        | (SOME propVal) => (
				  (* write out the property value *)
				    changeProperty conn {
					win = #requestor xevt,
					name = prop,
					mode = XTy.ReplaceProp,
					prop = propVal
				      };
				    selectionNotify conn {
					requestor = #requestor xevt,
					selection = #selection xevt,
					target = #target xevt,
					prop = #property xevt,
					time = timeStamp
				      })
			      (* end case *))
		        in
			  CML.spawn replyThread; ()
		        end
		  (* end case *)
		end (* handleEvt SelectionRequestXEvt *)
	    | handleEvt (XE.SelectionClearXEvt{selection, ...}) = (
trace(fn() => ["SelectionClearXEvt\n"]);
		case (findSel selection)
		 of NONE => ()  (* error ??? *)
		  | (SOME{relV, ...}) => (
		      removeSel selection;
		      SyncVar.iPut(relV, ()))
		(* end case *))
	    | handleEvt (XE.SelectionNotifyXEvt xevt) = (
trace(fn() => ["SelectionNotifyXEvt\n"]);
		case (findReq (#selection xevt), #property xevt)
		 of (NONE, _) => ()  (* error ?? *)
		  | (SOME replyV, NONE) => (
		      removeReq (#selection xevt);
		      SyncVar.iPut(replyV, NONE))
		  | (SOME replyV, SOME prop) => let
		      val propVal = getProperty conn (#requestor xevt, prop)
		      in
		        removeReq (#selection xevt);
		        SyncVar.iPut(replyV, propVal)
		      end
		(* end case *))
	    | handleEvt xevt = MLXError.impossible "SelectionServer.mkServer.handleEvt"
	(* handle a request *)
	  fun handleReq (REQ_AcquireSel{win, selection, time, ack}) = (
trace(fn() => ["REQ_AcquireSel\n"]);
		setSelectionOwner conn {
		    win = SOME win, selection = selection,
		    timestamp = XTy.TimeStamp time
		  };
trace(fn() => ["REQ_AcquireSel: check owner\n"]);
		case (getSelectionOwner conn {selection = selection})
		 of NONE => SyncVar.iPut(ack, NONE)
		  | (SOME id) => if (id = win)
		      then let
			val selReqCh = CML.channel() and relV = SyncVar.iVar()
			val res = SH{
				selection = selection,
				time = time,
				reqEvt = CML.recvEvt selReqCh,
				relEvt = SyncVar.iGetEvt relV,
				release =
				  fn () => CML.send(reqCh, REQ_ReleaseSel selection)
			      }
			in
			  insertSel (selection,
			    {owner=win, reqCh=selReqCh, relV=relV, time=time});
			  SyncVar.iPut(ack, SOME res)
			end
		      else SyncVar.iPut(ack, NONE)
		(* end case *))
	    | handleReq (REQ_ReleaseSel selection) = (
trace(fn() => ["REQ_ReleaseSel\n"]);
		removeSel selection;
		setSelectionOwner conn {
		    win = NONE, selection = selection,
		    timestamp = XTy.CurrentTime (* ??? *)
		  };
		XIo.flushOut conn)
	    | handleReq (REQ_RequestSel req) = let
		val replV = SyncVar.iVar()
		in
trace(fn() => ["REQ_RequestSel\n"]);
		  insertReq (#selection req, replV);
		  convertSelection conn {
		      selection = #selection req,
		      target = #target req,
		      property = SOME(#property req),
		      requestor = #win req,
		      timestamp = XTy.TimeStamp(#time req)
		    };
		  SyncVar.iPut (#ack req, SyncVar.iGetEvt replV)
		end
	  val evt = CML.choose [
		  CML.wrap (CML.recvEvt evtCh, handleEvt),
		  CML.wrap (CML.recvEvt reqCh, handleReq)
		]
	  fun loop () = (CML.sync evt; loop())
	  in
	    XDebug.xspawn ("SelectionServer", loop);
	    (evtCh, SelServer reqCh)
	  end (* mkServer *)

    fun acquireSelection (SelServer reqCh) (win, selection, time) = let
	  val replyV = SyncVar.iVar()
	  in
	    CML.send (reqCh, REQ_AcquireSel{
		win = win, selection = selection, time = time, ack = replyV
	      });
	    SyncVar.iGet replyV
	  end

    fun selectionOf (SH{selection, ...}) = selection

    fun timeOf (SH{time, ...}) = time

    fun requestEvt (SH{reqEvt, ...}) = reqEvt

    fun releaseEvt (SH{relEvt, ...}) = relEvt

    fun releaseSelection (SH{release, ...}) = release ()

    fun requestSelection (SelServer reqCh) {
	  win, selection, target, property, time
	} = let
	  val replyV = SyncVar.iVar()
	  in
	    CML.send(reqCh, REQ_RequestSel{
		win=win, selection=selection, target=target, property=property,
		time=time, ack=replyV
	      });
	    SyncVar.iGet replyV
	  end

  end; (* SelectionServer *)
