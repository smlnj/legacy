(* selection.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * A window-level view of the low-level selection operations.
 *)

structure Selection : sig

    type selection_handle
    type atom (* = XProtTypes.atom *)
    type time (* = XTime.time *)

  (** Selection owner operations **)

    val acquireSelection : (Window.window * atom * time) -> selection_handle option

    val selectionOf : selection_handle -> atom
    val timeOf      : selection_handle -> time

    val selectionReqEvt : selection_handle -> {
	    target : atom,
	    time : time option,
	    reply : XProtTypes.prop_val option -> unit
	  } CML.event
	(* this event is enabled once for each request for the selection.  The
	 * target field is the requested target type; the time field is the
	 * server-time of the gesture that caused the request, and the reply
	 * field is a function for sending the reply.
	 *)

    val selectionRelEvt : selection_handle -> unit CML.event
	(* this event becomes enabled when the selection is lost; either by
	 * the owner releasing it, or by some other client acquiring ownership.
	 *)

    val releaseSelection : selection_handle -> unit
	(* release ownership of the selection *)


  (** Selection requestor operations **)

    val requestSelection : {
	    win : Window.window,
	    selection : atom,
	    target : atom,
	    property : atom,
	    time : time
	  } -> XProtTypes.prop_val option CML.event
	(* request the value of the selection. the win field is the requesting
	 * window, the selection field is the requested selection, the target
	 * field is the requested target type, and the time field is the server-
	 * time of the gesture causing the request.  This returns an event
	 * that will become enabled when the reply is received.
	 *)

  end = struct

    structure D = Display
    structure DT = DrawTypes

    type selection_handle = SelectionServer.selection_handle
    type atom = XProtTypes.atom
    type time = XTime.time

    fun selServer (D.SCREEN{dpy=D.DPY{selection_server, ...}, ...}) =
	  selection_server

    fun acquireSelection (DT.WIN{id, scr, ...}, selection, time) =
	  SelectionServer.acquireSelection (selServer scr) (id, selection, time)

    val releaseSelection = SelectionServer.releaseSelection
    val selectionOf = SelectionServer.selectionOf
    val timeOf = SelectionServer.timeOf
    val selectionReqEvt = SelectionServer.requestEvt
    val selectionRelEvt = SelectionServer.releaseEvt

    fun requestSelection {
	  win=DT.WIN{id, scr, ...}, selection, target, property, time
	} = SelectionServer.requestSelection (selServer scr) {
	      win  = id,
	      selection = selection,
	      target = target,
	      property = property,
	      time = time
	    }

  end (* Selection *)
