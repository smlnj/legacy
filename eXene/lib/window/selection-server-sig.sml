(* selection-server-sig.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This is the lowest-level interface to the ICCCM selection protocol.
 * There is one selection server per display connection.
 *
 * NOTES:
 *  - what about incremental transfers?
 *  - currently these operations take a window as an argument, since the
 *    protocol requires one.  The selection server could allocate an unmapped
 *    window to serve as the source of ids, which would make selections
 *    independent of specific windows.  Let's see how the higher-level interfaces
 *    work out first.
 *
 *)

signature SELECTION_SERVER =
  sig

    type selection_server
    type selection_handle

    type atom = XProtTypes.atom
    type time = XTime.time

    val mkServer : XDisplay.xdisplay
	    -> (XEventTypes.xevent CML.chan * selection_server)

  (** Selection owner operations **)

    val acquireSelection : selection_server
	  -> (XProtTypes.win_id * atom * time)
	    -> selection_handle option

    val selectionOf : selection_handle -> atom
    val timeOf      : selection_handle -> time

    val requestEvt : selection_handle -> {
	    target : atom,
	    time : time option,
	    reply : XProtTypes.prop_val option -> unit
	  } CML.event
	(* this event is enabled once for each request for the selection.  The
	 * target field is the requested target type; the time field is the
	 * server-time of the gesture that caused the request, and the reply
	 * field is a function for sending the reply.  Strictly speaking
         * this violates the ICCC specification, but applications may choose
         * to accept it.
	 *)

    val releaseEvt : selection_handle -> unit CML.event
	(* this event becomes enabled when the selection is lost; either by
	 * the owner releasing it, or by some other client acquiring ownership.
	 *)

    val releaseSelection : selection_handle -> unit
	(* release ownership of the selection *)


  (** Selection requestor operations **)

    val requestSelection : selection_server -> {
	    win : XProtTypes.win_id,
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

  end; (* SELECTION_SERVER *)

