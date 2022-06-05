(* property-server-sig.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This is the interface to the Property manager, which handles operations
 * on X-server properties.
 *)

signature PROPERTY_SERVER =
  sig

  (* the abstract connection to the property server *)
    type property_server

    type atom (* = XProtTypes.atom *)

  (* observed changes to property values *)
     datatype prop_change = NewValue | Deleted

    val mkServer : (XDisplay.xdisplay * AtomServer.atom_server)
	    -> (XEventTypes.xevent CML.chan * property_server)

    val unusedProperty : (property_server * XProtTypes.win_id) -> atom

    val watchProperty : (property_server * atom * XProtTypes.win_id * bool)
	  -> (prop_change * XTime.time) CML.event

  end; (* PROPERTY_SERVER *)

