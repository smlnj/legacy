(* property-sig.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * An interface to the property management routines.
 *)

signature PROPERTY =
  sig

    type atom (* = XProtTypes.atom *)

  (* raw data from server (in ClientMessage, property values, ...) *)
    datatype raw_format = Raw8 | Raw16 | Raw32
    datatype raw_data = RAW_DATA of {
    format : raw_format,
    data : Word8Vector.vector
      }

  (* X property values.  A property value has a type, which is an atom, and
   * a value.  The value is a sequence of 8, 16 or 32-bit items, represented
   * as a format and a string.
   *)
    datatype prop_val = PROP_VAL of {
    typ : atom,
    value : raw_data
      }

    exception PropAlloc
    (* raised, if there is not enough space to store a property value
     * on the server.
     *)

  (* an abstract interface to a property on a window *)
    type property

    val property : (Window.window * atom) -> property
    (* return the abstract representation of the named property on
     * the specified window.
     *)

    val unusedProperty : Window.window -> property
    (* generate a property on the specified window that is guaranteed
     * to be unused.  Note that once this property has been "deleted"
     * its name may be reused.
     * NOTE: eventually, properties will be finalized, but for the
     * time being, programs should delete any allocated properties they
     * are not using.
     *)

    val mkProperty : (Window.window * prop_val) -> property
    (* create a new property initialized to the given value *)

    val nameOfProp : property -> atom
    (* return the atom that names the given property *)

    val setProperty : (property * prop_val) -> unit
    (* set the value of the property *)

    val appendToProperty : (property * prop_val) -> unit
    (* append the property value to the property; the types
     * and formats must match.
     *)

    val prependToProperty : (property * prop_val) -> unit
    (* prepend the property value to the property; the types
     * and formats must match.
     *)

    val deleteProperty : property -> unit
    (* delete the named property *)

    exception RotateProps
    val rotateProperties : (property list * int) -> unit
    (* rotate the list of properties; raises RotateProps if the
     * properties do not belong to the same window.
     *)

    val getProperty : property -> prop_val option
    (* get the value of the property; if the property has not been
     * set, then NONE is returned.
     *)

    datatype prop_change = NewValue | Deleted

    val watchProperty : property -> (prop_change * XTime.time) CML.event
    (* returns an event for monitoring changes to a property's
     * state.  Note that once a property has been deleted, there
     * will be no more events, unless watchProperty is called again.
     *)

    (* Additions by ddeboer, May 2004. 
     * Dusty deBoer, KSU CIS 705, Spring 2004. *)
     
    (* xrdbOfScr: return the list of strings contained in the
     * XA_RESOURCE_MANAGER property of the root screen of the
     * specified screen. 
     * This should properly belong some other place than in ICCC,
     * as it has nothing to do with ICCC, except that it accesses
     * data in the screen type, and uses the GetProperty functions
     * of ICCC.
     *)
    val xrdbOfScr    : Display.screen -> string list
    (* end additions by ddeboer. *)
  end;

