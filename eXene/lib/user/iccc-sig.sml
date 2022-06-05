(* iccc.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Types and operations to support the X Inter-Client Communications Conventions.
 *)

signature ICCC =
  sig

    structure G : GEOMETRY
    structure EXB : EXENE_BASE

    eqtype atom
    type selection_handle

  (* Standard X atoms *)
    val atom_PRIMARY : atom
    val atom_SECONDARY : atom
    val atom_ARC : atom
    val atom_ATOM : atom
    val atom_BITMAP : atom
    val atom_CARDINAL : atom
    val atom_COLORMAP : atom
    val atom_CURSOR : atom
    val atom_CUT_BUFFER0 : atom
    val atom_CUT_BUFFER1 : atom
    val atom_CUT_BUFFER2 : atom
    val atom_CUT_BUFFER3 : atom
    val atom_CUT_BUFFER4 : atom
    val atom_CUT_BUFFER5 : atom
    val atom_CUT_BUFFER6 : atom
    val atom_CUT_BUFFER7 : atom
    val atom_DRAWABLE : atom
    val atom_FONT : atom
    val atom_INTEGER : atom
    val atom_PIXMAP : atom
    val atom_POINT : atom
    val atom_RECTANGLE : atom
    val atom_RESOURCE_MANAGER : atom
    val atom_RGB_COLOR_MAP : atom
    val atom_RGB_BEST_MAP : atom
    val atom_RGB_BLUE_MAP : atom
    val atom_RGB_DEFAULT_MAP : atom
    val atom_RGB_GRAY_MAP : atom
    val atom_RGB_GREEN_MAP : atom
    val atom_RGB_RED_MAP : atom
    val atom_STRING : atom
    val atom_VISUALID : atom
    val atom_WINDOW : atom
    val atom_WM_COMMAND : atom
    val atom_WM_HINTS : atom
    val atom_WM_CLIENT_MACHINE : atom
    val atom_WM_ICON_NAME : atom
    val atom_WM_ICON_SIZE : atom
    val atom_WM_NAME : atom
    val atom_WM_NORMAL_HINTS : atom
    val atom_WM_SIZE_HINTS : atom
    val atom_WM_ZOOM_HINTS : atom
    val atom_MIN_SPACE : atom
    val atom_NORM_SPACE : atom
    val atom_MAX_SPACE : atom
    val atom_END_SPACE : atom
    val atom_SUPERSCRIPT_X : atom
    val atom_SUPERSCRIPT_Y : atom
    val atom_SUBSCRIPT_X : atom
    val atom_SUBSCRIPT_Y : atom
    val atom_UNDERLINE_POSITION : atom
    val atom_UNDERLINE_THICKNESS : atom
    val atom_STRIKEOUT_ASCENT : atom
    val atom_STRIKEOUT_DESCENT : atom
    val atom_ITALIC_ANGLE : atom
    val atom_X_HEIGHT : atom
    val atom_QUAD_WIDTH : atom
    val atom_WEIGHT : atom
    val atom_POINT_SIZE : atom
    val atom_RESOLUTION : atom
    val atom_COPYRIGHT : atom
    val atom_NOTICE : atom
    val atom_FONT_NAME : atom
    val atom_FAMILY_NAME : atom
    val atom_FULL_NAME : atom
    val atom_CAP_HEIGHT : atom
    val atom_WM_CLASS : atom
    val atom_WM_TRANSIENT_FOR : atom


  (** Properties **)

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

    val property : (EXB.window * atom) -> property
    (* return the abstract representation of the named property on
     * the specified window.
     *)

    val unusedProperty : EXB.window -> property
    (* generate a property on the specified window that is guaranteed
     * to be unused.  Note that once this property has been "deleted"
     * its name may be reused.
     * NOTE: eventually, properties will be finalized, but for the
     * time being, programs should delete any allocated properties they
     * are not using.
     *)

    val mkProperty : (EXB.window * prop_val) -> property
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
    val xrdbOfScr    : EXB.screen -> string list
    
    (** end additions by ddeboer **)
     
    datatype prop_change = NewValue | Deleted

    val watchProperty : property -> (prop_change * EXB.XTime.time) CML.event
    (* returns an event for monitoring changes to a property's
     * state.  Note that once a property has been deleted, there
     * will be no more events, unless watchProperty is called again.
     *)


  (* Hints about the window size *)
    datatype size_hints
      = HINT_USPosition
      | HINT_PPosition
      | HINT_USSize
      | HINT_PSize
      | HINT_PMinSize of G.size
      | HINT_PMaxSize of G.size
      | HINT_PResizeInc of G.size
      | HINT_PAspect of { min : (int * int), max : (int * int) }
      | HINT_PBaseSize of G.size
      | HINT_PWinGravity of EXB.gravity

  (* Window manager hints *)
    datatype wm_hints
      = HINT_Input of bool        (* does this application rely on the window *)
                      (* manager to get keyboard input? *)
                      (* Initial window state (choose one) *)
      | HINT_WithdrawnState         (* for windows that are not mapped *)
      | HINT_NormalState            (* most want to start this way *)
      | HINT_IconicState            (* application wants to start as an icon *)
      | HINT_IconTile of EXB.tile     (* tile to be used as icon *)
      | HINT_IconPixmap of EXB.pixmap     (* pixmap to be used as icon *)
      | HINT_IconWindow of EXB.window     (* window to be used as icon *)
      | HINT_IconMask of EXB.pixmap   (* icon mask bitmap *)
      | HINT_IconPosition of G.point      (* initial position of icon *)
      | HINT_WindowGroup of EXB.window    (* the group leader *)

  (** atom operations **)
    val internAtom : EXB.display -> string -> atom
    val lookupAtom : EXB.display -> string -> atom option
    val nameOfAtom : EXB.display -> atom -> string

  (** Selection owner operations **)

    val acquireSelection : (EXB.window * atom * EXB.XTime.time)
      -> selection_handle option
    (* acquire the named selection *)

    val selectionOf : selection_handle -> atom
    val timeOf      : selection_handle -> EXB.XTime.time

    val selectionReqEvt : selection_handle -> {
        target : atom,
        time : EXB.XTime.time option,
        reply : prop_val option -> unit
      } CML.event
    (* this event is enabled once for each request for the selection.  The
     * target field is the requested target type; the time field is the
     * server-time of the gesture that caused the request, and the reply
     * field is a function for sending the reply.  If the time field is
     * NONE, this means a value of CurrentTime was used.  Strictly speaking
     * this violates the ICCC specification, but applications may choose
     * to accept it.
     *)

    val selectionRelEvt : selection_handle -> unit CML.event
    (* this event becomes enabled when the selection is lost; either by
     * the owner releasing it, or by some other client acquiring ownership.
     *)

    val releaseSelection : selection_handle -> unit
    (* release ownership of the selection *)


  (** Selection requestor operations **)

    val requestSelection : {
        win : EXB.window,
        selection : atom,
        target : atom,
        property : atom,
        time : EXB.XTime.time
      } -> prop_val option CML.event
    (* request the value of the selection. the win field is the requesting
     * window, the selection field is the requested selection, the target
     * field is the requested target type, and the time field is the server-
     * time of the gesture causing the request.  This returns an event
     * that will become enabled when the reply is received.
     *)

  end (* signature ICCC *)
