(* exene-win.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * The types and operations provided by eXene to support windows and window
 * environments.
 *)

signature EXENE_WIN =
  sig

    structure G : GEOMETRY
    structure EXB : EXENE_BASE
    structure ICCC : ICCC
    structure Interact : INTERACT

    type window

  (* user-level window attributes *)
    datatype window_attr
      = WA_Background_None
      | WA_Background_ParentRelative
      | WA_Background_Pixmap of EXB.pixmap
      | WA_Background_Tile of EXB.tile
      | WA_Background_Color of EXB.color
      | WA_Border_CopyFromParent
      | WA_Border_Pixmap of EXB.pixmap
      | WA_Border_Tile of EXB.tile
      | WA_Border_Color of EXB.color
      | WA_BitGravity of EXB.gravity
      | WA_WinGravity of EXB.gravity
      | WA_Cursor_None
      | WA_Cursor of EXB.cursor

  (* window stacking modes *)
    datatype stack_mode = Above | Below | TopIf | BottomIf | Opposite

  (* window configuration values *)
    datatype window_config
      = WC_Origin of G.point
      | WC_Size of G.size
      | WC_BorderWid of int
      | WC_StackMode of stack_mode
      | WC_RelStackMode of (window * stack_mode)


  (** window operations **)
    exception BadWindowGeometry

    val createSimpleTopWin : EXB.screen -> {
          geom : G.win_geom,
          border : EXB.color,
          backgrnd : EXB.color
        (* modified ddeboer, Jul 2004; original: 
        } -> (window * Interact.in_env) *)
        } -> (window * Interact.in_env * unit CML.chan)
    val createSimpleSubwin : window -> {
          geom : G.win_geom,
          border : EXB.color option,
          backgrnd : EXB.color option
        } -> window
    val createTransientWin : EXB.window -> {
          geom : G.win_geom,
          border : EXB.color,
          backgrnd : EXB.color
        } -> (window * Interact.in_env)
    val createSimplePopupWin : EXB.screen -> {
          geom : G.win_geom,
          border : EXB.color,
          backgrnd : EXB.color
        } -> (window * Interact.in_env)
    val createInputOnlyWin : window -> G.rect -> window

  (* raise this exception on operations, such as drawing, that are illegal for
   * InputOnly windows. *)
    exception InputOnly

  (* set the properties of a top-level window *)
    val setWMProperties : window -> {
          win_name : string option,
          icon_name : string option,
          argv : string list,
          size_hints : ICCC.size_hints list,
          wm_hints : ICCC.wm_hints list,
          class_hints : {res_class : string, res_name : string} option
        } -> unit

  (* set the window-manager protocols for a window *)
    val setWMProtocols : window -> EXB.atom list -> bool

  (* Various routines to reconfigure window layout *)
    val configureWin : window -> window_config list -> unit
    val moveWin : window -> G.point -> unit
    val resizeWin : window -> G.size -> unit
    val moveAndResizeWin : window -> G.rect -> unit

  (* map a point in the window's coordinate system to the screen's
   * coordinate system *)
    val winPtToScrPt : window -> G.point -> G.point

    val setCursor : window -> EXB.cursor option -> unit

    val setBackground : window -> EXB.color option -> unit
    (* set the background color attribute of the window.  Note that this does
     * not have an immediate affect on the window's contents, but if it is done
     * before the window is mapped, the window will come up with the right color.
     *)

    val changeWinAttrs : window -> window_attr list -> unit
    (* Set various window attributes *)

    val mapWin : window -> unit
    val unmapWin : window -> unit
    val withdrawWin : window -> unit
    val destroyWin : window -> unit

    val screenOfWin : window -> EXB.screen
    val displayOfWin : window -> EXB.display

    (* added ddeboer *)
    val grabKeyboard : window -> int
    val ungrabKeyboard : window -> int
  end (* EXENE_WIN *)
