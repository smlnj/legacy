(* xevttypes.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure XEventTypes =
  struct

    local
      open Geometry XProtTypes

      val & = Word.andb
      val ++ = Word.orb
      val << = Word.<<
      infix ++ << &

    in

  (* X event names *)
    datatype xevent_name
      = XEVT_KeyPress
      | XEVT_KeyRelease
      | XEVT_ButtonPress
      | XEVT_ButtonRelease
      | XEVT_EnterWindow
      | XEVT_LeaveWindow
      | XEVT_PointerMotion
      | XEVT_PointerMotionHint
      | XEVT_Button1Motion
      | XEVT_Button2Motion
      | XEVT_Button3Motion
      | XEVT_Button4Motion
      | XEVT_Button5Motion
      | XEVT_ButtonMotion
      | XEVT_KeymapState
      | XEVT_Exposure
      | XEVT_VisibilityChange
      | XEVT_StructureNotify
      | XEVT_ResizeRedirect
      | XEVT_SubstructureNotify
      | XEVT_SubstructureRedirect
      | XEVT_FocusChange
      | XEVT_PropertyChange
      | XEVT_ColormapChange
      | XEVT_OwnerGrabButton

  (* The types of the information carried by some XEvents *)
    type key_xevtinfo = {       (* KeyPress and KeyRelease *)
        root : win_id,          (* the root of the source window *)
        event : win_id,         (* the window in which this was generated *)
        child : win_id option,  (* the child of the event window that is the *)
                                (* ancestor of the source window *)
        same_screen : bool,     (* *)
        root_pt : point,        (* event coords in the root window *)
        event_pt : point,       (* event coords in the event window *)
        keycode : keycode,      (* the keycode of the depressed key *)
        mod_state : modkey_state,   (* the modifier-key state *)
        mbut_state : mbutton_state, (* the mouse button state *)
        time : XTime.time
        }
    type button_xevtinfo = {    (* ButtonPress and ButtonRelease *)
        root : win_id,          (* the root of the source window *)
        event : win_id,         (* the window in which this was generated *)
        child : win_id option,  (* the child of the event window that is the *)
                                (* ancestor of the source window *)
        same_screen : bool,     (* *)
        root_pt : point,        (* event coords in the root window *)
        event_pt : point,       (* event coords in the event window *)
        button : mbutton,       (* the button that was pressed *)
        mod_state : modkey_state,   (* the modifier-key state *)
        mbut_state : mbutton_state, (* the mouse button state *)
        time : XTime.time
      }
    type inout_xevtinfo = {     (* EnterNotify and LeaveNotify *)
        root : win_id,          (* the root window for the pointer position *)
        event : win_id,         (* the event window *)
        child : win_id option,  (* the child of event containing the pointer *)
        same_screen : bool,     (* *)
        root_pt : point,        (* final pointer position in root coords *)
        event_pt : point,       (* final pointer position in event coords *)
        mode : focus_mode,      (* *)
        detail : focus_detail,  (* *)
        mod_state : modkey_state,   (* the modifier-key state *)
        mbut_state : mbutton_state, (* the mouse button state *)
        focus : bool,           (* true, if event is the focus *)
        time : XTime.time
      }
    type focus_xevtinfo = {     (* FocusIn and FocusOut *)
        event : win_id,         (* the window that gained the focus *)
        mode : focus_mode,
        detail : focus_detail
      }


  (* X event messages *)
    datatype xevent
      = KeyPressXEvt of key_xevtinfo
      | KeyReleaseXEvt of key_xevtinfo
      | ButtonPressXEvt of button_xevtinfo
      | ButtonReleaseXEvt of button_xevtinfo
      | MotionNotifyXEvt of {
        root : win_id,          (* the root of the source window *)
        event : win_id,         (* the window in which this was generated *)
        child : win_id option,  (* the child of the event window that is the *)
                                (* ancestor of the source window *)
        same_screen : bool,     (* *)
        root_pt : point,        (* event coords in the root window *)
        event_pt : point,       (* event coords in the event window *)
        hint : bool,            (* true, if PointerMotionHint is selected *)
        mod_state : modkey_state,   (* the modifier-key state *)
        mbut_state : mbutton_state, (* the mouse button state *)
        time : XTime.time
      }
      | EnterNotifyXEvt of inout_xevtinfo
      | LeaveNotifyXEvt of inout_xevtinfo
      | FocusInXEvt of focus_xevtinfo
      | FocusOutXEvt of focus_xevtinfo
      | KeymapNotifyXEvt of {}
      | ExposeXEvt of {
        window : win_id,        (* the exposed window *)
        rects : rect list,      (* the exposed rectangle.  This is a list, so *)
                                (* that multiple events can be packed *)
        count : int             (* number of subsequent expose events *)
      }
      | GraphicsExposeXEvt of {
        drawable : drawable_id,
        rect : rect,            (* the obscured rectangle. *)
        count : int,            (* the # of additional GraphicsExpose events *)
        major_opcode : word,    (* the graphics operation code *)
        minor_opcode : word     (* always 0 for core protocol *)
      }
      | NoExposeXEvt of {
        drawable : drawable_id,
        major_opcode : word,    (* the graphics operation code *)
        minor_opcode : word     (* always 0 for core protocol *)
      }
      | VisibilityNotifyXEvt of {
        window : win_id,        (* the window with changed visibility state *)
        state : visibility      (* the new visibility state *)
      }
      | CreateNotifyXEvt of {
        parent : win_id,        (* the created window's parent *)
        window : win_id,        (* the created window *)
        rect : rect,            (* the window's rectangle *)
        border_wid : int,       (* the width of the border *)
        override_redirect : bool    (* *)
      }
      | DestroyNotifyXEvt of {
        event : win_id,         (* the window on which this was generated *)
        window : win_id         (* the destroyed window *)
      }
      | UnmapNotifyXEvt of {
        event : win_id,         (* the window on which this was generated *)
        window : win_id,        (* the window being unmapped *)
        from_config : bool      (* true, if parent was resized *)
      }
      | MapNotifyXEvt of {
        event : win_id,         (* the window on which this was generated *)
        window : win_id,        (* the window being mapped *)
        override_redirect : bool    (* *)
      }
      | MapRequestXEvt of {
        parent : win_id,        (* the parent *)
        window : win_id         (* the mapped window *)
      }
      | ReparentNotifyXEvt of {
        event : win_id,         (* the window on which this was generated *)
        parent : win_id,        (* the new parent *)
        window : win_id,        (* the re-rooted window *)
        corner : point,         (* the upper-left corner *)
        override_redirect : bool    (* *)
      }
      | ConfigureNotifyXEvt of {
        event : win_id,         (* the window on which this was generated *)
        window : win_id,        (* the reconfigured window *)
        sibling : win_id option,    (* the sibling that window is above (if any) *)
        rect : rect,            (* the window's rectangle *)
        border_wid : int,       (* the width of the border *)
        override_redirect : bool    (* *)
      }
      | ConfigureRequestXEvt of {
        parent : win_id,        (* the parent *)
        window : win_id,        (* the window to reconfigure *)
        sibling : win_id option,    (* the new sibling (if any) *)
        x : int option,         (* the window's rectangle *)
        y : int option,
        wid : int option,
        ht : int option,
        border_wid : int option,    (* the width of the border *)
        stack_mode : stack_mode option (* the mode for stacking windows *)
      }
      | GravityNotifyXEvt of {
        event : win_id,         (* the window on which this was generated *)
        window : win_id,        (* the window being moved *)
        corner : point          (* upper-left corner of window *)
      }
      | ResizeRequestXEvt of {
        window : win_id,        (* the window to resize *)
        req_sz : size           (* the requested new size *)
      }
      | CirculateNotifyXEvt of {
        event : win_id,         (* the window on which this was generated *)
        window : win_id,        (* the window being circulated *)
        parent : win_id,        (* the parent *)
        place : stack_pos       (* the new place *)
      }
      | CirculateRequestXEvt of {
        parent : win_id,        (* the parent *)
        window : win_id,        (* the window to circulate *)
        place : stack_pos       (* the place to circulate the window to *)
      }
      | PropertyNotifyXEvt of {
        window : win_id,        (* the window with the changed property *)
        atom : atom,            (* the affected property *)
        time : XTime.time,      (* when the property was changed *)
        deleted : bool          (* true, if the property was deleted *)
      }
      | SelectionClearXEvt of {
        owner : win_id,         (* the current owner of the selection *)
        selection : atom,       (* the selection *)
        time : XTime.time       (* the last-change time *)
      }
      | SelectionRequestXEvt of {
        owner : win_id,         (* the owner of the selection *)
        selection : atom,       (* the selection *)
        target : atom,          (* the requested type for the selection *)
        requestor : win_id,     (* the requesting window *)
        property : atom option, (* the property to store the selection in *)
        time : timestamp        (* *)
      }
      | SelectionNotifyXEvt of {
        requestor : win_id,     (* the requestor of the selection *)
        selection : atom,       (* the selection *)
        target : atom,          (* the requested type of the selection *)
        property : atom option, (* the property to store the selection in *)
        time : timestamp        (* *)
      }
      | ColormapNotifyXEvt of {
        window : win_id,        (* the affected window *)
        cmap : colormap_id option,  (* the colormap *)
        new : bool,             (* true, if the colormap attribute is changed *)
        installed : bool        (* true, if the colormap is installed *)
      }
      | ClientMessageXEvt of {
        window : win_id,        (* *)
        typ : atom,             (* the type of the message *)
        value : raw_data        (* the message value *)
      }
      | ModifierMappingNotifyXEvt       (* really a MappingNotify event *)
      | KeyboardMappingNotifyXEvt of {  (* really a MappingNotify event *)
        first_keycode : keycode,
        count : int
      }
      | PointerMappingNotifyXEvt    (* really a MappingNotify event *)

    fun maskOfXEvt XEVT_KeyPress        = XEVTMASK(0w1 << 0w0)
      | maskOfXEvt XEVT_KeyRelease      = XEVTMASK(0w1 << 0w1)
      | maskOfXEvt XEVT_ButtonPress     = XEVTMASK(0w1 << 0w2)
      | maskOfXEvt XEVT_ButtonRelease       = XEVTMASK(0w1 << 0w3)
      | maskOfXEvt XEVT_EnterWindow     = XEVTMASK(0w1 << 0w4)
      | maskOfXEvt XEVT_LeaveWindow     = XEVTMASK(0w1 << 0w5)
      | maskOfXEvt XEVT_PointerMotion       = XEVTMASK(0w1 << 0w6)
      | maskOfXEvt XEVT_PointerMotionHint   = XEVTMASK(0w1 << 0w7)
      | maskOfXEvt XEVT_Button1Motion       = XEVTMASK(0w1 << 0w8)
      | maskOfXEvt XEVT_Button2Motion       = XEVTMASK(0w1 << 0w9)
      | maskOfXEvt XEVT_Button3Motion       = XEVTMASK(0w1 << 0w10)
      | maskOfXEvt XEVT_Button4Motion       = XEVTMASK(0w1 << 0w11)
      | maskOfXEvt XEVT_Button5Motion       = XEVTMASK(0w1 << 0w12)
      | maskOfXEvt XEVT_ButtonMotion        = XEVTMASK(0w1 << 0w13)
      | maskOfXEvt XEVT_KeymapState     = XEVTMASK(0w1 << 0w14)
      | maskOfXEvt XEVT_Exposure        = XEVTMASK(0w1 << 0w15)
      | maskOfXEvt XEVT_VisibilityChange    = XEVTMASK(0w1 << 0w16)
      | maskOfXEvt XEVT_StructureNotify     = XEVTMASK(0w1 << 0w17)
      | maskOfXEvt XEVT_ResizeRedirect      = XEVTMASK(0w1 << 0w18)
      | maskOfXEvt XEVT_SubstructureNotify  = XEVTMASK(0w1 << 0w19)
      | maskOfXEvt XEVT_SubstructureRedirect    = XEVTMASK(0w1 << 0w20)
      | maskOfXEvt XEVT_FocusChange     = XEVTMASK(0w1 << 0w21)
      | maskOfXEvt XEVT_PropertyChange      = XEVTMASK(0w1 << 0w22)
      | maskOfXEvt XEVT_ColormapChange      = XEVTMASK(0w1 << 0w23)
      | maskOfXEvt XEVT_OwnerGrabButton     = XEVTMASK(0w1 << 0w24)

    fun maskOfXEvtList l = let
      fun f ([], m) = XEVTMASK m
        | f (xevt::r, m) = let
          val (XEVTMASK m') = maskOfXEvt xevt
          in
            f (r, m ++ m')
          end
      in
        f (l, 0w0)
      end

    fun unionXEvtMasks (XEVTMASK m1, XEVTMASK m2) = XEVTMASK(m1 ++ m2)
    fun interXEvtMasks (XEVTMASK m1, XEVTMASK m2) = XEVTMASK(m1 & m2)

  end (* local *)
  end (* XEventTypes *)
