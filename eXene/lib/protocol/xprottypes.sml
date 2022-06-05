(* xprottypes.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This is an ML interface to the low-level base types supported by
 * the X11 protocol.
 *)

structure XProtTypes =
  struct

  (* X authentication information *)
    datatype authentication = AUTH of {
	family : int,
	addr : string,
	dpy : string,
	name : string,
	data : Word8Vector.vector
      }

  (* X atoms *)
    datatype atom = XAtom of word

  (* X resource ids.  These are used to name windows, pixmaps, fonts, graphics
   * contexts, cursors and colormaps.  We collapse all of these types into xid and
   * leave it to a higher level interface to distinguish them.  Type synonyms
   * are defined for documentary purposes.
   * NOTE: the X11 protocol spec guarantees that XIDs can be represented in
   * 29-bits.
   *)
    datatype xid = XID of word

    type win_id = xid
    type pixmap_id = xid
    type drawable_id = xid (* either win_id or pixmap_id *)
    type font_id = xid
    type gc_id = xid
    type fontable_id = xid (* either font_id or gc_id *)
    type cursor_id = xid
    type colormap_id = xid

    datatype pixel = PIXEL of int  (* index into colormap *)
    datatype plane_mask = PLANEMASK of word

    datatype visual_id = VISUALID of word (* should this be int?? *)

    datatype keycode = KEYCODE of int
    datatype keysym = NoSymbol | KEYSYM of int

    val AnyKey = (KEYCODE 0)

  (* X time stamps are either the Current Time, or an X Server time value *)
    datatype timestamp = CurrentTime | TimeStamp of XTime.time


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

  (* modes for ChangeProperty *)
    datatype change_prop_mode = ReplaceProp | PrependProp | AppendProp

  (* polygon shapes *)
    datatype shape = ComplexShape | NonconvexShape | ConvexShape

  (* RGB colors *)
    datatype rgb = RGB of {red : word, green : word, blue : word}

  (* Color items *)
    datatype color_item = COLORITEM of {
	pixel : pixel,
	red : word option,
	green : word option,
	blue : word option
      }

  (* text/font items, used by PolyText[8,16] *)
    datatype text_font
      = FontItem of font_id		(* set new font *)
      | TextItem of (int * string)	(* text item *)

  (* modifier keys and mouse buttons *)
    datatype modkey = ShiftKey | LockKey | ControlKey
      | Mod1Key | Mod2Key | Mod3Key | Mod4Key | Mod5Key
      | AnyModifier
    datatype mbutton = MButton of int
  (* A modifier key state vector *)
    datatype modkey_state = AnyModKey | MKState of word
  (* A Mouse button state vector *)
    datatype mbutton_state = MBState of word

  (* modes for AllowEvents *)
    datatype event_mode
      = AsyncPointer | SyncPointer | ReplayPointer
      | AsyncKeyboard | SyncKeyboard | ReplayKeyboard
      | AsyncBoth | SyncBoth

  (* keyboard focus modes *)
    datatype focus_mode = FocusNormal | FocusWhileGrabbed | FocusGrab | FocusUngrab
    datatype focus_detail
      = FocusAncestor | FocusVirtual | FocusInferior | FocusNonlinear
      | FocusNonlinearVirtual | FocusPointer | FocusPointerRoot | FocusNone

  (* input focus modes *)
    datatype input_focus
      = InputFocus_None
      | InputFocus_PointerRoot
      | InputFocus_Window of win_id
    datatype focus_revert = RevertToNone | RevertToPointerRoot | RevertToParent

  (* SendEvent targets *)
    datatype send_evt_to
      = SendEvtTo_PointerWindow
      | SendEvtTo_InputFocus
      | SendEvtTo_Window of win_id

  (* input device grab modes *)
    datatype grab_mode = SynchronousGrab | AsynchronousGrab

  (* input device grab status *)
    datatype grab_status
      = GrabSuccess | AlreadyGrabbed | GrabInvalidTime | GrabNotViewable | GrabFrozen

  (* input device mapping status *)
    datatype mapping_status = MappingSuccess | MappingBusy | MappingFailed

  (* visibility *)
    datatype visibility
      = VisibilityUnobscured
      | VisibilityPartiallyObscured
      | VisibilityFullyObscured

  (* window stacking modes *)
    datatype stack_mode = Above | Below | TopIf | BottomIf | Opposite

  (* window circulation request *)
    datatype stack_pos = PlaceOnTop | PlaceOnBottom

  (* window backing-store classes *)
    datatype backing_store = BS_NotUseful | BS_WhenMapped | BS_Always

  (* window map states *)
    datatype map_state = WinIsUnmapped | WinIsUnviewable | WinIsViewable

  (* rectangle list orderings for SetClipRectangles *)
    datatype rect_order
      = UnsortedOrder | YSortedOrder | YXSortedOrder | YXBandedOrder

  (* font drawing direction *)
    datatype font_draw_dir = FontLeftToRight | FontRightToLeft

  (* font properties *)
    datatype font_prop = FontProp of {
	name : atom,		(* the name of the property *)
	value : Word32.word	(* the property value: interpret according to the *)
				(* property. *)
      }

  (* per-character font info *)
    datatype char_info = CharInfo of {
	left_bearing : int,
	right_bearing : int,
	char_wid : int,
	ascent : int,
	descent : int,
	attributes : word
      }

  (* graphics functions *)
    datatype graphics_op
      = OP_Clr			(* 0 *)
      | OP_And			(* src AND dst *)
      | OP_AndNot		(* src AND NOT dst *)
      | OP_Copy			(* src *)
      | OP_AndInverted		(* NOT src AND dst *)
      | OP_Nop			(* dst *)
      | OP_Xor			(* src XOR dst *)
      | OP_Or			(* src OR dst *)
      | OP_Nor			(* NOT src AND NOT dst *)
      | OP_Equiv		(* NOT src XOR dst *)
      | OP_Not			(* NOT dst *)
      | OP_OrNot		(* src OR NOT dst *)
      | OP_CopyNot		(* NOT src *)
      | OP_OrInverted		(* NOT src OR dst *)
      | OP_Nand			(* NOT src OR NOT dst *)
      | OP_Set			(* 1 *)

  (* gravity (both window and bit) *)
    datatype gravity
      = ForgetGravity		(* bit gravity only *)
      | UnmapGravity		(* window gravity only *)
      | NorthWestGravity
      | NorthGravity
      | NorthEastGravity
      | WestGravity
      | CenterGravity
      | EastGravity
      | SouthWestGravity
      | SouthGravity
      | SouthEastGravity
      | StaticGravity

  (* event masks *)
    datatype event_mask = XEVTMASK of word

  (* Value lists *)
    datatype value_mask = VALMASK of word
    datatype value_list = VALS of word option Array.array

  (* classes for QueryBestSize *)
    datatype bestsz_class
      = CursorShape		(* largest size that can be displayed *)
      | TileShape		(* size tiled fastest *)
      | StippleShape		(* size stippled fastest *)

  (* resource close-down modes *)
    datatype close_down_mode = DestroyAll | RetainPermanent | RetainTemporary

  (* X hosts *)
    datatype xhost
      = InternetHost of string
      | DECnetHost of string
      | ChaosHost of string

  (* image byte-orders and bitmap bit-orders *)
    datatype order = MSBFirst | LSBFirst

  (* image formats *)
    datatype image_format
      = XYBitmap	(* depth 1, XYFormat *)
      | XYPixmap	(* depth == drawable depth *)
      | ZPixmap		(* depth == drawable depth *)

    datatype pixmap_format = FORMAT of {
	depth : int,
	bits_per_pixel : int,
	scanline_pad : raw_format 
      }

    datatype display_class
      = StaticGray | GrayScale | StaticColor | PseudoColor | TrueColor | DirectColor

  (* this is a merging of the protocol types of depth and visuals *)
    datatype visual_depth
      = Depth of int		    (* a depth with no visuals *)
      | VisualDepth of {
	  id : visual_id,	    (* the associated visual id *)
	  depth : int,		    (* the depth *)
	  class : display_class,
	  cmap_entries : int,
	  bits_per_rgb : int,
	  red_mask : word,
	  green_mask : word,
	  blue_mask : word
	}

  end (* XProtTypes *)
