(* cursor.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure Cursor =
  struct

  (* The names of the standard cursors (from X11/cursorfont.h) *)
    datatype std_cursor = STD_CURSOR of int
    val x_cursor		= STD_CURSOR 0
    val arrow			= STD_CURSOR 2
    val based_arrow_down	= STD_CURSOR 4
    val based_arrow_up		= STD_CURSOR 6
    val boat			= STD_CURSOR 8
    val bogosity		= STD_CURSOR 10
    val bottom_left_corner	= STD_CURSOR 12
    val bottom_right_corner	= STD_CURSOR 14
    val bottom_side		= STD_CURSOR 16
    val bottom_tee		= STD_CURSOR 18
    val box_spiral		= STD_CURSOR 20
    val center_ptr		= STD_CURSOR 22
    val circle			= STD_CURSOR 24
    val clock			= STD_CURSOR 26
    val coffee_mug		= STD_CURSOR 28
    val cross			= STD_CURSOR 30
    val cross_reverse		= STD_CURSOR 32
    val crosshair		= STD_CURSOR 34
    val diamond_cross		= STD_CURSOR 36
    val dot			= STD_CURSOR 38
    val dotbox			= STD_CURSOR 40
    val double_arrow		= STD_CURSOR 42
    val draft_large		= STD_CURSOR 44
    val draft_small		= STD_CURSOR 46
    val draped_box		= STD_CURSOR 48
    val exchange		= STD_CURSOR 50
    val fleur			= STD_CURSOR 52
    val gobbler			= STD_CURSOR 54
    val gumby			= STD_CURSOR 56
    val hand1			= STD_CURSOR 58
    val hand2			= STD_CURSOR 60
    val heart			= STD_CURSOR 62
    val icon			= STD_CURSOR 64
    val iron_cross		= STD_CURSOR 66
    val left_ptr		= STD_CURSOR 68
    val left_side		= STD_CURSOR 70
    val left_tee		= STD_CURSOR 72
    val leftbutton		= STD_CURSOR 74
    val ll_angle		= STD_CURSOR 76
    val lr_angle		= STD_CURSOR 78
    val man			= STD_CURSOR 80
    val middlebutton		= STD_CURSOR 82
    val mouse			= STD_CURSOR 84
    val pencil			= STD_CURSOR 86
    val pirate			= STD_CURSOR 88
    val plus			= STD_CURSOR 90
    val question_arrow		= STD_CURSOR 92
    val right_ptr		= STD_CURSOR 94
    val right_side		= STD_CURSOR 96
    val right_tee		= STD_CURSOR 98
    val rightbutton		= STD_CURSOR 100
    val rtl_logo		= STD_CURSOR 102
    val sailboat		= STD_CURSOR 104
    val sb_down_arrow		= STD_CURSOR 106
    val sb_h_double_arrow	= STD_CURSOR 108
    val sb_left_arrow		= STD_CURSOR 110
    val sb_right_arrow		= STD_CURSOR 112
    val sb_up_arrow		= STD_CURSOR 114
    val sb_v_double_arrow	= STD_CURSOR 116
    val shuttle			= STD_CURSOR 118
    val sizing			= STD_CURSOR 120
    val spider			= STD_CURSOR 122
    val spraycan		= STD_CURSOR 124
    val star			= STD_CURSOR 126
    val target			= STD_CURSOR 128
    val tcross			= STD_CURSOR 130
    val top_left_arrow		= STD_CURSOR 132
    val top_left_corner		= STD_CURSOR 134
    val top_right_corner	= STD_CURSOR 136
    val top_side		= STD_CURSOR 138
    val top_tee			= STD_CURSOR 140
    val trek			= STD_CURSOR 142
    val ul_angle		= STD_CURSOR 144
    val umbrella		= STD_CURSOR 146
    val ur_angle		= STD_CURSOR 148
    val watch			= STD_CURSOR 150
    val xterm			= STD_CURSOR 152

    datatype cursor = CURSOR of {
	id : XProtTypes.cursor_id,
	dpy : Display.display
      }

  (* identity test *)
    fun sameCursor (CURSOR{id=id1, dpy=dpy1}, CURSOR{id=id2, dpy=dpy2}) =
	  ((id1 = id2) andalso Display.sameDisplay(dpy1, dpy2))

  (* return the named standard cursor *)
    fun stdCursor dpy (STD_CURSOR cursor) = let
	  open XDisplay Display
	  val DPY{xdpy=XDPY{nextXId, ...}, ...} = dpy
	  val FontBase.FONT{id=fontId, ...} = Display.openFont dpy "cursor"
	  val cursorId = nextXId()
	  in
	  (* The cursor font contains the shape glyph followed by the mask
	   * glyph; so character position 0 contains a shape, 1 the mask for 0,
	   * 2 a shape, etc.
	   *)
	    dpyRequest dpy (XRequest.encodeCreateGlyphCursor{
		cursor = cursorId,
		src_font = fontId,
		mask_font = SOME fontId,
		src_chr = cursor,
		mask_chr = cursor+1,
		fore_rgb = ColorServer.blackRGB,
		back_rgb = ColorServer.whiteRGB
	      });
	    CURSOR{id=cursorId, dpy=dpy}
	  end

  (* change the color of a cursor *)
    fun recolorCursor {cursor as CURSOR{id, dpy}, fore_rgb, back_rgb} =
	  Display.dpyRequest dpy (XRequest.encodeRecolorCursor {
	      cursor = id,
	      fore_color = ColorServer.chkRGB fore_rgb,
	      back_color = ColorServer.chkRGB back_rgb
	    })

  (* change the cursor of an active grab *)
    fun changeActiveGrabCursor dpy cursor =
	  Display.dpyRequest dpy (XRequest.encodeChangeActivePointerGrab {
	      evt_mask = XEventTypes.maskOfXEvtList [
		  XEventTypes.XEVT_PointerMotion,
		  XEventTypes.XEVT_ButtonPress,
		  XEventTypes.XEVT_ButtonRelease
		],
	      cursor = case cursor of (CURSOR{id, ...}) => SOME id,
	      time = XProtTypes.CurrentTime
	    })

  end (* Cursor *)
