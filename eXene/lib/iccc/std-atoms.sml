(* std-atoms.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * These are the pre-defined X11 atoms (extracted from X11/Xatom.h)
 *)

structure StdAtoms =
  struct

      val atom_PRIMARY			= XProtTypes.XAtom 0w1
      val atom_SECONDARY		= XProtTypes.XAtom 0w2
      val atom_ARC			= XProtTypes.XAtom 0w3
      val atom_ATOM			= XProtTypes.XAtom 0w4
      val atom_BITMAP			= XProtTypes.XAtom 0w5
      val atom_CARDINAL			= XProtTypes.XAtom 0w6
      val atom_COLORMAP			= XProtTypes.XAtom 0w7
      val atom_CURSOR			= XProtTypes.XAtom 0w8
      val atom_CUT_BUFFER0		= XProtTypes.XAtom 0w9
      val atom_CUT_BUFFER1		= XProtTypes.XAtom 0w10
      val atom_CUT_BUFFER2		= XProtTypes.XAtom 0w11
      val atom_CUT_BUFFER3		= XProtTypes.XAtom 0w12
      val atom_CUT_BUFFER4		= XProtTypes.XAtom 0w13
      val atom_CUT_BUFFER5		= XProtTypes.XAtom 0w14
      val atom_CUT_BUFFER6		= XProtTypes.XAtom 0w15
      val atom_CUT_BUFFER7		= XProtTypes.XAtom 0w16
      val atom_DRAWABLE			= XProtTypes.XAtom 0w17
      val atom_FONT			= XProtTypes.XAtom 0w18
      val atom_INTEGER			= XProtTypes.XAtom 0w19
      val atom_PIXMAP			= XProtTypes.XAtom 0w20
      val atom_POINT			= XProtTypes.XAtom 0w21
      val atom_RECTANGLE		= XProtTypes.XAtom 0w22
      val atom_RESOURCE_MANAGER		= XProtTypes.XAtom 0w23
      val atom_RGB_COLOR_MAP		= XProtTypes.XAtom 0w24
      val atom_RGB_BEST_MAP		= XProtTypes.XAtom 0w25
      val atom_RGB_BLUE_MAP		= XProtTypes.XAtom 0w26
      val atom_RGB_DEFAULT_MAP		= XProtTypes.XAtom 0w27
      val atom_RGB_GRAY_MAP		= XProtTypes.XAtom 0w28
      val atom_RGB_GREEN_MAP		= XProtTypes.XAtom 0w29
      val atom_RGB_RED_MAP		= XProtTypes.XAtom 0w30
      val atom_STRING			= XProtTypes.XAtom 0w31
      val atom_VISUALID			= XProtTypes.XAtom 0w32
      val atom_WINDOW			= XProtTypes.XAtom 0w33
      val atom_WM_COMMAND		= XProtTypes.XAtom 0w34
      val atom_WM_HINTS			= XProtTypes.XAtom 0w35
      val atom_WM_CLIENT_MACHINE	= XProtTypes.XAtom 0w36
      val atom_WM_ICON_NAME		= XProtTypes.XAtom 0w37
      val atom_WM_ICON_SIZE		= XProtTypes.XAtom 0w38
      val atom_WM_NAME			= XProtTypes.XAtom 0w39
      val atom_WM_NORMAL_HINTS		= XProtTypes.XAtom 0w40
      val atom_WM_SIZE_HINTS		= XProtTypes.XAtom 0w41
      val atom_WM_ZOOM_HINTS		= XProtTypes.XAtom 0w42
      val atom_MIN_SPACE		= XProtTypes.XAtom 0w43
      val atom_NORM_SPACE		= XProtTypes.XAtom 0w44
      val atom_MAX_SPACE		= XProtTypes.XAtom 0w45
      val atom_END_SPACE		= XProtTypes.XAtom 0w46
      val atom_SUPERSCRIPT_X		= XProtTypes.XAtom 0w47
      val atom_SUPERSCRIPT_Y		= XProtTypes.XAtom 0w48
      val atom_SUBSCRIPT_X		= XProtTypes.XAtom 0w49
      val atom_SUBSCRIPT_Y		= XProtTypes.XAtom 0w50
      val atom_UNDERLINE_POSITION	= XProtTypes.XAtom 0w51
      val atom_UNDERLINE_THICKNESS	= XProtTypes.XAtom 0w52
      val atom_STRIKEOUT_ASCENT		= XProtTypes.XAtom 0w53
      val atom_STRIKEOUT_DESCENT	= XProtTypes.XAtom 0w54
      val atom_ITALIC_ANGLE		= XProtTypes.XAtom 0w55
      val atom_X_HEIGHT			= XProtTypes.XAtom 0w56
      val atom_QUAD_WIDTH		= XProtTypes.XAtom 0w57
      val atom_WEIGHT			= XProtTypes.XAtom 0w58
      val atom_POINT_SIZE		= XProtTypes.XAtom 0w59
      val atom_RESOLUTION		= XProtTypes.XAtom 0w60
      val atom_COPYRIGHT		= XProtTypes.XAtom 0w61
      val atom_NOTICE			= XProtTypes.XAtom 0w62
      val atom_FONT_NAME		= XProtTypes.XAtom 0w63
      val atom_FAMILY_NAME		= XProtTypes.XAtom 0w64
      val atom_FULL_NAME		= XProtTypes.XAtom 0w65
      val atom_CAP_HEIGHT		= XProtTypes.XAtom 0w66
      val atom_WM_CLASS			= XProtTypes.XAtom 0w67
      val atom_WM_TRANSIENT_FOR		= XProtTypes.XAtom 0w68

  end; (* StdAtoms *)
