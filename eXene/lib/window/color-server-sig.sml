(* color-server-sig.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * The color server manages colors for a given screen.
 *)

signature COLOR_SERVER =
  sig

  (* Color specifications.  Eventually, this will be extended to R5
   * device independent color specifications.
   *)
    datatype color_spec
      = CMS_Name of string
      | CMS_RGB of {red : word, green : word, blue : word}

  (* A color.  Note: this type is abstract at the user level *)
    datatype color = COLOR of {		(* A color in the colormap *)
	pixel : XProtTypes.pixel,	  (* Its pixel value *)
	rgb : XProtTypes.rgb		  (* Its RGB value *)
      }

    type color_server

    val mkColorServer : (XDisplay.xdisplay * XDisplay.xscreen) -> color_server

    exception BadRGB
    exception NoColorCell

    val getColor : color_server -> color_spec -> color
	(* Get the specified color.  This raises NoColorCell if the color
	 * map is already full.
	 *)

    val blackOf : color_server -> color
    val whiteOf : color_server -> color

    val sameColor : (color * color) -> bool

  (* The following are used for drawing 0s or 1s on pixmaps *)
    val color0 : color
    val color1 : color

  (* RGB values (these are needed for coloring cursors, but will probably go away) *)
    datatype rgb = RGB of {red : word, green : word, blue : word}

    val rgbOfColor : color -> rgb

    val chkRGB : rgb -> rgb

    val whiteRGB : rgb
    val blackRGB : rgb

  end (* COLOR_SERVER *)
