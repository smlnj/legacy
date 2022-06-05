(* exene-base.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * These are the basic types and operations supported by eXene.
 *)

signature EXENE_BASE =
  sig

    val version : {major : int, minor : int, rev : int, date : string}
    val versionName : string

    structure G : GEOMETRY

  (** opaque types **)
    type display
    type screen
    type window
    type font
    type pixmap
    type tile
    type cursor
    eqtype std_cursor
    eqtype atom
    type color

  (* server time *)
    structure XTime : sig
    type time
    val toReal : time -> real
    val +  : (time * time) -> time
    val -  : (time * time) -> time
    val <  : (time * time) -> bool
    val <= : (time * time) -> bool
    val >  : (time * time) -> bool
    val >= : (time * time) -> bool
      end

  (* authentication information *)
    datatype authentication = AUTH of {
    family : int,
    addr : string,
    dpy : string,
    name : string,
    data : Word8Vector.vector
      }

  (** identity tests **)
    val sameDisplay : display * display -> bool
    val sameScreen  : screen * screen -> bool
    val sameWindow  : window * window -> bool
    val sameFont    : font * font -> bool
    val samePixmap  : pixmap * pixmap -> bool
    val sameTile    : tile * tile -> bool
    val sameCursor  : cursor * cursor -> bool
    val sameColor   : color * color -> bool


  (** Display operations **)
    exception BadAddr of string
    val openDisplay : (string * authentication option) -> display
    val closeDisplay : display -> unit
    val defaultScreenOf : display -> screen
    val screensOf : display -> screen list
    val ringBell : display -> int -> unit
    val maxReqLen : display -> int


  (** Screen operations **)
    val displayOfScr : screen -> display
    val sizeOfScr    : screen -> G.size
    val sizeMMOfScr  : screen -> G.size
    val depthOfScr   : screen -> int
    
  
    datatype display_class
      = StaticGray | GrayScale | StaticColor | PseudoColor | TrueColor | DirectColor

    val displayClassOfScr : screen -> display_class


  (** window, pixmap and tile geometry functions **)
    val depthOfWin    : window -> int
    val depthOfPixmap : pixmap -> int
    val depthOfTile   : tile -> int
    val sizeOfWin     : window -> G.size
    val sizeOfPixmap  : pixmap -> G.size
    val sizeOfTile    : tile -> G.size
    val geomOfWin     : window
      -> {pos : G.point, sz : G.size, depth : int, border : int}
    val geomOfPixmap  : pixmap
      -> {pos : G.point, sz : G.size, depth : int, border : int}
    val geomOfTile    : tile
      -> {pos : G.point, sz : G.size, depth : int, border : int}


  (** Images **)
    datatype image = IMAGE of {
    sz : G.size,
    data : Word8Vector.vector list list
      }

    exception BadImageData

    val imageFromAscii : int * string list list -> image


  (** Pixmap and tile operations **)
    exception BadPixmapParameter

    val createPixmap : screen -> G.size * int -> pixmap
    val createPixmapFromAsciiData : screen -> int * string list list -> pixmap
    val createPixmapFromImage     : screen -> image -> pixmap
    val destroyPixmap : pixmap -> unit

    val createTileFromAsciiData : screen -> int * string list list -> tile
    val createTileFromImage     : screen -> image -> tile
    val createTileFromPixmap    : pixmap -> tile

    val createImageFromPixmap     : pixmap -> image
    val createImageFromTile       : tile -> image


  (** window hash tables **)
    type 'a window_map
    exception WindowNotFound
    val newMap : unit -> '1a window_map
    val insert : 'a window_map -> (window * 'a) -> unit
    val lookup : 'a window_map -> window -> 'a
    val remove : 'a window_map -> window -> 'a
    val list : 'a window_map -> 'a list


  (** Colors **)
    exception BadRGB
    exception NoColorCell

    datatype color_spec
      = CMS_Name of string
      | CMS_RGB of {red : word, green : word, blue : word}

    val white : color_spec
    val black : color_spec

    val color0 : color
    val color1 : color

    val colorOfScr : screen -> color_spec -> color
    val blackOfScr : screen -> color
    val whiteOfScr : screen -> color

(** NOTE: the following should go away soon, but we need it for recolorCursor **)
(** Maybe not; see rgbOfColor function **)

    datatype rgb = RGB of {red : word, green : word, blue : word}

    val rgbOfColor : color -> rgb

  (** Cursors **)
    val stdCursor : display -> std_cursor -> cursor
    val recolorCursor : {cursor : cursor, fore_rgb : rgb, back_rgb : rgb} -> unit
    val changeActiveGrabCursor : display -> cursor -> unit


  (* gravity (both window and bit) *)
    datatype gravity
      = ForgetGravity       (* bit gravity only *)
      | UnmapGravity        (* window gravity only *)
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

  end (* EXENE_BASE *)
