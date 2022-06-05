(* geometry.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * The signature of the basic geometry types and operations.
 *)

signature GEOMETRY =
  sig

  (* geometric types (from Xlib.h) *)
    datatype point = PT of {x : int, y : int}
    datatype line = LINE of point * point
    datatype size = SIZE of {wid : int, ht : int}
    datatype rect = RECT of {x : int, y : int, wid : int, ht : int}
    datatype arc = ARC of {
	x : int, y : int,
	wid : int, ht : int,
	angle1 : int, angle2 : int
      }

  (* The geometry of a window w.r.t. its parent. *)
    datatype win_geom = WGEOM of {
	pos : point,
	sz : size,
	border : int
      }

  (* points *)
    val originPt : point
    val xCoordOfPt : point -> int
    val yCoordOfPt : point -> int
    val addPt : point * point -> point
    val subPt : point * point -> point
    val scalePt : int * point -> point
    val lessThanPt : point * point -> bool
    val lessEqPt : point * point -> bool

  (* sizes *)
    val addSz : size * size -> size
    val subSz : size * size -> size
    val scaleSz : int * size -> size
    val addSzToPt : point * size -> point
    val limitPt : (size * point) -> point

  (* rectangles *)
    val mkRect : (point * size) -> rect
    val originOfRect : rect -> point
    val sizeOfRect : rect -> size
    val originAndSzOfRect : rect -> (point * size)
    val cornerOfRect : rect -> point
    val clipPt : (rect * point) -> point
    val translate : rect * point -> rect
    val rtranslate : rect * point -> rect
    val intersect : rect * rect -> bool
    exception Intersection
    val intersection : rect * rect -> rect
    val union : rect * rect -> rect
    val within : point * rect -> bool
    val inside : rect * rect -> bool
    val boundBox : point list -> rect

  end (* GEOMETRY *)
