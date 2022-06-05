(* bsp2d-sig.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * The interface to the binary space partition functor for 2 dimesions.
 *)

signature BSP_2D =
  sig

    type object
	(* the type of objects in the space *)

    type bsp
	(* the partioning of space *)

    type box = {minX : real, minY : real, maxX : real, maxY : real}

    val partitionSpace : int -> (box * object list) -> bsp
	(* Given an integer n, and a box and list of non-overlapping objects
	 * contained in  the box, return a partition of the box, such that no
	 * leaf box contains more than n objects.  Note, the partition is
	 * really based on the bounding boxes of the objects.
	 *)

    val pickObject : (bsp * real * real) -> object list
	(* using the partition of space, find all of te objects that contain
	 * the point.
	 *)

  end;

