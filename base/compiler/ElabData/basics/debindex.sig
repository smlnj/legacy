(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* debindex.sig *)

(* I moved this into the elaborator library.  It may be moved
 * back to FLINT if the elaborator gets "cleaned up", i.e., if
 * it is made to be unaware of such middle-end specifics.
 * (08/2001 Blume) *)

signature DEB_INDEX =
sig
  eqtype depth
  eqtype index

  val top  : depth
  val next : depth -> depth
  val prev : depth -> depth
  val eq   : depth * depth -> bool
  val getIndex : depth * depth -> index
  val cmp  : depth * depth -> order

  val dp_print : depth -> string
  val dp_key : depth -> int
  val dp_toint: depth -> int
  val dp_fromint: int -> depth

  val di_print : index -> string
  val di_key : index -> int
  val di_toint: index -> int
  val di_fromint: int -> index

  val innermost : index
  val innersnd : index
  val di_inner : index -> index

end (* signature DEB_INDEX *)
