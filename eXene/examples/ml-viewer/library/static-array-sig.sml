(* static-array-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Generic array signature
 *
 *)

signature STATIC_ARRAY =
  sig
    type elem
    type array

    exception Subscript
    exception Size

    val array : int * elem -> array
    val sub : array * int -> elem
    val update : (array * int * elem) -> unit
    val length : array -> int
    val tabulate : int * (int -> elem) -> array
    val arrayoflist : elem list -> array

  end (* STATIC_ARRAY *)

