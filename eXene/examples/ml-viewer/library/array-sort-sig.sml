(* array-sort-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Signature for in-place sorting of arrays
 *
 *)

signature ARRAY_SORT =
  sig

    val sort : ('a * 'a -> LibBase.relation) -> 'a Array.array -> 'a Array.array
    val sorted : ('a * 'a -> LibBase.relation) -> 'a Array.array -> bool

  end (* ARRAY_SORT *)

