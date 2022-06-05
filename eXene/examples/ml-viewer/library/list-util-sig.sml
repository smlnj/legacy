(* list-util-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * General list utilities
 *)

signature LIST_UTIL = sig

  exception Zip
  exception Split

  val find : ('a -> bool) -> 'a list -> 'a list
    (* Given a filter function and a list,
     * finds all accepted elements in the list.
     *)

  val findOne : ('a -> bool) -> 'a list -> 'a option
    (* Given a filter function and a list,
     * finds the first accepted element in the list,
     * or NONE.
     *)

  val remove : ('a -> bool) -> 'a list -> 'a list
    (* remove pred l = find (not o pred) l
     *)

  val removeOne : ('a -> bool) -> 'a list -> 'a list
    (* Given a filter function and a list,
     * returns the list of all elements in the list
     * except the first accepted element.
     *)

  val filter : ('a -> 'b option) -> 'a list -> 'b list
    (* maps the given function across the list, discarding the NONEs *)

  val splitp : ('a -> bool) -> 'a list -> ('a list * 'a list)
    (* Given a filter function and a list,
     * returns the sublist of all elements
     * before the first accepted element, plus the sublist of 
     * from the first accepted element until the end.
     *)

  val prefix : ('a -> bool) -> 'a list -> 'a list
  val suffix : ('a -> bool) -> 'a list -> 'a list
    (* prefix pred l = #1(splitp pred l)
     * suffix pred l = case #2(splitp pred l) of [] => [] | a::tl => tl
     *)

  val split : int -> 'a list -> ('a list * 'a list)
    (* split n l = (l1,l2) where l = l1@l2 and length l1 = n
     * Raises BadArg if n < 0; raises Split if length l < n
     *)

  val flatten : 'a list list -> 'a list
    (* Given a list [l1, l2, ..., ln] of lists of 'a,
     * return l1 @ l2 @ ... @ ln
     *)

  val zip : ('a list * 'b list) -> ('a * 'b) list
  val unzip : ('a * 'b) list -> ('a list * 'b list)
    (* Zip and unzip two lists; raise Zip if lists of unequal length.
     *)

  val fromTo : (int * int) -> int list
    (* fromto (lo,hi) generates list of ints from a to b
     * Raises BadArg if b < a
     *)

  val genList : (int * (int -> 'a)) -> 'a list
    (* genList (cnt,genfn) generates the list 
     * [genfn 0, genfn 1, ... , genfn(cnt-1)]
     * Raises BadArg if cnt < 0.
     *)

end (* LIST_UTIL *)

