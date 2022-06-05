(* iterate-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 *)

signature ITERATE =
  sig

    val iterate : ('a -> 'a) -> int -> 'a -> 'a
     (* iterate f cnt init = f(f(...f(f(init))...)) (cnt times)
      * iterate f 0 init = init
      * raises BadArg of int if cnt < 0
      *)

    val repeat : (int * 'a -> 'a) -> int -> 'a -> 'a
     (* repeat f cnt init 
      *     = #2(iterate (fn (i,v) => (i+1,f(i,v))) cnt (0,init))
      *)

  end (* ITERATE *)
