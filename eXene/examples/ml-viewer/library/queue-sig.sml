(* queue-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Imperative fifos
 *
 *)

signature QUEUE =
  sig
    type '1a queue

    exception Dequeue

    val mkQueue : unit -> '1a queue
    val isEmpty : 'a queue -> bool
    val enqueue : 'a queue * 'a -> unit
    val dequeue : 'a queue -> 'a
    val head : 'a queue -> 'a
    val length : 'a queue -> int
    val contents : 'a queue -> 'a list
    val app : ('a -> 'b) -> 'a queue -> unit
    val revapp : ('a -> 'b) -> 'a queue -> unit
    val map : ('a -> '2b) -> 'a queue -> '2b queue
    val fold : ('a * 'b -> 'b) -> 'b -> 'a queue -> 'b
    val revfold : ('a * 'b -> 'b) -> 'b -> 'a queue -> 'b

  end
