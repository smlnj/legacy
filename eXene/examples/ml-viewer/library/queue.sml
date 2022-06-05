(* queue.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Imperative fifos
 *
 *)

structure Queue : QUEUE =
  struct
    type '1a queue = '1a Fifo.fifo ref

    exception Dequeue = Fifo.Dequeue

    fun mkQueue () = ref Fifo.empty

    fun enqueue (q,x) = q := (Fifo.enqueue (!q, x))

    fun dequeue q = let 
          val (newq, x) = Fifo.dequeue (!q) 
          in
            q := newq;
            x
          end
  
    fun head q = Fifo.head (!q)
    fun isEmpty q = Fifo.isEmpty (!q)
    fun length q = Fifo.length (!q)
    fun contents q = Fifo.contents (!q)
    fun app f q = Fifo.app f (!q)
    fun revapp f q = Fifo.revapp f (!q)
    fun fold f b q = Fifo.fold f b (!q)
    fun revfold f b q = Fifo.revfold f b (!q)
    fun map f q = ref(Fifo.map f (!q))

  end
