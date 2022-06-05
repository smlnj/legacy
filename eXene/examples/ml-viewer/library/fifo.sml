(* fifo.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Applicative fifos
 *
 *)

structure Fifo : FIFO =
  struct
    datatype 'a fifo = Q of {front: 'a list, rear: 'a list}

    exception Dequeue

    val empty = Q{front=[],rear=[]}

    fun isEmpty (Q{front=[],rear=[]}) = true
      | isEmpty _ = false

    fun enqueue (Q{front,rear},x) = Q{front=front,rear=(x::rear)}

    fun dequeue (Q{front=(hd::tl),rear}) = (Q{front=tl,rear=rear},hd)
      | dequeue (Q{rear=[],...}) = raise Dequeue
      | dequeue (Q{rear,...}) = dequeue(Q{front=rev rear,rear=[]})

    fun head (Q{front=(hd::_),...}) = hd
      | head (Q{rear=[],...}) = raise Dequeue
      | head (Q{rear,...}) = hd(rev rear)

    fun length (Q {rear,front}) = (List.length rear) + (List.length front)

    fun contents (Q {rear, front}) = (front @ (rev rear))

    fun app f (Q{front,rear}) = (List.app f front; List.revapp f rear)
    fun revapp f (Q{front,rear}) = (List.app f rear; List.revapp f front)
    fun map f (Q{front,rear}) = 
          Q{front = List.map f front, rear = rev(List.map f(rev rear))}
    fun fold f b (Q{front,rear}) = List.fold f front (List.revfold f rear b)
    fun revfold f b (Q{front,rear}) = List.fold f rear (List.revfold f front b)

  end

