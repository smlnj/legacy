(* buffer.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This is an implementation unbounded buffered channels.  Send operations never
 * block, but accept/receive operations may.
 * Caveat: since the buffer is unbounded it is possible for the buffer to consume
 * excessive amounts of space, if the number of send operations greatly exceeds
 * the number of accepts over some time period.
 *)

signature BUFFER_CHAN =
  sig
    structure CML : CONCUR_ML
    type 'a buffer_chan
    val buffer : unit -> '1a buffer_chan
    val bufferIn : '1a CML.chan -> '1a buffer_chan
    val bufferOut : '1a CML.chan -> '1a buffer_chan
    val bufferSend : ('a buffer_chan * 'a) -> unit
    val bufferAccept : 'a buffer_chan -> 'a
    val bufferReceive : 'a buffer_chan -> 'a CML.event
  end (* BUFFER_CHAN *)

functor BufferChan (CML : CONCUR_ML) : BUFFER_CHAN =
  struct
    structure CML = CML

    open CML

    datatype 'a buffer_chan = BC of {inch : 'a chan, outch : 'a chan}

    local
(** We need the ('a chan) type constraint here because of a SML/NJ compiler bug *)
      fun mkBuffer (inCh, outCh : 'a chan) = let
	    val inEvt = receive inCh
	    fun loop ([], []) = loop([accept inCh], [])
	      | loop (front as (x::r), rear) = sync (choose [
		    wrap (inEvt, fn y => loop(front, y::rear)),
		    wrap (transmit(outCh, x), fn () => loop(r, rear))
		  ])
	      | loop ([], rear) = loop(List.rev rear, [])
	    in
	      spawn (fn () => loop([], []));
	      BC{inch=inCh, outch=outCh}
	    end
    in
    fun buffer () = mkBuffer(channel(), channel())
    fun bufferIn inCh = mkBuffer(inCh, channel())
    fun bufferOut outCh = mkBuffer(channel(), outCh)
    end (* local *)

    fun bufferSend (BC{inch, ...}, x) = send(inch, x)
    fun bufferAccept (BC{outch, ...}) = accept outch
    fun bufferReceive (BC{outch, ...}) = receive outch

  end (* functor BufferChan *)
