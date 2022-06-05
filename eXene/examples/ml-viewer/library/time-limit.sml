(* time-limit.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

structure TimeLimit : sig
    exception TimeOut
    val timeLimit : System.Timer.time -> ('a -> 'b) -> 'a -> 'b
  end = struct

    exception TimeOut

    fun timeLimit t f x = let
	  val setitimer = System.Unsafe.CInterface.setitimer
	  val zeroTime = System.Timer.TIME{sec=0, usec=0}
	  fun timerOn () = setitimer (0, t, t)
	  fun timerOff () = setitimer (0, zeroTime, zeroTime)
	  val escapeCont = callcc (fn k => (
		callcc (fn k' => (throw k k'));
		timerOff();
		raise TimeOut))
	  fun handler _ = escapeCont
	  in
	    System.Signals.setHandler (System.Signals.SIGALRM, SOME handler);
	    timerOn();
	    ((f x) handle ex => (timerOff(); raise ex))
	      before timerOff()
	  end

  end; (* TimeLimit *)
