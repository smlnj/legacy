(* acc.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * The accumulator of the calculator.
 *)

signature ACC =
  sig

    datatype op_t = Plus | Minus | Divide | Times
    datatype acc_msg = Op of op_t | Clear | Equal | Val of int
    datatype out_val = OVal of int | OInfinity | OOverflow

    type acc

    val mkAcc : unit -> acc
    val sendAcc : acc -> acc_msg -> unit
    val evtOf : acc -> out_val CML.event

  end (* ACC *)

structure Acc : ACC =
  struct

    datatype op_t = Plus | Minus | Divide | Times
    datatype acc_msg = Op of op_t | Clear | Equal | Val of int
    datatype out_val = OVal of int | OInfinity | OOverflow

    datatype acc = Acc of (acc_msg CML.chan * out_val CML.chan)

    fun ratorOf Plus = Int.+
      | ratorOf Minus = Int.-
      | ratorOf Times = Int.*
      | ratorOf Divide = Int.div

    fun mkAcc () = let
	  val msg_chan = CML.channel()
	  val val_chan = CML.channel()
	  fun get_msg () = CML.recv msg_chan
	  fun put_val v = CML.send(val_chan,OVal v)
	  fun put_inf () = CML.send(val_chan, OInfinity)
	  fun put_ovfl () = CML.send(val_chan, OOverflow)

	  fun update (v,v') = let
		val newval = 10*v + v'
		in
		  put_val newval;
		  newval
		end
		  handle Overflow => v
	  fun doErr Div = put_inf ()
	    | doErr Overflow = put_ovfl ()
	    | doErr a = raise a
	  fun initState () = (case (get_msg ())
		 of Op _ => initState ()
		  | Clear => doClear ()
		  | Equal => initState ()
		  | Val v => (put_val v; readNum (SOME v, NONE))
		(* end case *))
	  and readNum arg = (case (get_msg ())
		 of (Op rator') => (case arg
		       of (NONE,NONE) => initState ()   (* impossible *)
			| (NONE,SOME (st, rator)) => readNum (NONE, SOME (st, rator'))
			| (SOME v, NONE) => readNum (NONE, SOME (v, rator'))
			| (SOME v, SOME (st, rator)) => let
			    val newval = (ratorOf rator) (st, v)
			    in
			      put_val newval;
			      readNum(NONE, SOME (newval, rator')) 
			    end
			      handle err => (doErr err; initState ())
		      (* end case *))
		  | Clear => doClear ()
		  | Equal => doEqual arg
		  | Val v' => (case arg
		       of (NONE, st) => (put_val v'; readNum (SOME v', st))
			| (SOME v, st) => readNum (SOME(update (v,v')), st)
		      (* end case *))
		(* end case *))
	  and doClear () = (put_val 0;initState())
	  and doEqual (SOME v, SOME (st, rator)) = (
		(put_val ((ratorOf rator) (st, v))) handle err => doErr err;
		initState())
	    | doEqual _ = initState()
      
	  in
	    CML.spawn initState;
	    Acc(msg_chan, val_chan)
	  end (* mkAcc *)

    fun sendAcc (Acc(msg_chan, _)) msg = CML.send (msg_chan, msg)

    fun evtOf (Acc(_, val_chan)) = CML.recvEvt val_chan

  end (* Acc *)

