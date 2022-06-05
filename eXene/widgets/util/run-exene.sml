(* run-exene.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This structure provides a higher-level interface to invoking applications.
 * Users may set the shell variable "DISPLAY" to specify the display connection.
 *)

structure RunEXene : sig

    val run : (Widget.root -> unit) -> unit

    type options = {
	dpy : string option,		(* specify the display to connect to *)
	timeq : LargeInt.int option	(* specify the CML time quantum in ms. *)
      }

    val runWArgs : (Widget.root -> unit) -> options -> unit

  end = struct

    structure W = Widget
    structure EXB = EXeneBase

    fun mkRoot optDpy = let
	  val (dpy, auth) = GetDpy.getDpy optDpy
	  in
            Widget.mkRoot (dpy, auth)
	      handle (EXeneBase.BadAddr s) => (
	        TextIO.output (TextIO.stdErr, String.concat[
		    "eXene: unable to open display \"", dpy, "\"\n",
		    "  ", s, "\n"
		  ]);
	        RunCML.shutdown OS.Process.failure)
	  end

  (* the default time quantum *)
    val defaultTimeQ = Time.fromMilliseconds 20 (* ms *)

    fun run doit = let
	  fun runIt () = doit (mkRoot NONE)
	  in
	    ignore(RunCML.doit (runIt, SOME defaultTimeQ))
	  end

    type options = {
	dpy : string option,		(* specify the display to connect to *)
	timeq : LargeInt.int option	(* specify the CML time quantum in ms. *)
      }

    fun runWArgs doit (opts : options) = let
	  fun runIt () = doit (mkRoot (#dpy opts))
	  val timeQ = (case (#timeq opts)
		 of NONE => defaultTimeQ
		  | (SOME ms) => if (ms <= 0) then defaultTimeQ 
                                 else Time.fromMilliseconds ms
		(* end case *))
	  in
	    ignore (RunCML.doit (runIt, SOME timeQ))
	  end

  end; (* RunEXene *)
