(* export.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

structure Export =
  struct

    fun export () =
	  RunCML.exportFn("basicwin", BasicWin.main, SOME(Time.fromMilliseconds 20));

  end

