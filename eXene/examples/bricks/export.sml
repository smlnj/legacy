(* export.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

structure Export =
  struct

    fun export () =
	  RunCML.exportFn("bricks", BadBricks.main, SOME(Time.fromMilliseconds 20));

  end

