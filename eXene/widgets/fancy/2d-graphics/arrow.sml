(* arrow.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * skeleton code for arrow heads.
 *)

structure Arrow : sig

    val mkArrow : {
	    hd : {x:real, y:real}, tl : {x:real, y:real},
	    len : real, wid : real
	  } -> {
	    p0 : {x:real, y:real}, p1 : {x:real, y:real}, p2 : {x:real, y:real}
	  }

  end = struct

    val pi = Math.pi
    val halfPi = 0.5 * pi

    fun mkArrow {len, wid, hd as {x=x1, y=y1}, tl={x=x2, y=y2}} = let
	  val dx = x2 - x1 and dy = y2 - y1
	  val theta = Math.atan2(dy, dx)
	  val cosTheta = Math.cos theta and sinTheta = Math.sin theta
	  val alct = len * cosTheta and alst = len * sinTheta
	  val awct = wid * cosTheta and awst = wid * sinTheta
	  in
	    { p0 = {x=x1 + (alct + awst), y=y1 + (alst - awct)},
	      p1 = hd,
	      p2 = {x=x1 + (alct - awst), y=y1 + (alst + awct)}
	    }
	  end

  end (* Arrow *)

