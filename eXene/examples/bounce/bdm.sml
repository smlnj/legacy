(* bounce-dm.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure BounceDM =
  struct
    datatype dm_msg
      = DrawBall of (int * EXeneBase.tile * Geometry.point)
      | EraseBall of (int * EXeneBase.tile * Geometry.point)
      | Redraw of int

  (* the bounce DM is actually responsible for drawing the heads *)
    fun bounceDM win = let
	  open CML Geometry EXeneBase EXeneWin
	  val drawCh = channel ()
	  val dpy = displayOfWin win
	  val pen = Drawing.newPen [
		  Drawing.PV_Function Drawing.OP_Xor,
		  Drawing.PV_Foreground(color1),
		  Drawing.PV_Background(color0)
		]
	  val drawable = Drawing.drawableOfWin win
	  fun redraw () = Drawing.clearDrawable drawable
	  fun draw (icon, pt) = let
		val {pos=PT{x, y}, sz=SIZE{wid, ht}, ...} = geomOfTile icon
		in
		  Drawing.textureBlt drawable pen {src = icon, dst_pos = pt}
		end
	  val evt = receive drawCh
	  fun loop1 seqn = (case (poll evt)
		 of NONE => (loop2 seqn)
		  | SOME(DrawBall(n, pm, pt)) => (
		      if (n = seqn) then draw(pm, pt) else (); loop1 seqn)
		  | SOME(EraseBall(n, pm, pt)) => (
		      if (n = seqn) then draw(pm, pt) else (); loop1 seqn)
		  | SOME(Redraw n) => (redraw(); loop1 n))
	  and loop2 seqn = (case (sync evt)
		 of (DrawBall(n, pm, pt)) => (if (n = seqn)
			then (draw(pm, pt); loop1 seqn) else loop2 seqn)
		  | (EraseBall(n, pm, pt)) => (if (n = seqn)
			then (draw(pm, pt); loop1 seqn) else loop2 seqn)
		  | (Redraw n) => (redraw(); loop2 n))
	  in
	    XDebug.xspawn("bounceDM", fn () => loop2 0);
	    drawCh
	  end (* bounceDM *)

  end (* BounceDM *)
