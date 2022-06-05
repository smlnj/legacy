(* ball.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure Ball =
  struct

    datatype ball_msg
      = KILL of Geometry.point
      | REDRAW of (int * Geometry.size)
      | KILL_ALL

    val updatesPerSec = 10

    local
      structure MChan = Multicast
      open Geometry BounceDM

    (* clip a point to keep a ball in the window.  If we hit a wall, then
     * we adjust the vector.  The clipped point should be computed
     * to lie on the vector, but for now we assume small vectors
     * and just truncate the coordinates. *)
      fun clip (ballRadius, SIZE{wid, ht}) = let
	    val maxX = wid - ballRadius and maxY = ht - ballRadius
	    fun clipCoord (coord : int, delta, minCoord, maxCoord) = 
		  if (coord <= minCoord)
		    then (minCoord, ~delta)
		  else if (coord >= maxCoord)
		    then (maxCoord, ~delta)
		    else (coord, delta)
	    fun clip' (PT{x=x0, y=y0}, PT{x=dx0, y=dy0}) = let
		  val (x1, dx1) = clipCoord(x0+dx0, dx0, ballRadius, maxX)
		  val (y1, dy1) = clipCoord(y0+dy0, dy0, ballRadius, maxY)
		  in
		    (PT{x=x1, y=y1}, PT{x=dx1, y=dy1})
		  end
	    in
	      clip'
	    end

      fun mkIconFn win = let
	    val ballIcons =
		  map (EXeneBase.createTileFromImage (EXeneWin.screenOfWin win))
		    Heads.headDataList
	    val n = List.length ballIcons
	    val ch = CML.channel()
	    fun loop i = if (i = n)
		  then loop 0
		  else (CML.send(ch, List.nth(ballIcons, i)); loop(i+1))
	    in
	      XDebug.xspawn("newIcon", fn () => loop 0);
	      (fn () => CML.recv ch)
	    end

      val delayEvt =
	    CML.timeOutEvt
		(Time.fromMicroseconds(Int32.fromInt
					   (1000000 div updatesPerSec)))
    in

    fun mkBallFn (win, mchan, drawCh) = let
	  val newIcon = mkIconFn win
	  fun newBall (seqn, pos, vec, sz) = let
		val ballIcon = newIcon()
		val ballRadius = let
		      val {sz=SIZE{wid, ...}, ...} = EXeneBase.geomOfTile ballIcon
		      in
			wid div 2
		      end
		val offset = PT{x=ballRadius, y=ballRadius}
		fun drawBall (n, pos) =
		      CML.send(drawCh, DrawBall(n, ballIcon, subPt(pos, offset)))
		fun moveBall (n, oldPos, newPos) = (
		      drawBall(n, oldPos); drawBall(n, newPos))
		val clipFn = clip (ballRadius, sz)
		fun ball (inEvt, pos, vec, clipFn) = let
		      fun loop (seqn, pos, vec, clipFn) = CML.select [
			      CML.wrap(delayEvt, fn () => let
				val (newPos, newVec) = clipFn(pos, vec)
				in
				  if (pos <> newPos)
				    then moveBall(seqn, pos, newPos)
				    else ();
				  loop(seqn, newPos, newVec, clipFn)
				end),
			      CML.wrap(inEvt,
				fn (KILL(PT{x, y})) => let
				      val deathZone = RECT{
					    x = x-ballRadius, y = y-ballRadius,
					    wid = 2*ballRadius, ht = 2*ballRadius}
				      in
					if within(pos, deathZone)
					  then drawBall(seqn, pos)
					  else loop(seqn, pos, vec, clipFn)
				      end
				 | (REDRAW(seqn', newSz)) => let
				      val clipFn = clip (ballRadius, newSz)
				      val (newPos, _) = clipFn(pos, PT{x=0, y=0})
				      in
					drawBall(seqn', pos);
					loop (seqn', newPos, vec, clipFn)
				      end
				 | KILL_ALL => drawBall(seqn, pos))
			    ]
		      in
			drawBall (seqn, pos);
			loop (seqn, pos, vec, clipFn)
		      end
		in
		  XDebug.xspawn (
		    "Ball",
		    fn () =>
		      ball(MChan.recvEvt(MChan.port mchan), pos, vec, clipFn));
		  ()
		end
	  in
	    newBall
	  end

    end (* local *)
  end (* Ball *)
