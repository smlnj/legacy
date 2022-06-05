structure Sim : SIM = struct

    structure V = struct

	type vect = real * real

	val zero = (0.0, 0.0)
	fun add ((x1, y1): vect, (x2, y2)) = (x1 + x2, y1 + y2)
	fun sub ((x1, y1): vect, (x2, y2)) = (x1 - x2, y1 - y2)
	fun neg ((x, y): vect) = (~x, ~y)
	fun scmul (c, (x, y): vect) = (c * x, c * y)
	fun sqnorm ((x, y): vect) = x * x + y * y
	fun proj2d ((x, y): vect) = { x = x, y = y }
    end

    type vect = V.vect

    type 'a body = { p: vect, v: vect, m: real, data: 'a }

    datatype 'a msg =
	NEW_DT of real
      | NEW_N of int
      | NEW_BODY of 'a body
      | QUERY of 'a body list CML.chan
      | STOP

    fun run { G, bodies, dt, msgchan, n } = let
	fun accels [] = []
	  | accels [_] = [V.zero]
	  | accels ({ p = ph, v = vh, m = mh, data } :: tl) = let
		fun addh (b, ab, (ah, atl)) = let
		    val { p = pb, v = vb, m = mb, data } = b
		    val distv = V.sub (pb, ph)
		    val dist2 = V.sqnorm distv
		    val dist = Math.sqrt dist2
		    val geom = G / (dist2 * dist)
		    val ah = V.add (ah, V.scmul (geom * mb, distv))
		    val ab = V.sub (ab, V.scmul (geom * mh, distv))
		in
		    (ah, ab :: atl)
		end
		val (ah, atl) =
		    ListPair.foldr addh (V.zero, []) (tl, accels tl)
	    in
		ah :: atl
	    end

	fun step (bl, dt) = let
	    fun oneBody ({ p, v, m, data }, a) =
		{ p = V.add (p, V.scmul (dt, v)),
		  (* p = V.add (p, V.add (V.scmul (dt, v),
				       V.scmul (0.5 * dt * dt, a))), *)
		  v = V.add (v, V.scmul (dt, a)), m = m, data = data }
	in
	    ListPair.map oneBody (bl, accels bl)
	end

	fun steps n (bl, dt) = let
	    fun loop (0, bl) = bl
	      | loop (n, bl) = loop (n - 1, step (bl, dt))
	in
	    loop (n, bl)
	end

	val always = CML.alwaysEvt ()

	fun loop (bl, dt, n) =
	    CML.select [CML.wrap (CML.recvEvt msgchan, handleMsg (bl, dt, n)),
		 	CML.wrap (always,
				  fn () => loop (steps n (bl, dt), dt, n))]

	and handleMsg (bl, dt, n) m =
	    (case m of
		 NEW_DT dt => loop (bl, dt, n)
	       | NEW_N n => loop (bl, dt, n)
	       | NEW_BODY b => loop (b :: bl, dt, n)
	       | QUERY c => (CML.send (c, bl); loop (bl, dt, n))
	       | STOP => ())
    in
	CML.spawn (fn () => loop (bodies, dt, n))
    end
end