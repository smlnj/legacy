signature SIM = sig

    structure V : sig
	type vect
	val proj2d: vect -> { x: real, y: real }
    end

    type vect = V.vect

    type 'a body = { p: vect, v: vect, m: real, data: 'a }

    datatype 'a msg =
	NEW_DT of real
      | NEW_N of int
      | NEW_BODY of 'a body
      | QUERY of 'a body list CML.chan
      | STOP

    val run:
	{ G: real, bodies: 'a body list, dt: real, n: int,
	  msgchan: 'a msg CML.chan }
	-> CML.thread_id
end
