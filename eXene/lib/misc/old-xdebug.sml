(* xdebug.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * NOTE: this should be modified to use CIO streams directly.
 *)

structure XDebug =
  struct

(*****
    val debugStream = ref CIO.std_out
*****)

    val debug = ref 1
    val listLen = ref 16
    val lineLen = ref 20

(*****
    local
      open CML RunCML

      val debugCh : string chan = channel()
      val killCh : unit chan = channel()

      fun mkServer () = let
	    val evt = choose [
		  wrap (receive debugCh, fn s => (CIO.output(!debugStream, s); true)),
		  wrap (receive killCh, fn () => false)]
	    fun loop () = if (sync evt) then loop() else ()
	    in
	      spawn loop;
	      send (debugCh, "START\n")
	    end

      fun killServer () = (send(debugCh, (2, "END\n")); send(killCh, ()))

      val _ = logServer("XDebug:printer", mkServer, killServer)
    in
******)

(******
    fun pr s = if !debug then send(debugCh, s) else ()
******)
    fun pr lvl s = if (!debug >= lvl) then (CIO.print s) else ()

    val pr1 = pr 1
    val pr2 = pr 2
    val pr3 = pr 3
    val pr4 = pr 4

    fun prBuf lvl s = let
	  val pr = pr lvl
	  fun f (i, 1, 0) = (pr "\n  "; pr(makestring(ordof(s, i))))
	    | f (i, 1, _) = pr(makestring(ordof(s, i)))
	    | f (i, n, 0) = (pr "\n  "; f (i, n, !lineLen))
	    | f (i, n, k) = (
		pr(makestring(ordof(s, i)));
		pr ", ";
		f(i+1, n-1, k-1))
	  val n = String.size s
	  in
	    pr "[ ";
	    if (n <= !listLen)
	      then (f(0, n, !lineLen); pr " ]\n")
	      else (f(0, !listLen, !lineLen); pr " ...]\n")
	  end

    fun xspawn (name, f) = let
	  open CML
	  fun wrapf () = (
		pr2 (implode["xspawn ", name, " ", tidToString(getTid()), "\n"]);
		f ())
		  handle (MLXError.XERROR s) => pr1 (implode[
			    "exception (XERROR ", s, ") in ", name, " thread\n"])
		       | ex => pr1 (implode[
			    "exception ", System.exn_name ex, " in ", name,
			    " thread\n"])

	  in
	    spawn wrapf
	  end

    fun diag (f, s) x = (f x) handle ex => (
	  pr1 (implode["exception ", System.exn_name ex, " in ", s, "\n"]);
	  raise ex)

(******
    end (* local *)
******)
  end (* XDebug *)
