(* xshutdown.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This is the shutdown server for eXene.  Log open connections and close them on
 * shutdown.
 *)

structure XShutdown : sig

    val logConnection   : XIo.connection -> unit
    val unlogConnection : XIo.connection -> unit

  end = struct
    local
      datatype req
	= LogConn of XIo.connection
	| UnlogConn of XIo.connection
	| Shutdown

      val reqCh : req CML.chan = CML.channel()
      val replyCh : unit CML.chan = CML.channel()

      fun startServer () = let
	    fun loop conns = (case (CML.recv reqCh)
		 of LogConn arg => loop (arg::conns)
		  | UnlogConn conn => let
		      fun remove [] = []
			| remove (c :: r) =
			    if XIo.sameConn(c, conn) then r else (c :: (remove r))
		      in
			loop (remove conns)
		      end
		  | Shutdown => (
let val tid = CML.getTid() in
TraceCML.trace(XDebug.libTM, fn () => [CML.tidToString tid, " ***** shutdown *****\n"])
end;
		      app XIo.closeConn conns;
		      CML.send(replyCh, ())))
	    in
	      CML.spawn (fn () => loop []); ()
	    end

      fun shutdown () = (CML.send(reqCh, Shutdown); CML.recv replyCh)

      val _ = RunCML.logChannel("eXene-shutdown:reqCh", reqCh)
      val _ = RunCML.logChannel("eXene-shutdown:replyCh", replyCh)
      val _ = RunCML.logServer("eXene-shutdown", startServer, shutdown)

    in

    fun logConnection arg = CML.send(reqCh, LogConn arg)
    fun unlogConnection sock = CML.send(reqCh, UnlogConn sock)

    end (* local *)
  end (* XShutdown *)
