(* xio.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This code implements the low-level I/O of the X-protocol.
 *
 * NOTE: the implementation of close doesn't really work, since the socket may
 * end up being closed before the output buffer is actually flushed (race condition).
 *)

signature XIO =
  sig

    exception LostReply
    exception ErrorReply of XErrors.xerror

    type connection

    val openConn : ('a, Socket.active Socket.stream) Socket.sock -> connection
    val closeConn : connection -> unit

    val sameConn : (connection * connection) -> bool

    val request : connection -> Word8Vector.vector -> unit
    val requestAndChk : connection -> Word8Vector.vector -> unit CML.event
    val requestReply : connection -> Word8Vector.vector
	  -> Word8Vector.vector CML.event
    val requestMultiReply : connection
	  -> (Word8Vector.vector * (Word8Vector.vector -> int))
	    -> Word8Vector.vector CML.event
    val requestWithExposures : connection
	  -> (Word8Vector.vector * (unit -> Geometry.rect list) SyncVar.ivar)
	    -> unit

    val flushOut : connection -> unit

    val waitForXEvent : connection -> XEventTypes.xevent CML.event

    val readXError : connection -> (word * Word8Vector.vector)

  end

structure XIo : XIO =
  struct

    exception LostReply
    exception ErrorReply of XErrors.xerror

    structure W = Word
    structure W8A = Word8Array
    structure W8V = Word8Vector
    structure SV = SyncVar

    val emptyV = W8V.fromList[]

    fun newBuf sz = Word8Array.array(sz, 0w0)
    val bufSz = 2048

  (* time to wait before flushing a non-empty output buffer *)
    val flushTimeOut = CML.timeOutEvt(Time.fromMilliseconds 50)

  (* request messages sent to the sequencer by clients *)
    datatype req_msg
      = RequestFlush
      | RequestQuit
      | Request of W8V.vector
      | RequestAndChk of (W8V.vector * reply CML.chan)
      | RequestReply of (W8V.vector * reply CML.chan)
      | RequestReplies of (W8V.vector * reply CML.chan * (W8V.vector -> int))
      | RequestExposures of (W8V.vector * (unit -> Geometry.rect list) SV.ivar)

  (* replies from the sequencer to client requests *)
    and reply
      = ReplyLost		    (* The reply was lost somewhere in transit *)
      | Reply of W8V.vector	    (* A normal reply *)
      | ReplyError of W8V.vector    (* The server returned an error message *)

  (* messages from the sequencer to the output buffer *)
    datatype out_msg
      = OutFlush
      | OutQuit
      | OutMsg of W8V.vector

(* +DEBUG *)
fun strToHex s =
      String.translate
	(fn c => StringCvt.padLeft #"0" 2 (Int.fmt StringCvt.HEX (Char.ord c)) ^ ".")
	 s
fun outMsgToStr OutFlush = "OutFlush"
  | outMsgToStr OutQuit = "OutQuit"
  | outMsgToStr (OutMsg v) = concat[
	"OutMsg \"",
	strToHex(Byte.unpackStringVec (Word8VectorSlice.slice (v, 0, SOME 4))),
	"..\" (", Int.toString(W8V.length v), " bytes)"
      ]
(* -DEBUG *)

  (** The input stream manager **
   * This monitors the input stream from the X-server, and breaks it up into
   * individual messages, which are sent on outCh to be unmarshalled and routed
   * by the sequencer.  Each message to the sequencer is a record consisting
   * of the message code and the message data.
   *)
    fun inbuf (outCh, sock) () = let
	  val stdMsgSz = 32
	(* read n bytes from the socket *)
	  fun readVec (n, hdr) = let
		fun read (0, [v]) = v
		  | read (0, vl) = W8V.concat(List.rev vl)
		  | read (n, vl) = let
		      val v = Socket.recvVec(sock, n)
		      in
			case (W8V.length v)
(**** NOTE: we need a more graceful way to signal that the socket has closed!! ***)
			 of 0 => raise Fail "Socket closed"
			  | len => read (n - len, v::vl)
			(* end case *)
		      end
		in
		  read (n, hdr)
		end
	  fun getMsg () = let
		val msg = readVec (stdMsgSz, [])
		in
		  case W8V.sub(msg, 0)
		   of 0w1 => let  (* reply *)
			val extraLen = LargeWord.toIntX(PackWord32Big.subVec(msg, 1))
			in
			  if (extraLen > 0)
			    then {
				code = 0w1,
				msg = readVec (4 * extraLen, [msg])
			      }
			    else {code = 0w1, msg = msg}
			end
		    | k => {code = k, msg = msg}
		  (* end case *)
		end
(* +DEBUG *)
val getMsg = fn () => let
      val (res as {code, msg}) = getMsg ()
      in
	XDebug.trace (XDebug.ioTM, fn () => [
	    "XIo.getMsg: buf = \"",
	    strToHex(Byte.unpackStringVec (Word8VectorSlice.slice (msg, 0, SOME 8))),
	    "..\", code = ", Word8.toString code,
	    ", len = ", Int.toString(W8V.length msg), "\n"
	  ]);
	res
      end
(* -DEBUG *)
	  fun loop () = (
		CML.send (outCh, getMsg());
		loop ())
	  in
	    loop () handle _ => CML.exit()
	  end


  (** The output stream manager. **)
    fun outbuf (inCh, sock) () = let
	  fun quit () = (Socket.close sock; CML.exit())
	  fun flushBuf strs = SockUtil.sendVec(sock, W8V.concat(rev strs))
(* +DEBUG *)
val flushBuf = fn strs => (
XDebug.trace (XDebug.ioTM, fn () => [
    "Flush: ", Int.toString (List.length strs), " msgs, ",
    Int.toString(List.foldl (fn (s, n) => W8V.length s + n) 0 strs), " bytes\n"
  ]);
flushBuf strs)
(* -DEBUG *)
	  fun insert (s, (strs, nb)) = let
		val n = W8V.length s
		in
		  if (n+nb > bufSz)
		    then (flushBuf strs; ([s], n))
		    else (s::strs, n+nb)
		end
(*****
	  fun loop ([], _) = (case CML.recv inCh
	       of OutFlush => loop([], 0)
		| (OutMsg s) => loop([s], W8V.length s)
		| OutQuit => quit())
	    | loop (buf as (strs, _)) = CML.select [
		  CML.wrap(flushTimeOut, fn _ => (flushBuf strs; loop([], 0))),
		  CML.wrap(CML.recvEvt inCh,
		    fn OutFlush => (flushBuf strs; loop([], 0))
		     | (OutMsg s) => loop(insert(s, buf))
		     | OutQuit => (flushBuf strs; quit()))
		]
*****)
fun prMsg msg = (
XDebug.trace (XDebug.ioTM, fn () => ["outbuf.loop: ", outMsgToStr msg, "\n"]);
msg)
	  fun loop (buf, _) = (
XDebug.trace (XDebug.ioTM, fn () => [
"outbuf.loop: waiting ", Int.toString(List.length buf), "\n"]);
		case buf
		 of [] => (case prMsg(CML.recv inCh)
		       of OutFlush => loop([], 0)
			| (OutMsg s) => loop([s], W8V.length s)
			| OutQuit => quit()
		      (* end case *))
		  | strs => CML.select [
			CML.wrap(flushTimeOut, fn _ => (flushBuf strs; loop([], 0))),
			CML.wrap(CML.wrap(CML.recvEvt inCh, prMsg),
			  fn OutFlush => (flushBuf strs; loop([], 0))
			   | (OutMsg s) => loop(insert(s, (buf, 0)))
			   | OutQuit => (flushBuf strs; quit()))
		      ]
		(* end case *))
	  in
	    loop ([], 0)
	  end (* outbuf *)


  (** The sequencer **
   * The sequencer is responsible for matching replies with requests. All requests to
   * the X-server go through the sequencer, as do all messages from the X-server.
   * The sequencer communicates on five fixed channels:
   *   reqCh  -- request messages from clients
   *   inCh   -- reply, error and event messages from the server (via the input buffer)
   *   outCh  -- requests messages to the output buffer
   *   xevtCh -- X-events to the X-event buffer
   *   errCh  -- errors to the error handler
   * In addition, the sequencer sends replies to clients on the reply channel that
   * was bundled with the request.
   *)
    local
    (* the kind of reply that is pending for an outstanding request in the
     * outstanding request queue.  We use words to represent the sequence
     * numbers.
     *)
      datatype outstanding_reply
	= ErrorChk of (W.word * reply CML.chan)
	| OneReply of (W.word * reply CML.chan)
	| MultiReply of (W.word * reply CML.chan * (W8V.vector -> int) * W8V.vector list)
	| ExposureReply of (W.word * (unit -> Geometry.rect list) SV.ivar)

(* +DEBUG *)
fun seqnToStr n = W.fmt StringCvt.DEC n
fun dumpPendingQ (seqn, ([], [])) = XDebug.errTrace (fn () => [
	"PendingQ(", seqnToStr seqn, "): empty\n"
      ])
  | dumpPendingQ (seqn, (front, rear)) = let
      fun item2s (ErrorChk(n, _)) = "  ErrorChk #" ^ (seqnToStr n) ^ "\n"
	| item2s (OneReply(n, _)) = "  OneReply #" ^ (seqnToStr n) ^ "\n"
	| item2s (MultiReply(n, _, _, _)) = "  MultiReply #" ^ (seqnToStr n) ^ "\n"
	| item2s (ExposureReply(n, _)) = "  ExposureReply #" ^ (seqnToStr n) ^ "\n"
      fun dump ([], l) = (rev l)
	| dump (x::r, l) = dump(r, (item2s x) :: l)
      in
	XDebug.errTrace (fn () =>
	    "PendingQ(" :: (seqnToStr seqn) :: "):\n"
	      :: (dump(front @ (rev rear), []))
	  )
      end
(* -DEBUG *)

      fun seqnOf (ErrorChk(seqn, _)) = seqn
	| seqnOf (OneReply(seqn, _)) = seqn
	| seqnOf (MultiReply(seqn, _, _, _)) = seqn
	| seqnOf (ExposureReply(seqn, _)) = seqn

      fun sendReply arg = (CML.spawn(fn () => CML.send arg); ())

      fun sendReplies (ch, replies) = let
	    fun loop [] = () | loop (s::r) = (CML.send(ch, Reply s); loop r)
	    in
	      CML.spawn (fn () => loop(rev replies)); ()
	    end

      fun insert (x, (front, rear)) = (front, x::rear)

    (* Synchronize the queue of outstanding requests with the sequence number n.
     * Return the pair (f, q), where q is the synchronized queue and f is true
     * if the head of q has sequence number b.
     *)
      fun syncOutstandingQ (n, q) = let
	    fun discardReply (ErrorChk(_, ch)) = sendReply(ch, Reply emptyV)
	      | discardReply (OneReply(_, ch)) = sendReply(ch, ReplyLost)
	      | discardReply (MultiReply(_, ch, _, [])) = sendReply(ch, ReplyLost)
	      | discardReply (MultiReply(_, ch, _, replies)) = sendReplies(ch, replies)
	      | discardReply (ExposureReply(_, syncV)) =
		  SV.iPut (syncV, fn () => raise LostReply)
	    fun scan (q' as ([], [])) = (false, q')
	      | scan ([], rear) = scan (rev rear, [])
	      | scan (q' as ((rpend :: r), rear)) = let
		  val seqn = seqnOf rpend
		  in
		    if (seqn < n)
		      then (discardReply rpend; scan (r, rear))
		      else if (seqn > n)
			then (false, q')
			else (true, q')
		  end
	    in
	      scan q
	    end

    (* extract the outstanding request corresponding to the given reply message (with
     * sequence number n).  If all of the expected replies have been received,
     * then send the extracted reply to the requesting client.
     *)
      fun extractReply (n, reply, q) = (
	    case (syncOutstandingQ(n, q))
	     of (true, (OneReply(_, ch)::r, rear)) => (
		  sendReply(ch, Reply reply); (r, rear))
	      | (true, (MultiReply(seqn, ch, remain, replies)::r, rear)) => (
		  if ((remain reply) = 0)
		    then (sendReplies(ch, reply::replies); (r, rear))
		    else (MultiReply(seqn, ch, remain, reply::replies)::r, rear))
	      | _ => 
(* DEBUG *) (dumpPendingQ(n, q);
MLXError.impossible "[XIo.extractReply: bogus pending reply queue]"
(* DEBUG *) )
	    (* end case *))

    (* extract the outstanding request corresponding to the given exposure message
     * (with seqence number n).
     *)
      fun extractExpose (n, reply, q) = (
	    case (syncOutstandingQ(n, q))
	     of (true, (ExposureReply(_, syncV)::r, rear)) => (
		  SV.iPut (syncV, fn () => reply); (r, rear))
(* for now, just drop it.  When the gc-server supports graphics-exposures, these
 * shouldn't happen. *)
	      | _ => q
(* +DEBUG 
(dumpPendingQ(n, q);
MLXError.impossible "[XIo.extractExpose: bogus pending reply queue]")
-DEBUG *)
	    (* end case *))

    (* extract the outstanding request corresponding to the given error message
     * (with seqence number n).
     *)
      fun extractErr (n, err, q) = (
	    case (syncOutstandingQ(n, q))
	     of (true, (ErrorChk(_, ch)::r, rear)) => (
		  sendReply(ch, ReplyError err); (r, rear))
	      | (true, (OneReply(_, ch)::r, rear)) => (
		  sendReply(ch, ReplyError err); (r, rear))
	      | (true, (MultiReply(_, ch, _, _)::r, rear)) => (
		  sendReply(ch, ReplyError err); (r, rear))
	      | (true, (ExposureReply(_, syncV)::r, rear)) => (
		  SV.iPut (syncV, fn () => raise ErrorReply(XReply.decodeError err));
		  (r, rear))
	      | (false, q') => q'
	      | _ =>
(* DEBUG *) (dumpPendingQ(n, q);
MLXError.impossible "[XIo.extractErr: bogus pending reply queue]"
(* DEBUG *) )
	    (* end case *))

      fun syncWithXEvt (n, q) = (
	    case (syncOutstandingQ(n, q))
	     of (true, (ErrorChk(_, ch)::r, rear)) => (
		  sendReply(ch, Reply emptyV); (r, rear))
	      | (_, q) => q
	    (* end case *))
    in
    fun sequencer (reqCh, inCh, outCh, xevtCh, errCh) () = let
	  fun quit () = (CML.send(outCh, OutQuit); CML.exit())
	  val inEvt = CML.recvEvt inCh
	  val reqEvt = CML.recvEvt reqCh
	  fun doRequest (req, (lastIn, lastOut, pending)) = (
		CML.send(outCh, OutMsg req);
		(lastIn, lastOut+0w1, pending))
	  fun doRequestAndChk ((req, replyCh), (lastIn, lastOut, pending)) = let
		val n = lastOut+0w1
		in
		  CML.send(outCh, OutMsg req);
		  (lastIn, n, insert(ErrorChk(n, replyCh), pending))
		end
	  fun doRequestReply ((req, replyCh), (lastIn, lastOut, pending)) = let
		val n = lastOut+0w1
		in
		  CML.send(outCh, OutMsg req);
		  (lastIn, n, insert(OneReply(n, replyCh), pending))
		end
	  fun doRequestReplies ((req, replyCh, remain), (lastIn, lastOut, pending)) = let
		val n = lastOut+0w1
		in
		  CML.send(outCh, OutMsg req);
		  (lastIn, n, insert(MultiReply(n, replyCh, remain, []), pending))
		end
	  fun doRequestExposures ((req, syncV), (lastIn, lastOut, pending)) = let
		val n = lastOut+0w1
		in
		  CML.send(outCh, OutMsg req);
		  (lastIn, n, insert(ExposureReply(n, syncV), pending))
		end
	(* gobble requests w/o blocking and then flush the buffer *)
	  fun gobbleAndFlush arg = let
		fun loop arg = (case (CML.recvPoll reqCh)
		       of NONE => arg
			| (SOME RequestFlush) => loop arg
			| (SOME RequestQuit) => quit()
			| (SOME(Request req)) => loop (doRequest(req, arg))
			| (SOME(RequestAndChk req)) => loop (doRequestAndChk (req, arg))
			| (SOME(RequestReply req)) => loop (doRequestReply (req, arg))
			| (SOME(RequestReplies req)) =>
			    loop (doRequestReplies (req, arg))
			| (SOME(RequestExposures req)) =>
			    loop (doRequestExposures (req, arg))
		      (* end case *))
		val res = loop arg
		in
		  CML.send(outCh, OutFlush);
		  res
		end
	(* the is the main sequencer loop; we keep track of the sequence number
	 * of the last message in, the sequence number of the last message out,
	 * and the queue of pending requests.
	 *)
	  fun loop (arg as (lastReqIn, lastReqOut, pending)) = let
	      (* handle a request from a client *)
		fun reqWrap RequestFlush = gobbleAndFlush arg
		  | reqWrap RequestQuit = quit()
		  | reqWrap (Request req) = (
		      CML.send(outCh, OutMsg req);
		      (lastReqIn, lastReqOut+0w1, pending))
		  | reqWrap (RequestAndChk req) =
		      gobbleAndFlush (doRequestAndChk (req, arg))
		  | reqWrap (RequestReply req) =
		      gobbleAndFlush (doRequestReply (req, arg))
		  | reqWrap (RequestReplies req) =
		      gobbleAndFlush (doRequestReplies (req, arg))
		  | reqWrap (RequestExposures req) =
		      gobbleAndFlush (doRequestExposures (req, arg))
	      (* handle a server-message (from the input buffer) *)
		fun inWrap {code : Word8.word, msg} = let
(** NOTE: this doesn't work if there are 2^17 outgoing messages between
 ** replies/events.  We need to track (lastReqOut - lastReqIn), and if it
 ** gets bigger than some reasonable size, generate a synchronization
 ** (i.e., GetInputFocus message).
 **)
		      fun getSeqN () = let
			    val shortSeqN = W.fromLargeWord(PackWord16Big.subVec(msg, 1))
			    val seqn' = W.orb(
				  W.andb(lastReqIn, W.notb 0wxffff),
				  shortSeqN)
			    in
			      if (seqn' < lastReqIn)
(* NOTE: we should check for (seqn' + 0x10000) > lastReqOut *)
				then seqn' + 0wx10000
				else seqn'
			    end
		      in
			case code
			 of 0w0 => let (* error message *)
			      val seqn = getSeqN()
			      in
				CML.send(errCh, (seqn, msg));
				(seqn, lastReqOut, extractErr(seqn, msg, pending))
			      end
			  | 0w1 => let (* reply message *)
			      val seqn = getSeqN()
			      in
			      (seqn, lastReqOut, extractReply(seqn, msg, pending))
			      end
			  | 0w11 => ( (* KeymapNotify event *)
			      CML.send (xevtCh, (code, msg));
			      ( lastReqIn, lastReqOut,
				syncWithXEvt(lastReqIn, pending)
			      ))
			  | 0w13 => let (* GraphicsExpose event *)
			      val seqn = getSeqN()
			      open XEventTypes
			      fun pack (rl, GraphicsExposeXEvt{rect, count=0, ...}) =
				    rect::rl
				| pack (rl, GraphicsExposeXEvt{rect, ...}) = (
				    case (CML.recv inCh)
				     of {code = 0w13, msg=s} =>
					  pack (rect::rl,
					    XReply.decodeGraphicsExpose s)
				      | _ => (
					  MLXError.warning
				  	    "[XIo.sequencer: misleading GraphicsExpose count]";
					  rect::rl)
				    (* end case *))
			      val rects = pack ([], XReply.decodeGraphicsExpose msg)
			      in
				( seqn,
				  lastReqOut,
				  extractExpose(seqn, rects, pending)
				)
			      end
			  | 0w14 => let (* NoExpose event *)
			      val seqn = getSeqN()
			      in
				(seqn, lastReqOut, extractExpose(seqn, [], pending))
			      end
			  | _ => let (* other event messages *)
			      val seqn = getSeqN()
			      in
				CML.send (xevtCh, (code, msg));
				(seqn, lastReqOut, syncWithXEvt(seqn, pending))
			      end
			(* end case *)
		      end
		in
		  loop (
		    CML.select [
			CML.wrap (reqEvt, reqWrap),
			CML.wrap (inEvt, inWrap)
		      ])
		end (* loop *)
	  in
	    loop (0w0, 0w0, ([], []))
	  end (* sequencer *)
    end (* local *)


  (** The X-event buffer **
   *
   * The X-event buffer decodes and buffers X-events.  This thread also packs
   * expose events.  It communicates on two channels as follows:
   *   xevtMsgCh  --  raw messages from the sequencer
   *   xevtCh     --  decoded events to the window registry
   *)
    fun xeventBuffer (xevtMsgCh, xevtCh) = let
	  open XEventTypes
	  fun decode (k, s) = #2(XReply.decodeXEvent (k, s))
	  fun packExposeEvts (e as ExposeXEvt{window, ...}) = let
		fun pack (rl, ExposeXEvt{rects, count=0, ...}) = rects@rl
		  | pack (rl, ExposeXEvt{rects, ...}) =
		      pack (rects@rl, decode(CML.recv xevtMsgCh))
		  | pack (rl, _) = (
		      MLXError.warning "[XIo.sequencer: misleading Expose count]";
		      rl)
		in
		  ExposeXEvt{window = window, rects = pack([], e), count = 0}
		end
	  fun doXEvent (msg, q) = (case (decode msg)
	       of (e as ExposeXEvt _) => (packExposeEvts e) :: q
		| e => (e :: q))
	  val getXEvt = CML.recvEvt xevtMsgCh
	  fun routeP () = let
		fun loop ([], []) = loop(doXEvent(CML.recv xevtMsgCh, []), [])
		  | loop ([], rear) = loop(rev rear, [])
		  | loop (front as (x::r), rear) =
		      loop (CML.select [
			  CML.wrap (getXEvt, fn msg => (front, doXEvent(msg, rear))),
			  CML.wrap (CML.sendEvt(xevtCh, x), fn () => (r, rear))
			])
		in
		  loop ([], [])
		end
	  in
	    routeP
	  end (* xeventBuffer *)


  (** The connection **)

    datatype connection = CONN of {
	  conn_id : unit ref,
	  xevt_ch : XEventTypes.xevent CML.chan,
	  req_ch : req_msg CML.chan,
	  err_ch : (W.word * W8V.vector) CML.chan,
	  flush : unit -> unit,
	  close : unit -> unit
	}

  (* Create the threads and internal channels to manage a connection to the
   * X server.  We assume that the connection request/reply has already been
   * dealt with.
   *)
    fun openConn sock = let
	  val inStrm = CML.channel() and outStrm = CML.channel()
	  val xevtStrm = CML.channel() and xevtMsgStrm = CML.channel()
	  val reqStrm = CML.channel() and errStrm = CML.channel()
	  val exposeStrm = CML.channel()
	  fun flushFn () = CML.send (reqStrm, RequestFlush)
	  fun closeFn () = (
		XDebug.trace (XDebug.ioTM, fn () => ["close connection\n"]);
		flushFn(); CML.send(reqStrm, RequestQuit))
	  in
(******
	    CML.spawn (sequencer (reqStrm, inStrm, outStrm, xevtMsgStrm, errStrm));
	    CML.spawn (inbuf (inStrm, sock));
	    CML.spawn (outbuf (outStrm, sock));
	    CML.spawn (xeventBuffer (xevtMsgStrm, xevtStrm));
******)
(* DEBUG *) XDebug.xspawn ("Sequencer", sequencer (
		reqStrm, inStrm, outStrm, xevtMsgStrm, errStrm));
(* DEBUG *) XDebug.xspawn ("Inbuf", inbuf (inStrm, sock));
(* DEBUG *) XDebug.xspawn ("Outbuf", outbuf (outStrm, sock));
(* DEBUG *) XDebug.xspawn ("XEventBuffer", xeventBuffer (xevtMsgStrm, xevtStrm));
	    CONN{
		conn_id = ref (),
		xevt_ch = xevtStrm,
		req_ch = reqStrm,
		err_ch = errStrm,
		flush = flushFn,
		close = closeFn
	      }
	  end

    fun closeConn (CONN{close, ...}) = close()

    fun sameConn (CONN{conn_id=a, ...}, CONN{conn_id=b, ...}) = (a = b)

    fun request (CONN{req_ch, ...}) s = (CML.send(req_ch, Request s))

    fun replyWrapper ReplyLost = raise LostReply
      | replyWrapper (ReplyError s) = raise ErrorReply(XReply.decodeError s)
      | replyWrapper (Reply s) = s

(** NOTE: these should be done using a guard event eventually *)
  (* Generate a request to the server and check on its successful completion. *)
    fun requestAndChk (CONN{req_ch, ...}) s = let
	  val replyCh1 = CML.channel() and replyCh2 = CML.channel()
	  in
	    CML.send (req_ch, RequestAndChk(s, replyCh1));
	    CML.send (req_ch, RequestReply(XRequest.requestGetInputFocus, replyCh2));
	    CML.wrap (CML.recvEvt replyCh1,
	      fn (ReplyError s) => raise ErrorReply(XReply.decodeError s)
	       | _ => ())
	  end

    fun requestReply (CONN{req_ch, ...}) s = let
	  val replyCh = CML.channel()
	  in
	    CML.send (req_ch, RequestReply(s, replyCh));
	    CML.wrap (CML.recvEvt replyCh, replyWrapper)
	  end

    fun requestMultiReply (CONN{req_ch, ...}) (s, remain) = let
	  val replyCh = CML.channel()
	  in
	    CML.send (req_ch, RequestReplies(s, replyCh, remain));
	    CML.wrap (CML.recvEvt replyCh, replyWrapper)
	  end

    fun requestWithExposures (CONN{req_ch, ...}) (s, syncV) = let
	  val replyCh = CML.channel()
	  in
	    CML.send (req_ch, RequestExposures(s, syncV))
	  end

    fun flushOut (CONN{flush, ...}) = flush()

    fun waitForXEvent (CONN{xevt_ch, ...}) = CML.recvEvt xevt_ch
    fun readXError (CONN{err_ch, ...}) = CML.recv err_ch

  end (* XIo *)

