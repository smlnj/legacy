(* atom-server.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A Client-side server for atoms.
 *)

signature ATOM_SERVER =
  sig

    type atom = XProtTypes.atom
    type atom_server

    val mkServer : XDisplay.xdisplay -> atom_server

    val internAtom : atom_server -> string -> atom
    val lookupAtom : atom_server -> string -> atom option
    val nameOfAtom : atom_server -> atom -> string

  end (* ATOM_SERVER *)

structure AtomServer : ATOM_SERVER =
  struct

    type atom = XProtTypes.atom

    datatype request
      = REQ_intern of (string * atom SyncVar.ivar)
      | REQ_lookup of (string * atom option SyncVar.ivar)
      | REQ_name of (atom * string SyncVar.ivar)

    datatype atom_server = AtomServer of request CML.chan

    fun intern conn arg = XReply.decodeInternAtomReply (
	  CML.sync (XIo.requestReply conn (XRequest.encodeInternAtom arg)))

    fun mkServer (XDisplay.XDPY{conn, ...}) = let
	  val reqCh = CML.channel()
(** NOTE: we are currently not using the local table; we also need to have
 ** a string --> atom mapping, and should initialize it with the standard atoms.
 **)
	  val atomTbl = XAtomTbl.mkTable (32, Fail "AtomTbl")
	  val insert = XAtomTbl.insert atomTbl
	  val find = XAtomTbl.find atomTbl
	  fun handleReq (REQ_intern(id, replyV)) =
		SyncVar.iPut(replyV, intern conn {name = id, only_if_exists = false})
	    | handleReq (REQ_lookup(id, replyV)) = (
		case (intern conn {name = id, only_if_exists = true})
		 of (XProtTypes.XAtom 0w0) => SyncVar.iPut(replyV, NONE)
		  | atom => SyncVar.iPut(replyV, SOME atom)
		(* end case *))
	    | handleReq (REQ_name(atom, replyV)) = let
		val name = XReply.decodeGetAtomNameReply (
		      CML.sync (XIo.requestReply conn
			(XRequest.encodeGetAtomName{atom = atom})))
		in
		  SyncVar.iPut(replyV, name)
		end
	  fun loop () = (handleReq(CML.recv reqCh); loop())
	  in
	    CML.spawn loop;
	    AtomServer reqCh
	  end (* mkServer *)

    fun rpc reqFn (AtomServer reqCh) arg = let
	  val replyV = SyncVar.iVar()
	  in
	    CML.send(reqCh, reqFn(arg, replyV));
	    SyncVar.iGet replyV
	  end

    val internAtom = rpc REQ_intern
    val lookupAtom = rpc REQ_lookup
    val nameOfAtom = rpc REQ_name

  end; (* AtomServer *)

