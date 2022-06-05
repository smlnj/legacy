(* load-file.sml
 *)

functor LoadFile (IO : sig
    type instream
    val open_in : string -> instream
    val close_in : instream -> unit
    val inputc : instream -> int -> string
  end) : sig

    val loadFile : (string * {first : int, last : int} option)
	  -> {kind:ViewBuffer.token_kind, space:int, text:string} list list

  end = struct
    structure VB = ViewBuffer
    structure U = Mlex.UserDeclarations

    fun loadFile (fname, range) = (let
	  val strm = IO.open_in fname
	  val lexer = Mlex.makeLexer (IO.inputc strm)
	  fun scanFile lines = let
		fun scanLine (toks, lines) = (case (lexer())
		       of U.EOF => (rev toks) :: lines
			| U.NL => scanFile ((rev toks) :: lines)
			| (U.TOK tok) => scanLine(tok :: toks, lines)
			| (U.COM lst) => scanList(lst, toks, lines)
			| (U.STR lst) => scanList(lst, toks, lines)
		      (* end case *))
		and scanList ([], toks, lines) = scanLine(toks, lines)
		  | scanList (U.NL :: r, toks, lines) =
		      scanList (r, [], (rev toks) :: lines)
		  | scanList (U.TOK tok :: r, toks, lines) =
		      scanList (r, tok :: toks, lines)
		  | scanList _ = raise Fail "loadFile"
		in
		  scanLine ([], lines)
		end
	  val lines = (scanFile [])
	  in
	    IO.close_in strm;
	    case range
	     of NONE => rev lines
	      | SOME{first, last} => let
		  fun skip (0, lines) = lines
		    | skip (i, []) = []
		    | skip (i, _::lines) = skip(i-1, lines)
		  fun proj (0, _, lines) = lines
		    | proj (_, [], lines) = lines
		    | proj (i, l::r, lines) = proj (i-1, r, l::lines)
		  in
		    proj ((last-first)+1, skip ((length lines)-last, lines), [])
		  end
	  end
	    handle _ => [])

  end; (* LoadFile *)
