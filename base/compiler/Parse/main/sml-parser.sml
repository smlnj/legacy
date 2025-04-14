(* sml-parser.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This parser recognizes the Successr ML language (plus some SML/NJ extensions)
 *)

structure SMLParser : SMLNJ_PARSER =
struct

local (* imports *)

  structure CI = CompInfo
  structure SL = SourceLoc
  structure SM = SourceMap
  structure SR = Source
  structure EM = ErrorMsg

in

    structure LrVals = SMLLrValsFun(structure Token = LrParser.Token)
    structure Lex = SMLLexFun(structure Tokens = LrVals.Tokens)
    structure P = JoinWithArg(
	structure ParserData = LrVals.ParserData
	structure Lex = Lex
	structure LrParser = LrParser)

  (* the following two functions are also defined in build/computil.sml *)
    val addLines = Stats.addStat(Stats.makeStat "Source Lines")

    datatype parseResult = datatype ParseResult.parseResult

    exception AbortLex

    val dummyEOF = LrVals.Tokens.EOF(0,0)
    val dummySEMI = LrVals.Tokens.SEMICOLON(0,0)

    fun parse (source : Source.source) =
	let val sourceStream = SR.instream source
	    val sourcemap = SR.sourcemap source
	    val interactive = SR.interactive source

	    (* err : (int * int) -> EM.severity -> string -> EM.bodyPrinter -> unit *)
            (* to match the arg type expected by P.parse in oneparse *)
            fun err (lo: int, hi: int) = EM.error (SL.REGION (lo, hi))

            (* parseerror : string * int * int -> unit *)
	    (* where int = P.pos = SourceLoc.charpos *) 			     
	    fun parseerror (msg, p1, p2) =
	        EM.error (SL.REGION (p1, p2)) EM.COMPLAIN msg EM.nullErrorBody

	    val lexarg =
		{comLevel = ref 0,
		 charlist = ref (nil : string list),
		 stringtype = ref false,
		 stringstart = ref 0,
		 brack_stack = ref (nil: int ref list)}

	    val doprompt = ref true
	    val prompt = ref (!ParserControl.primaryPrompt)
	    fun inputc_sourceStream _ = TextIO.input sourceStream
	    fun getline k =
	        (if !doprompt
		 then
		   (if CI.errors () then raise AbortLex else ();
		    if !(#comLevel lexarg) > 0 orelse !(#charlist lexarg) <> nil
		    then Control_Print.say (!ParserControl.secondaryPrompt)
		    else Control_Print.say (!prompt);
		    Control_Print.flush();
		    doprompt := false)
		 else ();
		 let val str = TextIO.input sourceStream
		  in doprompt := ((String.sub (str, size str - 1) = #"\n") handle _ => false);
		     str
		 end)
	    val lexer = Lex.makeLexer (if interactive then getline else inputc_sourceStream) lexarg
	    val lexer' = ref(LrParser.Stream.streamify lexer)
	    val lookahead = if interactive then 0 else 30

	    fun oneparse () =
		let val _ = prompt := !ParserControl.primaryPrompt
		    val (nextToken, rest) = LrParser.Stream.get (!lexer')
		 in if P.sameToken (nextToken, dummySEMI)
		    then (lexer' := rest; oneparse ())
		    else if P.sameToken  (nextToken, dummyEOF)
			 then EOF
			 else
			   let val _ = prompt := !ParserControl.secondaryPrompt;
			       val initialLinePos = SM.lastLinePos sourcemap
			       val (result, lexer'') =
				     P.parse (lookahead, !lexer', parseerror, err)
			       val linesRead = SM.newlineCount
						 (sourcemap, SL.REGION (initialLinePos,
									SM.lastLinePos sourcemap))
			    in addLines linesRead;
			       lexer' := lexer'';
			       if CI.errors () then ERROR else PARSE result
			   end
		end handle LrParser.ParseError => ABORT
			 | AbortLex => ABORT
		(* oneparse *)

	 in oneparse
	end (* parse *)

end (* top local (imports *)
end (* structure SMLParser *)
