(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor ParseGenParserFun(structure Parser : ARG_PARSER
                          structure Header : HEADER
			  sharing type Parser.pos = Header.pos
			  sharing type Parser.result = Header.parseResult
			  sharing type Parser.arg = Header.inputSource =
			                  Parser.lexarg
			 ) : PARSE_GEN_PARSER =

 struct
      structure Header = Header
      val parse = fn file =>
          let
	      val in_str = open_in file
	      val source = Header.newSource(file,in_str,std_out)
	      val error = fn (s : string,i:int,_) =>
		              Header.error source i s
	      val stream =  Parser.makeLexer (fn i => (input(in_str,i)))
		            source
	      val (result,_) = (Header.lineno := 1; 
				Header.text := nil;
		                Parser.parse(15,stream,error,source))
	   in (close_in in_str; (result,source))
	   end
  end;
