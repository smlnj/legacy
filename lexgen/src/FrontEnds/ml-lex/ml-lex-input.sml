(* ml-lex-input.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Driver for ml-lex input format.
 *)

structure MLLexInput =
  struct

    structure MLLexLrVals =
      MLLexLrValsFun(structure Token = LrParser.Token)
    structure MLLexLex = 
      MLLexLexFun(structure Tok = MLLexLrVals.Tokens)
    structure MLLexParser =
      Join(structure ParserData = MLLexLrVals.ParserData
           structure Lex = MLLexLex
           structure LrParser = LrParser)    

    fun parseFile fname = let
          fun parseErr (msg, line, _) = 
	        (print (Int.toString line);
		 print ": ";
		 print msg;
		 print "\n")
	  val strm = TextIO.openIn fname
	  val lexer =
		MLLexParser.makeLexer (fn n => TextIO.inputN (strm, n))
	  in
	    #1(MLLexParser.parse(15, lexer, parseErr, ()))
	    before TextIO.closeIn strm
	  end

  end
