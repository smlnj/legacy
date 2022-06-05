(* parse_strings.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * 
 *)

signature FOL_PARSE_STRING =
sig
  structure F : FOL 
  val read_term : string -> F.term 
  val read_formula: string -> F.form
  
end 

structure FolParseString : FOL_PARSE_STRING = 
struct 

structure F = Fol 

fun parse_stream stm = 
          let val linenum = ref 1
	      fun perror (s,a,b) = 
		     output(std_out,((Integer.makestring (!linenum))
				     ^":"
				     ^(Integer.makestring a)
				     ^"-"
				     ^(Integer.makestring b)
				     ^".")^s); 
              val lexer = P.makeLexer
				(fn _ => input_line stm)
				({comLevel = ref 0,
				  lineNum = linenum,
				  linePos = ref [0],
                                  stringtext = ref "", 
				  err = (fn (a,b,s) => perror(s,a,b))})
              val (res,_) = P.parse(0,lexer,perror,())
          in res end


fun read_term s = 
          let val Fol.Term res = parse_stream (open_string ("@" ^ s))
          in res end

fun read_formula s = 
          let val Fol.Form res = parse_stream (open_string ("#" ^s))
          in res end


end 
