signature EXPRESSION_PARSE_STRING =
sig
    structure F : EXPRESSION
    val read_expression : string -> F.expression
end

structure ExpressionParseString : EXPRESSION_PARSE_STRING =
struct

structure F = Expression

fun parse_stream stm = let
    val linenum = ref 1
    fun perror (s,a,b) = 
	output(std_out,((Integer.makestring (!linenum))
			^":"
			^(Integer.makestring a)
			^"-"
			^(Integer.makestring b)
			^".")^s); 
    val lexer = expressionParser.makeLexer
	(fn _ => input_line stm)
	({comLevel = ref 0,
	  lineNum = linenum,
	  linePos = ref [0],
	  stringtext = ref "", 
	  err = (fn (a,b,s) => perror(s,a,b))})
    val (res,_) = expressionParser.parse(0,lexer,perror,())
in 
    res 
end

fun read_expression str = parse_stream (open_string str) handle LexError => (CIO.print "Error in input\n"; F.Error)

end

