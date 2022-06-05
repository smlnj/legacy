

       type pos = int;

       type svalue = Tokens.svalue;
       type ('a,'b) token = ('a,'b) Tokens.token;
       type lexresult = (svalue,pos) Tokens.token;

       val pos = 0;

       fun error(err_str,_,_) = output(std_out,("error: "^err_str^"\n"));

type lexarg =  {comLevel : int ref, 
	        lineNum : int ref,
                stringtext : string ref,
	        linePos : int list ref, (* offsets of lines in file *)
	        err: pos*pos*string->unit} 

type arg = lexarg

val eof = fn ({comLevel,err,linePos,lineNum,stringtext}:lexarg) => 
           let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end 


       %%
       %structure expressionLex
       %header (functor expressionLexFun(structure Tokens : expression_TOKENS));
       %arg ({comLevel,lineNum,err,linePos,stringtext});
       var = [A-Za-df-z];
       num = [0-9][0-9]*;
       ws = [\ \t\n];
       %%

"+" => (Tokens.PLUS(0,0));
"-" => (Tokens.MINUS(0,0));
"*" => (Tokens.TIMES(0,0));
"/" => (Tokens.DIVIDE(0,0));
"^" => (Tokens.EXP (0,0));
"e" => (Tokens.E(0,0));
"~" => (Tokens.NEG(0,0));
"cos" => (Tokens.COS(0,0));
"sin" => (Tokens.SIN(0,0));


{num} => 
	(Tokens.NUM (revfold (fn(a,r)=>ord(a)-ord("0")+10*r) (explode yytext) 0,0,0));       

"(" => (Tokens.LPAREN(0,0));
")" => (Tokens.RPAREN(0,0));

{ws}+ => (continue());
{var} => (Tokens.VAR(yytext,0,0));
