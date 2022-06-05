(* fol.lex
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * 
 *)


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
       %structure fol_lex
       %header (functor fol_lex(structure Tokens : fol_TOKENS));
       %arg ({comLevel,lineNum,err,linePos,stringtext});
       ident = [A-Za-z] [A-Za-z0-9_]*;
       num = [0-9][0-9]*;
       alphanum = [A-Za-z0-9]*;
       ws = [\ \t\n];
       %%

<INITIAL>"&" => (Tokens.AND(0,0));
<INITIAL>"|" => (Tokens.OR(0,0));
<INITIAL>"~" => (Tokens.NEG(0,0));
<INITIAL>"->" => (Tokens.IMPLIES(0,0));
<INITIAL>"exists" => (Tokens.EXISTS(0,0));
<INITIAL>"forall" => (Tokens.FORALL(0,0));

<INITIAL>"=" => (Tokens.EQUAL(0,0));
<INITIAL>"<" => (Tokens.LANGLE(0,0));
<INITIAL>">" => (Tokens.RANGLE(0,0));
<INITIAL>"<=" => (Tokens.LEQ(0,0));
<INITIAL>">=" => (Tokens.GEQ(0,0));

<INITIAL>"+" => (Tokens.PLUS(0,0));
<INITIAL>"-" => (Tokens.MINUS(0,0));
<INITIAL>"*" => (Tokens.TIMES(0,0));
<INITIAL>"/" => (Tokens.DIVIDE(0,0));
<INITIAL>{num} => 
	(Tokens.NUM (revfold (fn(a,r)=>ord(a)-ord("0")+10*r) (explode yytext) 0,0,0));       

<INITIAL>":" => (Tokens.COLON(0,0));
<INITIAL>"(" => (Tokens.LPAREN(0,0));
<INITIAL>")" => (Tokens.RPAREN(0,0));
<INITIAL>"." => (Tokens.DOT(0,0));
<INITIAL>"," => (Tokens.COMMA(0,0));
<INITIAL>"'" => (Tokens.QUOTE(0,0));

<INITIAL>"#" => (Tokens.FORMPREFIX(0,0));
<INITIAL>"@" => (Tokens.TERMPREFIX(0,0));



<INITIAL>{ws}+ => (continue());
<INITIAL>{ident} => (Tokens.IDENT(yytext,0,0));
<INITIAL>. => (error (("lexer: ignoring bad character "^yytext),0,0); continue());






