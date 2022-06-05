(* graph-lex
 *
 * COPYRIGHT (c) 1994 AT&T Bell Labortories.
 *
 * Scanner specification for "dot" graph files.
 *)

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

type lexstate = {
       lineNum : int ref,
       stringstart : int ref,
       commentState : int option ref,
       charlist : string list ref,
       complain : string -> unit
     }
type arg = lexstate
       
fun mkSymbol (s,i) = Tokens.SYMBOL(s,i,i+(size s))
fun addString (charlist,s:string) = charlist := s :: (!charlist)
fun makeString charlist = (concat(rev(!charlist)) before charlist := [])

local open Format in
fun eof ({lineNum,stringstart,commentState,charlist,complain}:lexstate) = (
      case !commentState of
        SOME l => formatf "warning: non-terminated comment in line %d\n"
                    complain [INT l]
      | _ => ();
      Tokens.EOF(0,0)
    )
end

%%
%header (functor GraphLexFun(structure Tokens : Graph_TOKENS));
%arg ({lineNum,stringstart,commentState,charlist,complain});
%s QS COM EOLCOM;
ws=[\t\ ];
idchars=[A-Za-z_0-9];
id=[_A-Za-z]{idchars}*;
num=[0-9]+;
frac="."{num};
real="-"?{num}?{frac};
whole="-"?{num}"."?;
%%
<INITIAL>\n		=> (inc lineNum; continue());
<INITIAL>{ws}+		=> (continue());
<INITIAL>\"		=> (charlist := [];
                            stringstart := yypos;
                            YYBEGIN QS;
                            continue());
<INITIAL>"/*"   	=> (commentState := SOME(!lineNum); 
                            YYBEGIN COM; 
                            continue());
<INITIAL>"//"   	=> (YYBEGIN EOLCOM; 
                            continue());
<INITIAL>"@"		=> (Tokens.AT(yypos,yypos+1));
<INITIAL>"."		=> (Tokens.DOT(yypos,yypos+1));
<INITIAL>"="		=> (Tokens.EQUAL(yypos,yypos+1));
<INITIAL>")"    	=> (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"]"    	=> (Tokens.RBRACKET(yypos,yypos+1));
<INITIAL>"}"    	=> (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"("    	=> (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>"["    	=> (Tokens.LBRACKET(yypos,yypos+1));
<INITIAL>"{"    	=> (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>","    	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>";"    	=> (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>":"    	=> (Tokens.COLON(yypos,yypos+1));
<INITIAL>"->"		=> (Tokens.EDGEOP(yypos,yypos+2));
<INITIAL>"--"		=> (Tokens.EDGEOP(yypos,yypos+2));
<INITIAL>edge		=> (Tokens.EDGE(yypos,yypos+4));
<INITIAL>node		=> (Tokens.NODE(yypos,yypos+4));
<INITIAL>strict		=> (Tokens.STRICT(yypos,yypos+6));
<INITIAL>graph		=> (Tokens.GRAPH(yypos,yypos+5));
<INITIAL>digraph	=> (Tokens.DIGRAPH(yypos,yypos+7));
<INITIAL>subgraph	=> (Tokens.SUBGRAPH(yypos,yypos+8));
<INITIAL>{id}		=> (mkSymbol(yytext,yypos));
<INITIAL>{whole}	=> (mkSymbol(yytext,yypos));
<INITIAL>{real}		=> (mkSymbol(yytext,yypos));

<INITIAL>\h		=> (complain "non-Ascii character";
                   	    continue());
<INITIAL>.		=> (complain ("illegal token: "^(StringUtil.expandStr yytext));
                    	    continue());

<COM>\n			=> (inc lineNum; continue());
<COM>"*/"		=> (commentState := NONE; YYBEGIN INITIAL; continue());
<COM>.			=> (continue());

<EOLCOM>\n		=> (inc lineNum; YYBEGIN INITIAL; continue());
<EOLCOM>.		=> (continue());

<QS>\"			=> (YYBEGIN INITIAL; 
                            mkSymbol(makeString charlist,!stringstart));
<QS>\n			=> (complain "unclosed string";
			    YYBEGIN INITIAL;
                            mkSymbol(makeString charlist,!stringstart));
<QS>[^"\\\n]*		=> (addString(charlist,yytext); continue());
<QS>\\\n         	=> (continue());
<QS>\\\\         	=> (addString(charlist,"\\\\"); continue());
<QS>\\\"         	=> (addString(charlist,"\""); continue());
<QS>\\	         	=> (addString(charlist,"\\"); continue());

