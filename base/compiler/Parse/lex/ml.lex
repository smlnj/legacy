(* Parse/lex/ml.lex
 *
 * Copyright 1989 by AT&T Bell Laboratories
 * Copyright 2025 by The Fellowship of SML/NJ (www.smlnj.org)
 *)

structure EM = ErrorMsg;
structure UD = UserDeclarations;

val sourceMap = Source.sourcemap (CompInfo.source ())
fun err (region: int * int) (msg: string) =
    ErrorMsg.errorRegion (SourceLoc.REGION region, msg)
fun warn (region: int * int) (msg: string) =
    ErrorMsg.warnRegion (SourceLoc.REGION region, msg)

structure TokTable = TokenTable(Tokens);

type svalue = Tokens.svalue

type pos = int
type arg = UD.arg  

type lexresult = (svalue, pos) Tokens.token

type ('a, 'b) token = ('a, 'b) Tokens.token

fun eof arg = let val pos = UD.eof arg in Tokens.EOF(pos,pos) end

local
  fun cvt radix (s, i) =
	#1(valOf(IntInf.scan radix Substring.getc (Substring.extract(s, i, NONE))))
in
val atoi = cvt StringCvt.DEC
val xtoi = cvt StringCvt.HEX
end (* local *)

fun mysynch (srcmap, initpos, pos, args) = ()
(* -- not needed, no #line directives
    let fun cvt digits = getOpt(Int.fromString digits, 0)
	val resynch = SourceMap.resynch srcmap
     in case args
          of [col, line] =>
	       resynch (initpos, pos, cvt line, cvt col, NONE)
           | [file, col, line] =>
	       resynch (initpos, pos, cvt line, cvt col, SOME file)
           | _ => impossible "ill-formed args in (*#line...*)"
    end
*)

fun has_quote s = CharVector.exists (fn #"`" => true | _ => false) s

fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)
%%
%reject
%s A S F Q AQ L LL LLC LLCQ;
%header (functor MLLexFun (structure Tokens : ML_TOKENS));
%arg ({
  comLevel,
  charlist,
  stringtype,
  stringstart,
  brack_stack});
idchars=[A-Za-z'_0-9];
id=[A-Za-z]{idchars}*;
ws=("\012"|[\t\ ])*;
nrws=("\012"|[\t\ ])+;
eol=("\013\010"|"\010"|"\013");
some_sym=[!%&$+/:<=>?@~|#*]|\-|\^;
sym={some_sym}|"\\";
quote="`";
full_sym={sym}|{quote};
num=[0-9]+;
frac="."{num};
exp=[eE](~?){num};
real=(~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
xdigit=[0-9a-fA-F];
hexnum={xdigit}+;
bad_escape="\\"[\000-\008\011\012\014-\031 !#$%&'()*+,\-./:;<=>?@A-Z\[\]_`c-eg-mo-qsuw-z{}|~\127];

%%
<INITIAL>{ws}	=> (continue());
<INITIAL>{eol}	=> (SourceMap.newline sourceMap yypos; continue());
<INITIAL>"_overload" => (if !ParserControl.overloadKW then
                             Tokens.OVERLOAD(yypos,yypos+1)
                         else REJECT());
<INITIAL>"_"	=> (Tokens.WILD(yypos,yypos+1));
<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>"{"	=> (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"	=> (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"["	=> (Tokens.LBRACKET(yypos,yypos+1));
<INITIAL>"#["	=> (Tokens.VECTORSTART(yypos,yypos+1));
<INITIAL>"]"	=> (Tokens.RBRACKET(yypos,yypos+1));
<INITIAL>";"	=> (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"("	=> (if (null(!brack_stack))
                    then ()
                    else inc (hd (!brack_stack));
                    Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"	=> (if (null(!brack_stack))
                    then ()
                    else if (!(hd (!brack_stack)) = 1)
                         then ( brack_stack := tl (!brack_stack);
                                charlist := [];
                                YYBEGIN Q)
                         else dec (hd (!brack_stack));
                    Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"."		=> (Tokens.DOT(yypos,yypos+1));
<INITIAL>"..."		=> (Tokens.DOTDOTDOT(yypos,yypos+3));
<INITIAL>"'"{idchars}+
			=> (TokTable.makeTyvar(yytext,yypos));
<INITIAL>{id}	        => (TokTable.checkId(yytext, yypos));
<INITIAL>{full_sym}+    => (if !ParserControl.quotation
                            then if (has_quote yytext)
                                 then REJECT()
                                 else TokTable.checkSymId(yytext,yypos)
                            else TokTable.checkSymId(yytext,yypos));
<INITIAL>{sym}+         => (TokTable.checkSymId(yytext,yypos));
<INITIAL>{quote}        => (if !ParserControl.quotation
                            then (YYBEGIN Q;
                                  charlist := [];
                                  Tokens.BEGINQ(yypos,yypos+1))
                            else (err (yypos, yypos+1)
				   "quotation implementation error";
                                  Tokens.BEGINQ(yypos,yypos+1)));
<INITIAL>{real}
	=> (Tokens.REAL((yytext, RealLit.fromString yytext), yypos, yypos+size yytext));
<INITIAL>[1-9][0-9]*
	=> (Tokens.INT((yytext, atoi(yytext, 0)), yypos, yypos+size yytext));
<INITIAL>{num}
	=> (Tokens.INT0((yytext, atoi(yytext, 0)), yypos, yypos+size yytext));
<INITIAL>~{num}
	=> (Tokens.INT0((yytext, atoi(yytext, 0)), yypos, yypos+size yytext));
<INITIAL>"0x"{hexnum}
	=> (Tokens.INT0((yytext, xtoi(yytext, 2)), yypos, yypos+size yytext));
<INITIAL>"~0x"{hexnum}
	=> (Tokens.INT0((yytext, IntInf.~(xtoi(yytext, 3))), yypos, yypos+size yytext));
<INITIAL>"0w"{num}
	=> (Tokens.WORD((yytext, atoi(yytext, 2)), yypos, yypos+size yytext));
<INITIAL>"0wx"{hexnum}
	=> (Tokens.WORD((yytext, xtoi(yytext, 3)), yypos, yypos+size yytext));

<INITIAL>\"	=> (charlist := [""]; stringstart := yypos;
                    stringtype := true; YYBEGIN S; continue());
<INITIAL>\#\"	=> (charlist := [""]; stringstart := yypos;
                    stringtype := false; YYBEGIN S; continue());
<INITIAL>"(*#line"{nrws}  =>
                   (YYBEGIN L; stringstart := yypos; comLevel := 1; continue());
<INITIAL>"(*"	=> (YYBEGIN A; stringstart := yypos; comLevel := 1; continue());
<INITIAL>\h	=> (err (yypos,yypos)
		      (concat[
			  "non-Ascii character (ord ",
			  Int.toString(Char.ord(String.sub(yytext, 0))), ")"
			]);
		    continue());
<INITIAL>.	=> (err (yypos,yypos) "illegal token";
		    continue());
<L>[0-9]+                 => (YYBEGIN LL; charlist := [yytext]; continue());
<LL>\.                    => ((* cheat: take n > 0 dots *) continue());
<LL>[0-9]+                => (YYBEGIN LLC; UD.addString(charlist, yytext); continue());
<LL>0*               	  => (YYBEGIN LLC; UD.addString(charlist, "1");    continue()
		(* note hack, since ml-lex chokes on the empty string for 0* *));
<LLC>"*)"                 => (YYBEGIN INITIAL;
		              comLevel := 0; charlist := []; continue());
<LLC>{ws}\"		  => (YYBEGIN LLCQ; continue());
<LLCQ>[^\"]*              => (UD.addString(charlist, yytext); continue());
<LLCQ>\""*)"              => (YYBEGIN INITIAL;
		              comLevel := 0; charlist := []; continue());
<L,LLC,LLCQ>"*)" => (warn (!stringstart, yypos+1)
                       "ill-formed (*#line...*) taken as comment";
                     YYBEGIN INITIAL; comLevel := 0; charlist := []; continue());
<L,LLC,LLCQ>.    => (warn (!stringstart, yypos+1)
                       "ill-formed (*#line...*) taken as comment";
                     YYBEGIN A; continue());
<A>"(*"		=> (inc comLevel; continue());
<A>{eol}	=> (SourceMap.newline sourceMap yypos; continue());
<A>"*)" => (dec comLevel; if !comLevel=0 then YYBEGIN INITIAL else (); continue());
<A>.		=> (continue());
<S>\"	        => (let val s = UD.makeString charlist
                        val s = if size s <> 1 andalso not(!stringtype)
                                 then (err (!stringstart,yypos)
				         "character constant not length 1";
                                       substring(s^"x",0,1))
                                 else s
                        val t = (s,!stringstart,yypos+1)
                    in YYBEGIN INITIAL;
                       if !stringtype then Tokens.STRING t else Tokens.CHAR t
                    end);
<S>{eol}	=> (err (!stringstart,yypos) "unclosed string";
		    SourceMap.newline sourceMap yypos;
		    YYBEGIN INITIAL; Tokens.STRING(UD.makeString charlist,!stringstart,yypos));
<S>\\{eol}     	=> (SourceMap.newline sourceMap  (yypos+1);
		    YYBEGIN F; continue());
<S>\\{ws}   	=> (YYBEGIN F; continue());
<S>\\a		=> (UD.addString(charlist, "\007"); continue());
<S>\\b		=> (UD.addString(charlist, "\008"); continue());
<S>\\f		=> (UD.addString(charlist, "\012"); continue());
<S>\\n		=> (UD.addString(charlist, "\010"); continue());
<S>\\r		=> (UD.addString(charlist, "\013"); continue());
<S>\\t		=> (UD.addString(charlist, "\009"); continue());
<S>\\v		=> (UD.addString(charlist, "\011"); continue());
<S>\\\\		=> (UD.addString(charlist, "\\"); continue());
<S>\\\"		=> (UD.addString(charlist, "\""); continue());
<S>\\\^[@-_]	=> (UD.addChar(charlist,
			Char.chr(Char.ord(String.sub(yytext,2))-Char.ord #"@"));
		    continue());
<S>\\\^.	=>
	(err (yypos,yypos+2) "illegal control escape; must be one of \
	  \@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_";
	 continue());
<S>\\u{xdigit}{4}
		=> (let
                    val x = Word.toIntX (valOf (Word.fromString (String.substring(yytext, 2, 4))))
                    in
		      if x>255
			then err (yypos,yypos+4)
		                 (concat["illegal string escape '",
					 yytext, "' is too large"])
			else UD.addChar(charlist, Char.chr x);
		      continue()
		    end);
<S>\\[0-9]{3}	=> (let val SOME x = Int.fromString (String.substring(yytext, 1, 3))
		    in
		      if x>255
			then err (yypos,yypos+4) 
		                 (concat["illegal string escape '",
					 yytext, "' is too large"])
			else UD.addChar(charlist, Char.chr x);
		      continue()
		    end);
<S>{bad_escape}	=> (err (yypos,yypos+1) "illegal string escape";
		    continue());


<S>[\000-\031]  => (err (yypos,yypos+1) "illegal non-printing character in string";
                    continue());
<S>({idchars}|{some_sym}|\[|\]|\(|\)|{quote}|[,.;^{}])+|.  => (UD.addString(charlist,yytext); continue());
<F>{eol}	=> (SourceMap.newline sourceMap yypos; continue());
<F>{ws}		=> (continue());
<F>\\		=> (YYBEGIN S; stringstart := yypos; continue());
<F>.		=> (err (!stringstart,yypos) "unclosed string";
		    YYBEGIN INITIAL; Tokens.STRING(UD.makeString charlist,!stringstart,yypos+1));
<Q>"^`"	=> (UD.addString(charlist, "`"); continue());
<Q>"^^"	=> (UD.addString(charlist, "^"); continue());
<Q>"^"          => (YYBEGIN AQ;
                    let val x = UD.makeString charlist
                    in
                    Tokens.OBJL(x,yypos,yypos+(size x))
                    end);
<Q>"`"          => ((* a closing quote *)
                    YYBEGIN INITIAL;
                    let val x = UD.makeString charlist
                    in
                    Tokens.ENDQ(x,yypos,yypos+(size x))
                    end);
<Q>{eol}        => (SourceMap.newline sourceMap yypos; UD.addString(charlist,"\n"); continue());
<Q>.            => (UD.addString(charlist,yytext); continue());

<AQ>{eol}       => (SourceMap.newline sourceMap yypos; continue());
<AQ>{ws}        => (continue());
<AQ>{id}        => (YYBEGIN Q;
                    let val hash = HashString.hashString yytext
                    in
                    Tokens.AQID(FastSymbol.rawSymbol(hash,yytext),
				yypos,yypos+(size yytext))
                    end);
<AQ>{sym}+      => (YYBEGIN Q;
                    let val hash = HashString.hashString yytext
                    in
                    Tokens.AQID(FastSymbol.rawSymbol(hash,yytext),
				yypos,yypos+(size yytext))
                    end);
<AQ>"("         => (YYBEGIN INITIAL;
                    brack_stack := ((ref 1)::(!brack_stack));
                    Tokens.LPAREN(yypos,yypos+1));
<AQ>.           => (err (yypos,yypos+1)
		        ("ml lexer: bad character after antiquote "^yytext);
                    Tokens.AQID(FastSymbol.rawSymbol(0w0,""),yypos,yypos));
