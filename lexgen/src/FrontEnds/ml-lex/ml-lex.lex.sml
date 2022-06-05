functor MLLexLexFun(structure Tok: MLLex_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1
	      }

	fun getc (Stream {strm, pos, id, lineNo}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0)
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof (Stream {strm, ...}) = TSIO.endOfStream strm

      end

    datatype 'a yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * 'a action * 'a yymatch
    withtype 'a action = yyInput.stream * 'a yymatch -> 'a

    datatype yystart_state = 
RE | DEFS | RECB | STRING | CHARCLASS | LEXSTATES | ACTION | INITIAL
    structure UserDeclarations = 
      struct

type pos = int
type svalue = Tok.svalue
type ('a,'b) token = ('a,'b) Tok.token
type lexresult= (svalue,pos) token

open Tok

val eof = fn () => EOF(~1,~1)
val error = (* fn (e,l : int,_) =>
      output(std_out,"line " ^ (makestring l) ^
	     ": " ^ e ^ "\n") *)
     fn _ => ()

local
val text = ref ([] : string list)
in
fun clrAction () = (text := ["("])
fun updAction (str) = (text := str :: (!text))
fun getAction () = (concat (rev (!text)))
end

(* what to do (i.e. switch start states) after recognizing an action *)
val afterAction = ref (fn () => ())

(* paren counting for actions *)
val pcount = ref 0
val inquote = ref false
fun inc r = if !inquote then () else r := !r + 1
fun dec r = if !inquote then () else r := !r - 1

structure SIS = RegExp.SymSet
fun uniChar s = let
      fun toW32 (c : Char.char) : Word32.word = 
	(case c of #"0" => 0w0 | #"1" => 0w1 | #"2" => 0w2 | #"3" => 0w3
	 	 | #"4" => 0w4 | #"5" => 0w5 | #"6" => 0w6 | #"7" => 0w7
	 	 | #"8" => 0w8 | #"9" => 0w9 | #"a" => 0w10 | #"A" => 0w10
		 | #"b" => 0w11 | #"B" => 0w11 | #"c" => 0w12 | #"C" => 0w12
		 | #"d" => 0w13 | #"D" => 0w13 | #"e" => 0w14 | #"E" => 0w14
		 | #"f" => 0w15 | #"F" => 0w15
		 | _ => raise Fail "invalid unicode escape sequence")
      fun iter (#"u"::_, v) = v
        | iter (c::cs,   v) = iter (cs, 0w16*v + (toW32 c))
	| iter _ = raise Fail "invalid unicode escape sequence"
      val uni = iter (List.rev (String.explode s), 0w0)
      in iter (List.rev (String.explode s), 0w0)
      end

val highAscii = SIS.interval(0w128, 0w255)



      end

    local
    fun mk yyins = let
        (* current start state *)
          val yyss = ref INITIAL
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yyins
	(* get one char of input *)
	  val yygetc = yyInput.getc 
	(* create yytext *)
	  fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
          open UserDeclarations
          fun lex 
(yyarg as ()) = let
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    fun continue() = 
let
fun yyAction0 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN DEFS; LEXMARK(!yylineno, !yylineno))
      end
fun yyAction1 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (DECLS(yytext, !yylineno, !yylineno))
      end
fun yyAction2 (strm, lastMatch) = (yystrm := strm; (lex()))
fun yyAction3 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; LEXMARK(!yylineno, !yylineno))
      end
fun yyAction4 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN LEXSTATES; STATES(!yylineno, !yylineno))
      end
fun yyAction5 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm;
        (clrAction(); pcount := 1; inquote := false; 
	            YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN DEFS);
		    HEADER(!yylineno, !yylineno))
      end
fun yyAction6 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (STRUCT(!yylineno, !yylineno))
      end
fun yyAction7 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm;
        (clrAction(); pcount := 1; inquote := false;
                    YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN DEFS);
		    ARG(!yylineno, !yylineno))
      end
fun yyAction8 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (COUNT(!yylineno, !yylineno))
      end
fun yyAction9 (strm, lastMatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (REJECTTOK(!yylineno, !yylineno))
      end
fun yyAction10 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (UNICODE(!yylineno, !yylineno))
      end
fun yyAction11 (strm, lastMatch) = (yystrm := strm; (lex()))
fun yyAction12 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (ID(yytext, !yylineno, !yylineno))
      end
fun yyAction13 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; EQ(!yylineno, !yylineno))
      end
fun yyAction14 (strm, lastMatch) = (yystrm := strm; (lex()))
fun yyAction15 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (QMARK(!yylineno, !yylineno))
      end
fun yyAction16 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (STAR(!yylineno, !yylineno))
      end
fun yyAction17 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (PLUS(!yylineno, !yylineno))
      end
fun yyAction18 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (BAR(!yylineno, !yylineno))
      end
fun yyAction19 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (LP(!yylineno, !yylineno))
      end
fun yyAction20 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (RP(!yylineno, !yylineno))
      end
fun yyAction21 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (DOLLAR(!yylineno, !yylineno))
      end
fun yyAction22 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (SLASH(!yylineno, !yylineno))
      end
fun yyAction23 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (DOT(!yylineno, !yylineno))
      end
fun yyAction24 (strm, lastMatch) = (yystrm := strm; (YYBEGIN RECB; lex()))
fun yyAction25 (strm, lastMatch) = (yystrm := strm; (YYBEGIN STRING; lex()))
fun yyAction26 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN CHARCLASS; LB(!yylineno, !yylineno))
      end
fun yyAction27 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN LEXSTATES; LT(!yylineno, !yylineno))
      end
fun yyAction28 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (GT(!yylineno, !yylineno))
      end
fun yyAction29 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm;
        (clrAction(); pcount := 1; inquote := false;
                    YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN RE);
		    ARROW(!yylineno, !yylineno))
      end
fun yyAction30 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN DEFS; SEMI(!yylineno, !yylineno))
      end
fun yyAction31 (strm, lastMatch) = (yystrm := strm; (lex()))
fun yyAction32 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (ID(yytext, !yylineno, !yylineno))
      end
fun yyAction33 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (REPS(valOf (Int.fromString yytext), !yylineno, !yylineno))
      end
fun yyAction34 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (COMMA(!yylineno, !yylineno))
      end
fun yyAction35 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; RCB(!yylineno, !yylineno))
      end
fun yyAction36 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; RBD(!yylineno, !yylineno))
      end
fun yyAction37 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; RB(!yylineno, !yylineno))
      end
fun yyAction38 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (DASH(!yylineno, !yylineno))
      end
fun yyAction39 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (CARAT(!yylineno, !yylineno))
      end
fun yyAction40 (strm, lastMatch) = (yystrm := strm; (YYBEGIN RE; lex()))
fun yyAction41 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (CHAR(valOf (String.fromString yytext), !yylineno, !yylineno))
      end
fun yyAction42 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (UNICHAR(uniChar yytext, !yylineno, !yylineno))
      end
fun yyAction43 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (CHAR(String.substring (yytext, 1, 1), !yylineno, !yylineno))
      end
fun yyAction44 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (CHAR(yytext, !yylineno, !yylineno))
      end
fun yyAction45 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (LEXSTATE(yytext, !yylineno, !yylineno))
      end
fun yyAction46 (strm, lastMatch) = (yystrm := strm; (lex()))
fun yyAction47 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (COMMA(!yylineno, !yylineno))
      end
fun yyAction48 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; GT(!yylineno, !yylineno))
      end
fun yyAction49 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN DEFS; SEMI(!yylineno, !yylineno))
      end
fun yyAction50 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm;
        (if !pcount = 0
		    then ((!afterAction)();
			  ACT(getAction(), !yylineno, !yylineno))
		    else (updAction ";"; lex()))
      end
fun yyAction51 (strm, lastMatch) = (yystrm := strm;
      (updAction "("; inc pcount; lex()))
fun yyAction52 (strm, lastMatch) = (yystrm := strm;
      (updAction ")"; dec pcount; lex()))
fun yyAction53 (strm, lastMatch) = (yystrm := strm; (updAction "\\\""; lex()))
fun yyAction54 (strm, lastMatch) = (yystrm := strm; (updAction "\\\\"; lex()))
fun yyAction55 (strm, lastMatch) = (yystrm := strm; (updAction "\\"; lex()))
fun yyAction56 (strm, lastMatch) = (yystrm := strm;
      (updAction "\""; inquote := not (!inquote); lex()))
fun yyAction57 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (updAction yytext; lex())
      end
fun yyQ110 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ115(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"&"
              then if inp = #"%"
                  then yyQ114(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ115(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ115(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
and yyQ114 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ113(strm', lastMatch)
            else if inp < #"&"
              then if inp = #"%"
                  then yystuck(lastMatch)
                  else yyQ113(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ113(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ113 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ110(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"&"
              then if inp = #"%"
                  then yyQ114(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ110(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ110(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
and yyQ115 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ115(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"&"
              then if inp = #"%"
                  then yyQ114(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ115(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ115(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ112 (strm, lastMatch) = yyAction0(strm, yyNO_MATCH)
fun yyQ111 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ113(strm', lastMatch)
            else if inp < #"&"
              then if inp = #"%"
                  then yyQ112(strm', lastMatch)
                  else yyQ113(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ113(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ7 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ110(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"&"
              then if inp = #"%"
                  then yyQ111(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ110(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ110(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch) = yyAction50(strm, yyNO_MATCH)
fun yyQ103 (strm, lastMatch) = yyAction51(strm, yyNO_MATCH)
fun yyQ104 (strm, lastMatch) = yyAction52(strm, yyNO_MATCH)
fun yyQ105 (strm, lastMatch) = yyAction56(strm, yyNO_MATCH)
fun yyQ106 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction57(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #"#"
                  then yyQ106(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                else if inp < #"#"
                  then if inp = #"\""
                      then yyAction57(strm, yyNO_MATCH)
                      else yyQ106(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                else if inp = #"("
                  then yyAction57(strm, yyNO_MATCH)
                else if inp < #"("
                  then yyQ106(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                else if inp <= #")"
                  then yyAction57(strm, yyNO_MATCH)
                  else yyQ106(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp = #"]"
              then yyQ106(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp < #"]"
              then if inp = #"\\"
                  then yyAction57(strm, yyNO_MATCH)
                  else yyQ106(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ106(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
              else yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch) = yyAction53(strm, yyNO_MATCH)
fun yyQ109 (strm, lastMatch) = yyAction54(strm, yyNO_MATCH)
fun yyQ107 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyAction55(strm, yyNO_MATCH)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ108(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                  else yyAction55(strm, yyNO_MATCH)
            else if inp = #"\\"
              then yyQ109(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
              else yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ106(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp < #"*"
              then if inp = #"#"
                  then yyQ106(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                else if inp < #"#"
                  then if inp = #"\""
                      then yyQ105(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                      else yyQ106(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                else if inp = #"("
                  then yyQ103(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                else if inp = #")"
                  then yyQ104(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                  else yyQ106(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ107(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp < #"\\"
              then if inp = #";"
                  then yyQ102(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                  else yyQ106(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ106(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch) = yyAction46(strm, yyNO_MATCH)
fun yyQ98 (strm, lastMatch) = yyAction47(strm, yyNO_MATCH)
fun yyQ99 (strm, lastMatch) = yyAction48(strm, yyNO_MATCH)
fun yyQ100 (strm, lastMatch) = yyAction49(strm, yyNO_MATCH)
fun yyQ101 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ101(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ101(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                      else yyAction45(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ101(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ101(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ101(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ101(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ101(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then if yyInput.eof(strm)
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp < #"-"
              then if inp = #"\^N"
                  then if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp < #"\^N"
                  then if inp = #"\v"
                      then if yyInput.eof(strm)
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                    else if inp < #"\v"
                      then if inp <= #"\b"
                          then if yyInput.eof(strm)
                              then UserDeclarations.eof(yyarg)
                              else yystuck(lastMatch)
                          else yyQ97(strm', lastMatch)
                    else if inp = #"\r"
                      then yyQ97(strm', lastMatch)
                    else if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp = #"!"
                  then if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp < #"!"
                  then if inp = #" "
                      then yyQ97(strm', lastMatch)
                    else if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp = #","
                  then yyQ98(strm', lastMatch)
                else if yyInput.eof(strm)
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp = #"?"
              then if yyInput.eof(strm)
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp < #"?"
              then if inp = #"<"
                  then if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp < #"<"
                  then if inp = #";"
                      then yyQ100(strm', lastMatch)
                    else if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp = #">"
                  then yyQ99(strm', lastMatch)
                else if yyInput.eof(strm)
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp = #"["
              then if yyInput.eof(strm)
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp < #"["
              then if inp <= #"@"
                  then if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                  else yyQ101(strm', lastMatch)
            else if inp = #"a"
              then yyQ101(strm', lastMatch)
            else if inp < #"a"
              then if yyInput.eof(strm)
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp <= #"z"
              then yyQ101(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ93 (strm, lastMatch) = yyAction37(strm, yyNO_MATCH)
fun yyQ94 (strm, lastMatch) = yyAction39(strm, yyNO_MATCH)
fun yyQ25 (strm, lastMatch) = yyAction44(strm, yyNO_MATCH)
fun yyQ30 (strm, lastMatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ31 (strm, lastMatch) = yyAction43(strm, yyNO_MATCH)
fun yyQ39 (strm, lastMatch) = yyAction42(strm, yyNO_MATCH)
fun yyQ38 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ39(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ39(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ39(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ39(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ39(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ37 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ38(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ38(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ38(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ38(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ36 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ37(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ37(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ37(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ37(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ37(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ37(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ32 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction43(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyAction43(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ36(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyAction43(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ36(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ34 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ35(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ35(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ33 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ34(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < #"0"
              then yyAction43(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ34(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"b"
              then yyQ30(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"#"
                  then yyQ31(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                else if inp < #"#"
                  then if inp = #"\v"
                      then yyQ31(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                    else if inp < #"\v"
                      then if inp = #"\n"
                          then yyAction44(strm, yyNO_MATCH)
                          else yyQ31(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                    else if inp = #"\""
                      then yyQ30(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                      else yyQ31(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                else if inp = #":"
                  then yyQ31(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                else if inp < #":"
                  then if inp <= #"/"
                      then yyQ31(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                      else yyQ33(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ30(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
            else if inp = #"s"
              then yyQ31(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"o"
                  then yyQ31(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                else if inp < #"o"
                  then if inp = #"n"
                      then yyQ30(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                      else yyQ31(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                else if inp = #"r"
                  then yyQ30(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
            else if inp = #"v"
              then yyQ31(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"t"
                  then yyQ30(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                  else yyQ32(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ31(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch) = yyAction36(strm, yyNO_MATCH)
fun yyQ95 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"]"
              then yyQ96(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\\"
              then yyQ26(strm', lastMatch)
            else if inp < #"\\"
              then if inp = #"\v"
                  then yyQ25(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then if yyInput.eof(strm)
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"-"
                  then yyQ95(strm', lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = #"_"
              then yyQ25(strm', lastMatch)
            else if inp < #"_"
              then if inp = #"]"
                  then yyQ93(strm', lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ25(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ92 (strm, lastMatch) = yyAction40(strm, yyNO_MATCH)
fun yyQ3 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ25(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\v"
                  then yyQ25(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then if yyInput.eof(strm)
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"\""
                  then yyQ92(strm', lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = #"]"
              then yyQ25(strm', lastMatch)
            else if inp < #"]"
              then if inp = #"\\"
                  then yyQ26(strm', lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ25(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ87 (strm, lastMatch) = yyAction31(strm, yyNO_MATCH)
fun yyQ88 (strm, lastMatch) = yyAction34(strm, yyNO_MATCH)
fun yyQ89 (strm, lastMatch) = yyAction35(strm, yyNO_MATCH)
fun yyQ90 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ90(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"0"
              then yyAction33(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ90(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ91(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction32(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ91(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                      else yyAction32(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ91(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction32(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ91(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                  else yyAction32(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction32(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction32(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ91(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ91(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                  else yyAction32(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ91(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then if yyInput.eof(strm)
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp < #"-"
              then if inp = #"\^N"
                  then if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp < #"\^N"
                  then if inp = #"\v"
                      then if yyInput.eof(strm)
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                    else if inp < #"\v"
                      then if inp <= #"\b"
                          then if yyInput.eof(strm)
                              then UserDeclarations.eof(yyarg)
                              else yystuck(lastMatch)
                          else yyQ87(strm', lastMatch)
                    else if inp = #"\r"
                      then yyQ87(strm', lastMatch)
                    else if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp = #"!"
                  then if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp < #"!"
                  then if inp = #" "
                      then yyQ87(strm', lastMatch)
                    else if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp = #","
                  then yyQ88(strm', lastMatch)
                else if yyInput.eof(strm)
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp = #"["
              then if yyInput.eof(strm)
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp < #"["
              then if inp = #":"
                  then if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp < #":"
                  then if inp <= #"/"
                      then if yyInput.eof(strm)
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                      else yyQ90(strm', lastMatch)
                else if inp <= #"@"
                  then if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                  else yyQ91(strm', lastMatch)
            else if inp = #"{"
              then if yyInput.eof(strm)
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp < #"{"
              then if inp <= #"`"
                  then if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                  else yyQ91(strm', lastMatch)
            else if inp = #"}"
              then yyQ89(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ40 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
fun yyQ41 (strm, lastMatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ42 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ42(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ42(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                      else yyAction12(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ42(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction12(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ42(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ42(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ42(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ42(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch) = yyAction3(strm, yyNO_MATCH)
fun yyQ86 (strm, lastMatch) = yyAction6(strm, yyNO_MATCH)
fun yyQ85 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ86(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ84 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ85(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ83 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"u"
              then yyQ84(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ82 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ83(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ81 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ82(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ80 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"u"
              then yyQ81(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ79 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ80(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ45 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ79(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch) = yyAction11(strm, yyNO_MATCH)
fun yyQ77 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"l"
              then yyQ78(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ76 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"l"
              then yyQ77(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ46 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"u"
              then yyQ76(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ75 (strm, lastMatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ74 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ75(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ73 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"d"
              then yyQ74(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ72 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"o"
              then yyQ73(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ71 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ72(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ70 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ71(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ47 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"n"
              then yyQ70(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ69 (strm, lastMatch) = yyAction9(strm, lastMatch)
fun yyQ68 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ69(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ67 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ68(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ66 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ67(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ65 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"j"
              then yyQ66(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ48 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ65(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ64 (strm, lastMatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ63 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ64(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ62 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"n"
              then yyQ63(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ61 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"u"
              then yyQ62(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ49 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"o"
              then yyQ61(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ60 (strm, lastMatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ59 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yystuck(lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yystuck(lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yystuck(lastMatch)
                      else yyQ59(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ59(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ59(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"("
              then yyQ60(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ58 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"g"
              then yyQ59(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ50 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ58(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ57 (strm, lastMatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ56 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yystuck(lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yystuck(lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yystuck(lastMatch)
                      else yyQ56(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ56(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ56(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"("
              then yyQ57(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ55 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ56(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ54 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ55(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ53 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"d"
              then yyQ54(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ52 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ53(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ51 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ52(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ43 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"g"
              then yystuck(lastMatch)
            else if inp < #"g"
              then if inp = #"b"
                  then yystuck(lastMatch)
                else if inp < #"b"
                  then if inp = #"&"
                      then yystuck(lastMatch)
                    else if inp < #"&"
                      then if inp = #"%"
                          then yyQ44(strm', lastMatch)
                          else yystuck(lastMatch)
                    else if inp = #"a"
                      then yyQ50(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"d"
                  then yystuck(lastMatch)
                else if inp < #"d"
                  then yyQ49(strm', lastMatch)
                else if inp = #"f"
                  then yyQ46(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"s"
              then yyQ45(strm', lastMatch)
            else if inp < #"s"
              then if inp = #"i"
                  then yystuck(lastMatch)
                else if inp < #"i"
                  then yyQ51(strm', lastMatch)
                else if inp = #"r"
                  then yyQ48(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"u"
              then yyQ47(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ1 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"%"
              then yyQ43(strm', lastMatch)
            else if inp < #"%"
              then if inp = #"\r"
                  then yyQ40(strm', lastMatch)
                else if inp < #"\r"
                  then if inp = #"\t"
                      then yyQ40(strm', lastMatch)
                    else if inp < #"\t"
                      then if yyInput.eof(strm)
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                    else if inp <= #"\n"
                      then yyQ40(strm', lastMatch)
                    else if yyInput.eof(strm)
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp = #" "
                  then yyQ40(strm', lastMatch)
                else if yyInput.eof(strm)
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp = #"A"
              then yyQ42(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"="
                  then yyQ41(strm', lastMatch)
                else if yyInput.eof(strm)
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ42(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ42(strm', lastMatch)
                else if yyInput.eof(strm)
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp <= #"z"
              then yyQ42(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ8 (strm, lastMatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ9 (strm, lastMatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ10 (strm, lastMatch) = yyAction15(strm, yyNO_MATCH)
fun yyQ11 (strm, lastMatch) = yyAction16(strm, yyNO_MATCH)
fun yyQ12 (strm, lastMatch) = yyAction17(strm, yyNO_MATCH)
fun yyQ13 (strm, lastMatch) = yyAction18(strm, yyNO_MATCH)
fun yyQ14 (strm, lastMatch) = yyAction19(strm, yyNO_MATCH)
fun yyQ15 (strm, lastMatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ16 (strm, lastMatch) = yyAction21(strm, yyNO_MATCH)
fun yyQ17 (strm, lastMatch) = yyAction22(strm, yyNO_MATCH)
fun yyQ18 (strm, lastMatch) = yyAction23(strm, yyNO_MATCH)
fun yyQ19 (strm, lastMatch) = yyAction24(strm, yyNO_MATCH)
fun yyQ20 (strm, lastMatch) = yyAction25(strm, yyNO_MATCH)
fun yyQ21 (strm, lastMatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ22 (strm, lastMatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ23 (strm, lastMatch) = yyAction28(strm, yyNO_MATCH)
fun yyQ24 (strm, lastMatch) = yyAction30(strm, yyNO_MATCH)
fun yyQ29 (strm, lastMatch) = yyAction29(strm, yyNO_MATCH)
fun yyQ28 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yystuck(lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yystuck(lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yystuck(lastMatch)
                      else yyQ28(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ28(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ28(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"("
              then yyQ29(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ27 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ28(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #","
              then yyQ25(strm', lastMatch)
            else if inp < #","
              then if inp = #"\""
                  then yyQ20(strm', lastMatch)
                else if inp < #"\""
                  then if inp = #"\r"
                      then yyQ8(strm', lastMatch)
                    else if inp < #"\r"
                      then if inp = #"\n"
                          then yyQ9(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ8(strm', lastMatch)
                              else yyQ25(strm', lastMatch)
                          else yyQ25(strm', lastMatch)
                    else if inp = #" "
                      then yyQ8(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"("
                  then yyQ14(strm', lastMatch)
                else if inp < #"("
                  then if inp = #"$"
                      then yyQ16(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"*"
                  then yyQ11(strm', lastMatch)
                else if inp = #")"
                  then yyQ15(strm', lastMatch)
                  else yyQ12(strm', lastMatch)
            else if inp = #"?"
              then yyQ10(strm', lastMatch)
            else if inp < #"?"
              then if inp = #";"
                  then yyQ24(strm', lastMatch)
                else if inp < #";"
                  then if inp = #"/"
                      then yyQ17(strm', lastMatch)
                    else if inp < #"/"
                      then if inp = #"."
                          then yyQ18(strm', lastMatch)
                          else yyQ25(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"="
                  then yyQ27(strm', lastMatch)
                else if inp = #"<"
                  then yyQ22(strm', lastMatch)
                  else yyQ23(strm', lastMatch)
            else if inp = #"{"
              then yyQ19(strm', lastMatch)
            else if inp < #"{"
              then if inp = #"\\"
                  then yyQ26(strm', lastMatch)
                else if inp < #"\\"
                  then if inp = #"["
                      then yyQ21(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = #"}"
              then yyQ25(strm', lastMatch)
            else if inp < #"}"
              then yyQ13(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ25(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of RE => yyQ0(!(yystrm), yyNO_MATCH)
    | DEFS => yyQ1(!(yystrm), yyNO_MATCH)
    | RECB => yyQ2(!(yystrm), yyNO_MATCH)
    | STRING => yyQ3(!(yystrm), yyNO_MATCH)
    | CHARCLASS => yyQ4(!(yystrm), yyNO_MATCH)
    | LEXSTATES => yyQ5(!(yystrm), yyNO_MATCH)
    | ACTION => yyQ6(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ7(!(yystrm), yyNO_MATCH)
  (* end case *))
end
	    in continue() end
          in 
            lex 
          end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
