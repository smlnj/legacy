(* vtty.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * A simple virtual terminal built on top of the text widget.  This supports
 * an interface that is compatible with the TextIO structure in CML.
 *
 * TODO:
 *    Flow control (^S/^Q)
 *    User-defined erase, kill, etc.
 *)

structure Vtty : VTTY =
  struct

    structure TextIO = TextIO
    structure W = Widget

    open CML Widget

    datatype vtty = VTTY of {
	widget : widget,
	instrm : TextIO.instream,
	outstrm : TextIO.outstream
      }

    local
      val tabStop = 8
      fun expandTab col = let
	    val s = "               "
	    val lenS = String.size s
	    fun expand i = if (i <= lenS)
		  then substring(s, 0, i)
		  else s ^ (expand (i - lenS))
	    in
	      expand (tabStop - Int.rem(col, tabStop))
	    end

      datatype draw_cmd = Erase | Draw of string

    (** the echo server **
     * The echo server monitors the stream of keyboard events and echos
     * keystrokes on the terminal and forwards completed lines to the
     * instream buffer.
     *)
      fun mkEchoServer (keyEvt, trans, drawCh, putData) = let
	    open Interact
	    fun beep () = () (** NOP for now **)
	    val look = lookupString trans
	    fun loop curLine = (case (msgBodyOf (sync keyEvt))
		 of (KEY_Press arg) => let
		      fun tab () = (
			    send (drawCh, Draw "\t");
			    loop (#"\t" :: curLine))
		      fun newLine () = (
			    send (drawCh, Draw "\n");
			    putData (implode (rev (#"\n"::curLine)));
			    loop [])
		      fun erase () = loop (case curLine
			     of [] => (beep(); [])
			      | (c::r) => (send (drawCh, Erase); r))
		      fun flowOn () = loop curLine  (** NOP for now **)
		      fun flowOff () = loop curLine  (** NOP for now **)
		      in
			    case ((look arg) handle _ => "")
			     of "" => loop curLine
			      | "\t" => tab()
			      | "\^M" => newLine()  (* <cr> mapped to newline *)
			      | "\n" => newLine()
			      | "\127" => erase()   (* <del> mapped to backspace *)
			      | "\008" => erase()
			      | "\^Q" => flowOn()
			      | "\^S" => flowOff()
			      | s => (
				  send (drawCh, Draw s);
				  loop ((explode s) @ curLine))
		      end
		  | _ => loop curLine)
	    in
	      spawn (fn () => loop []);
	      ()
	    end (* mkEchoServer *)


    (** the text history buffer **
     * this buffers complete lines of text for redisplay when the widget is resized.
     *)
      datatype history_req
	= SetLen of int
	| PushLn of string
	| MapText of {nlines : int, ln_wid : int}

      datatype history_buf = HB of {
	  req_ch : history_req chan,
	  reply_ch : string list chan
	}

      fun mkHistoryBuf len = let
	    val reqCh = channel() and replyCh = channel()
	    fun config (maxLen, initRear) = let
		  fun prefix (0, l) = []
		    | prefix (_, []) = []
		    | prefix (n, x::r) = x :: prefix(n-1, r)
		  fun shift ([], []) = ([], [])
		    | shift ([], rear) = shift(rev rear, [])
		    | shift (_::front, rear) = (front, rear)
		  fun server (n, front, rear) = (case (recv reqCh)
			 of (SetLen len) => config (len, prefix (len, rear@(rev front)))
			  | (PushLn s) => if (n < maxLen)
			      then server (n+1, front, s::rear)
			      else let
			        val (front, rear) = shift (front, rear)
			        in
				  server (n, front, s::rear)
				end
			  | (MapText{nlines, ln_wid}) => let
			      fun getLines (_, [], lines) = lines
				| getLines (0, _, lines) = lines
				| getLines (n, s::r, lines) = let
				    val len = size s
				    fun getLn (0, _, lines) = lines
				      | getLn (n, 0, lines) = getLines(n, r, lines)
				      | getLn (n, i, lines) =
					  getLn (n-1, i-ln_wid,
					    substring(s, i-ln_wid, ln_wid)::lines)
				    in
				      if (len > ln_wid)
					then let
					  val tailLen = Int.rem(len, ln_wid)
					  val i = (len - tailLen)
					  in
					    getLn (n-1, i,
					      substring(s, i, tailLen)::lines)
					  end
					else getLines (n-1, r, s::lines)
				    end
			      in
				send(replyCh, getLines(nlines, rear@(rev front), []));
				server (n, front, rear)
			      end
			(* end case *))
		  in
		    server (List.length initRear, [], initRear)
		  end
	    in
	      spawn (fn () => config(len, []));
	      HB{req_ch = reqCh, reply_ch = replyCh}
	    end (* mkHistoryBuf *)

    (* push a line into a history buffer *)
      fun pushLine (HB{req_ch, ...}, ln) = send(req_ch, PushLn ln)

    (* set the length of a history buffer *)
      fun setLength (HB{req_ch, ...}, len) =
	    if (len <= 0) then send(req_ch, SetLen 1) else send(req_ch, SetLen len)

    (* map the maximum suffix (that will fit) of a history buffer onto a
     * rectangular array of characters.  The suffix is returned as a list
     * of at most "numLines" strings, each string being at most "lineWid"
     * characters.  The strings are in top-down order.
      *)
      fun mapText (HB{req_ch, reply_ch}, numLines, lineWid) = (
	    send(req_ch, MapText{nlines=numLines, ln_wid=lineWid});
	    recv reply_ch)


    (** the draw server **
     * The draw server receives strings from the output stream and the echo
     * server.  It draws the text for these strings and merges them into
     * complete lines of text, which are buffered in a text history buffer.
     *)
      fun mkDrawServer (tw, getDataEvt, echoCh, cmdInEvt, twCmdCh) = let
	    val setCursor = TextWidget.moveCursor tw
	    fun write (r,c,s)= TextWidget.writeText tw
		               {at=TextWidget.ChrCrd{col=c,row=r},text=s}
	    val scrollUp = TextWidget.scrollUp tw
	    fun clearToEOL (r,c) = TextWidget.clearToEOL tw (TextWidget.ChrCrd{col=c,row=r})
	    fun clear() = TextWidget.clearToEOS tw (TextWidget.ChrCrd{col=0,row=0})
	    val hb = mkHistoryBuf 0
	    val echoEvt = recvEvt echoCh
	    fun fillText l = let val row = ref 0
				 val _ = clear()
			     in app (fn s => (write(!row,0,s); row := !row+1)) l
			     end
	    fun config (curLnLen, curLn) = let
		  val {rows, cols} = TextWidget.charSizeOf tw
		  val _ = setLength (hb, rows-1)
		  val text = mapText (hb, rows-1, cols)
		  val row = length text
		  val col = length curLn
		  fun redrawCurLn l = let
			fun f (_, []) = ()
			  | f (i, ln::r) = write (row, i, String.str ln)
			in
			  f (0, l)
			end
(*** TYAN CODE:
		  fun redrawCurLn l = app (fn i => write (row, i, nth (l, i)))
		                          (0 thru col)
***)
(*** I moved the following into the body of the let
		  val _ = fillText text;
		  val _ = redrawCurLn (rev curLn);
***)
                (* keep track if there is any user input on the line.  We allow
                 * user input to follow output on the same line.
		 *)
		  fun server (arg as (cursorR, cursorC, curLnLen, curLn)) = let
		        open Interact
			fun handleOutput s = server (List.foldl
			       (fn (c, (row,col,len,ln)) => case c
				 of #"\^M" => (pushLine(hb, implode (rev ln));
					(row+1,0,len,[]))
			          | #"\n" =>  (pushLine(hb, implode (rev ln));
					(row+1,0,len,[]))
			          | _ => (write (row,col,String.str c);
					(row,col+1,len,c::ln)))
			      arg (explode s))
			fun handleEcho Erase =
			      if (cursorC > 0)
				then (
				  write (cursorR,cursorC-1," ");
				  server (cursorR,cursorC-1,curLnLen,tl curLn))
			        else server arg
			  | handleEcho (Draw s) = handleOutput s
			fun handleCmd msg = (send (twCmdCh,msg);
			      case (msgBodyOf msg)
			       of (CI_Resize _) => config(curLnLen, curLn)
				| _ => server arg
			      (* end case *))
			in
			  if (cursorR >= rows)
			    then (
			      scrollUp {from=rows-1,nlines=1};
			      server (cursorR-1,cursorC,curLnLen,curLn))
			    else select [
			        wrap (getDataEvt, handleOutput),
			        wrap (echoEvt, handleEcho),
			        wrap (cmdInEvt, handleCmd)
			      ]
			end (* server *)
		  in
		    fillText text;
		    redrawCurLn (rev curLn);
		    server (row, col, curLnLen, curLn)
		  end (* config *)
	    in
	      spawn (fn () => config (0, []));
	      ()
	    end (* mkDrawServer *)
    in

    fun mkVtty root size = let
	  val textWidget = TextWidget.mkTextWidget root size
	  val twidget = TextWidget.widgetOf textWidget
        (* tky thinks these might need to be here *)
	  val (putData, instrm) = let
		val ch = CML.channel()
		in
		  (fn s => CML.send(ch, s), TextIO.openChanIn ch)
		end
	  val (getDataEvt, outstrm) = let
		val ch = CML.channel()
		in
		  (CML.recvEvt ch, TextIO.openChanOut ch)
		end
	(* realize the vtty. *)
	  fun realizeVtty {env, win, sz} = let
		open Interact
		val InEnv{k=keyEvt, ci=ciEvt, ...} = env
		val echoCh = channel ()
		val twCmdInCh = channel ()
		val inEnv = replaceCI(
			replaceKey(ignoreMouse env, nullStream),
			recvEvt twCmdInCh)
		in
		  realizeFn twidget {env=inEnv, win=win, sz=sz};
		  mkDrawServer (textWidget, getDataEvt, echoCh, ciEvt, twCmdInCh);
		  mkEchoServer (keyEvt, defaultTranslation, echoCh, putData)
		end (* realizeVtty *)
	  in
	    VTTY{
		widget = mkWidget{
		    root = root,
                    args= fn () => {background = NONE}, 
		    boundsOf = boundsFn twidget,
		    realize = realizeVtty
		  },
		instrm = instrm,
		outstrm = outstrm
	      }
	  end (* mkVtty *)

    end (* local *)

    fun widgetOf (VTTY{widget, ...}) = widget

    fun openVtty (VTTY{instrm, outstrm, ...}) = (instrm,outstrm)

  end (* Vtty *)
