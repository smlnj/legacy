(* text-widget.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * A simple text widget: currently this only supports one fixed-width font (9x15).
 *)

structure TextWidget :> TEXT_WIDGET =
  struct
    structure W = Widget

    open Geometry EXeneBase W

    val caextract = CharArraySlice.vector o CharArraySlice.slice

    fun impossible (f,msg) = raise LibBase.Impossible("TextWidget."^f^": "^msg)

    datatype char_coord = ChrCrd of {col : int, row : int}

    fun min (a : int, b) = if (a < b) then a else b
    fun max (a : int, b) = if (a > b) then a else b

    val fontName = "9x15"
    val pad = 2
    val totPad = pad+pad

  (* Get the character dimensions from a (fixed-width) font *)
    fun fontInfo font = let
	  val {ascent, descent} = Font.fontHt font
	  in
	    (ascent + descent, Font.textWidth font "M", ascent)
	  end

  (* A description of the various size parameters of a text window *)
    datatype text_sz = TSZ of {
	sz : size,
	rows : int, cols : int,
	char_ht : int, char_wid : int, ascent : int
      }

  (* make a text window size descriptor from a window size and font. *)
    fun mkTextSz (sz as SIZE{wid, ht}, font) = let
	  val (charHt, charWid, ascent) = fontInfo font
	  in
	    TSZ{
		sz = sz,
		rows = Int.quot(ht - totPad, charHt),
		cols = Int.quot(wid - totPad, charWid),
		char_ht = charHt, char_wid = charWid, ascent = ascent
	      }
	  end

  (* return true if the character coordinate is in the text window *)
    fun inTextWin (TSZ{rows, cols, ...}, ChrCrd{row, col}) =
	  ((0 <= row) andalso (row < rows)) andalso
	  ((0 <= col) andalso (col < cols))

  (* clip a string to insure that it does not exceed the text length *)
    fun clipString (TSZ{cols, ...}, col, s) = let
	  val len = String.size s
	  in
	    if ((col + len) <= cols)
	      then s
	      else substring(s, 0, cols-col)
	  end


  (*** The text buffer ***
   * This is a two dimensional array of characters with highlighting information.
   *)
    local
      datatype text_line = TL of (CharArray.array * (int * int) list)
    in
    abstype text_buf = TB of {sz : size, arr : text_line Array.array}
    with
      local
	fun revappend ([], l) = l
	  | revappend (x::r, l) = revappend (r, x::l)

      (* Update the highlight region list of a line to reflect the writing of a
       * length "len" normal-mode string starting in column "col".
       *)
	fun insN (_, _, [] : (int * int) list) = []
	  | insN (col, len, format) = let
	      val endCol = col+len
	      fun prefix ([], _) = format (* text falls after highlight regions *)
		| prefix ((c, n)::r, l) = let
		    val endC = c+n
		    in
		      if (endC <= col) 
			then prefix(r, (c, n)::l)
		      else if (endCol <= c)
			then format (* text falls between highlight regions *)
		      else if (c < col)
			then if (endC <= endCol)
			  then suffix ((c, col-c)::l, r)
			  else revappend (l, (c, col-c)::(endCol, endC-endCol)::r)
		      else if (endC <= endCol)
			then suffix (l, r)  (* text covers (c, n) *)
			else revappend (l, (endCol, endC-endCol)::r)
		    end 
	      and suffix (pre, []) = revappend (pre, [])
		| suffix (pre, (c, n)::r) = let
		    val endC = c+n
		    in
		      if (endC <= endCol)
			then suffix(pre, r)
		      else if (c < endCol)
			then revappend(pre, (endCol, endC-endCol)::r)
			else revappend(pre, r)
		    end
	      in
		prefix (format, [])
	      end (* insN *)

      (* Update the highlight region list of a line to reflect the writing of a
       * length len highlighted string starting in column col.
       *)
	fun insH (col, len, [] : (int * int) list) = [(col, len)]
	  | insH (col, len, format) = let
	      val endCol = col+len
	      fun prefix ([], l) = revappend(l, [(col, len)])
		| prefix ((c, n)::r, l) = let
		    val endC = c+n
		    in
		      if (endC < col)
			then prefix (r, (c, n)::l)
		      else if (endCol < c)
			then revappend (l, (col, len)::(c, n)::r)
		      else if (c < col)
			then if (endC < endCol)
			  then suffix (l, c, endCol, r)
			  else format
		      else if (endC < endCol)
			then suffix (l, col, endCol, r)
			else revappend (l, (col, endC-col)::r)
		    end
	      and suffix (pre, col, endCol, []) = revappend (pre, [(col, endCol-col)])
		| suffix (pre, col, endCol, (c, n)::r) = let
		    val endC = c+n
		    in
		      if (c > endCol)
			then revappend (pre, (col, endCol-col)::(c, n)::r)
		      else if (endC < endCol)
			then suffix (pre, col, endCol, r)
			else revappend (pre, (col, endC-col)::r)
		    end
	      in
		prefix (format, [])
	      end (* insH *)

	fun leftShift (col, delta, format) = let
              val endCol = col + delta
              fun filter [] = []
                | filter ((c, n)::r) = let
                    val endc = c+n
                    in
                      if c < col then
                        if endc <= col then (c,n)::(filter r)
                        else if endc <= endCol then (c,col-c)::(filter r)
                        else (c,col-c)::(endCol-delta,endc-endCol)::(filter r)
                      else if c < endCol then
                        if endc <= endCol then filter r
                        else (endCol-delta,endc-endCol)::(filter r)
                      else (c-delta,n)::(map (fn (c,n) => (c-delta,n)) r)
                    end
	      in
		filter format
	      end (* leftShift *)

	fun rightShift (col, endCol, delta, format) = let
              fun filter [] = []
                | filter ((c, n)::r) =
                    if c+n <= col
                      then (c,n)::(filter r)
                    else if c < col
                      then (c,min(n+delta,endCol-c))::(filter r)
                    else let val c' = c + delta 
                    in
                      if c' < endCol then (c',min(n,endCol-c'))::(filter r)
                      else []
		    end
	      in
		filter format
	      end (* rightShift *)

	fun newTextLn cols = TL(CharArray.array(cols, #" "), [])

      (* write a string into a bytearray starting at col. *)
	fun writeStr (ba, col, str) = let
	      fun cpy (i, j) = (
		    CharArray.update(ba, i, String.sub(str, j));
		    cpy(i+1, j+1))
	      in
		(cpy(col, 0)) handle _ => ()
	      end

      (* copy a block of nchars from fromcol to tocol.
       * NOTE: the updating of the highlight list is not exact, as
       * we assume copyText is followed by a clearLine or a writeText, 
       * which will restore consistency.
       * we also assume that all characters to the right of 
       * min(fromcol,tocol) are affected.
       *)
	fun copyText (TB{arr,sz=SIZE{wid,...}}, row, fromcol, tocol, nchars) = let
	      val TL(ba, format) = Array.sub(arr, row)
              val delta = tocol - fromcol
              fun copyStr (0,_,_) = ()
                | copyStr (cnt,indx,inc) = (
                      CharArray.update(ba,indx+delta,CharArray.sub(ba,indx));
                      copyStr(cnt-1,indx+inc,inc))
	      in
                if delta > 0 then (
	  	  copyStr (nchars, fromcol+nchars-1, ~1);
		  Array.update(arr,row,TL(ba,rightShift(fromcol,wid,delta,format)))
                )
                else (
	  	  copyStr (nchars, fromcol, 1);
		  Array.update(arr,row,TL(ba,leftShift(tocol,~delta,format)))
                )
	      end

      in

      (* create a text buffer of the specified size *)
	fun mkTextBuf (TSZ{rows, cols, ...}) = let
	      fun loop (0, l) = TB{sz=SIZE{wid=cols,ht=rows},arr=Array.fromList l}
		| loop (i, l) = loop (i-1, (newTextLn cols)::l)
	      in
		loop (rows, [])
	      end

      (* write a string in normal mode into a text array *)
	fun writeNText (TB{arr,...}, row, col, str) = let
	      val TL(ba, format) = Array.sub(arr, row)
	      in
		writeStr (ba, col, str);
		Array.update(arr, row, TL(ba, insN(col, String.size str, format)))
	      end

      (* write a string in highlighted mode into a text array *)
	fun writeHText (TB{arr,...}, row, col, str) = let
	      val TL(ba, format) = Array.sub(arr, row)
	      in
		writeStr (ba, col, str);
		Array.update(arr, row, TL(ba, insH(col, String.size str, format)))
	      end

      (* insert a string into a text array, shifting chars to the right *)
	fun insertBufText (
	      tbuf as TB{sz=SIZE{wid,...},...}, row, col, str, highlight
	    ) = let
	      val slen = size str
	      val eolcnt = wid - col - slen
	      in
		if (eolcnt > 0)
		  then copyText (tbuf, row, col, col+slen, eolcnt)
		  else (); 
                if highlight
		  then writeHText (tbuf, row, col, str)
		  else writeNText (tbuf, row, col, str)
	      end

      (* clear the given line of text *)
	fun clearTextLn (TB{arr,...}, ChrCrd{row, col}) = let
	      val TL(ba, format) = Array.sub(arr, row)
	      in
		if (col = 0)
		  then Array.update(arr, row, newTextLn(CharArray.length ba))
		  else let
		    fun clr i = (CharArray.update(ba, i, #" "); clr(i+1))
		    val newFormat = insN (col, (CharArray.length ba) - col, format)
		    in
		      (clr col) handle _ => ();
		      Array.update(arr, row, TL(ba, newFormat))
		    end
	      end

      (* delete cnt chars at the given position *)
        fun deleteTextChars (tbuf as TB{sz=SIZE{wid,...},...}, row, col, cnt) = let
          val eolcnt = wid - col - cnt
          in
            if eolcnt > 0 then (
              copyText (tbuf, row, col+cnt, col, eolcnt); 
              clearTextLn (tbuf, ChrCrd{row=row, col=wid-cnt})
            )
            else clearTextLn (tbuf, ChrCrd{row=row, col=col})
          end

      (* clear the given block of text *)
	fun clearText {text = TB{arr=ar,...}, from, to} = let
	      val cols = let val TL(ba, _) = Array.sub(ar, 0)
		    in CharArray.length ba end
	      fun clearLn i = Array.update(ar, i, newTextLn cols)
	      fun loop i = if (i < to)
		    then (clearLn i; loop(i+1))
		    else ()
	      in
		loop from
	      end

      (* Move a block of text up; "from" is the bottom of the text to be moved,
       * "to" is the line to move "from" to, and "nlines" is the size of the
       * block being moved.  It is assumed that the top line of the moved
       * block will end up at the top of the screen.
       *)
	fun moveTextUp {text as TB{arr=ar,...}, from, to, nlines} = let
	      fun copy (i, j) = if (i <= to)
		    then (Array.update(ar, i, Array.sub(ar, j)); copy(i+1, j+1))
		    else ()
	      in
		copy (0, from-to);
		clearText {text = text, from = to+1, to = from+1}
	      end

      (* Move a block of text down; "from" is the top of the text to be moved,
       * "to" is the line to move "from" to, and "nlines" is the size of the
       * block being moved.  It is assumed that the bottom line of the moved
       * block will end up at the bottom of the screen.
       *)
	fun moveTextDown {text as TB{arr=ar,...}, from, to, nlines} = let
	      val rows = Array.length ar
	      fun copy (i, j) = if (i >= to)
		    then (Array.update(ar, i, Array.sub(ar, j)); copy(i-1, j-1))
		    else ()
	      in
		copy (rows-1, (from+nlines)-1);
		clearText {text = text, from = from, to = to}
	      end

      (* Delete a block of text; "from" is the start of the block, "nlines" is the
       * number of lines to delete.  The text below the delete block is scrolled up
       * to fill the space, with blank lines filling from the bottom.
       *)
	fun deleteText {text as TB{arr=ar,...}, from, nlines} = let
	      val rows = Array.length ar
	      fun copy (i, j) = if (j < rows)
		    then (Array.update(ar, i, Array.sub(ar, j)); copy(i+1, j+1))
		    else ()
	      in
		copy (from, from+nlines);
		clearText {text = text, from = rows-nlines, to = rows}
	      end

      (* extract the text starting in column "col" of length "len" in row "row".
       * This is returned as a list of strings: the first in normal mode, the
       * second in highlighted mode, the third in normal, etc.
       *)
	fun explodeRow {text = TB{arr=text,...}, row, col, len} = (
	      case (Array.sub(text, row))
	       of TL(ba, []) => [caextract(ba, col, SOME len)]
		| TL(ba, l) => let
		    val endCol = col+len
		    fun ext (col, len) = caextract(ba, col, SOME len)
		    fun prefix [] = [ext (col, len)]
		      | prefix ((c, n)::r) = let
			  val endC = c+n
			  in
			    if (endC <= col)
			      then prefix r
			    else if (endCol <= c)
			      then [ext (col, len)]
			    else if (c < col)
			      then if (endC < endCol)
				then suffix (endC, r, [ext(col, endC-col), ""])
				else ["", ext (col, len)]
			      else if (endC < endCol)
				then suffix (endC, r, [ext(c, n), ext(col, c-col)])
				else [ext(col, c-col), ext(c, endCol-c)]
			  end
		    and suffix (i, [], l) = revappend(l, [ext(i, endCol-i)])
		      | suffix (i, (c, n)::r, l) = let
			  val endC = c+n
			  in
			    if (endCol <= c)
			      then revappend (l, [ext(i, endCol-i)])
			    else if (endC < endCol)
			      then suffix (endC, r, ext(c, n)::ext(i, c-i)::l)
			      else revappend (l, [ext(i, c-i), ext(c, endCol-c)])
			  end
		    in
		      prefix l
		    end
	      (* end case *))

      (* resize a text buffer.  If the new size is smaller, then stuff is
       * dropped from the bottom and right.  If the new size is larger, then
       * blank space is added to the bottom and right.
       *)
	fun resizeTextBuf (TB{arr=oldA,...}, newSz as TSZ{rows, cols, ...}) = let
	      val (newTB as (TB{arr=newA,...})) = mkTextBuf newSz
	      fun copy row = let
		    val TL(newBA, _) = Array.sub(newA, row)
		    val TL(oldBA, oldHL) = Array.sub(oldA, row)
		    fun cpy col = (
			  CharArray.update(newBA, col, CharArray.sub(oldBA, col));
			  cpy (col+1))
		    fun clipHL ([], l) = revappend(l, [])
		      | clipHL ((c, n)::r, l) =
			  if (c >= cols)
			    then revappend (l, [])
			  else if ((c+n) <= cols)
			    then clipHL (r, (c, n)::l)
			    else revappend (l, [(c, cols-c)])
		    in
		      Array.update(newA, row, TL(newBA, clipHL(oldHL, [])));
		      (cpy 0) handle _ => ();
		      copy (row+1)
		    end
	      in
		(copy 0) handle _ => ();
		newTB
	      end (* resizeTextBuf *)

      end (* local *)
    end (* abstype text_buf *)
    end (* local *)


  (*** The text window ***
   * This is a dumb text window that supports drawing text in normal and
   * highlighted mode text.
   *)
    abstype text_win = TW of {
	root : Widget.root,
	win : EXeneBase.window,
	font : EXeneBase.font,
	rows : int, cols : int,
	char_ht : int, char_wid : int, char_ascent : int,
	draw_text : {col : int, row : int, s : string} -> unit,
	highlight_text : {col : int, row : int, s : string} -> unit,
	stipple : {col : int, row : int, highlight : bool} -> unit,
	clear_line : {row : int, start_col : int, end_col : int} -> unit,
	clear_blk : {start_row : int, end_row : int} -> unit,
	char_blt : {row : int, from : int, to : int, nchars : int} 
           -> rect list CML.event,
	line_blt : {from : int, to : int, nlines : int} -> rect list CML.event
      }
    with
      local
	open Drawing

      (* Blt a block of text within a line *)
	fun charBlt (win, TSZ{char_ht, char_wid, sz=SIZE{wid, ...}, ...}) = let
	      val pixelBlt = pixelBltEvt (drawableOfWin win) defaultPen
	      fun blt {row, from, to, nchars} = let
		    val Y = (char_ht * row) + pad
		    in
		      pixelBlt {
			  src = WSRC win,
			  src_rect = RECT{
			      x = pad + from*char_wid, y = Y,
			      wid = nchars*char_wid, ht = char_ht
			    },
			  dst_pos = PT{x=pad + to*char_wid, y=Y}
			}
		    end
	      in
		blt
	      end (* charBlt *)

      (* Blt a block of text by lines *)
	fun lineBlt (win, TSZ{char_ht, char_wid, sz=SIZE{wid, ...}, ...}) = let
	      val pixelBlt = pixelBltEvt (drawableOfWin win) defaultPen
	      val textWid = wid - totPad
	      fun blt {from, to, nlines} = let
		    val fromY = (char_ht * from) + pad
		    val toY = (char_ht * to) + pad
		    in
		      pixelBlt {
			  src = WSRC win,
			  src_rect = RECT{
			      x = pad, y = fromY,
			      wid = textWid, ht = (char_ht * nlines)
			    },
			  dst_pos = PT{x=pad, y=toY}
			}
		    end
	      in
		blt
	      end (* lineBlt *)

      (* a stipple pattern for the cursor *)
	val cursorStippleData = (16, [[
                "0x8888", "0x2222", "0x1111", "0x4444",
                "0x8888", "0x2222", "0x1111", "0x4444",
                "0x8888", "0x2222", "0x1111", "0x4444",
                "0x8888", "0x2222", "0x1111", "0x4444"
              ]])

      in

    (* make a text window of the given size *)
      fun mkTextWin (root, win, font, size) = let
	    val TSZ{sz=SIZE{wid, ht}, rows, cols, char_ht, char_wid, ascent} = size
	    val (pen, highlighter, normalStipple, highlightStipple) = let
		  val black = blackOfScr(EXeneWin.screenOfWin win)
		  val white = whiteOfScr(EXeneWin.screenOfWin win)
                  val stipple = tile root "lightGray"
		  in
		    (newPen[PV_Foreground black, PV_Background white],
		     newPen[PV_Foreground white, PV_Background black],
		     newPen[
			PV_Foreground black, PV_FillStyle_Stippled, PV_Stipple stipple],
		     newPen[
			PV_Foreground white, PV_FillStyle_Stippled, PV_Stipple stipple])
		  end
	    fun ccToPt {row, col} =
		  {x = (col * char_wid) + pad, y = (row * char_ht) + pad}
	    fun drawText (clear, draw) {row, col, s} = let
		  val {x, y} = ccToPt {row=row, col=col}
		  in
		    clear (RECT{x=x, y=y, wid=char_wid*(String.size s), ht=char_ht});
		    draw (PT{x=x, y=y+ascent}, s)
		  end
            fun stipple {row, col, highlight} = let
		  val {x, y} = ccToPt {row=row, col=col}
		  val rect = (RECT{x=x, y=y, wid=char_wid, ht=char_ht})
		  in
		    if highlight
		      then fillRect (drawableOfWin win) highlightStipple rect
		      else fillRect (drawableOfWin win) normalStipple rect
		  end
	    val clrArea = clearArea (drawableOfWin win)
	    fun clearLn {row, start_col, end_col} = let
		  val {x, y} = ccToPt {row=row, col=start_col}
		  in
		    clrArea (RECT{
			x=x, y=y, wid=(end_col-start_col)*char_wid, ht=char_ht
		      })
		  end
	    fun clearBlk {start_row, end_row} = let
		  val {x, y} = ccToPt {row=start_row, col=0}
		  in
		    clrArea (RECT{
			x=x, y=y, wid=(wid-totPad), ht=(end_row - start_row)*char_ht
		      })
		  end
	    in
	      TW{
		  root = root,
		  win = win,
		  font = font,
		  rows = rows, cols = cols,
		  char_ht = char_ht, char_wid = char_wid, char_ascent = ascent,
		  draw_text = drawText (clrArea, drawString (drawableOfWin win) pen font),
		  highlight_text =  drawText (
		      fillRect (drawableOfWin win) pen, drawString (drawableOfWin win) highlighter font),
		  stipple = stipple,
		  clear_line = clearLn,
		  clear_blk = clearBlk,
		  char_blt = charBlt (win, size),
		  line_blt = lineBlt (win, size)
		}
	    end (* mkTextWin *)

    (* create a new text window descriptor to reflect a change in the window size *)
      fun resizeTextWin (TW{win, font, root, ...}, newSize) = mkTextWin (root, win, font, newSize)

    (* draw a string in normal mode at the given position *)
      fun drawNText {win=TW{draw_text, ...}, row, col, text} =
	    draw_text {row=row, col=col, s=text}

    (* draw a string in highlight mode at the given position *)
      fun drawHText {win=TW{highlight_text, ...}, row, col, text} =
	    highlight_text {row=row, col=col, s=text}

    (* stipple a normal mode character position*)
      fun stippleNChar {win=TW{stipple, ...}, row, col} =
	    stipple{row=row, col=col, highlight=false}

    (* stipple a highlight mode character position*)
      fun stippleHChar {win=TW{stipple, ...}, row, col} =
	    stipple{row=row, col=col, highlight=true}

    (* clear a character *)
      fun clearWinChar (TW{clear_line, ...}, ChrCrd{row, col}) =
	    clear_line {row = row, start_col = col, end_col = col+1}

    (* clear from a character position to the end of the line *)
      fun clearWinLn (TW{clear_line, cols, ...}, ChrCrd{row, col}) =
	    clear_line {row = row, start_col = col, end_col = cols}

    (* clear from a row to the end of the screen *)
      fun clearWin {win = TW{clear_blk, ...}, from, to} =
	    clear_blk {start_row = from, end_row = to}

    (* delete characters *)
      fun deleteWinChars (TW{clear_line, char_blt, cols,...}, ChrCrd{row, col}, cnt) = let 
          val eolcnt = cols - col - cnt
          in
              if eolcnt > 0 then let
                    val evt = char_blt {row=row,from=col+cnt,to=col,nchars=eolcnt}
                  in
	            clear_line {row=row, start_col = cols-cnt, end_col = cols};
                    CML.sync evt
                  end
              else (clear_line {row = row, start_col = col, end_col = cols}; [])
          end

    (* insert text *)
      fun insertWinText (tw, ChrCrd{row, col}, str, highlight) = let 
          val TW{draw_text, highlight_text, char_blt, cols,...} = tw
          val cnt = size str
          val txtfn = if highlight then highlight_text else draw_text
          val eolcnt = cols - col - cnt
          in
              if 0 >= eolcnt
	          then (txtfn {row=row, col=col, s=str}; [])
                  else let
                    val evt = char_blt {row=row,from=col,to=col+cnt,nchars=eolcnt}
                  in
	            txtfn {row=row, col=col, s=str};
                    CML.sync evt
                  end
          end

    (* scroll a region of text up; "from" is the bottom line of the text, "to"
     * is where "from" is move to, and "nlines" is the size of the block.
     *)
      fun scrollWinUp {win=TW{line_blt, clear_blk, ...}, from, to, nlines=0} = (
	    clear_blk {start_row=to+1, end_row=from+1}; [])
	| scrollWinUp {win=TW{line_blt, clear_blk, ...}, from, to, nlines} = let
	    val evt = line_blt {from=from-to, to=0, nlines=nlines}
	    in
	      clear_blk {start_row=to+1, end_row=from+1};
	      CML.sync evt
	    end

    (* scroll a region of text down; "from" is the top line of the text, "to"
     * is where "from" is moved to, and "nlines" is the size of the block.
     *)
      fun scrollWinDown {win=TW{clear_blk, ...}, from, to, nlines=0} = (
	    clear_blk {start_row=from, end_row=to}; [])
        | scrollWinDown {win=TW{line_blt, clear_blk, ...}, from, to, nlines} = let
	    val evt = line_blt {from=from, to=to, nlines=nlines}
	    in
	      clear_blk {start_row=from, end_row=to};
	      CML.sync evt
	    end

    (* delete a region of text; "from" is the start of the block, "nlines" is the
     * number of lines to delete.  The text below the delete block is scrolled up
     * to fill the space, with blank lines filling from the bottom.
     *)
      fun deleteWinLines {win=TW{rows, clear_blk, ...}, from, to, nlines=0} =
	    (clear_blk {start_row=from, end_row=rows}; [])
        | deleteWinLines {win=TW{rows, line_blt, clear_blk, ...}, from, to, nlines} = let
	    val evt = line_blt {from=to, to=from, nlines=nlines}
	    in
	      clear_blk {start_row=from+nlines, end_row=rows};
	      CML.sync evt
	    end

      end (* local *)
    end (* abstype text_win *)


  (*** The internal text widget state ***
   * The internal state of the text widget consists of the current size, a text
   * buffer, a text window and a cursor.
   *)
    datatype text = TXT of {
	size : text_sz,
	txt_buf : text_buf,
	txt_win : text_win,
	cursor : {is_on : bool, pos : char_coord}
      }

  (* draw the cursor *)
    fun drawCursor (TXT{txt_buf, txt_win, cursor={pos=ChrCrd{row, col}, ...}, ...}) = (
	  case (explodeRow{text=txt_buf, row=row, col=col, len=1})
	   of (""::_ ) => stippleHChar {win=txt_win, col=col, row=row}
	    | _ => stippleNChar {win=txt_win, col=col, row=row}
	  (* end case *))

  (* erase the cursor *)
    fun eraseCursor (TXT{txt_buf, txt_win, cursor={pos=ChrCrd{row, col}, ...}, ...}) = (
	  case (explodeRow{text=txt_buf, row=row, col=col, len=1})
	  of [] => clearWinChar (txt_win, ChrCrd{row=row, col=col})
	    | (""::s::_) => drawHText {win=txt_win, col=col, row=row, text=s}
	    | (" ":: _) => clearWinChar (txt_win, ChrCrd{row=row, col=col})
	    | (s::_) => drawNText {win=txt_win, col=col, row=row, text=s}
	  (* end case *))

  (* redraw damaged lines (but not the cursor) *)
    fun redrawText (TXT{size, txt_buf, txt_win, ...}, damage) = let
	  val TSZ{rows, cols, char_ht, char_wid, ...} = size
	  val damageVec = Array.array(rows, NONE)
	  fun mark (i, minCol, maxCol) = (case Array.sub(damageVec, i)
		 of NONE => Array.update(damageVec, i, SOME(minCol, maxCol))
		  | SOME(a, b) =>
		      Array.update(damageVec, i, SOME(min(minCol, a), max(maxCol, b))))
	  fun markDamage [] = ()
	    | markDamage (RECT{x, y, wid, ht} :: r) = let
		val topLn = Int.quot(y - pad, char_ht)
		val botLn =
		      Int.min(Int.quot((y - pad) + ht + (char_ht-1), char_ht), rows)
		val minC = Int.quot(x - pad, char_wid)
		val maxC =
		      Int.min(Int.quot((x - pad) + wid + (char_wid-1), char_wid), cols)
		fun f i = if (i < botLn) then (mark(i, minC, maxC); f(i+1)) else ()
		in
		  f topLn;
		  markDamage r
		end
	  fun redrawDamagedLines row = (
		case Array.sub(damageVec, row)
		 of NONE => ()
		  | SOME(minCol, maxCol) => let
		      val strs = explodeRow{
			      text=txt_buf, row=row, col=minCol, len=maxCol-minCol}
		      fun drawN (_, []) = ()
			| drawN (i, ""::r) = drawH(i, r)
			| drawN (i, s::r) = (
			    drawNText{win=txt_win, row=row, col=i, text=s};
			    drawH (i + String.size s, r))
		      and drawH (_, []) = ()
			| drawH (i, s::r) = (
			    drawHText{win=txt_win, row=row, col=i, text=s};
			    drawN (i + String.size s, r))
		      in
			drawN (minCol, strs)
		      end
		(* end case *);
		redrawDamagedLines (row+1))
	  in
(* TextIO.print "redraw start\n"; *)
	    markDamage damage;
	    (redrawDamagedLines 0) handle _ => ()
(* ;TextIO.print "redraw done\n" *)
	  end

  (* redraw (including the cursor) *)
    fun redraw (txt as TXT{cursor, ...}, damage) = (
	  redrawText (txt, damage);
	  case cursor of {is_on=true, pos} => drawCursor txt | _ => ())

  (* complete a area operation by redrawing any missing rectangles *)
    fun repair (_, []) = ()
      | repair (txt, rl) = redrawText (txt, rl)

  (* resize the text buffer and text window *)
    fun resize (TXT{txt_buf, txt_win, ...}, font, RECT{wid, ht, ...}) = let
	  val newSize = mkTextSz(SIZE{wid=wid, ht=ht}, font)
	  in
(** DO WE NEED TO REFRESH?? **)
(* TextIO.print "resize start\n"; *)
	    TXT{
		txt_buf = resizeTextBuf (txt_buf, newSize),
		txt_win = resizeTextWin (txt_win, newSize),
		size = newSize,
		cursor = {is_on = false, pos = ChrCrd{row=0, col=0}}
	      }
	  end (* resize *)


  (* return the size info of the widget state *)
    fun getInfo (TXT{size, ...}) = size

  (* return the cursor info of the widget state *)
    fun getCursorInfo (TXT{cursor, ...}) = cursor

  (* scroll the text from line "from" up "n" lines. *)
    fun scrollUp (txt, from, n) = let
	  val TXT{size=TSZ{rows, ...}, txt_buf, txt_win, cursor} = txt
	  val to = from - n
	  val blkSz = to + 1
	  val interfere = (case cursor
		 of {is_on=true, pos = ChrCrd{row, ...}} => (from >= row)
		  | _ => false)
	  in
	    if ((n > 0) andalso (~1 <= to) andalso (from < rows))
	      then if interfere
		then (
		  eraseCursor txt;
		  moveTextUp {text=txt_buf, from=from, to=to, nlines=blkSz};
		  repair (
		    txt, scrollWinUp {win=txt_win, from=from, to=to, nlines=blkSz});
		  drawCursor txt)
		else (
		  moveTextUp {text=txt_buf, from=from, to=to, nlines=blkSz};
		  repair (
		    txt, scrollWinUp {win=txt_win, from=from, to=to, nlines=blkSz}))
	      else ()
	  end (* scrollUp *)

  (* scroll the text starting at line "from" down "n" lines. *)
    fun scrollDown (txt, from, n) = let
	  val TXT{size=TSZ{rows, ...}, txt_buf, txt_win, cursor} = txt
	  val to = from + n
	  val blkSz = rows - to
	  val interfere = (case cursor
		 of {is_on=true, pos = ChrCrd{row, ...}} => (from <= row)
		  | _ => false)
	  in
	    if ((n > 0) andalso (0 <= from) andalso (to <= rows))
	      then if interfere
		then (
		  eraseCursor txt;
		  moveTextDown {text=txt_buf, from=from, to=to, nlines=blkSz};
		  repair (
		    txt, scrollWinDown {win=txt_win, from=from, to=to, nlines=blkSz});
		  drawCursor txt)
		else (
		  moveTextDown {text=txt_buf, from=from, to=to, nlines=blkSz};
		  repair (
		    txt, scrollWinDown {win=txt_win, from=from, to=to, nlines=blkSz}))
	      else ()
	  end (* scrollDown *)

  (* delete "nlines" starting from "from" *)
    fun deleteLines (txt, from, nlines) = let
	  val TXT{size=TSZ{rows, ...}, txt_buf, txt_win, cursor} = txt
	  val to = from + nlines
	  val blkSz = rows - to
	  val interfere = (case cursor
		 of {is_on=true, pos = ChrCrd{row, ...}} => (from <= row)
		  | _ => false)
	  in
	    if ((nlines > 0) andalso (0 <= from) andalso (to <= rows))
	      then if interfere
		then (
		  eraseCursor txt;
		  deleteText {text=txt_buf, from=from, nlines=nlines};
	          repair (
		    txt, deleteWinLines {win=txt_win, from=from, to=to, nlines=blkSz});
		  drawCursor txt)
                else (
		  deleteText {text=txt_buf, from=from, nlines=nlines};
	          repair (
		    txt, deleteWinLines {win=txt_win, from=from, to=to, nlines=blkSz}))
	      else ()
	  end

  (* clear from "pos" to the end of the line *)
    fun clearEOL (txt, pos as ChrCrd{row, col}) = let
	  val TXT{size, txt_buf, txt_win, cursor} = txt
	  val interfere = (case cursor
		 of {is_on=true, pos=ChrCrd{row=cr, col=cc}} =>
		      (cr = row) andalso (col <= cc)
		  | _ => false)
	  in
	    if (inTextWin (size, pos))
	      then (
		clearTextLn (txt_buf, pos);
		clearWinLn (txt_win, pos);
		if interfere then (drawCursor txt) else ())
	      else ()
	  end

  (* clear from "pos" to the end of the screen *)
    fun clearEOS (txt, pos as ChrCrd{row, col}) = let
	  val (pos as ChrCrd{row, ...}) = (if (col <> 0)
		then (clearEOL(txt, pos); ChrCrd{row=row+1, col=0})
		else pos)
	  val TXT{size as TSZ{rows, ...}, txt_buf, txt_win, cursor} = txt
	  val interfere = (case cursor
		 of {is_on=true, pos = ChrCrd{row=cr, ...}} => (row <= cr)
		  | _ => false)
	  in
	    if (inTextWin (size, pos))
	      then (
		clearText {text = txt_buf, from = row, to = rows};
		clearWin {win = txt_win, from = row, to = rows};
		if interfere then (drawCursor txt) else ())
	      else ()
	  end

  (* Will text drawing interfere with cursor? *)
    fun fixCursor (TXT{cursor={is_on=false, ...}, ...}, _, _) = ()
      | fixCursor (txt, ChrCrd{row, col}, str) = let
	  val TXT{cursor={pos=ChrCrd{row=cr, col=cc}, ...}, ...} = txt
	  in
	    if ((cr = row) andalso (cc >= col) andalso (cc < (col+String.size str)))
	      then drawCursor txt
	      else ()
	  end

  (* draw "str" at "pos" in normal mode *)
    fun writeStr (txt, pos as ChrCrd{row, col}, str) = let
	  val TXT{size, txt_buf, txt_win, ...} = txt
	  in
	    if (inTextWin (size, pos))
	      then let
		val str = clipString(size, col, str)
		in
		  writeNText (txt_buf, row, col, str);
		  drawNText {win=txt_win, row=row, col=col, text=str};
		  fixCursor (txt, pos, str)
		end
	      else ()
	  end

  (* draw "str" at "pos" in highlighted mode *)
    fun highlightStr (txt, pos as ChrCrd{row, col}, str) = let
	  val TXT{size, txt_buf, txt_win, ...} = txt
	  in
	    if (inTextWin (size, pos))
	      then let
		val str = clipString(size, col, str)
		in
		  writeHText (txt_buf, row, col, str);
		  drawHText {win=txt_win, row=row, col=col, text=str};
		  fixCursor (txt, pos, str)
		end
	      else ()
	  end

  (* Insert text at pos.
   *)
    fun insertText (txt, pos as ChrCrd{row, col}, str, highlight) = let
	  val TXT{size, txt_buf, txt_win, cursor} = txt
	  val interfere = (case cursor
		 of {is_on=true, pos=ChrCrd{row=cr, col=cc}} =>
		      (cr = row) andalso (col <= cc)
		  | _ => false)
	  in
	    if (inTextWin (size, pos))
	      then let
		val str = clipString(size, col, str)
                in
		  insertBufText (txt_buf, row, col, str, highlight);
		  repair(txt, insertWinText (txt_win, pos, str, highlight));
		  if interfere then (drawCursor txt) else ()
		end
	      else ()
	  end

  (* Delete cnt characters at position pos.
   * Fill with spaces on right.
   * Assume cnt > 0.
   *)
    fun deleteChars (txt, pos as ChrCrd{row, col}, cnt) = let
	  val TXT{size, txt_buf, txt_win, cursor} = txt
	  val interfere = (case cursor
		 of {is_on=true, pos=ChrCrd{row=cr, col=cc}} =>
		      (cr = row) andalso (col <= cc)
		  | _ => false)
	  in
	    if (inTextWin (size, pos))
	      then (
		deleteTextChars (txt_buf, row, col, cnt);
		repair(txt, deleteWinChars (txt_win, pos, cnt));
		if interfere then (drawCursor txt) else ())
	      else ()
	  end

    fun moveCursor (txt, newPos) = let
	  val TXT{size, txt_buf, txt_win, cursor as {is_on, pos}} = txt
	  in
	    if (inTextWin (size, newPos) andalso (pos <> newPos))
	      then let
		val newTxt = TXT{
			size = size, txt_buf = txt_buf, txt_win = txt_win,
			cursor = {is_on=is_on, pos=newPos}
		      }
		in
		  if is_on then (eraseCursor txt; drawCursor newTxt) else ();
		  newTxt
		end
	      else txt
	  end

    fun setCursor (txt as TXT{size, txt_buf, txt_win, cursor = {is_on, pos}}, on) = let
	  val newTxt = TXT{
		  size = size, txt_buf = txt_buf, txt_win = txt_win,
		  cursor = {is_on = on, pos = pos}
		}
	  in
	    case (is_on, on)
	     of (true, false) => (eraseCursor txt)
	      | (false, true) => (drawCursor newTxt)
	      | _ => ()
	    (* end case *);
	    newTxt
	  end


  (*** The text widget ***
   * The text widget is represented by a request/reply pair of communication
   * channels.
   *)

    datatype req_msg
      = GetInfo
      | GetCursorInfo
      | ScrollUp of {from : int, nlines : int}
      | ScrollDown of {from : int, nlines : int}
      | DeleteLines of {lnum : int, nlines : int}
      | ClearLine of char_coord
      | ClearScr of char_coord
      | WriteStr of {pos : char_coord, str : string}
      | HighlightStr of {pos : char_coord, str : string}
      | InsertText of {pos : char_coord, str : string, highlight : bool}
      | DeleteChars of {pos : char_coord, cnt : int}
      | MoveCursor of char_coord
      | SetCursor of bool

    datatype reply_msg
      = Info of text_sz
      | CursorInfo of {is_on : bool, pos : char_coord}

    datatype text_widget = TW of {
	widget : widget,
	query : req_msg -> reply_msg,
	cmd : req_msg -> unit
      }

  (* create a new text widget *)
    fun mkTextWidget root {rows : int, cols : int} = let
	  val rows = max(rows, 1) and cols = max(cols, 1)
	  val reqCh = CML.channel() and replyCh = CML.channel()
	  val reqEvt = CML.recvEvt reqCh
	  val font = Font.openFont (displayOf root) fontName
	  val (charHt, charWid, _) = fontInfo font
	  fun realize {env, win, sz} = let
		open Interact
		val InEnv{ci, co, ...} = ignoreInput env
		val tsz = mkTextSz (sz, font)
		val text = TXT{
			size = tsz,
			txt_buf = mkTextBuf tsz,
			txt_win = mkTextWin (root, win, font, tsz),
			cursor = {is_on = false, pos=ChrCrd{row=0, col=0}}
		      }
		fun server text = let
		      fun handleCI msg = (case (msgBodyOf msg)
			   of (CI_Redraw damage) => (
				redraw (text, damage);
				server text)
			    | (CI_Resize newR) =>
				server (resize (text, font, newR))
			    | (CI_OwnDeath) => CML.exit()
			    | _ => impossible("realize",
				  "[TextWidget: unexpected CI message]"))
		      fun handleReq (GetInfo) = (
			    CML.send (replyCh, Info(getInfo text));
			    server text)
			| handleReq (GetCursorInfo) = (
			    CML.send (replyCh, CursorInfo(getCursorInfo text));
			    server text)
			| handleReq (ScrollUp{from, nlines}) = (
			    scrollUp (text, from, nlines);
			    server text)
			| handleReq (ScrollDown{from, nlines}) = (
			    scrollDown (text, from, nlines);
			    server text)
			| handleReq (DeleteLines{lnum, nlines}) = (
			    deleteLines (text, lnum, nlines);
			    server text)
			| handleReq (ClearLine cc) = (
			    clearEOL (text, cc);
			    server text)
			| handleReq (ClearScr cc) = (
			    clearEOS (text, cc);
			    server text)
			| handleReq (HighlightStr{pos, str}) = (
			    highlightStr (text, pos, str);
			    server text)
			| handleReq (WriteStr{pos, str}) = (
			    writeStr (text, pos, str);
			    server text)
			| handleReq (InsertText{pos, str, highlight}) = (
			    insertText (text, pos, str, highlight);
			    server text)
			| handleReq (DeleteChars{pos, cnt}) = (
			    deleteChars (text, pos, cnt);
			    server text)
			| handleReq (MoveCursor cc) =
			    server (moveCursor (text, cc))
			| handleReq (SetCursor on) =
			    server (setCursor (text, on))
		      in
			CML.sync (CML.choose [
			    CML.wrap(ci, handleCI),
			    CML.wrap(reqEvt, handleReq)
			  ])
		      end
		in
		  XDebug.xspawn("textWidgetServer", fn () => server text);
		  ()
		end
	  in
	    TW{
		widget = mkWidget{
		    root = root,
                    args= fn () => {background = NONE}, 
		    boundsOf = fn () => {
			  x_dim = DIM{
			      base=totPad, incr=charWid, min=1, nat=cols, max=NONE
			    },
			  y_dim = DIM{
			      base=totPad, incr=charHt, min=1, nat=rows, max=NONE
			    }
		        },
		    realize = realize
		  },
		query = (fn req => (CML.send(reqCh, req); CML.recv replyCh)),
		cmd = (fn req => CML.send(reqCh, req))
	      }
	  end (* mkTextWidget *)

    fun widgetOf (TW{widget, ...}) = widget

    fun getInfo (TW{query, ...}) = (case (query GetInfo)
	   of (Info info) => info
	    | _ => impossible ("getInfo","[]"))

    fun charSizeOf tw = let val TSZ{rows, cols, ...} = getInfo tw
	  in
	    {rows=rows, cols=cols}
	  end
    fun sizeOf tw = let val TSZ{sz, ...} = getInfo tw
	  in
	    sz
	  end
    fun ptToCoord tw pt = let
	  val TSZ{sz, char_ht, char_wid, ...} = getInfo tw
	  val PT{x, y} = limitPt(sz, pt)
	  in
	    ChrCrd{
	        row = Int.quot(y - pad, char_ht),
		col = Int.quot(x - pad, char_wid)
	      }
	  end
    fun coordToRect tw (ChrCrd{row, col}) = let
	  val TSZ{char_wid, char_ht, rows, cols, ...} = getInfo tw
	  val row = if (row < 0) then 0 else if (row < rows) then row else (rows-1)
	  val col = if (col < 0) then 0 else if (col < cols) then col else (cols-1)
	  in
	    RECT{x=(col*char_wid)+pad, y=(row*char_ht)+pad, wid=char_wid, ht=char_ht}
	  end

    fun scrollUp (TW{cmd, ...}) arg = cmd (ScrollUp arg)
    fun scrollDown (TW{cmd, ...}) arg = cmd (ScrollDown arg)

    fun writeText (TW{cmd, ...}) {at, text} =
	  cmd (WriteStr{pos=at, str=text})
    fun highlightText (TW{cmd, ...}) {at: char_coord, text : string} =
	  cmd (HighlightStr{pos=at, str=text})

    fun insertLn (TW{cmd, ...}) {lnum, text} = (
	  cmd (ScrollDown{from=lnum, nlines=1});
	  cmd (WriteStr{pos=ChrCrd{row=lnum, col=0}, str=text}))

    fun insertText (TW{cmd, ...}) {at, text = ""} = ()
      | insertText (TW{cmd, ...}) {at, text } =
	  cmd (InsertText{pos=at, str=text, highlight=false})

    fun insertHighlightText (TW{cmd, ...}) {at: char_coord, text : string} =
	  cmd (InsertText{pos=at, str=text, highlight=true})

    fun deleteLn (TW{cmd, ...}) lnum = cmd (DeleteLines{lnum = lnum, nlines = 1})
    fun deleteLns (TW{cmd, ...}) arg = cmd (DeleteLines arg)
    fun deleteChars (TW{cmd, ...}) {at : char_coord, cnt : int} =
          if cnt > 0 then cmd (DeleteChars{pos=at, cnt = cnt})
          else ()

    fun clearToEOL (TW{cmd, ...}) coord = cmd (ClearLine coord)
    fun clearToEOS (TW{cmd, ...}) coord = cmd (ClearScr coord)
    fun clear (TW{cmd, ...}) = cmd (ClearScr(ChrCrd{col=0, row=0}))

    fun getCursorInfo (TW{query, ...}) = (case (query GetCursorInfo)
	   of (CursorInfo info) => info
	    | _ => impossible ("getCursorInfo","[]"))

    fun cursorPos tw = let val {pos, ...} = getCursorInfo tw
	  in
	    pos
	  end

    fun moveCursor (TW{cmd, ...}) pos = cmd(MoveCursor pos)
    fun cursorOn (TW{cmd, ...}) = cmd (SetCursor true)
    fun cursorOff (TW{cmd, ...}) = cmd (SetCursor false)

  end (* TextWidget *)
