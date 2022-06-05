(* view-buffer.sml
 *
 * This is the buffer (text-pool) for the viewer.
 *)

structure VDebug =
  struct
    local open TraceCML in

    val tm = traceModule(XDebug.eXeneTM, "viewer")

    fun pr s = trace (tm, fn () => s)
    fun prf (s, fmt) = trace (tm, fn () => [Format.format s fmt])

    end (* local *)
  end;

structure ViewBuffer =
  struct

    structure G = Geometry
    structure TextCanvas = TextCanvas
    structure TC = TextCanvas (* local name *)

    type typeball = TC.typeball

  (* the different kinds of displayed objects *)
    datatype token_kind
      = Comment
      | Keyword
      | Symbol
      | Ident

    datatype line = LN of {
	len : int,
	elems : {space:int, kind:token_kind, text:string} list
      }

    datatype text_pool = TP of {
	lines : line Vector.vector,
	view : {
	    start : int,	(* the first visible line *)
	    stop : int,		(* the last visible line *)
	    ht : int,		(* the height of the view area in lines; *)
				(* (start+ht-1) >= stop. *)
	    maxCols : int	(* the widest visible line *)
	  } ref,
	char_wid : int,
	ascent : int,
	descent : int,
	line_ht : int,
	font : Font.font,
	fill_tb : typeball,
	comment_tb : typeball,
	keyword_tb : typeball,
	symbol_tb : typeball,
	ident_tb : typeball
      }

  (* establish the view parameters *)
    fun mkView (lines, start, nrows) = let
	  val nLines = Vector.length lines
	  val start = if (start < 0) then 0
		else if (start < nLines) then start
		else (nLines - 1)
	  val maxRow = min (nLines, start + nrows)
	  fun maxWid (i, m) = if (i < maxRow)
		then let
		  val LN{len, ...} = Vector.sub(lines, i)
		  in
		    maxWid (i+1, if (m < len) then len else m)
		  end
		else m
	  in
	    { start = start, stop = maxRow-1, ht = nrows,
	      maxCols = maxWid (start, 0)
	    }
	  end

  (* return the size of the viewed buffer and the current view *)
    fun getView (TP{lines, view=ref{start, ht, ...}, ...}) =
	  {view_start = start, view_ht = ht, nlines = Vector.length lines}

  (* set the top of the view *)
    fun setViewTop (TP{lines, view as ref{start, ht, ...}, ...}, newTop) =
	  view := mkView (lines, newTop, ht)

    fun mkViewBuffer {
	  src, nrows, font, char_wid, ascent, descent, line_ht,
	  fill_tb, comment_tb, keyword_tb, symbol_tb, ident_tb
	} = let
	  fun mkLn l = let
		fun len ([], n) = n
		  | len ({space, kind, text}::r, n) = len(r, n+space+(size text))
		in
		  LN{elems = l, len = len(l, 0)}
		end
	  val lines = Vector.vector(map mkLn src)
	  in
	    TP{
		lines = lines,
		view = ref (mkView (lines, 0, nrows)),
		font = font,
		char_wid = char_wid,
		ascent = ascent,
		descent = descent,
		line_ht = line_ht,
		fill_tb = fill_tb,
		comment_tb = comment_tb,
		keyword_tb = keyword_tb,
		symbol_tb = symbol_tb,
		ident_tb = ident_tb
	      }
	  end

    fun getLine (TP{lines, view = ref{start, ht, ...}, ...}, n) = let
(*val _ = VDebug.prf("getLine: n = %d\n", [Format.INT n])*)
	  val (LN{elems, ...}) = Vector.sub(lines, n + start)
	  in
	    elems
	  end
	    handle _ => []

    fun mkFill (TP{char_wid, fill_tb, ...}, nChars) =
	  TC.Fill{tb=fill_tb, chrWid=nChars, pixWid=(nChars*char_wid)}

    fun charWid {space, kind, text} = space + size text

    fun pixWid (TP{char_wid, font, ...}, {space, kind, text}) =
	  (space * char_wid) + (Font.textWidth font text)

  (* extract the appropriate typeball from a textpool *)
    fun getTypeBall (TP{comment_tb, ...}, Comment) = comment_tb
      | getTypeBall (TP{keyword_tb, ...}, Keyword) = keyword_tb
      | getTypeBall (TP{symbol_tb, ...}, Symbol) = symbol_tb
      | getTypeBall (TP{ident_tb, ...}, Ident) = ident_tb

  (* resize the view *)
    fun resize (TP{lines, line_ht, view as ref {start, ...}, ...}, G.SIZE{ht, ...}) =
	  view := mkView(lines, start, ht quot line_ht)

  (* return the number of rows *)
(*    fun numRows (TP{view = ref {start, stop, ...}, ...}) = (stop - start) + 1*)
    fun numRows (TP{view = ref {ht, ...}, ...}) = ht

  (* return the maximum number of displayed columns in any row *)
    fun maxCols (TP{view = ref {maxCols, ...}, ...}) = maxCols

  (* return the text of a given row *)
    fun getRow (tp as TP{lines, char_wid, fill_tb, line_ht, ascent, ...}) n = let
	  fun mk ([], l) = rev l
	    | mk ({space=0, kind, text} :: r, l) =
		mk (r, TC.Text{tb=getTypeBall(tp, kind), text=text} :: l)
	    | mk ({space, kind, text} :: r, l) =
		mk (r, TC.Text{tb=getTypeBall(tp, kind), text=text}
		  :: mkFill(tp, space) :: l)
	  in
	    { at = G.PT{x = 0, y = n*line_ht + ascent},
	      elems = mk (getLine(tp, n), [])
	    }
	  end

  (* return the text elements in the given row between the start and
   * stop character positions (inclusive), along with the origin of
   * the first element.
   *)
    fun getText (tp as TP{char_wid, ascent, line_ht, font, ...}) {row, start, stop} =
	  let
	  val nChars = (stop - start) + 1
	(* scan1 finds the start of the interval *)
	  fun scan1 ([], col, x) = (x, [])
	    | scan1 ((item as {space, kind, text})::r, col, x) = let
		val w = charWid item
		in
		  if (w <= col)
		    then scan1 (r, col - w, x + pixWid(tp, item))
		    else scan2 (item, r, col, x)
		end
	(* scan2 returns the list of text elements that comprise the interval *)
	  and scan2 ({space, kind, text}, elems, col, x) = let
		fun mk (_, 0, l) = l
		  | mk ([], n, l) = mkFill(tp, n) :: l
		  | mk ({space, kind, text}::r, n, l) = if (space < n)
		      then let
			val (l, n) = if (space = 0)
			      then (l, n)
			      else (mkFill(tp, space) :: l, n-space)
			val len = size text
			val tb = getTypeBall(tp, kind)
			in
			  if (len < n)
			    then mk (r, n - len, TC.Text{tb=tb, text=text} :: l)
			    else TC.Text{tb=tb, text=substring(text, 0, n)} :: l
			end
		      else mkFill(tp, n) :: l
		val (col, x, nChars, fill) = if (space > col)
		      then let
			val nSpaces = space-col
			in
			  ( 0, x+char_wid*col, nChars-nSpaces,
			    [mkFill(tp, nSpaces)]
			  )
			end
		      else (col-space, x+char_wid*space, nChars, [])
		val (x, item) = if (col > 0)
		      then let
			val w = Font.substrWidth font (text, 0, col)
			in
			  ( x+w, {
			        space=0, kind=kind,
				text=substring(text, col, (size text) - col)
			      }
			  )
			end
		      else (x, {space=0, kind=kind, text=text})
		in
		  (x, mk(item::elems, nChars, fill))
		end
	  val (x, textElems) = scan1(getLine(tp, row), start, 0)
	  in
(* +DEBUG **
let fun prElem (TC.Text{text, ...}) = VDebug.pr["T<", text, ">"]
      | prElem (TC.Fill{chrWid, ...}) =
	  VDebug.pr["F<", Makestring.padLeft("", chrWid), ">"]
in
VDebug.prf("getText(%2d) [%d..%d] = \"", [
    Format.INT row, Format.INT start, Format.INT stop
  ]);
revapp prElem textElems;
VDebug.prf("\" @ (%d, %d)\n", [Format.INT x, Format.INT(row*line_ht + ascent)])
end;
** -DEBUG *)
	    {at = G.PT{x = x, y = row*line_ht + ascent}, elems = rev textElems}
	  end

  (* return the height of the given row *)
    fun getRowHt (TP{line_ht, ...}, _) = line_ht

  (* return the ascent and descent of the given row *)
    fun getRowScent (TP{ascent, descent, ...}, _) = {ascent=ascent, descent=descent}

  (* return the y-coordinate of a row's baseline; this is the same as
   * the y-coordinate (rowToY) plus the ascent.
   *)
    fun baselineOfRow (TP{ascent, line_ht, ...}, row) = (row*line_ht + ascent)

  (* return the y-coordinate of the top of a row. *)
    fun rowToY (TP{line_ht, ...}, row) = row*line_ht

  (* return the x-coordinate of a character coordinate *)
    fun coordToX (tp as TP{char_wid, font, ...}, TC.CC{row, col}) = let
	  val textWidth = Font.textWidth font
	  fun findCol ([], _, x) = x (* ?? *)
	    | findCol ({space, kind, text}::r, col, x) =
		if (col <= space)
		  then x + char_wid*col
		else let
		    val col = col - space and x = x + char_wid*space
		    val n = size text
		    in
		      if (col < n)
			then x + (Font.substrWidth font (text, 0, col))
			else findCol (r, col-n, x + textWidth text)
		    end
	  in
	    findCol (getLine (tp, row), col, 0)
	  end

  (* map a character coordinate to the pixel origin of the specified
   * character cell.
   *)
    fun coordToPt (arg as (TP{line_ht, ...}, TC.CC{row, ...})) =
	  G.PT{x = coordToX arg, y = row*line_ht}

  (* map a character coordinate into a rectangle bounding its contents *)
    fun coordToRect (tp as TP{font, char_wid, line_ht, ...}, TC.CC{row, col}) = let
	  val textWidth = Font.textWidth font
	  val substrWid = Font.substrWidth font
	  fun findCol ([], _, x) = (x, 0) (* ?? *)
	    | findCol ({space, kind, text}::r, col, x) =
		if (col < space)
		  then (x + char_wid*col, char_wid)
		  else let
		    val col = col - space and x = x + char_wid*space
		    val n = size text
		    in
		      if (col < n)
			then (
			    x + (substrWid (text, 0, col)),
			    substrWid (text, col, 1)
			  )
			else findCol (r, col-n, x + textWidth text)
		    end
	  val (x, w) = findCol (getLine(tp, row), col, 0)
	  in
	    G.RECT{x=x, y=(row*line_ht), ht=line_ht, wid=w}
	  end

  (* map a character coordinate onto the corresponding single-character 
   * typeballed type element
   *)
    fun coordToElem (tp, TC.CC{row, col}) = let
	  fun scan ([], _) = mkFill(tp, 1)
	    | scan ({space, kind, text}::r, i) = if (i < space)
		  then mkFill(tp, 1)
		else if (i < size text)
		  then TC.Text{tb=getTypeBall(tp, kind), text=substring(text, i, 1)}
		  else scan(r, i - size text)
	  in
	    scan (getLine (tp, row), col)
	  end

  (* given a row and x-coordinate, return the full character coordinate *)
    fun xPosToCoord (tp as TP{char_wid, font, ...}, row, x) = let
	  val textWidth = Font.textWidth font
	  fun findX ([], col, _) = col
	    | findX ({space, kind, text}::r, col, x) = let
		fun scanText (col, x) = let
		      val wid = textWidth text
		      fun scan ([], _) = MLXError.impossible "Viewer.xPosToCoord"
			| scan (w::r, col) =
			    if (x < w) then col else scan(r, col+1)
		      in
			if (x < wid)
			  then scan (tl(Font.charPositions font text), col)
			  else findX (r, col + size text, x - wid)
		      end
		fun scanSpace (0, col, x) = scanText (col, x)
		  | scanSpace (space, col, x) = if (x < char_wid)
		      then col
		      else scanSpace (space-1, col+1, x-char_wid)
		in
		  scanSpace (space, col, x)
		end
	  in
	    TC.CC{row=row, col=findX (getLine(tp, row), 0, x)}
	  end

  (* given an inclusive range of pixels in the y-dimension, return the
   * minimum inclusive range of rows covered by the pixel range.
   *)
    fun pixelRngToRowRng (TP{line_ht, view = ref{ht, ...}, ...}, y1, y2) =
	  (y1 quot line_ht, min(ht-1, y2 quot line_ht))

  (* given a row and an inclusive range of pixels in the x-dimension,
   * return the minimum inclusive range of columns covered in the
   * row by the pixel range.
   *)
(** NOTE: this should be made more efficient!!! **)
    fun pixelRngToColRng (tp, row, x1, x2) = let
	  val TC.CC{col=c1, ...} = xPosToCoord (tp, row, x1)
	  val TC.CC{col=c2, ...} = xPosToCoord (tp, row, x2)
	  in
	    (c1, c2)
	  end

  (* map a point to a character coordinate *)
    fun ptToCoord (tp as TP{line_ht, ...}, G.PT{x, y}) =
	  xPosToCoord(tp, y quot line_ht, x)

  end; (* ViewBuffer *)
