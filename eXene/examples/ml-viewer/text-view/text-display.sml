(* text-display.sml
 *
 *)

functor TextDisplay (
    structure TextPool : TEXT_POOL
      sharing TextPool.TextCanvas = TextCanvas
  ) : TEXT_DISPLAY =
  struct

(* +DEBUG **
val tm = TraceCML.traceModule(XDebug.widgetsTM, "TextDisplay")
fun pr s = TraceCML.trace(tm, fn () => s)
fun prf (fmt, items) = TraceCML.trace(tm, fn () => [Format.format fmt items])
** -DEBUG *)
    structure W = TextPool.TextCanvas.W
    structure TextPool = TextPool

    structure TP = TextPool
    structure TC = TP.TextCanvas
    structure G = Geometry

    structure TC' : sig
       type typeball
       datatype typeball_val
         = TBV_Font of Font.font		(* font *)
         | TBV_Lineheight of int		(* total height of line *)
         | TBV_Ascent of int			(* height of line above baseline *)
         | TBV_Underline of bool		(* underline mode *)
         | TBV_Foregrnd of W.EXB.color_spec	(* forground (text) color *)
         | TBV_Backgrnd of W.EXB.color_spec	(* background color *)
         | TBV_Undergrnd of W.EXB.color_spec	(* color of underline *)
       datatype text_elem
         = Text of {tb : typeball, text : string}
         | Fill of {tb : typeball, chrWid : int, pixWid : int}
       datatype char_coord = CC of {row:int, col:int}
     end = TC
    open TC'

    datatype text_display = TD of {
	canvas : TC.text_canvas,
	text : TP.text_pool,
	size : G.size ref
      }

  (* *)
    fun mkTextDisplay {canvas, text, size} = TD{
	    canvas = canvas,
	    text = text,
	    size = ref size
	  }

  (* update the size of the display *)
    fun resize (TD{text, size, ...}, sz) = (size := sz; TP.resize(text, sz))

  (* return size *)
    fun sizeOf (TD{size, ...}) = !size

  (* return a typeball for the display *)
    fun mkTypeBall (TD{canvas, ...}, vl) = TC.mkTypeBall(canvas, vl)

  (* return the default typeball for the display. *)
    fun defaultTypeBall (TD{canvas, ...}) = TC.defaultTypeBall canvas

  (* copy a typeball, updating some attributes *)
    val copyTypeBall = TC.copyTypeBall

  (* Scroll a region vertically, returning the vacated rectangle and a list
   * of damaged rectangles that must be redrawn.  The region coordinates
   * are in pixels: "from" is the y-coord of the top of the region; "ht"
   * is the height of the region; and "to" is the y-coord of the new
   * top of the region.
   *)
    fun scrollV (td as TD{canvas, ...}) = let
	  val blt = TC.blt canvas
	  fun scroll {from, to, ht} = let
		val G.SIZE{wid, ...} = sizeOf td
		val damageEvt = blt {
			dst_pos = G.PT{x=0, y=to},
			src_rect = G.RECT{x=0, y=from, wid=wid, ht=ht}
		      }
		val (yv, hv) = if (from < to)
		      then (from, to-from)
		      else (to+ht, from-to)
		in
		  {vacated = G.RECT{x=0, y=yv, wid=wid, ht=hv}, damage = damageEvt}
		end
	  in
	    scroll
	  end

  (* Scroll a region horizontally, returning the vacated rectangle and a
   * list of damaged rectangles that must be redrawn.  The region coordinates
   * are in pixels: "from" is the x-coord of the l.h.s. of the region;
   * "wid" is the width of the region; and "to" is the x-coord of new
   * l.h.s. of the region.
   *)
    fun scrollH (td as TD{canvas, ...}) = let
	  val blt = TC.blt canvas
	  fun scroll {from, to, wid} = let
		val G.SIZE{ht, ...} = sizeOf td
		val damageEvt = blt {
			dst_pos = G.PT{x=to, y=0},
			src_rect = G.RECT{x=from, y=0, ht=ht, wid=wid}
		      }
		val (xv, wv) = if (from < to)
		      then (from, to - from)
		      else (to+wid, from-to)
		in
		  {vacated = G.RECT{x=xv, y=0, wid=wv, ht=ht}, damage = damageEvt}
		end
	  in
	    scroll
	  end

  (* Scroll the contents of a line horizontally. *)
    fun scrollLine (TD{canvas, text, ...}) = let
	  val blt = TC.blt canvas
	  fun scroll {from as CC{row, col}, to, wid} = let
		val G.RECT{x, y, ht, ...} = TP.coordToRect (text, from)
		val damageEvt = blt {
			dst_pos = G.PT{x=to, y=y},
			src_rect = G.RECT{x=x, y=y, ht=ht, wid=wid}
		      }
		val (xv, wv) = if (x < to)
		      then (x, to - x)
		      else (to+wid, x-to)
		in
		  {vacated = G.RECT{x=xv, y=y, wid=wv, ht=ht}, damage = damageEvt}
		end
	  in
	    scroll
	  end

  (* Scroll the text vertically so that the specified row is at the
   * top of the display (i.e., scroll the text up by the specified number
   * of rows.
   *)
    fun scrollUp (td as TD{text, ...}) = let
	  val scrollV = scrollV td
	  fun scroll row = let
		val from = TP.rowToY (text, row)
		val G.SIZE{ht, ...} = sizeOf td
		in
		  scrollV {from=from, to=0, ht=ht-from}
		end
	  in
	    scroll
	  end

  (* Scroll the text vertically so that the top of the screen occupies
   * the specified row (i.e., scroll the text down by the specified number
   * of rows).
   *)
    fun scrollDown (td as TD{text, ...}) = let
	  val scrollV = scrollV td
	  fun scroll row = let
		val to = TP.rowToY (text, row)
		val G.SIZE{ht, ...} = sizeOf td
		in
		  scrollV {from=0, to=to, ht=ht-to}
		end
	  in
	    scroll
	  end

  (* clear the specified rectangle *)
    fun clearRect (TD{canvas, ...}) = TC.clearRect canvas

  (* clear from the character coordinate to the end of its line *)
    fun clearToEOL' (clearRect, td as TD{text, ...}) = let
	  fun clear (cc as CC{row, col}) = let
		val G.PT{x, y} = TP.coordToPt(text, cc)
		val G.SIZE{wid, ...} = sizeOf td
		val ht = TP.getRowHt (text, row)
		in
		  clearRect (G.RECT{x=x, y=y, wid=wid-x, ht=ht})
		end
	  in
	    clear
	  end
    fun clearToEOL td = clearToEOL' (clearRect td, td)

  (* clear the lines [start..stop] *)
    fun clearLines' (clearRect, td as TD{text, ...}) = let
	  fun clear {start, stop} = let
		val y = TP.rowToY (text, start)
		val G.SIZE{wid, ...} = sizeOf td
		fun computeHt (row, ht) = if (row <= stop)
		      then computeHt (row+1, ht + TP.getRowHt (text, row))
		      else ht
		in
		  clearRect (G.RECT{x=0, y=y, wid=wid, ht=computeHt(start, 0)})
		end
	  in
	    clear
	  end
    fun clearLines td = clearLines' (clearRect td, td)

  (* Clear the area from the coordinate start to the coordinate stop. *)
    fun clearArea (td as TD{text, ...}) = let
	  val clearRect = clearRect td
	  val clearToEOL = clearToEOL' (clearRect, td)
	  val clearLines = clearLines' (clearRect, td)
	  fun clear {start as CC{row=r1, col=c1}, stop=CC{row=r2, col=c2}} =
		if (r1 < r2)
		  then let
		    val r1 = if (c1 > 0) then (clearToEOL start; r1+1) else r1
		    val G.PT{x, y} = TP.coordToPt (text, CC{row=r2, col=c2+1})
		    in
		      if (r1 < r2) then clearLines {start=r1, stop=r2-1} else ();
		      clearRect (G.RECT{
			  x=0, y=y, ht=TP.getRowHt(text, r1), wid=x
			})
		    end
		else if (r1 = r2) andalso (c1 <= c2)
		  then let
		    val G.PT{x=x1, y} = TP.coordToPt(text, start)
		    val x2 = TP.coordToX(text, CC{row=r1, col=c2+1})
		    in
		      clearRect (G.RECT{
			  x=x1, y=y, ht=TP.getRowHt(text, r1), wid=x2-x1
			})
		    end
		  else ()
	  in
	    clear
	  end

  (* redraw the damaged region *)
    fun redraw (TD{text, size, ...}) damageRects = let
	  val numRows = TP.numRows text
	(* redraw the whole canvas *)
	  fun drawAll () = let
	        val getRow = TP.getRow text
		fun draw i = if (i < numRows)
		      then (TC.draw(getRow i); draw(i+1))
		      else ()
		in
		  draw 0
		end
	(* redraw the damaged regions *)
	  fun redraw' rects = let
		val getText = TP.getText text
		fun pixelRngToRowRng (y1, y2) = TP.pixelRngToRowRng(text, y1, y2)
		fun pixelRngToColRng (row, x1, x2) =
		      TP.pixelRngToColRng(text, row, x1, x2)
		val damage = Array.array(numRows, [])
		fun min (a:int, b) = if (a < b) then a else b
		fun max (a:int, b) = if (a > b) then a else b
		fun union (row, x1, x2) = let
		      fun ins [] = [(x1, x2)]
			| ins ((rng as (x1', x2')) :: r) = if (x2 < x1')
			      then (x1, x2)::rng::r
			    else if (x2' < x1)
			      then rng::(ins r)
			      else (min(x1, x1'), max(x2, x2')) :: r
		      in
		        Array.update(damage, row, ins (Array.sub (damage, row)))
		      end
	      (* for each rectangle, compute the affected rows and add the
	       * rectangle's span to the damaged pixel intervals.
	       *)
		fun markPixelDamage ([], minRow, maxRow) = (minRow, maxRow)
		  | markPixelDamage (G.RECT{x, y, wid, ht} :: rest, minRow, maxRow) =
		      let
		      val (r1, r2) = pixelRngToRowRng (y, y+ht-1)
		      val start = x and stop = x+wid-1
		      fun mark row = if (row <= r2)
			    then (union (row, start, stop); mark(row+1))
			    else ()
		      in
			mark r1;
		        markPixelDamage (rest, min(r1, minRow), max(r2, maxRow))
		      end
		val (minRow, maxRow) = markPixelDamage (rects, numRows, ~1)
	      (* for each damaged row, compute the damaged region in
	       * character coordinates, and redraw.
	       *)
		fun draw row = if (row <= maxRow)
		      then (case (Array.sub(damage, row))
			 of [] => ()
			  | [(x1, x2)] => let
			      val (c1, c2) = pixelRngToColRng (row, x1, x2)
			      in
				TC.draw (getText {row=row, start=c1, stop=c2})
			      end
			  | ((x1, x2)::r) => let
			      val (c1, c2) = pixelRngToColRng (row, x1, x2)
			      fun cvt (start, stop, []) =
				    TC.draw (getText {row=row, start=c1, stop=c2})
				| cvt (start, stop, (x1, x2)::r) = let
				    val (c1, c2) = pixelRngToColRng (row, x1, x2)
				    in
				      if (stop < (c1-1))
					then (
					  TC.draw (getText {
					      row=row, start=start, stop=stop
					    });
					  cvt (c1, c2, r))
					else
					  cvt (start, c2, r)
				    end
			      in
				cvt (c1, c2, r)
			      end
			(* end case *);
			draw (row+1))
		      else ()
		in
		  draw minRow
		end
	  in
	    case damageRects
	     of [G.RECT{x=0, y=0, wid, ht}] => let
		  val G.SIZE{wid=w, ht=h} = !size
		  in
		    if ((wid = w) andalso (h = ht))
		      then drawAll ()
		      else redraw' damageRects
		  end
	      | _ => redraw' damageRects
	    (* end case *)
	  end (* redraw *)

  end (* TEXT_DISPLAY *)
