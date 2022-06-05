(* text-canvas.sml
 *
 * NOTE: one optimization might be to exploit the situation in which a pen uses
 * the default background.  This can be done when redrawing text, and when filling
 * the background.
 *)

structure TextCanvas : TEXT_CANVAS =
  struct
    structure W : WIDGET = Widget
    structure EXB : EXENE_BASE = W.EXB
    structure G = Geometry

(* +DEBUG **
val tm = TraceCML.traceModule(XDebug.widgetsTM, "TextCanvas")
fun pr s = TraceCML.trace(tm, fn () => s)
fun prf (fmt, items) = TraceCML.trace(tm, fn () => [Format.format fmt items])
** -DEBUG *)

  (* a text canvas is a proto-widget for drawing text *)
    datatype text_canvas = TextCanvas of {
	win : EXB.window,		(* the window *)
	drawable : Drawing.drawable,	(* the drawable surface *)
	font : Font.font,		(* the default font *)
	foregrnd : EXB.color,		(* the default foreground color *)
	backgrnd : EXB.color,		(* the default background color *)
	dflt_pen : Drawing.pen		(* the default pen *)
      }

    fun mkTextCanvas {win, size, font, foregrnd, backgrnd} = let
	  val scr = EXeneWin.screenOfWin win
	  fun defaultColor (NONE, colorFn) = colorFn scr
	    | defaultColor (SOME c, _) = EXB.colorOfScr scr c
	  val foregrnd = defaultColor(foregrnd, EXB.blackOfScr)
	  val backgrnd = defaultColor(backgrnd, EXB.whiteOfScr)
	  in
	    EXeneWin.changeWinAttrs win [
		EXeneWin.WA_Background_Color backgrnd,
		EXeneWin.WA_BitGravity EXeneBase.NorthWestGravity
	      ];
	    TextCanvas{
		win = win,
		drawable = Drawing.drawableOfWin win,
		font = font,
		foregrnd = foregrnd,
		backgrnd = backgrnd,
		dflt_pen = Drawing.newPen [
		    Drawing.PV_Foreground foregrnd, Drawing.PV_Background backgrnd
		  ]
	      }
	  end

  (* clear a canvas to its background color *)
    fun clear (TextCanvas{drawable, ...}) = Drawing.clearDrawable drawable

  (* specifies canvas, font, color, etc. for writing text *)
    datatype typeball = TB of {
	  drawable : Drawing.drawable,	(* the text display that this typeball is *)
					(* associated with. *)
	  foreColor : EXB.color_spec,	(* the foreground color *)
	  fore : Drawing.pen,		(* pen to draw foreground *)
	  backColor : EXB.color_spec,	(* the foreground color *)
	  back : Drawing.pen,		(* pen to draw background *)
	  under : Drawing.pen option,	(* pen to draw underline; if enabled *)
	  font : Font.font,		(* font used *)
	  line_ht : int,		(* the height the line *)
	  ascent : int
      }

    datatype typeball_val
      = TBV_Font of Font.font		(* font *)
      | TBV_Lineheight of int		(* total height of line *)
      | TBV_Ascent of int		(* height of line above baseline *)
      | TBV_Underline of bool		(* underline mode *)
      | TBV_Foregrnd of EXB.color_spec	(* forground (text) color *)
      | TBV_Backgrnd of EXB.color_spec	(* background color *)
      | TBV_Undergrnd of EXB.color_spec	(* color of underline *)

  (* create a new typeball *)
    fun mkTypeBall (TextCanvas{win, drawable, font, foregrnd, backgrnd, ...}, vl) =
	  let
	  val font = ref font
	  val foreColor = ref EXB.black
	  val fore = ref foregrnd
	  val backColor = ref EXB.white
	  val back = ref backgrnd
	  val lineHt = ref NONE
	  val ascent = ref NONE
	  val underline = ref false
	  val under = ref foregrnd
	  val colorOf = EXB.colorOfScr (EXeneWin.screenOfWin win)
	  fun doVal (TBV_Font f) = font := f
	    | doVal (TBV_Lineheight n) = lineHt := SOME n
	    | doVal (TBV_Ascent n) = ascent := SOME n
	    | doVal (TBV_Underline b) = underline := b
	    | doVal (TBV_Foregrnd c) = (foreColor := c; fore := colorOf c)
	    | doVal (TBV_Backgrnd c) = (backColor := c; back := colorOf c)
	    | doVal (TBV_Undergrnd c) = under := colorOf c
	  val _ = app doVal vl
	  fun mkPen (f, b) = Drawing.newPen[
		  Drawing.PV_Function Drawing.OP_Copy,
		  Drawing.PV_Foreground(!f), Drawing.PV_Background(!b)
		]
	  val forePen = mkPen(fore, back)
	  val underPen = if !underline
		then if EXB.sameColor(!fore, !under)
		  then SOME forePen
		  else SOME(mkPen(under, back))
		else NONE
	  val (fontHt, fontAscent) = let
		val {ascent, descent} = Font.fontHt (!font)
		in
		  (ascent+descent, ascent)
		end
	  fun max (NONE, x : int) = x
	    | max (SOME x, y) = if (x > y) then x else y
	  in
	    TB{
		drawable = drawable,
		foreColor = !foreColor,
		fore = forePen,
		backColor = !backColor,
		back = mkPen(back, fore),
		under = underPen,
		font = !font,
		line_ht = max(!lineHt, fontHt),
		ascent = max(!ascent, fontAscent)
	      }
	  end

  (* return the default typeball for the canvas *)
    fun defaultTypeBall txtCanvas = mkTypeBall(txtCanvas, [])

  (* copy a typeball, updating some attributes *)
    fun copyTypeBall (TB{...}, vl) = raise Fail "unimplemented"

    datatype text_elem
      = Text of {tb : typeball, text : string}
      | Fill of {tb : typeball, chrWid : int, pixWid : int}

    datatype char_coord = CC of {row:int, col:int}

  (* return the width (in pixels) of a text element *)
    fun pixWidthOf (Text{tb=TB{font, ...}, text}) = Font.textWidth font text
      | pixWidthOf (Fill{pixWid, ...}) = pixWid

  (* return the width (in characters) of a text element *)
    fun chrWidthOf (Text{text, ...}) = size text
      | chrWidthOf (Fill{chrWid, ...}) = chrWid

  (* return the width of a text string using the given typeball *)
    fun textWidth (TB{font, ...}) = Font.textWidth font

  (* return the substring of a text element *)
    fun substr (Text{tb, text}, i, n) =
	  Text{tb=tb, text=String.substring(text, i, n)}
      | substr (Fill{tb, chrWid, pixWid}, i, n) =
	  if ((i < 0) orelse (n < 0) orelse (chrWid < i+n))
	    then raise String.Substring
	    else Fill{tb=tb, chrWid=n-i, pixWid=(pixWid*(n-i)) quot chrWid}

  (* return the font of a typeball *)
    fun fontOf (TB{font, ...}) = font

  (* do a copyBlt on the canvas *)
    fun blt (TextCanvas{drawable, dflt_pen, ...}) =
	  Drawing.copyBltEvt drawable dflt_pen

  (* clear the specified rectangle to the background color *)
    fun clearRect (TextCanvas{drawable, ...}) = Drawing.clearArea drawable

    fun imageStr (TB{drawable, font, fore, ...}) =
	  Drawing.imageString drawable fore font

    fun fill (TB{drawable, back, line_ht, ascent, ...}) = let
	  val draw = Drawing.fillRect drawable back
	  in
	    fn (G.PT{x, y}, wid) =>
	      draw (G.RECT{x=x, y=y-ascent, wid=wid, ht=line_ht})
	  end

(** What about background??? **)
    fun draw {at=G.PT{x, y}, elems} = let
	  fun drawIt ([], _) = ()
	    | drawIt (Text{tb, text} :: r, x) = (
		imageStr tb (G.PT{x=x, y=y}, text);
		drawIt (r, x + textWidth tb text))
	    | drawIt (Fill{tb, pixWid, ...} :: r, x) = (
		fill tb (G.PT{x=x, y=y}, pixWid);
		drawIt (r, x+pixWid))
	  in
	    drawIt (elems, x)
	  end

    fun drawText tb = let
	  val draw = imageStr tb
          in
	    fn {at, text} => draw(at, text)
	  end

    fun drawFill tb = let
	  val draw = fill tb
	  in
	    fn {at, wid} => draw (at, wid)
	  end

(**
  (* Cursors *)
    datatype text_cursor
      = NoCursor
      | BoxCursor of ??
      | OutlineCursor of ??
      | CaretCursor of ??
      | BarCursor of ??
      | XtermCursor of ??
      | GlyphCursor of ??

    fun setCursor : (text_canvas * text_cursor) -> unit
	(* set the type of the cursor *)

    fun moveCursor : (text_canvas * char_coord) -> unit
	(* set the current cursor position *)

    fun cursorOn (TextCanvas{...}) = ??
	(* enable display of the text cursor *)

    fun cursorOff (TextCanvas{...}) = ??
	(* disable display of the text cursor *)
**)

  end; (* structure TextCanvas *)
