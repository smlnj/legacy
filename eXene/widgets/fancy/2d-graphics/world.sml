(* world.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This is the internal representation of the structured graphics
 * canvas.
 *)

structure World : WORLD =
  struct

    structure W = Widget
    structure EXB = W.EXB
    structure C = Canvas
    structure D = Drawing
    structure G = Geometry
    structure R = Region
    structure V = WorldView
    structure SB = Scrollbar
    structure IntMap = IntBinaryMap

    type box = {minX : real, maxX : real, minY : real, maxY : real}

  (* this should be part of the Region structure *)
    fun mkRegion [] = R.empty
      | mkRegion [r] = R.rectangle r
      | mkRegion (r::rr) = List.foldr
	  (fn (r, rg) => R.union(R.rectangle r, rg))
	    (R.rectangle r)
	      rr

  (* specification of a width; either in absolute pixels or in universal
   * coordinates.
   *)
    datatype width_spec
      = WID_Pixel of int
(****
      | WID_UCoord of real
****)

  (* specification of an arrow head *)
    datatype arrow_spec
      = NoArrow
      | Arrow of {len : real, wid : real}

  (* the different ways to specify a border; either an absolute number of
   * pixels, or proportional border specified in universal coordinate units.
   *)
    datatype border_spec
      = BORDER_None
      | BORDER_Pixel2D of (EXB.color * int)
(****
      | BORDER_Pixel3D of {
	    relief : ThreeD.relief,
	    wid : int,
	    light : EXB.color,
	    dark : EXB.color
	  }
      | BORDER_UCoord of (EXB.color * real)
****)

    type object_id = int

  (* an object that is (at least) partially visible on the canvas *)
    datatype 'a vis_object = VObj of {
	obj : 'a object,
	region : R.region,
	erase : unit -> unit,
	draw : unit -> unit
      }

    and 'a object = Obj of {
	id : object_id,
	desc : object_desc,
	depth : int,
	info : 'a option,
	visible : bool ref
      }

    and object_desc
      = OBJ_LINE of {
	    pts : {x : real, y : real} list,
	    head : {x : real, y : real} arrow_desc,
	    tail : {x : real, y : real} arrow_desc,
	    color : EXB.color,
	    width : width_spec
	  }
      | OBJ_RECT of {
	    x : real, y : real,
	    wid : real, ht : real,
	    fill : EXB.color option,
	    border : border_spec
	  }
      | OBJ_ELLIPSE of {
	    x : real, y : real,
	    wid : real, ht : real,
	    fill : EXB.color option,
	    border : border_spec
	  }
      | OBJ_DIAMOND of {
	    x : real, y : real,
	    wid : real, ht : real,
	    fill : EXB.color option,
	    border : border_spec
	  }
      | OBJ_TEXT of {
	    x : real, y : real,
	    wid : real, ht : real,	(* the size of the text when drawn in it *)
					(* natural size and transformed to *)
					(* unscaled world coordinates (i.e., *)
					(* inches). *)
	    text : string,
	    color : EXB.color,
	    ptSz : int,
	    fontStyle : ScalableFont.font_style
	  }

    and 'a arrow_desc
      = NO_ARROW
      | ARROW of {
	    p0 : 'a,
	    p1 : 'a,	(* this is the tip of the arrow *)
	    p2 : 'a
	  }

  (* Sort objects by depth, with deeper objects first.  Note: the sorting
   * function takes a ">" operator and sorts in nondecreasing order.
   *)
    fun sortVisObjs l = let
	  fun depth (VObj{obj=Obj{depth, ...}, ...}) = depth
	  in
	    ListMergeSort.sort (fn (obj1, obj2) => (depth obj1 < depth obj2)) l
	  end


  (** sets of visible objects, keyed by object ID **)

    fun visObjId (VObj{obj=Obj{id, ...}, ...}) = id

    fun insertObj (map, obj) = IntMap.insert(map, visObjId obj, obj)

    fun union (map1, map2) = IntMap.unionWith #2 (map1, map2)

    val emptyMap = IntMap.empty

  (* compute the region covered by a list of objects *)
    fun regionOfObjs objs =
	  List.foldr (fn (VObj{region, ...}, rg) => R.union(region, rg))
	    R.empty
	      objs

  (* append the objects in the set s to the list l *)
    fun appendObjs (s, l) = IntMap.foldl (fn (obj, l) => obj::l) l s

  (* create a list of the objects in s *)
    fun listObjs s = appendObjs (s, [])

  (* Given a set of objects, and a region that has been drawn in,
   * compute a list of the damaged objects (i.e., the ones that intersect
   * the region).  This requires a fixed-point compuatation, since
   * redrawing damaged objects can damage other objects.
   *)
(** NOTE: there might be some optimization based on depth; i.e., drawing a
 ** shallow object cannot damage a deeper object.
 **)
    fun computeDamagedObjs (objs, region) = let
	  fun testAndAdd (obj, (okayObjs, damagedObjs, damagedRegion)) = let
		val VObj{region, ...} = obj
		in
		  if (R.overlap(region, damagedRegion))
		    then (
		        okayObjs,
			insertObj(damagedObjs, obj),
			R.union(region, damagedRegion)
		      )
		    else (insertObj(okayObjs, obj), damagedObjs, damagedRegion)
		end
	  fun findFixedPt (okayObjs, damagedObjs, damagedRegion) = let
		val (okay, damaged, damagedRegion) =
		      IntMap.foldl testAndAdd
			(emptyMap, damagedObjs, damagedRegion)
			  okayObjs
		in
		  if ((IntMap.numItems damaged) = (IntMap.numItems damagedObjs))
		    then (listObjs damagedObjs)
		    else findFixedPt(okay, damaged, damagedRegion)
		end
	  in
	    findFixedPt (objs, emptyMap, region)
	  end

  (* convert a center coordinate, plus a width and height to a bounding box *)
    fun rectToBox {x, y, wid, ht} = let
	  val dx = (0.5 * wid) and dy = (0.5 * ht)
	  in
	    {minX = (x - dx), minY = (y - dy), maxX = (x + dx), maxY = (y + dy)}
	  end

    fun bbox (OBJ_LINE _) = raise Fail "bbox of line"
      | bbox (OBJ_RECT{x, y, wid, ht, ...}) =
	  rectToBox{x=x, y=y, wid=wid, ht=ht}
      | bbox (OBJ_ELLIPSE{x, y, wid, ht, ...}) =
	  rectToBox{x=x, y=y, wid=wid, ht=ht}
      | bbox (OBJ_DIAMOND{x, y, wid, ht, ...}) =
	  rectToBox{x=x, y=y, wid=wid, ht=ht}
      | bbox (OBJ_TEXT{x, y, wid, ht, ...}) =
	  rectToBox{x=x, y=y, wid=wid, ht=ht}

  (* return true, if a point is inside an object; for the time being, we just
   * test the point against the bounding box.
   *)
    fun pointInObj (obj, x, y) = let
	  val bb = bbox obj
	  in
	    (((#minX bb) <= x) andalso (x <= (#maxX bb))
	      andalso ((#minY bb) <= y) andalso (y <= (#maxY bb)))
	  end

(*****
    structure BSP = BSP2D (struct
	type object = (object_id * object_desc)
	fun boundingBox (_, obj) = bbox obj
	fun pointInObject ((_, obj), x, y) = pointInObj (obj, x, y)
      end)

    val partitionSpace = BSP.partitionSpace 5
*****)

  (* test to see if an object is visible *)
    fun isVisible (view, obj) = let
	  val vb = V.viewVBox view
	(* does a line segment  intersect the visible box? *)
	  fun lineInView ({x=x1, y=y1}, {x=x2, y=y2}) = let
		fun cmpMin (a, b, min) = (a < min) andalso (b < min)
		fun cmpMax (a, b, max) = (a > max) andalso (b > max)
		in
(**
TextIO.print(Format.format "lineInView: p1 = (%f, %f), p2 = (%f, %f)" [
Format.REAL x1, Format.REAL y1, Format.REAL x2, Format.REAL y2
]);
**)
		  if (cmpMin(x1, x2, #minX vb) orelse cmpMin(y1, y2, #minY vb)
		  orelse cmpMax(x1, x2, #maxX vb) orelse cmpMax(y1, y2, #maxY vb))
		    then false
		    else true (** We need to do more work here **)
		end
(**
val lineInView = fn arg => let val res = lineInView arg in
if res then TextIO.print "  is visible\n" else TextIO.print "  not visible\n"; res end
**)
	(* is any part of a rectangle (specified as center, wid, ht) in
	 * the visible box?
	 *)
	  fun rectInView {x, y, wid, ht} = let
		val hw = 0.5*wid and hh = 0.5*ht
		in
(**
TextIO.print(Format.format "rectInView: x = %f, y = %f, wid = %f, ht = %f" [
Format.REAL x, Format.REAL y, Format.REAL wid, Format.REAL ht
]);
**)
		  ((#minX vb) <= x+hw) andalso (x-hw <= (#maxX vb))
		  andalso ((#minY vb) <= y+hh) andalso (y-hh <= (#maxY vb))
		end
(**
val rectInView = fn arg => let val res = rectInView arg in
if res then TextIO.print "  is visible\n" else TextIO.print "  not visible\n"; res end
**)
	  in
	    case obj
	     of (OBJ_LINE{pts=(p1::r), head, tail, ...}) => let
		  fun testArrow (NO_ARROW) = false
		    | testArrow (ARROW{p0, p1, p2}) =
			lineInView(p0, p1) orelse lineInView(p1, p2)
		  fun test (p1, p2::r) =
			lineInView(p1, p2) orelse test (p2, r)
		    | test _ = false
		  in
		    testArrow head orelse testArrow tail orelse test (p1, r)
		  end
	      | (OBJ_LINE _) => raise Fail "line w/o points"
	      | (OBJ_RECT{x, y, wid, ht, ...}) =>
		  rectInView {x=x, y=y, wid=wid, ht=ht}
	      | (OBJ_ELLIPSE{x, y, wid, ht, ...}) =>
		  rectInView {x=x, y=y, wid=wid, ht=ht}
	      | (OBJ_DIAMOND{x, y, wid, ht, ...}) =>
		  rectInView {x=x, y=y, wid=wid, ht=ht}
	      | (OBJ_TEXT{x, y, wid, ht, ...}) =>
		  rectInView {x=x, y=y, wid=wid, ht=ht}
	    (* end case *)
	  end

    type cached_pen = {
	color : EXeneBase.color,
	lineWid : int,
	pen : Drawing.pen
      }

    datatype object_op = DRAW | ERASE

    datatype 'a world = WORLD of {
	canvas : C.canvas,		(* the drawing canvas *)
	background : EXeneBase.color,	(* the background color *)
	findFont : (ScalableFont.font_style * int) -> EXeneBase.font option,
	view : V.view ref,			
	canvasSz : G.size ref,
	idMap : 'a object IntMap.map ref,
					(* maps object IDs to objects *)
	visMap : 'a vis_object IntMap.map ref,
					(* a mapping of object IDs to the *)
					(* visible objects *)
	changed : (object_op * 'a vis_object) list ref,
					(* a list of visible objects that need *)
					(* to be displayed or erased *)
(*****
	posMap : BSP.bsp ref,		(* a map of object positions in world *)
					(* coordinates. *)
*****)
	pens : cached_pen list ref,	(* cached pens for drawing *)
	idCnt : int ref			(* for generating object IDs *)
      }


  (** basic drawing routines **)

    fun drawableOfWorld (WORLD{canvas, ...}) = C.drawableOfCanvas canvas

    fun drawObj (VObj{draw, ...}) = draw()
    fun eraseObj (VObj{erase, ...}) = erase()
    fun drawObjList l = app drawObj (sortVisObjs l)

  (* find or create a pen with the given foreground color and line width *)
    fun findPen (WORLD{background, pens, ...}) (c, w) = let
	  val penList = !pens
	  fun look [] = let
		val pen = D.newPen [
			D.PV_Function D.OP_Copy,
			D.PV_Foreground c,
			D.PV_Background background,
			D.PV_LineWidth w
		      ]
		in
		  pens := {color=c, lineWid=w, pen=pen} :: penList;
		  pen
		end
	    | look ({color, lineWid, pen}::r) =
		if ((lineWid = w) andalso W.EXB.sameColor(color, c))
		  then pen
		  else look r
	  in
	    look penList
	  end

  (* create the drawing and erasure operations for an object *)
    fun mkDrawFn (drawable, findPen, findFont, view, obj) = let
	(* convert a world coordinate rectangle, which is specified as a center
	 * point, plust width and height, to a pixel rectangle.
	 *)
	  fun uCoordToRect {x, y, wid, ht} = let
(**
val _ = TextIO.print(Format.format "uCoordToRect: x = %f, y = %f, wid = %f, ht = %f\n" [
Format.REAL x, Format.REAL y, Format.REAL wid, Format.REAL ht
])
**)
		val hw = 0.5*wid and hh = 0.5*ht
		val G.PT{x, y} = V.uCoordToPixel (view, x-hw, y+hh)
		val G.SIZE{wid, ht} = V.uCoordToSize (view, wid, ht)
		in
(**
TextIO.print(Format.format "        rect: x = %d, y = %d, wid = %d, ht = %d\n" [
Format.INT x, Format.INT y, Format.INT wid, Format.INT ht
]);
**)
		  G.RECT{x=x, y=y, wid=wid, ht=ht}
		end
	  in
	    case obj
	     of (OBJ_LINE{pts, head, tail, color, width=WID_Pixel w}) => let
		  val drawLines = D.drawLines drawable (findPen (color, w))
		  fun drawArrow NO_ARROW = ()
		    | drawArrow (ARROW{p0, p1, p2}) = drawLines [p0, p1, p2]
		  fun cvtPt {x, y} = V.uCoordToPixel(view, x, y)
		  fun cvtArrow (NO_ARROW) = NO_ARROW
		    | cvtArrow (ARROW{p0, p1, p2}) =
			  ARROW{p0 = cvtPt p0, p1 = cvtPt p1, p2 = cvtPt p2}
		  fun lineRegion (G.PT{x=x1, y=y1}, G.PT{x=x2, y=y2}) = let
			val (x, wid) = if (x1 < x2)
			      then (x1, (x2-x1)+1)
			      else (x2, (x1-x2)+1)
			val (y, ht) = if (y1 < y2)
			      then (y1, (y2-y1)+1)
			      else (y2, (y1-y2)+1)
			in
			  R.rectangle (G.RECT{x=x, y=y, wid=wid, ht=ht})
			end
		  fun arrowRegion NO_ARROW = R.empty
		    | arrowRegion (ARROW{p0, p1, p2}) =
			R.union (lineRegion(p0, p1), lineRegion(p1, p2))
		  fun mkRegion (p1::p2::r, region) =
			mkRegion (p2::r,
			  R.union (lineRegion(p1, p2), region))
		    | mkRegion (_, region) = region
		  val pts = map cvtPt pts
		  val head = cvtArrow head
		  val tail = cvtArrow tail
		  val region = mkRegion (pts,
			R.union(arrowRegion head, arrowRegion tail))
		  fun draw () = (drawLines pts; drawArrow head; drawArrow tail)
		  fun erase () = raise Fail "erase line"
		  in
		    (region, draw, erase)
		  end
	      | (OBJ_RECT{x, y, ht, wid, border, fill}) => let
		  val rect = uCoordToRect {x=x, y=y, wid=wid, ht=ht}
		  val region = R.rectangle rect
		  fun eraseFn () = (D.clearArea drawable rect)
		  in
		    case (border, fill)
		     of (BORDER_Pixel2D(c, w), NONE) => let
			  val drawRect = D.drawRect drawable (findPen (c, w))
			  in
			    (region, fn () => drawRect rect, eraseFn)
			  end
		      | (BORDER_None, SOME c) => let
			  val fillRect = D.fillRect drawable (findPen (c, 0))
			  in
			    (region, fn () => fillRect rect, eraseFn)
			  end
		      | (BORDER_Pixel2D(c, w), SOME c') => let
			  val drawRect = D.drawRect drawable (findPen (c, w))
			  val fillRect = D.fillRect drawable (findPen (c', w))
			  in
			    ( region,
			      fn () => (fillRect rect; drawRect rect),
			      eraseFn
			    )
			  end
		      | _ => raise Fail "rectangle w/o fill or border"
		    (* end case *)
		  end
(*****
	      | (OBJ_ELLIPSE{...}) =>
	      | (OBJ_DIAMOND{...}) =>
*****)
	      | (OBJ_TEXT{x, y, wid, ht, text, color, ptSz, fontStyle}) => let
		  val drawFontSz = V.scaleFontSz (view, ptSz)
		  in
		    case findFont (fontStyle, drawFontSz)
		     of NONE => (R.empty, fn () => (), fn () => ())
		      | (SOME f) => let
			val (rect as G.RECT{x, y, ...}) =
			      uCoordToRect {x=x, y=y, wid=wid, ht=ht}
			val region = R.rectangle rect
			fun eraseFn () = (D.clearArea drawable rect)
			val {ascent, ...} = Font.fontHt f
			val pos = G.PT{x=x, y=y+ascent}
			val draw = D.drawString drawable (findPen(color, 1)) f
			in
			  (region, fn () => draw (pos, text), eraseFn)
			end
		  end
	      | _ => raise Fail "mkDrawFn"
	    (* end case *)
	  end

  (* create the representation of a visible object; this means creating the
   * drawing and erasure functions.
   *)
    fun mkVisObj (world as WORLD w, obj as Obj{desc, ...}) = let
	  val (region, draw, erase) = mkDrawFn (
		  drawableOfWorld world,
		  findPen world, #findFont w, !(#view w), desc)
	  in
	    VObj{obj = obj, region = region, draw = draw, erase = erase}
	  end


  (* given a world, recompute the visibility of the objects in the world *)
    fun checkVisibility (world as WORLD w) = let
	  val view = !(#view w)
	  fun doObj (id, obj as Obj{visible, desc, ...}, vm) = let
		val isVis =isVisible (view, desc)
		in
		  visible := isVis;
		  if isVis
		    then IntMap.insert(vm, id, mkVisObj(world, obj))
		    else vm
		end
	  in
	    (#visMap w) := IntMap.foldli doObj IntMap.empty (!(#idMap w))
	  end

  (* compute the resolution of the screen in pixels per inch *)
    fun resolution root = let
	  fun realSz (G.SIZE{wid, ht}) = (real wid, real ht)
	  val (widP, htP) = realSz(W.sizeOfScr root)
	  val (widMM, htMM) = realSz(W.sizeMMOfScr root)
	  fun res (pixels, mm) = (25.4 * pixels) / mm
	  in
	    { x_res = res(widP, widMM), y_res = res(htP, htMM) }
	  end

    fun makeWorld (root, canvas, fontServer, bg, sz as G.SIZE{wid, ht}, initialBBox) = let
	  val {x_res, y_res} = resolution root
	  val pixelsPerIn = if Real.!=(x_res, y_res)
		then 0.5*(x_res+y_res)
		else x_res
	  fun padBBox {minX, maxX, minY, maxY} = let
(*
		val xPad = 0.5 * (maxX - minX) and yPad = 0.5 * (maxY - minY)
*)
		val xPad = 1.5 and yPad = 1.5
		in
		  { minX = minX - xPad, maxX = maxX + xPad,
		    minY = minY - yPad, maxY = maxY + yPad
		  }
		end
	  val bbox = (case initialBBox
		 of NONE => padBBox {
			minX = 0.0, maxX = (real wid) / pixelsPerIn,
			minY = 0.0, maxY = (real ht) / pixelsPerIn
		      }
(** if the given BBox is smaller than the view, need to grow the BBox **)
		  | (SOME bbox) => padBBox bbox 
		(* end case *))
	  val view = V.makeView {
		  minX = (#minX bbox), maxX = (#maxX bbox),
		  minY = (#minY bbox), maxY = (#maxY bbox),
		  scale = pixelsPerIn,
		  wid = wid, ht = ht
		}
	  in
(**
let val vb = V.viewVBox view in
TextIO.print(Format.format "mkWorld: wid = %d, ht = %d, pixels/in = %f\n" [
Format.INT wid, Format.INT ht, Format.REAL pixelsPerIn
]);
TextIO.print(Format.format "  bbox:  minX = %f, minY = %f, maxX = %f, maxY = %f\n" [
Format.REAL(#minX bbox), Format.REAL(#minY bbox), Format.REAL(#maxX bbox), Format.REAL(#maxY bbox)
]);
TextIO.print(Format.format "  vbox:  minX = %f, minY = %f, maxX = %f, maxY = %f\n" [
Format.REAL(#minX vb), Format.REAL(#minY vb), Format.REAL(#maxX vb), Format.REAL(#maxY vb)
]) end;
**)
	    WORLD{
		canvas = canvas,
		background = bg,
		findFont = ScalableFont.findFont fontServer,
		view = ref view,
		canvasSz = ref sz,
		idMap = ref IntMap.empty,
		visMap = ref IntMap.empty,
		changed = ref [],
(*****
		posMap = ref(partitionSpace (bbox, [])),
*****)
		pens = ref [],
		idCnt = ref 0
	      }
	  end (* makeWorld *)

    fun canvasOfWorld (WORLD{canvas, ...}) = canvas

    fun viewOfWorld (WORLD{view, ...}) = !view

  (* given a point in pixel coordinates, return the list of objects (sorted
   * by increasing depth) that contain that point.
   * NOTE: eventually, this will be done using the BSP tree.
   *)
    fun pickObj (WORLD w, pt) = let
	  val (x, y) = V.pixelToUCoord (!(#view w), pt)
	  fun scanObj (vobj as VObj{obj=Obj{desc, ...}, ...}, l) =
		if (pointInObj(desc, x, y)) then vobj::l else l
	  fun proj (VObj{obj=Obj{id, info=SOME info, ...}, ...}) = SOME(id, info)
	    | proj _ = NONE
	  in
	    List.mapPartial proj (
	      sortVisObjs (
	        IntMap.foldl scanObj [] (!(#visMap w))))
	  end

    fun addObject (world as WORLD w, obj, info, depth) = let
	  val id = !(#idCnt w)
	  val isVis = isVisible (!(#view w), obj)
	  val object = Obj{
		  id = id, desc = obj, depth = depth, info = info, visible = ref isVis
		}
	  in
	  (* add object to idMap *)
	    (#idMap w) := IntMap.insert(!(#idMap w), id, object);
	  (* if it is visible, add it to the visible map *)
	    if isVis
	      then let
		val vobj = mkVisObj (world, object)
		in
		  (#visMap w) := IntMap.insert(!(#visMap w), id, vobj);
		  (#changed w) := (DRAW, vobj) :: !(#changed w)
		end
	      else ();
(*****
	  (* add object to BSP; note that only objects with info are added *)
	    case info
	     of NONE => ()
	      | (SOME info) => (#posMap w) := partitionSpace(?, ?)
	    (* end case *);
*****)
	    (#idCnt w) := (id+1);
	    id
	  end

    fun delObject (WORLD w, id) = let
	  val (idMap', Obj{visible, depth, ...}) = IntMap.remove(!(#idMap w), id)
	  in
	    (#idMap w) := idMap';
	    if (!visible)
	      then let
		val (visMap', vobj) = IntMap.remove(!(#visMap w), id)
		in
		  (#visMap w) := visMap';
		  (#changed w) := (ERASE, vobj) :: !(#changed w)
		end
	      else ()
	  end

  (* ensure that the view of the canvas reflects any object additions
   * and deletions.
   *)
    fun updateCanvas (WORLD w) = let
	  fun split ([], dl, el) = (dl, el)
	    | split ((DRAW, vobj)::r, dl, el) = split (r, vobj::dl, el)
	    | split ((ERASE, vobj)::r, dl, el) = split (r, dl, vobj::el)
	  val (dl, el) = split (!(#changed w), [], [])
	  val damagedObjs = computeDamagedObjs (!(#visMap w), regionOfObjs el)
	  in
(**
TextIO.print(Format.format "updateCanvas: %d changed, %d erase, %d draw, %d damaged\n"
[Format.INT(length(!(#changed w))), Format.INT(length el), Format.INT(length dl),  Format.INT(length damagedObjs)]);
**)
	    app eraseObj el;
	    drawObjList (dl @ damagedObjs);
	    (#changed w) := []
	  end (* updateCanvas *)

    fun scrollAmount s = (case s
	   of (SB.ScrUp r) => SOME r
	    | (SB.ScrDown r) => SOME(~r)
	    | (SB.ScrStart r) => NONE
	    | (SB.ScrMove r) => NONE
	    | (SB.ScrEnd r) => NONE
	  (* end case *))

    fun scrollFn scrollView (world as WORLD w, s) = let
	  val v = !(#view w)
	  val clear = D.clearArea (drawableOfWorld world)
	  in
	    case scrollAmount s
             of NONE => ()
              | (SOME amt) => let
		  val (view', pixels) = scrollView (!(#view w), amt)
		  val G.SIZE{wid, ht} = !(#canvasSz w)
		  val newRect = G.RECT{x=0, y=0, wid=wid, ht=ht}
		  in
		    if (pixels <> 0)
		      then (
			(#view w) := view';
			checkVisibility world;
(* should do bitblt to reduce redrawing *)
			clear newRect;
			drawObjList (computeDamagedObjs(!(#visMap w),
			  R.rectangle newRect)))
		      else ()
		  end
	    (* end case *)
	  end

    fun vScroll arg = scrollFn V.vScrollView arg
    fun hScroll arg = scrollFn V.hScrollView arg

    fun zoom (world as WORLD w, zm) = let
	  val v = V.zoomView (!(#view w), zm)
	  val G.SIZE{wid, ht} = !(#canvasSz w)
	  in
	    (#view w) := v;
	    checkVisibility world;
	    D.clearDrawable (drawableOfWorld world);
	    drawObjList (computeDamagedObjs(!(#visMap w),
	      R.rectangle(G.RECT{x=0, y=0, wid=wid, ht=ht})))
	  end

    fun redrawCanvas (WORLD w, damageList) =
	  drawObjList (computeDamagedObjs(!(#visMap w), mkRegion damageList))

    fun resizeCanvas (WORLD w, sz as G.SIZE{wid, ht}) = (
	  (#canvasSz w) := sz;
	  (** RESIZE the VIEW **)())

  (* get the associated info (if any) of an object; this raises the NotFound
   * exception, if the ID is bogus.
   *)
    fun infoOfObject (WORLD w, id) = let
	  val Obj{info, ...} = valOf(IntMap.find(!(#idMap w), id))
	  in
	    info
	  end

  end (* World *)

