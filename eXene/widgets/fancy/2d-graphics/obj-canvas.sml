(* obj-canvas.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

structure ObjCanvas : OBJ_CANVAS =
  struct

    structure W = Widget
    structure EXB = W.EXB
    structure C = Canvas
    structure D = Drawing
    structure G = Geometry
    structure V = WorldView
    structure SFont = ScalableFont
    structure SB = Scrollbar
    structure SV = SyncVar

    datatype border_spec = datatype World.border_spec
    datatype width_spec = datatype World.width_spec
    datatype arrow_spec = datatype World.arrow_spec
    type object_id = World.object_id
    type box = World.box

    val attrs = [
	    (Attrs.attr_background,	Attrs.AT_Color,	Attrs.AV_Str "cornsilk1"),
	    (Attrs.attr_foreground,	Attrs.AT_Color,	Attrs.AV_Str "black"),
	    (Attrs.attr_fontSize,	Attrs.AT_Int,	Attrs.AV_Int 12),
	    (Attrs.attr_width,		Attrs.AT_Int,	Attrs.AV_Int 600),
	    (Attrs.attr_height,		Attrs.AT_Int,	Attrs.AV_Int 600)
	  ]

    fun getAttrs (root, view, args) = let
	  val getAttr = W.findAttr(W.attrs(view, attrs, args))
	  in
	    { background = Attrs.getColor (getAttr Attrs.attr_background),
	      foreground = Attrs.getColor (getAttr Attrs.attr_foreground),
	      fontSize = Attrs.getInt (getAttr Attrs.attr_fontSize),
	      width = Attrs.getInt (getAttr Attrs.attr_width),
	      height = Attrs.getInt (getAttr Attrs.attr_height)
	    }
	  end

  (* the external specification of a graphical object *)
    datatype gr_object
      = BOX of {
	  wid : real, ht : real,
	  fill : W.EXB.color option,
	  border : border_spec
	}
      | DIAMOND of {
	  wid : real, ht : real,
	  fill : W.EXB.color option,
	  border : border_spec
	}
      | ELLIPSE of {
	  wid : real, ht : real,
	  fill : W.EXB.color option,
	  border : border_spec
	}
      | LINE of {
	  pts : {x : real, y : real} list,
	  head : arrow_spec,
	  tail : arrow_spec,
	  wid : width_spec,
	  color : W.EXB.color
	}
      | TEXT of {
	  sz : int, style : SFont.font_style,
	  align : (W.halign * W.valign),
	  color : W.EXB.color,
	  text : string
	}

    datatype pt_stream = ENTER | EXIT | PT of Geometry.point | END

    datatype 'a req_msg
      = AddObj of (int * World.object_desc * 'a option * object_id SV.ivar)
      | DelObj of object_id
      | DelAll
      | Update
      | Pick of (G.point * (object_id * 'a) list SV.ivar)
      | Track of {
	    points : pt_stream CML.event,
	    obj : gr_object,
	    hotSpot : {x : real, y : real},
	    replV : {x : real, y : real} SV.ivar
	  }
      | Redraw of G.rect list
      | Resize of G.rect
      | Zoom of (real * unit SV.ivar)

    datatype 'a object_canvas = OC of {
	widget : W.widget,
	drawCanvas : C.canvas,
	findFont : (SFont.font_style * int) -> EXB.font option,
	reqCh : 'a req_msg CML.chan,
	pixelsPerIn : real,
	defaultColor : EXB.color
      }

 (* the server loop, when there are scroll bars *)
    fun scrollServer (world, reqEvt, hsb, vsb) = let
	  fun setSBVals () = let
		val v = World.viewOfWorld world
		val {top, vSize, left, hSize} = WorldView.viewSize v
		in
		(* we do this asynchronously, since there might be a potential
		 * deadlock with the scrollbar server.
		 *)
		  CML.spawn (fn () => (
		    SB.setVals hsb {top=SOME left, sz=SOME hSize};
		    SB.setVals vsb {top=SOME top, sz=SOME vSize}));
		  ()
		end
	  val reqEvt = CML.wrap (reqEvt, fn false => () | true => setSBVals())
	  val hsbEvt = CML.wrap (SB.evtOf hsb,
		fn s => (World.hScroll(world, s); setSBVals()))
	  val vsbEvt = CML.wrap (SB.evtOf vsb,
		fn s => (World.vScroll(world, s); setSBVals()))
	  fun loop () = (
		CML.select [ reqEvt, hsbEvt, vsbEvt ];
		loop ())
	  in
	    loop ()
	  end

 (* the object canvas server *)
    fun server (root, world, bbox, attrs, reqCh, sb) () = let
	  val canvas = World.canvasOfWorld world
	  val drawable = C.drawableOfCanvas canvas
	  val setCursor = C.setCursor canvas
	(* handle requests; return true, if the vbox or bbox might have changed *)
	  fun handleReq (AddObj(depth, desc, info, replyV)) = let
		val id = World.addObject(world, desc, info, depth)
		in
		  SV.iPut(replyV, id); false
		end
	    | handleReq (DelObj id) = (World.delObject (world, id); false)
	    | handleReq DelAll = raise Fail "DelAll"
	    | handleReq Update = (World.updateCanvas world; true)
	    | handleReq (Pick(pt, replyV)) = (
		SV.iPut (replyV, World.pickObj (world, pt)); true)
	    | handleReq (Track{points, obj, hotSpot, replV}) =
		raise Fail "Track object"
	    | handleReq (Redraw rl) = (World.redrawCanvas(world, rl); false)
	    | handleReq (Resize(G.RECT{wid, ht, ...})) =
		(World.resizeCanvas (world, G.SIZE{wid=wid, ht=ht}); true)
	    | handleReq (Zoom(z, ackV)) = (
		World.zoom(world, z);
		SV.iPut(ackV, ());
		true)
(***
	  fun loop () = (handleReq(CML.recv reqCh); loop())
***)
	  fun loop () = let
		val req = CML.recv reqCh
		in
		  handleReq req;
		  loop()
		end
	  in
	    case sb
	     of NONE => loop()
	      | (SOME{hsb, vsb}) => scrollServer (world,
		  CML.wrap(CML.recvEvt reqCh, handleReq), hsb, vsb)
	    (* end case *)
	  end (* server *)

  (* create an object canvas *)
    fun objCanvas (root, view, args) {bbox, fontServer, sb} = let
	  val attrs = getAttrs (root, view, args)
	  val w = #width attrs and h = #height attrs
	  val args = [
		  (Attrs.attr_background, Attrs.AV_Color(#background attrs))
		]
	  val (canvas, G.SIZE{wid, ht}, inEnv) = C.canvas (root, view, args)
		  (W.mkBounds{x_dim=W.flexDim w, y_dim=W.flexDim h})
	  val framedCanvas = Frame.frame (root, view, args) (C.widgetOf canvas)
	  val reqCh = CML.channel()
	  val world = World.makeWorld (
		root, canvas, fontServer, #background attrs,
		Canvas.sizeOf canvas, bbox)
	  val ocanvas = OC{
		  widget = Frame.widgetOf framedCanvas,
		  drawCanvas = canvas,
		  findFont = SFont.findFont fontServer,
		  reqCh = reqCh, defaultColor = #foreground attrs,
		  pixelsPerIn = V.pixelsPerInch(World.viewOfWorld world)
		}
	  in
	    CML.spawn (server (root, world, bbox, attrs, reqCh, sb));
	    (ocanvas, inEnv)
	  end (* objCanvas *)


 (** The object canvas operations **)

  (* get the underlying widget *)
    fun widgetOf (OC{widget, ...}) = widget

  (* set the cursor for the canvas *)
    fun setCursor (OC{drawCanvas, ...}) = C.setCursor drawCanvas

    fun makeLine {pts, head, tail, wid, color} = let
	  fun transArrow (NoArrow, _) = World.NO_ARROW
	    | transArrow (Arrow{len, wid}, selFn) = let
		val (hd, tl) = selFn pts
		in
		  World.ARROW(Arrow.mkArrow{
		      hd = hd, tl = tl, len = len, wid = wid
		    })
		end
	  fun firstLine (p0::p1::_) = (p0, p1)
	    | firstLine _ = raise Fail "makeLine.firstLine"
	  fun lastLine [p1, p0] = (p0, p1)
	    | lastLine (_::r) = lastLine r
	    | lastLine _ = raise Fail "makeLine.lastLine"
	  in
	    World.OBJ_LINE{
		pts = pts,
		head = transArrow(head, lastLine),
		tail = transArrow(tail, firstLine),
		width = wid, color = color
	      }
	  end (* makeLine *)

    fun placeObj (oc as OC{reqCh, ...}) {at : {x:real, y:real}, depth, obj, info} =
	  let
	  val replV = SV.iVar()
	  val objDesc = (case obj
		 of (BOX{wid, ht, fill, border}) => World.OBJ_RECT{
			x = #x at, y = #y at, wid = wid, ht = ht,
			fill = fill, border = border
		      }
		  | (ELLIPSE{wid, ht, fill, border}) => World.OBJ_ELLIPSE{
			x = #x at, y = #y at, wid = wid, ht = ht,
			fill = fill, border = border
		      }
		  | (LINE{pts, head, tail, wid, color}) => let
		      fun transPt {x, y} = {x = x + #x at, y = y + #y at}
		      in
			makeLine {
			    pts = map transPt pts,
			    head = head, tail = tail,
			    wid = wid, color = color
			  }
		      end
		  | (TEXT{sz, style, align=(hAlign, vAlign), color, text}) => let
		      val OC{findFont, pixelsPerIn, ...} = oc
		      val (sz, naturalFont) = (case findFont (style, sz)
			     of NONE => let
				  val (SOME f) = findFont(style, SFont.dfltFontSz)
				  in (SFont.dfltFontSz, f) end
			      | (SOME f) => (sz, f)
			    (* end case *))
		      val ht = let val {ascent, descent} = Font.fontHt naturalFont
			    in
			      real(ascent+descent) / pixelsPerIn
			    end
		      val wid = real(Font.textWidth naturalFont text) / pixelsPerIn
		      val x = (case hAlign
			     of W.HCenter => (#x at)
			      | W.HRight => ((#x at) - (0.5 * wid))
			      | W.HLeft => ((#x at) + (0.5 * wid))
			    (* end case *))
		      val y = (case vAlign
			     of W.VCenter => (#y at)
			      | W.VTop => (#y at) - (0.5 * ht)
			      | W.VBottom => (#y at) + (0.5 * ht)
			    (* end case *))
		      in
			World.OBJ_TEXT{
			    x = x, y = y, wid = wid, ht = ht,
			    text = text,
			    color = color,
			    ptSz = sz, fontStyle = style
			  }
		      end
		  | _ => raise Fail "placeObj"
		(* end case *))
	  in
	    CML.send(reqCh, AddObj(depth, objDesc, info, replV));
	    SV.iGet replV
	  end

    fun drawLine (oc as OC{reqCh, ...}) {pts, depth, wid, color} = let
	  val replV = SV.iVar()
	  val objDesc = World.OBJ_LINE{
		  pts = pts, head = World.NO_ARROW, tail = World.NO_ARROW,
		  width = wid, color = color
		}
	  in
	    CML.send(reqCh, AddObj(depth, objDesc, NONE, replV));
	    SV.iGet replV
	  end

    fun drawLine' (oc as OC{reqCh, ...}) {
	  pts, head, tail, depth, wid, color
	} = let
	  val replV = SV.iVar()
	  val objDesc = makeLine{
		  pts=pts, head=head, tail=tail, wid=wid, color=color
		}
	  in
	    CML.send(reqCh, AddObj(depth, objDesc, NONE, replV));
	    SV.iGet replV
	  end

  (* remove an object from the picture *)
    fun delObj (OC{reqCh, ...}) objId = CML.send(reqCh, DelObj objId)

  (* delete all of the existing objects *)
    fun delAll (OC{reqCh, ...}) = CML.send(reqCh, DelAll)

  (* update the display *)
    fun update (OC{reqCh, ...}) = CML.send(reqCh, Update)

  (* This is support for feedback.  Given a stream of pixel coordinates,
   * track the points with the object.  The stream of points will be
   * terminated with NONE, at which point the resulting event is enabled
   * with the final position (in world coordinates).
   *)
    fun trackObj (OC{reqCh, ...}) {points, obj, hotSpot} = let
	  val replV = SV.iVar ()
	  in
	    CML.send (reqCh,
	      Track{points=points, obj=obj, hotSpot=hotSpot, replV=replV});
	    SV.iGetEvt replV
	  end

  (* redraw damaged regions (specified as a list of rectangles) *)
    fun redraw (OC{reqCh, ...}) rl = CML.send(reqCh, Redraw rl)

  (* resize the canvas *)
    fun resize (OC{reqCh, ...}) r = CML.send(reqCh, Resize r)

  (* Zoom the canvas: a zoom of 1.0 is no change; a zoom of 2.0 doubles
   * the size of objects in the canvas (i.e., zooms in), while a zoom of
   * 0.5 halves the size of objects in the canvas.  This will not zoom
   * out beyond the canvas's bounding box.
   *)
    fun zoom (OC{reqCh, ...}) z = let
	  val ackV = SV.iVar()
	  in
	    CML.send(reqCh, Zoom(z, ackV));
	    SV.iGet ackV
	  end

  end;

