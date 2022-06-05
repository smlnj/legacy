(* image.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This structure supports images (client-side data) and creating server-side
 * pixmaps from images.
 *
 * TODO
 *   - support a left-pad
 *   - support Z format
 *)

structure Image : sig

    exception BadImageData

  (* Format for images, XY format.
   * Each string in the string list corresponds to a scan line in a plane.
   * The outer list corresponds to the list of planes, with plane 0 being
   * the last item in the list.
   * 
   * Multiple planes are not very useful right now, as the pixel type is
   * opaque. It seemed reasonable, however, to allow createImageFromPixmap
   * to work on all pixmaps, and the necessary changes were minimal.
   *
   *)
    datatype image = IMAGE of {
	sz : Geometry.size,
	data : Word8Vector.vector list list
      }

    val putImage : DrawTypes.pixmap -> {
	    src : image, src_rect : Geometry.rect, dst_pt : Geometry.point
	  } -> unit
    val imageFromAscii : (int * string list list) -> image
    val createPixmapFromImage : Display.screen -> image -> DrawTypes.pixmap
    val createPixmapFromAsciiData : Display.screen -> (int * string list list)
	  -> DrawTypes.pixmap
    val createImageFromPixmap : DrawTypes.pixmap -> image
    val createImageFromTile : DrawTypes.tile -> image

  end = struct

    structure W8V = Word8Vector
    structure W8 = Word8

    structure G = Geometry
    structure XTy = XProtTypes
    structure XReq = XRequest
    structure XDpy = XDisplay
    structure Dpy = Display
    structure DTy = DrawTypes

    exception BadImageData

    val w8vextract = Word8VectorSlice.vector o Word8VectorSlice.slice

    datatype image = IMAGE of {
	sz : Geometry.size,
	data : Word8Vector.vector list list
      }

  (* map a row of data coded as a string to a bit representation.  The data may
   * be either encoded in hex (with a preceeding "0x") or in binary (with a
   * preceeding "0b".
   *)
      fun stringToData (wid, s) = (case (String.explode s)
	   of (#"0" :: #"x" ::r) => let
		val nbytes = ((wid + 7) div 8)   (* # of bytes per line *)
		fun cvtChar c = if (Char.isDigit c)
			then Byte.charToByte c - Byte.charToByte #"0"
		      else if (Char.isHexDigit c)
			then if (Char.isUpper c)
			  then Byte.charToByte c - Byte.charToByte #"A"
			  else Byte.charToByte c - Byte.charToByte #"a"
			else raise BadImageData
		fun mkRow (0, [], l) = W8V.fromList(rev l)
		  | mkRow (0, _, _) = raise BadImageData
		  | mkRow (i, d1::d0::r, l) =
		      mkRow(i-1, r,
			W8.orb(W8.<<(cvtChar d1, 0w4), cvtChar d0) :: l)
		  | mkRow _ = raise BadImageData
		in
		  mkRow (nbytes, r, [])
		end
	    | (#"0" :: #"b" ::r) => let
		fun mkRow (0, _, [], b, l) = W8V.fromList(rev(b::l))
		  | mkRow (_, _, [], _, _) = raise BadImageData
		  | mkRow (i, 0w0, l1, b, l2) =
		      mkRow(i, 0wx80, l1, 0w0, b::l2)
		  | mkRow (i, m, #"0"::r, b, l) =
		      mkRow(i-1, W8.>>(m, 0w1), r, b, l)
		  | mkRow (i, m, #"1"::r, b, l) =
		      mkRow(i-1, W8.>>(m, 0w1), r, W8.orb(m, b), l)
		  | mkRow _ = raise BadImageData
		in
		  mkRow(wid, 0wx80, r, 0w0, [])
		end
	    | _ => raise BadImageData
	   (* end case *))

  (* reverse the bit-order of a byte *)
    fun revBits b = let
	  val revTbl = Byte.stringToBytes "\
		\\000\128\064\192\032\160\096\224\
		\\016\144\080\208\048\176\112\240\
		\\008\136\072\200\040\168\104\232\
		\\024\152\088\216\056\184\120\248\
		\\004\132\068\196\036\164\100\228\
		\\020\148\084\212\052\180\116\244\
		\\012\140\076\204\044\172\108\236\
		\\028\156\092\220\060\188\124\252\
		\\002\130\066\194\034\162\098\226\
		\\018\146\082\210\050\178\114\242\
		\\010\138\074\202\042\170\106\234\
		\\026\154\090\218\058\186\122\250\
		\\006\134\070\198\038\166\102\230\
		\\022\150\086\214\054\182\118\246\
		\\014\142\078\206\046\174\110\238\
		\\030\158\094\222\062\190\126\254\
		\\001\129\065\193\033\161\097\225\
		\\017\145\081\209\049\177\113\241\
		\\009\137\073\201\041\169\105\233\
		\\025\153\089\217\057\185\121\249\
		\\005\133\069\197\037\165\101\229\
		\\021\149\085\213\053\181\117\245\
		\\013\141\077\205\045\173\109\237\
		\\029\157\093\221\061\189\125\253\
		\\003\131\067\195\035\163\099\227\
		\\019\147\083\211\051\179\115\243\
		\\011\139\075\203\043\171\107\235\
		\\027\155\091\219\059\187\123\251\
		\\007\135\071\199\039\167\103\231\
		\\023\151\087\215\055\183\119\247\
		\\015\143\079\207\047\175\111\239\
		\\031\159\095\223\063\191\127\255"
	in
	  W8V.sub(revTbl, W8.toInt b)
	end

    (* Routines to re-order bits and bytes to the server's format (stolen from
     * XPutImage.c in Xlib).  We represent data in the following format:
     *
     *   scan-line unit = 1 byte
     *   byte-order     = MSB first (doen't matter for 1-byte scan units)
     *   bit-order      = MSB first (bit 0 is leftmost on display)
     *
     * This is the "1Mm" format of XPutImage.c in Xlib.  The relevant lines
     * in the conversion table are:
     *
     *         1Mm 2Mm 4Mm 1Ml 2Ml 4Ml 1Lm 2Lm 4Lm 1Ll 2Ll 4Ll
     *   1Mm:   n   n   n   R   S   L   n   s   l   R   R   R
     *   1Ml:   R   R   R   n   s   l   R   S   L   n   n   n
     *
     *   legend:
     *		n   no changes
     *		s   reverse 8-bit units within 16-bit units
     *		l   reverse 8-bit units within 32-bit units
     *		R   reverse bits within 8-bit units
     *		S   s+R
     *		L   l+R
     *)
      fun noSwap x = x
      fun swapBits data =
	    W8V.fromList(W8V.foldr (fn (b, l) => revBits b :: l) [] data)
      fun explodeV data = W8V.foldr (op ::) [] data
      fun swap2Bytes s = let
	    fun swap [] = []
	      | swap (a::b::r) = b::a::(swap r)
	      | swap _ = (MLXError.impossible "[swap2Bytes: bad image data]")
	    in
	      W8V.fromList (swap (explodeV s))
	    end
      fun swap4Bytes s = let
	    fun swap [] = []
	      | swap (a::b::c::d::r) = d::c::b::a::(swap r)
	      | swap _ = (MLXError.impossible "[swap4Bytes: bad image data]")
	    in
	      W8V.fromList (swap (explodeV s))
	    end
      fun swapBitsAnd2Bytes s = let
	    fun swap [] = []
	      | swap (a::b::r) = (revBits b)::(revBits a)::(swap r)
	      | swap _ = (MLXError.impossible "[swapBitsAnd2Bytes: bad image data]")
	    in
	      W8V.fromList (swap (explodeV s))
	    end
      fun swapBitsAnd4Bytes  s = let
	    fun swap [] = []
	      | swap (a::b::c::d::r) =
		  (revBits d)::(revBits c)::(revBits b)::(revBits a)::(swap r)
	      | swap _ = (MLXError.impossible "[swapBitsAnd4Bytes: bad image data]")
	    in
	      W8V.fromList (swap (explodeV s))
	    end
      fun swapFunc ( XTy.Raw8, XTy.MSBFirst, XTy.MSBFirst) = noSwap
	| swapFunc (XTy.Raw16, XTy.MSBFirst, XTy.MSBFirst) = noSwap
	| swapFunc (XTy.Raw32, XTy.MSBFirst, XTy.MSBFirst) = noSwap
	| swapFunc ( XTy.Raw8, XTy.MSBFirst, XTy.LSBFirst) = swapBits
	| swapFunc (XTy.Raw16, XTy.MSBFirst, XTy.LSBFirst) = swapBitsAnd2Bytes
	| swapFunc (XTy.Raw32, XTy.MSBFirst, XTy.LSBFirst) = swapBitsAnd4Bytes
	| swapFunc ( XTy.Raw8, XTy.LSBFirst, XTy.MSBFirst) = noSwap
	| swapFunc (XTy.Raw16, XTy.LSBFirst, XTy.MSBFirst) = swap2Bytes
	| swapFunc (XTy.Raw32, XTy.LSBFirst, XTy.MSBFirst) = swap4Bytes
	| swapFunc ( XTy.Raw8, XTy.LSBFirst, XTy.LSBFirst) = swapBits
	| swapFunc (XTy.Raw16, XTy.LSBFirst, XTy.LSBFirst) = swapBits
	| swapFunc (XTy.Raw32, XTy.LSBFirst, XTy.LSBFirst) = swapBits

      fun padToBits XTy.Raw8 = 0w8
        | padToBits XTy.Raw16 = 0w16
        | padToBits XTy.Raw32 = 0w32

      fun roundDown (nbytes, pad) = Word.toIntX(
	    Word.andb(Word.fromInt nbytes, Word.notb((padToBits pad) - 0w1)))

      fun roundUp (nbytes, pad) = let
	    val bits = (padToBits pad) - 0w1
	    in
	      Word.toIntX(Word.andb(Word.fromInt nbytes + bits, Word.notb bits))
	    end
        
    (* Pad and re-order image data as necessary to match the server's format. *)
      local
	val pad1 = W8V.tabulate(1, fn _ => 0w0)
	val pad2 = W8V.tabulate(2, fn _ => 0w0)
	val pad3 = W8V.tabulate(3, fn _ => 0w0)
      in
      fun adjustImageData (XDpy.XDPY dpyInfo) = let
	    fun extra (v, m) = Word.andb(Word.fromInt(W8V.length v), m)
	    val padScanLine = (case (#bitmap_scanline_pad dpyInfo)
		   of XTy.Raw8 => (fn s => s)
		    | XTy.Raw16 => (
		        fn s => (if (extra(s, 0w1) = 0w0) then s else W8V.concat[s, pad1]))
		    | XTy.Raw32 => (
		        fn s => (case (extra(s, 0w3))
			   of 0w0 => s
			    | 0w1 => W8V.concat[s, pad3]
			    | 0w2 => W8V.concat[s, pad2]
			    | _   => W8V.concat[s, pad1]
			  (* end case *)))
		  (* end case *))
	    val swapfn = swapFunc (
		    #bitmap_scanline_unit dpyInfo,
		    #image_byte_order dpyInfo,
		    #bitmap_bit_order dpyInfo)
	    in
              fn data => map (fn s => swapfn (padScanLine s)) data
	    end (* adjustImageData *)
      end (* local *)

  (* Copy rectangle from image into pixmap.
   * It wouldn't take much to generalize this to all drawables & pens.
   * Additional efficiency could be gained by having the extractRow
   * function extract rows already padded correctly for the display,
   * if possible.
   *)
    fun putImage pm {src=IMAGE{sz,data}, src_rect, dst_pt} = let
        (* clip src_rect to image *)
          val src_rect' = G.intersection(src_rect, G.mkRect(G.originPt, sz))
          val delta = G.subPt(G.originOfRect src_rect', G.originOfRect src_rect)
          val depth = List.length data
	  val DTy.PM{id, scr, scr_depth=Dpy.SCRDEPTH{draw_cmd, ...}, ...} = pm
	  val Dpy.SCREEN{dpy=Dpy.DPY{xdpy=xdpy as XDpy.XDPY dpyInfo, ...}, ...} = scr
          val scanlinePad = #bitmap_scanline_pad dpyInfo
          val scanlineUnit = #bitmap_scanline_unit dpyInfo
        (* Minimum no. of 4-byte words needed for PutImage.
         * There should be a function in XRequest to provide this.
         *)
          val rqstSz = 6
        (* Number of image bytes per each request *)
          val available = (Int.min(#max_req_len dpyInfo, 65536) - rqstSz) * 4
          fun putImageRqst (r as G.RECT{x,y,wid,ht}, dst_pt) = let
		val leftPad = Word.toIntX(
		      Word.andb(Word.fromInt x, padToBits scanlineUnit - 0w1))
		val byteOffset = (x - leftPad) div 8
		val numBytes = roundUp(wid + leftPad, XTy.Raw8) div 8
		val adjust = adjustImageData xdpy
	      (* Given the list of data for a plane, extract a list of substrings
	       * corresponding to given rectangle, to the nearest byte.
	       *)
		fun extractRect (rows : W8V.vector list) = let
		      fun skip (0, r) = r
			| skip (i, _::r) = skip(i-1, r)
			| skip (i, []) = MLXError.impossible "Image: extractRect(skip)"
		      fun extractRow (0, _) = []
			| extractRow (i, row::rest) =
			    if (byteOffset = 0 andalso numBytes = W8V.length row)
			      then row :: (extractRow(i-1, rest))
			      else (
				w8vextract(row, byteOffset, SOME numBytes))
				  :: (extractRow (i-1, rest))
			| extractRow (i,[]) = MLXError.impossible "Image: extractRow"
		      in
			extractRow(ht, skip(y, rows))
		      end (* extractRect *)
		val xdata = map extractRect data
		in
		  draw_cmd (DrawMaster.DMSG{
		      dst = id,
		      pen = Pen.defaultPen,
		      oper = DrawMaster.DOP_PutImage{
			  dst_pt = dst_pt,
			  size = G.SIZE{wid=wid,ht=ht},
			  depth = depth,
			  lpad = leftPad,
			  format = XTy.XYPixmap,
(*** THIS SHOULD BE
			  data = W8V.concat (List.concat (map adjust xdata))
***)
			  data = W8V.concat (map (W8V.concat o adjust) xdata)
			}
		    })
		end (* putImageRqst *)
        (* decompose putImage into multiple request smaller than max. size.
         * First try to use as many rows as possible. If there is only one
         * row left and it is still too large, decompose by columns.
         *)
	  fun putSubImage (r as G.RECT{x,wid,ht,y}, pt as G.PT{x=dx,y=dy}) = let
		val leftPad =
		      Word.toIntX(Word.andb(Word.fromInt x, padToBits scanlineUnit - 0w1))
		val bytesPerRow = (roundUp(wid + leftPad, scanlinePad) div 8) * depth
		in
		  if ((bytesPerRow * ht) <= available)
		    then putImageRqst (r,pt)
		  else if (ht > 1)
		    then let
		      val ht' = Int.max(1, available div bytesPerRow)
		      in
			putSubImage (G.RECT{x=x,y=y,wid=wid,ht=ht'}, pt);
			putSubImage (G.RECT{x=x,y=y+ht',wid=wid,ht=ht-ht'},
			  G.PT{x=dx,y=dy+ht'})
		      end
		    else let
		      val wid' = roundDown(available * 8, scanlinePad) - leftPad
		      in
			putSubImage (G.RECT{x=x,y=y,wid=wid',ht=1},pt);
			putSubImage (G.RECT{x=x+wid',y=y,wid=wid-wid',ht=1},
			  G.PT{x=dx+wid',y=dy})
		      end
		end
	  in
	    putSubImage (src_rect', G.addPt(dst_pt,delta))
	  end (* putImage *)

  (* create image data from an ascii representation *)
    fun imageFromAscii (wid, []) = raise BadImageData
      | imageFromAscii (wid, p0::rest) = let
	  fun mk (n, [], l) = (n,rev l)
	    | mk (n, s::r, l) = mk(n+1, r, stringToData(wid, s) :: l)
          val (ht, plane0) = mk(0, p0, [])
          fun chk data = let 
                val (h,plane) = mk(0,data,[]) 
                in
                    if h = ht then plane else raise BadImageData
                end
	  in
	    IMAGE{sz=G.SIZE{wid=wid, ht=ht}, data=plane0::(map chk rest)}
	  end

  (* create a pixmap from image data. *)
    fun createPixmapFromImage scr (image as IMAGE{sz, data}) = let
          val depth = length data
	  val pm = Pixmap.createPixmap scr (sz, depth)
	  in
	    putImage pm {
              src=image, 
              src_rect = G.mkRect(G.originPt,sz), 
              dst_pt = G.originPt};
	    pm
	  end (* createPixmapFromImage *)

  (* create a pixmap from ascii data. *)
    fun createPixmapFromAsciiData scr (wid, ascii_rep) =
	  createPixmapFromImage scr (imageFromAscii(wid, ascii_rep))

  (* create an image from a pixmap.
   * This should be better integrated with the draw-master, 
   * to avoid a possible race condition, i.e., we need to be sure
   * the draw-master flush has occurred before we ask for the image.
   *)
    fun createImageFromPixmap (DTy.PM{id, sz = sz as G.SIZE szinfo, scr_depth, scr}) = let
	  val Dpy.SCRDEPTH{depth,draw_cmd, ...} = scr_depth
	  val Dpy.DPY{xdpy=XDpy.XDPY dpyInfo,...} = Dpy.displayOfScr scr
	  val _ = draw_cmd (DrawMaster.DMSG_Flush)
	  val allPlanes = Word.notb 0w0
	  val msg = XReq.encodeGetImage { 
		  drawable = id, 
		  rect = G.mkRect(G.originPt, sz),
		  plane_mask = XTy.PLANEMASK allPlanes, 
		  format = XTy.XYPixmap
		}
	  val {depth, data, visualid} = 
		XReply.decodeGetImageReply (
		  CML.sync (XIo.requestReply (#conn dpyInfo) msg))
	  val swapfn = swapFunc (
		#bitmap_scanline_unit dpyInfo,
		#image_byte_order dpyInfo,
		#bitmap_bit_order dpyInfo)
	  val linesPerPlane = #ht szinfo
	  val bytesPerLine = roundUp (#wid szinfo, #bitmap_scanline_pad dpyInfo) div 8
	  val bytesPerPlane = bytesPerLine * linesPerPlane
	  fun doLine start = swapfn(w8vextract(data, start, SOME bytesPerLine))
	  fun mkLine (i, start) =
		if i = linesPerPlane
		  then []
		  else (doLine start)::(mkLine(i+1, start+bytesPerLine))
	  fun mkPlane (i, start) =
		if i = depth
		  then []
		  else (mkLine(0, start))::(mkPlane(i+1, start+bytesPerPlane))
	  in
	    IMAGE{sz=sz, data=mkPlane(0, 0)}
	  end (* createImageFromPixmap *)

    fun createImageFromTile (DTy.TILE pm) = createImageFromPixmap pm

  end (* Image *)
