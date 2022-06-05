(* font-server.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * The font server is responsible for mapping font names to fonts.  If two
 * different threads open the same font, then they will be able to share
 * the representation.  Eventually, we will do some kind of finalization of
 * fonts.
 *)

signature FONT_SERVER =
  sig

    type font_server

    exception FontNotFound

    val mkFontServer : XDisplay.xdisplay -> font_server
    val openFont : font_server -> string -> FontBase.font

  end (* FONT_SERVER *)

structure FontServer : FONT_SERVER =
  struct

  (* hash tables on font names *)
    structure FontTbl = HashTableFn (struct
	type hash_key = string
	fun hashVal s = HashString.hashString s
	fun sameKey (s1 : string, s2 : string) = (s1 = s2)
      end);

    structure G = Geometry
    structure XTy = XProtTypes

    exception FontNotFound

    datatype req_msg
      = OpenFont of string
    and reply_msg
      = Success of FontBase.font
      | Failure

    datatype font_server = FS of {
	req_ch : req_msg CML.chan,
	reply_ch : reply_msg CML.chan
      }

  (* given message encode and reply decode functions, send and receive a query *)
    fun query (encode, decode) conn = let
	  val requestReply = XIo.requestReply conn
	  fun ask msg = (decode (CML.sync (requestReply (encode msg))))
		handle XIo.LostReply => raise (MLXError.XERROR "[reply lost]")
		     | (XIo.ErrorReply err) =>
			raise (MLXError.XERROR(XPrint.xerrorToString err))
	  in
	    ask
	  end

    val queryFont = query (XRequest.encodeQueryFont, XReply.decodeQueryFontReply)

    fun mkFontServer (xdpy as XDisplay.XDPY{conn, nextXId, ...}) = let
	  open CML
	  val requestAndChk = XIo.requestAndChk conn
	  val queryFont = queryFont conn
	  val reqCh = CML.channel() and replyCh = CML.channel()
	  val fontMap = FontTbl.mkTable(32, Fail "FontMap")
	  val insert = FontTbl.insert fontMap
	  val find = FontTbl.find fontMap
	  fun mkFont id = let
		val {
			min_bounds, max_bounds, min_char, max_char,
			default_char, draw_dir, all_chars_exist, max_byte1,
			font_ascent, font_descent, properties, char_infos, ...
		      } = queryFont {font = id}
		fun inRange c = ((min_char <= c) andalso (c <= max_char))
		val charInfo = (case char_infos
		     of [] => (if (inRange default_char)
			  then fn _ => min_bounds
			  else (fn c =>
			    if (inRange c)
			      then min_bounds
			      else raise FontBase.NoCharInfo))
		      | l => let
			  val tbl = Array.fromList l
			  fun infoExists (XTy.CharInfo{
				char_wid=0, left_bearing=0, right_bearing=0, ...
			      }) = false
			    | infoExists _ = true
			  fun lookup c = if (inRange c)
				then (case (Array.sub(tbl, c - min_char))
				   of XTy.CharInfo{
					char_wid=0, left_bearing=0, right_bearing=0, ...
				      } => NONE
				    | cinfo => SOME cinfo)
				else NONE
			  fun getInfo dflt c = if (inRange c)
				then (case (lookup c) of NONE => dflt() | SOME c => c)
				else dflt()
			  in
			    case (lookup default_char)
			     of NONE => getInfo (fn () => raise FontBase.NoCharInfo)
			      | (SOME c) => getInfo (fn () => c)
			  end)
		val info = if (max_byte1 = 0)
		      then FontBase.FINFO8{
			  min_bounds = min_bounds,
			  max_bounds = max_bounds,
			  min_char = min_char,
			  max_char = max_char,
			  default_char = default_char,
			  draw_dir = draw_dir,
			  all_chars_exist = all_chars_exist,
			  font_ascent = font_ascent,
			  font_descent = font_descent,
			  properties = properties,
			  char_info = charInfo
			}
		      else MLXError.impossible "[mkFont: 16-bit font]"
		in
		  FontBase.FONT{id = id, xdpy = xdpy, info = info}
		end
	  fun openFont name = let
		val newId = nextXId()
val _ = XDebug.trace(XDebug.fontTM, fn () => ["openFont ", name, ", id = ", XPrint.xidToString newId, "\n"])
		val _ = CML.sync (requestAndChk (
			  XRequest.encodeOpenFont {font = newId, name = name}))
val _ = XDebug.trace(XDebug.fontTM, fn () => ["openFont: open succeeded\n"])
		val font = mkFont newId
		in
		  insert (name, font);
		  font
		end
	  fun getFont name = let
		val reply = (case find name
		       of SOME font => Success font
			| NONE => (Success (openFont name)) handle _ => Failure
		      (* end case *))
		in
		  reply
		end
	  fun loop () = let
		val (OpenFont fontName) = CML.recv reqCh
		in
		  CML.send (replyCh, getFont fontName);
		  loop ()
		end
	  in
	    XDebug.xspawn("FontServer", loop);
	    FS{req_ch = reqCh, reply_ch = replyCh}
	  end (* server *)

    fun doReq req (FS{req_ch, reply_ch}) arg = (
	  CML.send(req_ch, req arg);
	  case (CML.recv reply_ch) of Success f => f | Failure => raise FontNotFound)

    val openFont = doReq OpenFont

  end (* FontServer *)
