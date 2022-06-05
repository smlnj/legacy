(* font-size-server.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

signature SCALABLE_FONT =
  sig
    structure W : WIDGET

    datatype font_style = Normal | Italic | Bold

    val dfltFontSz : int

    type font_server

    val fontServer : (W.root * W.view * W.arg list) -> font_server
    val findFont : font_server -> (font_style * int) -> W.EXB.font option

  end

structure ScalableFont : SCALABLE_FONT =
  struct

    structure W = Widget

    open CML

    datatype font_style = Normal | Italic | Bold

  (* eventually, this should come from the style *)
    val dfltFontSz = 12   (* points *)
    val rmFont = "-adobe-times-medium-r-normal--*-%d-*-*-p-*-iso8859-1"
    val itFont = "-adobe-times-medium-i-normal--*-%d-*-*-p-*-iso8859-1"
    val bfFont = "-adobe-times-bold-r-normal--*-%d-*-*-p-*-iso8859-1"

    val fmtRmFont = Format.format rmFont
    val fmtItFont = Format.format itFont
    val fmtBfFont = Format.format bfFont

  (* note that font sizes are specified in tenths of a point *)
    fun fmtFontName (Normal, sz) = fmtRmFont [Format.INT(sz*10)]
      | fmtFontName (Italic, sz) = fmtItFont [Format.INT(sz*10)]
      | fmtFontName (Bold, sz) = fmtBfFont [Format.INT(sz*10)]

    datatype font_server = FS of {
        req : (font_style * int) chan,
        reply : W.EXB.font option chan
      }

    fun fontServer (root, view, args) = let
          val req = channel () and reply = channel ()
          val openFont = W.openFont root
          fun loadFont (_, 0) = NONE
            | loadFont (style, sz) =
		(SOME(openFont (fmtFontName (style, sz))))
                  handle Font.FontNotFound =>(
                    TextIO.output(TextIO.stdErr, concat[
		        "Font size", Int.toString sz, " : not found\n"]
		      );
                    loadFont(style, sz-1))

          fun handleReq (fonts, (style, reqsz)) = let
		fun match (sty, s, _) = (sty = style) andalso (s = reqsz)
                in
                  case List.find match fonts
		   of (SOME(_, _, f)) => (send(reply, SOME f); fonts)
                    | NONE => (case loadFont(style, reqsz)
			 of NONE => (send(reply, NONE); fonts)
			  | (SOME f) => (
			      send(reply, SOME f);
			      (style, reqsz, f)::fonts)
			(* end case *))
		  (* end case *)
                end

          fun loop flist = loop (handleReq(flist, recv req))
          in
            spawn (fn () => loop[]);
            FS{req=req,reply=reply}
          end

    fun findFont (FS{req,reply}) sz = (send(req, sz); recv reply)

  end
