signature VIEW_FONT =
  sig
    structure W : WIDGET

    val dfltFontSz : int

    type font_server

    val mkFontServer : W.root -> font_server
    val findFont : font_server -> int -> W.EXB.font option
  end

structure ViewFont : VIEW_FONT =
  struct
    structure W = Widget

    open CML

    val dfltFontSz = 14   (* points *)
    val fontType = "-adobe-times-medium-r-normal--%d-*-*-*-p-*-iso8859-1"

    datatype font_server = 
      FS of {
        req : int CML.chan,
        reply : W.EXB.font option CML.chan
      }

    fun mkFontServer root = let
          val req = channel () and reply = channel ()
          val openFont = Font.openFont (W.displayOf root)
          val fmtFontName = Format.format fontType
          fun loadFont 0 = NONE
            | loadFont sz = (SOME(openFont (fmtFontName [Format.INT sz])))
                handle Font.FontNotFound => 
                  (print("Font size" ^ (makestring sz) ^ " : not found\n");
                   loadFont(sz-1))

          fun handleReq (fonts, reqsz) = let
                val sz = truncate((real reqsz)*1.4)
                in
                  case ListUtil.findOne (fn (s,_) => s = sz) fonts of
                    SOME (_,f) => (send(reply, SOME f); fonts)
                  | NONE => 
                      (case loadFont sz of
                        NONE => (send(reply, NONE); fonts)
                      | SOME f => (send(reply, SOME f); (reqsz,f)::fonts)
                      )
                end

          fun loop flist = loop (handleReq(flist,accept req))
          in
            spawn (fn () => loop[]);
            FS{req=req,reply=reply}
          end

    fun findFont (FS{req,reply}) sz = (send(req, sz); accept reply)
  end
