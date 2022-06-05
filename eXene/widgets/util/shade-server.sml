
signature SHADE_SERVER =
  sig
    structure EXB : EXENE_BASE

    type shades
    type shade_server

    exception BadShade

    val mkShadeServer : EXB.screen -> shade_server

    val getShades : shade_server -> EXB.color -> shades
    
  end

structure ShadeServer : SHADE_SERVER =
  struct

    structure EXB = EXeneBase

    exception BadShade

    type shades = {
        light : Drawing.pen,
        base : Drawing.pen,
        dark : Drawing.pen
      }
 
    datatype req = GetShades of EXB.color
    type reply = shades option

    datatype shade_server = SS of {
        req : req CML.chan,
        reply : reply CML.chan
      }

    structure RGBTbl = HashTableFn (struct
          type hash_key = EXB.rgb
          fun sameKey (k1 : hash_key, k2) = (k1 = k2)
          fun hashVal (EXB.RGB{red, green, blue}) = red+green+blue
        end);
    type rgb_tbl = shades RGBTbl.hash_table

    fun monochrome scr = 
          EXB.displayClassOfScr scr = EXB.StaticGray andalso 
          EXB.depthOfScr scr = 1

    fun mkShadeServer scr = let
          exception NotFound
          val rgbTbl : rgb_tbl = RGBTbl.mkTable(32, NotFound)
          val rgbIns = RGBTbl.insert rgbTbl
          val rgbFind = RGBTbl.find rgbTbl
 
          val maxI = 0w65535
          fun lighten v c = Word.min(maxI,(v*c) div 0w100) handle _ => maxI
          fun darken v c = Word.min(maxI,(v*c) div 0w100) handle _ => maxI
              
          val lighten = lighten 0w140
          val darken = darken 0w60

          fun color (r,g,b) = 
                EXB.colorOfScr scr (EXB.CMS_RGB {red=r,green=g,blue=b})

          fun mkP c = Drawing.newPen [Drawing.PV_Foreground c]
          fun mkP' t = Drawing.newPen [
                         Drawing.PV_Foreground (EXB.blackOfScr scr),
                         Drawing.PV_Background (EXB.whiteOfScr scr),
                         Drawing.PV_Stipple t,
                         Drawing.PV_FillStyle_OpaqueStippled
                       ]

          fun bwShade (c,rgb) = let
                val lgray = EXB.createTileFromImage scr Images.lightGray
                val dgray = EXB.createTileFromImage scr Images.darkGray
                val (lt,dk) = if EXB.sameColor (c,EXB.whiteOfScr scr)
                                then (lgray,dgray)
                                else (dgray,lgray)
                val s = {light = mkP' lt, base = mkP c, dark = mkP' dk}
                in
                  rgbIns (rgb,s);
                  SOME s
                end handle _ => NONE

          fun grayShade (c,rgb) = let
                val lgray = EXB.colorOfScr scr (EXB.CMS_Name "gray87")
                val dgray = EXB.colorOfScr scr (EXB.CMS_Name "gray44")
                val (lt,dk) = if EXB.sameColor (c,EXB.whiteOfScr scr)
                                then (lgray,dgray)
                                else (dgray,lgray)
                val s = {light = mkP lt, base = mkP c, dark = mkP dk}
                in
                  rgbIns (rgb,s);
                  SOME s
                end handle _ => bwShade(c,rgb)

          fun colorShade (c,rgb) = let
                val EXB.RGB{red,blue,green} = rgb
                fun shade () = let
                      val lt = color (lighten red,lighten green,lighten blue)
                      val dk = color (darken red,darken green,darken blue)
                      val s = {light = mkP lt, base = mkP c, dark = mkP dk}
                      in
                        rgbIns (rgb,s);
                        SOME s
                      end handle _ => NONE
                in
                  if EXB.sameColor(c,EXB.whiteOfScr scr) orelse
                      EXB.sameColor(c,EXB.blackOfScr scr)
                    then grayShade(c,rgb)
                    else shade ()
                end

          val allocShade = if monochrome scr then bwShade else colorShade

          fun handleReq (GetShades c) = let
                val rgb = EXB.rgbOfColor c
                in
                  case rgbFind rgb of NONE => allocShade (c,rgb) | s => s
                end
          val req = CML.channel () and reply = CML.channel ()
          fun loop () = (CML.send(reply,handleReq(CML.recv req)); loop ())
          in
            XDebug.xspawn("ShadeServer", loop);
            SS{req = req, reply = reply}
          end

    fun getShades (SS{req,reply}) color = (
          CML.send(req, GetShades color);
          case CML.recv reply of SOME s => s | _ => raise BadShade
        )

  end

