(* message.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Text message widget.
 *)

signature MESSAGE = 
  sig

    structure W : WIDGET

    type message

    val message : (W.root * W.view * W.arg list) -> message

    val widgetOf : message -> W.widget
    val setText : (message * string) -> unit
    val getText : message -> string

  end (* MESSAGE *)


structure Message : MESSAGE = 
  struct
    structure W = Widget

    open Geometry

    datatype rqst = 
      SetText of string
    | GetText of string SyncVar.ivar
    | GetBounds of W.bounds SyncVar.ivar
    | DoRealize of { env : Interact.in_env, win : W.EXB.window, sz : size }

    datatype message = M of {widget : W.widget, req : rqst CML.chan}

    type textinfo = {text : string, textWid : int, textHt : int}

    fun getLine (font,text,starti,maxx) = let
          val charInfo = Font.charInfoOf font
          val endi = size text
          fun loop (i,curx,endWord,endWordX) = (
                if endi = i then (i,curx)
                else let
                  val c = String.sub(text,i)
                  in
                    if c = #"\n" then (i,curx)
                    else let
                      val Font.CharInfo{char_wid,...} = charInfo(Char.ord c)
                      val nextx = curx+char_wid
                      in
                        if nextx > maxx then
                          if endWord > starti then (endWord,endWordX)
                          else if i > starti then (i,curx)
                          else (i+1,nextx)
                        else let
                          val (endWord,endWordX) =
                                 if Char.isSpace c then (i+1,nextx)
                                 else (endWord,endWordX)
                          in loop (i+1,nextx,endWord,endWordX) end
                      end
                  end
               )
          in loop (starti,0,starti,0) end

    fun mkTextInfo (root,aspect,text,width,fontinfo,bw,padx,pady) = let
          val (font,fonta,fontd) = fontinfo
          val fonth = fonta + fontd
          val xdelta  = 2*(bw + padx)
          val ydelta  = 2*(bw + pady)
          val aspectDelta = Int.max(5, aspect div 10)
          val lowerBound = aspect - aspectDelta
          val upperBound = aspect + aspectDelta
          val SIZE{wid=scrWidth,...} = W.EXB.sizeOfScr(W.screenOf root)
          val wi = if width > 0 then (width,0)
                   else let
                     val width = scrWidth div 2
                     in (width, width div 2) end
          val endi = size text
          fun getSize (i,maxw,txtht,width) =
                if i = endi then (maxw,txtht)
                else if String.sub(text,i) = #"\n" 
                  then getSize(i+1,maxw,txtht+fonth,width)
                else let
                  val (nexti,linex) = getLine (font,text,i,width)
                  val maxw = Int.max(linex,maxw)
                  fun skipWS i = let
                        val c = String.sub (text,i)
                        in
                          if c = #"\n" then i+1
                          else if Char.isSpace c then skipWS(i+1)
                          else i
                        end
                  in
                    getSize((skipWS nexti) handle _ => nexti,
                              maxw,txtht+fonth,width)
                  end
          fun doLayout (width,inc) = let
                val answer as (textWid,textHt) = getSize (0,0,0,width)
                in
                  if inc <= 2 then answer
	          else let
                    val aspect = (100*(textWid + xdelta)) div (textHt + ydelta)
	            in
                      if aspect < lowerBound 
                        then doLayout(width+inc,inc div 2)
                      else if aspect > upperBound 
                        then doLayout(width-inc,inc div 2)
                      else answer
	            end
                end
          val (textWid,textHt) = doLayout wi
          in
            {text=text,textWid=textWid,textHt=textHt} 
          end

    type fontinfo = (W.EXB.font * int * int)
    fun mkFontInfo font = let
          val {ascent=fonta,descent=fontd} = Font.fontHt font
          in (font, fonta, fontd) end

    local open Attrs in
    val attrs = [
        (attr_aspect,         AT_Int,        AV_Int 150),
        (attr_background,     AT_Color,      AV_Str "white"),
        (attr_borderWidth,    AT_Int,        AV_Int 2),
        (attr_font,           AT_Font,       AV_Str "8x13"),
        (attr_foreground,     AT_Color,      AV_Str "black"),
        (attr_gravity,        AT_Gravity,    AV_Gravity W.Center),
        (attr_halign,         AT_HAlign,     AV_HAlign W.HLeft),
        (attr_padx,           AT_Int,        AV_NoValue),
        (attr_pady,           AT_Int,        AV_NoValue),
        (attr_relief,         AT_Relief,     AV_Relief W.Flat),
        (attr_text,           AT_Str,        AV_Str " "),
        (attr_width,          AT_Int,        AV_Int 0)
      ]
    end (* local *)

    type res = {
        aspect : int,
        bg : W.EXB.color,
        borderWidth : int,
        fontinfo : fontinfo,
        fg : W.EXB.color,
        gravity : W.gravity,
        justify : W.halign,
        padx : int,
        pady : int,
        relief : W.relief,
        shades : W.shades,
        textinfo : textinfo ref,
        width : int
      }

    fun getResources (root,attrs) : res = let
          open Attrs
          val aspect = getInt(attrs attr_aspect)
          val bg = getColor(attrs attr_background)
          val font = getFont(attrs attr_font)
          val fontinfo as (_,fonta,_) = mkFontInfo font
          val padx = case getIntOpt(attrs attr_padx) of
                       SOME i => i
                     | NONE => fonta div 2
          val pady = case getIntOpt(attrs attr_pady) of
                       SOME i => i
                     | NONE => fonta div 4
          val text = getString(attrs attr_text)
          val width = getInt(attrs attr_width)
          val borderWidth = getInt(attrs attr_borderWidth)
          in
            {
              aspect = aspect,
              bg = bg,
              borderWidth = borderWidth,
              fontinfo = fontinfo,
              fg = getColor(attrs attr_foreground),
              gravity = getGravity(attrs attr_gravity),
              justify = getHAlign(attrs attr_halign),
              padx = padx,
              pady = pady,
              relief = getRelief(attrs attr_relief),
              shades = W.shades root bg,
              textinfo = ref(mkTextInfo(root,aspect,text,width,fontinfo,
                            borderWidth,padx,pady)),
              width = width
            }
          end

    fun boundsOf ({textinfo,padx,pady,borderWidth,...} : res) = let
          val {textHt,textWid,...} = !textinfo
          val x = textWid + 2*(borderWidth + padx)
          val y = textHt + 2*(borderWidth + pady)
          in
            {x_dim= W.flexDim x,y_dim= W.flexDim y}
          end

    fun drawf (d,sz as SIZE{wid,ht},res : res) = let
          open Drawing
          val {borderWidth=bw,pady,padx,...} = res
          val (font,fonta,fontd) = #fontinfo res
          val {text,textHt,textWid} = !(#textinfo res)
          val y = (case #gravity res of
	            (W.NorthWest | W.North | W.NorthEast) => bw + pady
	          | (W.West | W.Center | W.East) => (ht - textHt) div 2
	          | _ => ht - bw - pady - textHt) + fonta
          val r = mkRect(originPt,sz)
          val fonth = fonta + fontd
          val txtPen = newPen[PV_Foreground (#fg res)]
          fun doText(y,i) = 
                if String.sub(text,i) = #"\n" then doText(y+fonth,i+1)
                else let
                  val (nexti,linewid) = getLine (font,text,i,textWid)
                  val x = (case #gravity res of
	                    (W.NorthWest | W.West | W.SouthWest) => bw + (#padx res)
	                  | (W.North | W.Center | W.South) => (wid - textWid) div 2
	                  | _ => wid - bw - padx - textWid)
                  val x = case #justify res of
                            W.HCenter => x + (textWid - linewid) div 2
                          | W.HRight => x + (textWid - linewid)
                          | W.HLeft => x
                  fun skipWS i = let
                        val c = String.sub (text,i)
                        in
                          if c = #"\n" then i+1
                          else if Char.isSpace c then skipWS(i+1)
                          else i
                        end
                  in
                    drawString d txtPen font 
                      (PT{x=x,y=y},substring(text,i,nexti-i));
                    doText(y+fonth,skipWS nexti)
                  end
          in
            fn () => (
              doText (y,0) handle _ => ();
              case #relief res of
                W.Flat => ()
              | relief => ThreeD.drawRect d 
                         {width=bw,relief=relief,rect=r} (#shades res)
            )
          end

    fun getText ({textinfo,...} : res) = let
          val {text,...} = !textinfo
          in text end

    fun realize (root,{env,win,sz},res,rqstChan) = let
          open CML Interact
          val d = Drawing.drawableOfWin win
          val rqstevt = recvEvt rqstChan
          val InEnv{ci,co,...} = ignoreInput env

          fun handleCI (CI_Redraw _, state as (draw,_)) = (draw ();state)
            | handleCI (CI_Resize (RECT{wid,ht,...}), _) = let
                val sz = SIZE{wid=wid,ht=ht}
                in
                  Drawing.clearDrawable d;
                  (drawf(d,sz,res),sz)
                end
            | handleCI (_,state) = state
  
          fun handleReq (SetText t,(draw,sz)) = let
                val ti = mkTextInfo(root,#aspect res,t,#width res,#fontinfo res,
                           #borderWidth res, #padx res, #pady res)
                val {textinfo,...} = res
                in
                  textinfo := ti;
                  Drawing.clearDrawable d;
                  sync (co CO_ResizeReq);
                  let val draw = drawf(d,sz,res) in draw(); (draw,sz) end
                end
            | handleReq (GetText ans,state) =
                (SyncVar.iPut(ans,getText res); state)
            | handleReq (GetBounds ans,state) = 
                (SyncVar.iPut(ans,boundsOf res); state)
            | handleReq (_,state) = state

          fun loop state =
                select [
                  wrap(rqstevt, fn req => loop(handleReq(req,state))),
                  wrap(ci,fn evt => loop(handleCI(msgBodyOf evt,state)))
                ]
          in loop (drawf (d,sz,res),sz) end

    fun init (root,res as {textinfo,...} : res,rqstChan) = let
          fun handleReq (SetText t) = let
                val ti = mkTextInfo(root,#aspect res,t,#width res,#fontinfo res,
                           #borderWidth res, #padx res, #pady res)
                in textinfo := ti end
            | handleReq (GetText ans) = SyncVar.iPut(ans,getText res)
            | handleReq (GetBounds ans) = SyncVar.iPut(ans,boundsOf res)
            | handleReq (DoRealize arg) = realize (root,arg,res,rqstChan)
          fun loop ()= (handleReq(CML.recv rqstChan);loop())
          in loop ()end

    fun message (root,view,args) = let
          open Attrs
          val attrs = W.findAttr (W.attrs(view, attrs, args))
          val res = getResources (root,attrs)
          val rqstChan = CML.channel ()
          fun boundsOf () = let
                val v = SyncVar.iVar ()
                in
                  CML.send(rqstChan, GetBounds v);
                  SyncVar.iGet v
                end
          in 
            CML.spawn(fn () => init (root,res,rqstChan));
            M {
              widget = W.mkWidget {
                root=root,
                args= fn () => {background = SOME (#bg res)},
                boundsOf = boundsOf,
                realize = fn arg => CML.send(rqstChan,DoRealize arg)
              }, 
              req = rqstChan
            }
          end

    fun widgetOf (M{widget,...}) = widget
    fun setText (M{req,...},v) = CML.send(req, SetText v)
    fun getText (M{req,...}) = let
          val v = SyncVar.iVar ()
          in
            CML.send(req, GetText v);
            SyncVar.iGet v
          end

  end (* Message *)
