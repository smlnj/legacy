(* text_list.sml
 *
 * COPYRIGHT (c) 1991,1992 by AT&T Bell Laboratories
 *
 * List widget, for text lists.
 *
 * NOTE: with the value restriction, it might be better to code this
 * as a functor.
 *)

signature TEXT_LIST =
  sig

    structure W : WIDGET

    exception BadIndex

    type 'a text_list
    datatype 'a list_evt = Set of 'a | Unset of 'a

    type 'a list_item (* = (string * 'a * W.wstate) *)
    val mkItem : (string * 'a * W.wstate) -> 'a list_item

    val text_list : (W.root * W.view * W.arg list) ->
	    '2a list_item list -> '2a text_list
         
    val widgetOf : 'a text_list -> W.widget

    val setChosen : 'a text_list -> (int * bool) list -> unit
    val setActive : 'a text_list -> (int * bool) list -> unit
    val getChosen : 'a text_list -> int list
    val getState : 'a text_list -> W.wstate list
    val evtOf : 'a text_list -> 'a list_evt CML.event

    val insert : 'a text_list -> (int * (string * 'a) list) -> unit
    val append : 'a text_list -> (int * (string * 'a) list) -> unit
    val delete : 'a text_list -> int list -> unit
(*
    val setOrigin : 'a text_list -> W.G.point -> unit
    val setHorz : 'a text_list -> int -> unit
    val setVert : 'a text_list -> int -> unit
    val getGeometry : viewport -> { rect : W.G.rect, childSz : W.G.size }
    val geomEvtOf : 'a text_list -> 
          {rect : W.G.rect, childSz : W.G.size} CML.event
*)

  end (* TEXT_LIST *)

structure TextList : TEXT_LIST = 
  struct

    structure W = Widget
    structure TI = ItemList

    exception BadIndex = TI.BadIndex

    open Geometry

    type 'a list_item = (string * 'a * W.wstate)
    fun mkItem x = x

    datatype 'a list_evt = Set of 'a | Unset of 'a

    datatype result = Okay | Error of exn

    datatype input = Bttn of (Interact.mbutton * point)

      (* Input server.
       * At present, it simply reports button down with which
       * button and where.
       *)
    fun input (m,inch) = let
          open Interact
          fun loop () =
                case msgBodyOf (CML.sync m) of
                  MOUSE_FirstDown {but,pt,...} => (
                    CML.send(inch,Bttn(but,pt));
                    waitUp ()
                  )
                | _ => loop ()
          and waitUp () =
                case msgBodyOf (CML.sync m) of
                  MOUSE_LastUp _ => loop ()
                | _ => waitUp ()
          in loop () end

    datatype 'a rqst
      = GetBounds of W.bounds SyncVar.ivar
      | DoRealize of { env : Interact.in_env, win : W.EXB.window, sz : size }
      | SetChosen of ((int * bool) list * result SyncVar.ivar)
      | SetActive of ((int * bool) list * result SyncVar.ivar)
      | Insert of ((int * (string * 'a) list) * result SyncVar.ivar)
      | Delete of (int list * result SyncVar.ivar)
      | GetChosen of int list SyncVar.ivar
      | GetState of W.wstate list SyncVar.ivar

    type 'a item = {
        label : string,         (* label of item *)
        lb : int,               (* left bearing of label *)
        wid : int,              (* width in pixels of label *)
        value : 'a              (* value of item *)
      }

    datatype 'a text_list = TL of {
        widget : W.widget,
        rqst : 'a rqst CML.chan,
        evt : 'a list_evt CML.event
    }

    val dfltFont = "-Adobe-Helvetica-Bold-R-Normal--*-120-*"

      (* Standard font information *)
    fun mkFontInfo font = let
          val {ascent=fonta,descent=fontd} = Font.fontHt font
          in (font, fonta, fontd) end

      (* x and y increments for scrolling and drawing
       * x increment is nominally the width of "0", which doesn't
       * work for non-constant width fonts.
       * y increment is height of item
       *)
    fun setXIncr font = Font.textWidth font "0"
    fun setYIncr ((_,fonta,fontd),bw) = 1 + fonta + fontd + 2*bw

    local open Attrs in
    val attrs = [
        (* (attr_multiset,            AT_Bool,    AV_Bool false), *)
        (* (attr_isVertical,          AT_Bool,    AV_Bool true), *)
        (* (attr_halign,              AT_HAlign,  AV_HAlign W.HLeft), *)
        (attr_borderWidth,         AT_Int,     AV_Int 2),
        (attr_font,                AT_Font,    AV_Str dfltFont),
        (attr_color,               AT_Color,   AV_NoValue),
        (attr_relief,              AT_Relief,  AV_Relief W.Flat),
        (attr_width,               AT_Int,     AV_Int 0),
        (attr_height,              AT_Int,     AV_Int 0),
        (attr_background,          AT_Color,   AV_Str "white"),
        (attr_foreground,          AT_Color,   AV_Str "black"),
        (attr_selectBorderWidth,   AT_Int,     AV_Int 1)
      ]
    val colorAttrs = [
        (attr_selectBackground,    AT_Color,   AV_Str "gray"),
        (attr_selectForeground,    AT_Color,   AV_Str "black")
      ]
    val monoAttrs = [
        (attr_selectBackground,    AT_Color,   AV_Str "black"),
        (attr_selectForeground,    AT_Color,   AV_Str "white")
      ]
    end (* local *)

    type res = {
        multi : bool,
        shades : W.shades,
        fontinfo : (W.EXB.font * int * int),
        fg : W.EXB.color,
        bg : W.EXB.color,
        relief : W.relief,
        borderWidth : int,
        maxslen : int ref,
        stipple : W.EXB.tile,
        xincr : int,
        yincr : int,
        width : int,
        height : int,
        selFg : W.EXB.color,
        selShades : W.shades,
        selBorderWidth : int
      }

    type 'a state = {items : ('a item) TI.items, top : int, numlines : int}

    fun mkRes (root,view,args) = let
          open Attrs
          val attrs = if W.isMonochrome root then monoAttrs@attrs
                      else colorAttrs@attrs
          val attrs = W.findAttr (W.attrs(view, attrs, args))
          val fontinfo as (f,_,_) = mkFontInfo(getFont(attrs attr_font))
          val relief = getRelief (attrs attr_relief)
          val borderWidth = getInt (attrs attr_borderWidth)
          val sborderWidth = getInt (attrs attr_selectBorderWidth)
          val forec = getColor (attrs attr_foreground)
          val backc = getColor (attrs attr_background)
          val sforec = getColor (attrs attr_selectForeground)
          val sbackc = getColor (attrs attr_selectBackground)
          in
            {
              multi = false,
              fontinfo = fontinfo,
              height = Int.max(0,getInt (attrs attr_height)),
              width = Int.max(0,getInt (attrs attr_width)),
              maxslen = ref 0,
              stipple = W.tile root "gray",
              xincr = setXIncr (#1 fontinfo),
              yincr = setYIncr (fontinfo,sborderWidth),
              fg = forec,
              bg = backc,
              shades = W.shades root backc,
              borderWidth = Int.max(borderWidth,0),
              selShades = W.shades root sbackc,
              selFg = sforec,
              selBorderWidth = Int.max(sborderWidth,0),
              relief = relief
            }
          end

    fun mk_item (res : res) = let
          val (font,_,_) = #fontinfo res
          fun mkItem (str,v) = let
                val Font.CharInfo {left_bearing=lb,right_bearing=rb,...}
                      = #overall_info (Font.textExtents font str)
                in
                  {label = str, lb = lb, wid = rb - lb, value = v}
                end
          in mkItem end

    fun mkItems (res : res, evtch, items : 'a list_item list) = let
          fun mkItem i = mk_item res i
          val maxslen = List.foldl (fn ((s,_,_),m) => Int.max(m,size s)) 0 items
          fun mki (s,v,state) = (mkItem(s,v),state)
          fun pickfn ({value,...} : 'a item, true) = CML.send(evtch,Set value)
            | pickfn ({value,...} : 'a item, _) = CML.send(evtch,Unset value)
          in 
            (#maxslen res) := maxslen;
            TI.items {items = map mki items,
                      multiple = #multi res,
                      pickfn = pickfn}
          end

    fun boundsOf (res as {yincr, xincr,maxslen,...} : res, items) = let
          val cnt = TI.numItems items
          val xbase = 2*(#borderWidth res + #selBorderWidth res)
          val (xmin,xnat,xmax) = case #width res of
                                   0 => (1,(!maxslen)+1,NONE)
                                 | w => (w,w,SOME w)
          val x_dim = W.DIM{base=xbase,incr=xincr,min=xmin,nat=xnat,max=xmax}
          val ybase = 2*(#borderWidth res)
(** ONCE WE HAVE SCROLL BARS, THIS CHANGES **)
          val (ymin,ynat,ymax) = case #height res of
                                   0 => (cnt,cnt,SOME cnt)
                                 | h => (h,h,SOME h)
          val y_dim = W.DIM{base=ybase,incr=yincr,min=ymin,nat=ynat,max=ymax}
          in {x_dim=x_dim,y_dim=y_dim} end

    fun drawfns (res : res,win) = let 
          open Drawing
          fun isActive (W.Active _) = true | isActive _ = false
          fun isOn (W.Active v) = v | isOn (W.Inactive v) = v
          val dr = drawableOfWin win
          val bw = #borderWidth res
          val txtPen = newPen[PV_Foreground (#fg res)]
          val iTxtPen = updatePen (txtPen,
                          [PV_FillStyle_Stippled, PV_Stipple (#stipple res)])
          val selTxtPen = newPen[PV_Foreground (#selFg res)]
          val iSelTxtPen = updatePen (txtPen,
                          [PV_FillStyle_Stippled, PV_Stipple (#stipple res)])

          fun drawItem (clr,bw,SIZE{wid,ht}) = let
                val sbw = #selBorderWidth res
                val yincr = #yincr res
                val selshades as {base=selbase,...} = #selShades res
                val {base,...} = #shades res
                val (font,fonta,_) = #fontinfo res
                val inset = bw + sbw + ((#xincr res) div 2)
                val iwid = wid - 2*bw
                fun d (({label,lb,...} : 'a item,st),y) = let
                      val r = RECT{x=bw,y=y,wid=iwid,ht=yincr}
                      val yt = y + fonta + sbw
                      val xt = inset - lb
                      in
                        if isOn st then let
                          val tpen = if isActive st then selTxtPen 
                                     else iSelTxtPen
                          in
                            fillRect dr selbase r;
	                    drawString dr tpen font (PT{x=xt,y=yt},label);
                            ThreeD.drawRect dr 
                              {rect=r,width=sbw,relief=ThreeD.Raised} selshades
                          end
                        else let
                          val tpen = if isActive st then txtPen else iTxtPen
                          in
                            if clr then fillRect dr base r else ();
	                    drawString dr tpen font (PT{x=xt,y=yt},label)
                          end;
                        y + yincr
                      end
                in d end

            (* Update items given by list of integers.
             * Assume the list is sorted. 
             *)
          fun update (me : 'a state, cl, sz) = let
                val {items,top,numlines} = me
                val bot = top + numlines
                val bw = #borderWidth res
                val yincr = #yincr res
                val drawItem = fn i => drawItem (true,bw,sz) i
                fun strip [] = []
                  | strip (l as (i::t)) = if i < top then strip t else l
                fun loop(_,_,[],_) = ()
                  | loop([],_,_,_) = ()
                  | loop(i::t,j,l::ls,y) =
                      if i >= bot then ()
                      else if i > j then loop(i::t,j+1,ls,y+yincr)
                      else (drawItem(l,y);loop(t,j+1,ls,y+yincr))
                fun draw [] = ()
                  | draw [i] = if i >= bot then ()
                               else (
                                 drawItem(TI.item(items,i),bw+yincr*(i-top));())
                  | draw (l as (i::t)) = 
                      if i >= bot then ()
                      else loop(l,i,TI.listItems(items,i,bot-i),bw+yincr*(i-top))
                in
                  draw (strip cl)
                end

            (* Redraw entire widget *)
          fun draw ({items,top,numlines} : 'a state, sz as SIZE{wid,ht}) = let
                val rect = RECT{x=0,y=0,wid=wid,ht=ht}
                val relief = #relief res
                val bw = #borderWidth res
                val shades as {base,...} = #shades res
                val il = TI.listItems (items,top,numlines)
                val drawItem = fn i => drawItem (false,bw,sz) i
                in
                  fillRect dr base rect;
                  List.foldl drawItem bw il;
                  ThreeD.drawRect dr {rect=rect,relief=relief,width=bw} shades
                end
          in (draw,update) end

      (* Returns whether to send a request for size change.
       * At present, we only do this if height attribute is 0,
       * meaning the user has not specified a fixed height, so
       * we try to fit the total number of items.
       *)
    fun newSize ({height,...} : res, _) = height = 0 

      (* Translate a point in window coordinates to
       * the index of an item. The y value must actually lie
       * within the item; we don't care about the x.
       *)
    fun ptToIndex (PT{x,y},res : res,top,numItems) = let
          val y' = (y - (#borderWidth res)) div (#yincr res)
          val index = y' + top
          in
            if y' < 0 orelse index >= numItems then NONE else SOME index
          end

      (* Given a window size, compute how many items can be displayed. *)
    fun getNumLines ({borderWidth,yincr,...} : res, SIZE{ht,...}) =
          Int.max(0,(ht - 2*borderWidth) div yincr)

      (* Generate a list of length len of consecutive integers 
       * starting at start.
       *)
    fun genl (start,len) = let
          fun loop (_,0,l) = rev l
            | loop (i,len,l) = loop(i+1,len-1,i::l)
          in loop (start,len,[]) end

    fun updateMax (items : ('a item) TI.items, maxslen) =
         maxslen := TI.revfold (fn ({label,...},m) => Int.max(m,size label)) 0 items

      (* Given the current top, the number of lines in the window,
       * the new number of items, and the list of items that have
       * been deleted, compute the new top and also whether the
       * window needs to be redisplayed.
       *)
    fun topOnDelete (top,numlines,numitems,l) = let
          val l = Index.chkSort l
          fun prei (cnt,[]) = (cnt,[])
            | prei (arg as (cnt,i::t)) = if i < top then prei(cnt+1,t) else arg
          val (cnt,dl) = prei(0,l)
          val top' = top - cnt
          in
            case dl of
              [] => (top',false)
            | (i::_) => if i >= top + numlines then (top',false)
                        else if top' + numlines <= numitems then (top',true)
                        else (Int.max(0,numitems - numlines),true)
          end

    fun realize ({env,win,sz}, res, items, rqst) = let
          val Interact.InEnv{ci,co,m,k} = Interact.ignoreKey env
          val inch = CML.channel ()
          val rcvi = CML.recvEvt inch
          val (draw,update) = drawfns (res, win)
          val sz = ref sz

          fun setChosen (l, {items, top, numlines}) = let
                val (items', optp) = TI.setChosen (items, l)
                val me' = {items=items', top = top, numlines = numlines}
                val l = map (fn (i,_) => i) l
                val l = case optp of NONE => l | SOME i => i::l
                in
                  update (me', Index.chkUSort l, !sz);
                  me'
                end

          fun handleReq (GetBounds ans, me as {items, top, numlines}) = 
                (SyncVar.iPut(ans,boundsOf(res, items)); me)
            | handleReq (GetChosen ans, me) =
                (SyncVar.iPut(ans,TI.getChosen(#items me)); me)
            | handleReq (GetState ans, me) =
                (SyncVar.iPut(ans,TI.getState(#items me)); me)
            | handleReq (SetActive (l,ans),me) = (let
                val items' = TI.setActive (#items me,l)
                val me' = {items=items',top = #top me, numlines = #numlines me}
                in
                  SyncVar.iPut(ans,Okay);
                  update (me',Index.chkUSort(map (fn (i,_) => i) l),!sz);
                  me'
                end handle e => (SyncVar.iPut(ans,Error e); me))
            | handleReq (SetChosen(l,ans),me) = (let
                val me' = setChosen (l,me)
                in 
                  SyncVar.iPut(ans,Okay); me'
                end handle e => (SyncVar.iPut(ans,Error e); me))
            | handleReq (Insert((i,l),ans),me as {top,items,numlines}) = (let
                val maxslen = List.foldl (fn ((s,_),m) => Int.max(m,size s)) 0 l
                val items' = TI.insert (items,i,map (mk_item res) l)
                val numitems' = TI.numItems items'
                val cnt = length l
                val bot = top + numlines
                val (top',redraw) = if i < top then (top + cnt,false)
                                    else if i >= bot then (top,false)
                                    else (top,true)
                val me' = {items=items',top = top', numlines = numlines}
                in
                  SyncVar.iPut(ans,Okay);
                  if newSize (res, items') 
                    then CML.sync(co Interact.CO_ResizeReq)
                    else ();
                  if redraw 
                    then update (me',genl(i,Int.min(bot-i,numitems'-i)),!sz) 
                    else ();
                  (#maxslen res) := maxslen;
                  me'
                end handle e => (SyncVar.iPut(ans,Error e); me))
            | handleReq (Delete(arg,ans), me as {top,items,numlines}) = (let
                val items' = TI.delete (items,arg)
                val numitems' = TI.numItems items'
                val (top',redraw) = topOnDelete(top,numlines,numitems',arg)
                val me' = {items=items',top = top', numlines = numlines}
                in
                  SyncVar.iPut(ans,Okay);
                  if newSize (res, items') 
                    then CML.sync(co Interact.CO_ResizeReq)
                    else ();
                  if redraw 
                    then draw (me',!sz)
                    else ();
                  updateMax (items',#maxslen res);
                  me'
                end handle e => (SyncVar.iPut(ans,Error e); me))
            | handleReq (_,me) = me

          fun handleI (Bttn (but,pt),me) = let
                fun onOff (Interact.MButton 1) = true | onOff _ = false
                in
                  case ptToIndex (pt,res,#top me,TI.numItems(#items me)) of
                    NONE => me
                  | SOME index => setChosen ([(index,onOff but)], me)
                end 
                
          fun handleCI (Interact.CI_Resize (RECT{wid,ht,...}),{items,top,numlines}) = let
                val newsz = SIZE{wid=wid,ht=ht}
                val numlines = getNumLines (res,newsz)
                in sz := newsz; {items=items,top=top,numlines=numlines} end
            | handleCI (Interact.CI_Redraw _,me) = (draw (me,!sz); me)
            | handleCI (_,me) = me

          fun main me =
                CML.select [
                  CML.wrap(rqst, fn r => main(handleReq(r,me))),
                  CML.wrap (ci, fn msg => main(handleCI(Interact.msgBodyOf msg,me))),
                  CML.wrap (rcvi, fn i => main(handleI(i,me)))
                ]
          in
            CML.spawn (fn () => input (m,inch));
            main {items=items, top=0, numlines=getNumLines (res,!sz)}
          end

    fun init (res, items, rqst) = let
          fun handleReq (GetBounds ans,items) = 
                (SyncVar.iPut(ans,boundsOf(res,items)); items)
            | handleReq (GetChosen ans,items) =
                (SyncVar.iPut(ans,TI.getChosen items); items)
            | handleReq (GetState ans, me) =
                (SyncVar.iPut(ans,TI.getState items); items)
            | handleReq (SetChosen (l,ans),items) = (let
                val (items',_) = TI.setChosen (items,l)
                in
                  SyncVar.iPut(ans,Okay);
                  items'
                end handle e => (SyncVar.iPut(ans,Error e); items))
            | handleReq (SetActive (l,ans),items) = (let
                val items' = TI.setActive (items,l)
                in
                  SyncVar.iPut(ans,Okay);
                  items'
                end handle e => (SyncVar.iPut(ans,Error e); items))
            | handleReq (Insert((i,il),ans),items) = (let
                val items' = TI.insert (items,i,map (mk_item res) il)
                val maxslen = List.foldl (fn ((s,_),m) => Int.max(m,size s)) 0 il
                in
                  SyncVar.iPut(ans,Okay);
                  (#maxslen res) := maxslen;
                  items'
                end handle e => (SyncVar.iPut(ans,Error e); items))
            | handleReq (Delete(arg,ans),items) = (let
                val items' = TI.delete (items,arg)
                in
                  updateMax (items',#maxslen res);
                  SyncVar.iPut(ans,Okay);
                  items'
                end handle e => (SyncVar.iPut(ans,Error e); items))
            | handleReq (DoRealize arg,items) = realize (arg,res,items,rqst)
          fun loop items = loop(handleReq(CML.sync rqst,items))
          in loop items end

    fun text_list (root,view,args) items = let
          val res = mkRes (root,view,args)
          val evtch = CML.channel()
          val items = mkItems (res, evtch, items)
          val rqst = CML.channel()
          fun boundsOf () = let
                val v = SyncVar.iVar ()
                in
                  CML.send(rqst, GetBounds v);
                  SyncVar.iGet v
                end
          val w = W.mkWidget {
                    root=root,
                    args = fn () => {background = SOME (#bg res)},
                    boundsOf = boundsOf,
                    realize = fn arg => CML.send(rqst,DoRealize arg)
                  }
          in
            CML.spawn (fn () => init (res,items,CML.recvEvt rqst));
            TL {widget=w,rqst=rqst,evt=CML.recvEvt evtch}
          end

    fun evtOf (TL{evt,...}) = evt
    fun widgetOf (TL{widget,...}) = widget

    fun setF f (TL{rqst,...}) arg = let
          val cv = SyncVar.iVar()
          in
            CML.send(rqst,f(arg,cv));
            case SyncVar.iGet cv
	     of Okay => ()
              | Error e => raise e
          end
    fun setChosen l = setF SetChosen l
    fun setActive l = setF SetActive l
    fun insert l = setF Insert l
    fun delete l = setF Delete l
    fun append tl (i,items) = insert tl (i+1,items)

    fun getF f (TL{rqst,...}) = let
          val cv = SyncVar.iVar()
          in
            CML.send(rqst,f cv);
            SyncVar.iGet cv
          end
    fun getChosen l = getF GetChosen l
    fun getState l = getF GetState l

  end (* TextList *)
