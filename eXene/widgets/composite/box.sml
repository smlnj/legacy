(* box.sml
 *
 * COPYRIGHT (c) 1991-1994 by AT&T Bell Laboratories. See COPYRIGHT file for details.
 *
 * Composite widget, using boxes and glue to lay out child widgets.
 *)

signature BOX =
  sig

    structure W : WIDGET

    exception BadIndex

    datatype box
      = HzTop of box list
      | HzCenter of box list
      | HzBottom of box list
      | VtLeft of box list
      | VtCenter of box list
      | VtRight of box list
      | Glue of { nat : int, min : int, max : int option }
      | WBox of W.widget

    type box_layout

    val mkLayout : W.root -> box -> box_layout
    val layout : (W.root * W.view * W.arg list) -> box -> box_layout
    val widgetOf : box_layout -> W.widget
    val insert : box_layout -> (int * box list) -> unit
    val append : box_layout -> (int * box list) -> unit
    val delete : box_layout -> int list -> unit
    val mapBox : box_layout -> int list -> unit
    val unmapBox : box_layout -> int list -> unit

  end (* BOX *)

structure Box : BOX =
  struct

    structure W = Widget
    structure I = Index
(* DEBUG
    structure F = Format
    fun rectToStr (Geometry.RECT{x,y,wid,ht}) = F.format "(%d,%d,%d,%d)" (map F.INT [x,y,wid,ht])
    fun print s = (TextIO.output(TextIO.stdErr,s); TextIO.flushOut TextIO.stdErr)
   END DEBUG *)

    exception BadIndex = I.BadIndex

    datatype box =
        HzTop of box list
      | HzCenter of box list
      | HzBottom of box list
      | VtLeft of box list
      | VtCenter of box list
      | VtRight of box list
      | Glue of { nat : int, min : int, max : int option }
      | WBox of W.widget

    open Geometry Widget BoxLayout

    datatype rqst =
        GetSize 
      | DoRealize of {
          env : Interact.in_env,
          win : W.EXB.window,
          sz : size
        }
      | Insert of (int * box list)
      | Delete of int list
      (* | Replace of (int * int option * box list) *)
      | Map of bool * int list

    datatype reply = Error of exn | Okay

    datatype box_layout = Box of {
        reqChan : rqst CML.chan,
        repChan : reply CML.chan,
        widget : W.widget
      }
      
    type box_rep = {
        widget : W.widget,
        win : W.EXB.window,
        rect : rect,
        co : Interact.cmd_out CML.event
      }

    type layout_rep = {
        rect : rect,
        clist : (bool * box_item * box_rep list) list
      }

    val flexDim = DIM{base=0,incr=1,min=0,nat=0,max=NONE}

    fun mkVGlue {nat, min, max} =
        G {
          x_dim = flexDim,
          y_dim = DIM{base=0,incr=1,min=min,nat=nat,max=max}
        }
    fun mkHGlue {nat, min, max} =
        G {
          y_dim = flexDim,
          x_dim = DIM{base=0,incr=1,min=min,nat=nat,max=max}
        }

    fun makeItem glueFn box = let
          val wl : widget list ref = ref []
          fun cvt (HzTop boxes) = HB(VTop, map hcvt boxes)
            | cvt (HzCenter boxes) = HB(VCenter, map hcvt boxes)
            | cvt (HzBottom boxes) = HB(VBottom, map hcvt boxes)
            | cvt (VtLeft boxes) = VB(VTop, map vcvt boxes)
            | cvt (VtCenter boxes) = VB(VCenter, map vcvt boxes)
            | cvt (VtRight boxes) = VB(VBottom, map vcvt boxes)
            | cvt (Glue arg) = glueFn arg
            | cvt (WBox w) = (wl := w::(!wl); W w)
          and hcvt (WBox w) = (wl := w::(!wl); W w)
            | hcvt (Glue arg) = mkHGlue arg
            | hcvt arg = cvt arg
          and vcvt (WBox w) =  (wl := w::(!wl); W w)
            | vcvt (Glue arg) = mkVGlue arg
            | vcvt arg = cvt arg
          val b = cvt box
          in
            (b, !wl)
          end

    fun genFns (HzTop boxes) = (fn cl => HB(VTop, cl), makeItem mkHGlue, boxes)
      | genFns (HzCenter boxes) = (fn cl => HB(VCenter, cl), makeItem mkHGlue, boxes)
      | genFns (HzBottom boxes) = (fn cl => HB(VBottom, cl), makeItem mkHGlue, boxes)
      | genFns (VtLeft boxes) = (fn cl => VB(VTop, cl), makeItem mkVGlue, boxes)
      | genFns (VtCenter boxes) = (fn cl => VB(VCenter, cl), makeItem mkVGlue, boxes)
      | genFns (VtRight boxes) = (fn cl => VB(VBottom, cl), makeItem mkVGlue, boxes)
      | genFns _ = raise LibBase.Impossible "Box.genFns"

      (* Compute bounds for box layout.
       *)
    val layoutSize = compSize

    fun cloop co () = (CML.sync co; cloop co ())
    fun cleanup ({win,co,...} : box_rep) = (
          EXeneWin.destroyWin win;
          CML.spawn (cloop co);
	  ()
        )

    fun mapfn mkMapped = let
          val mf = case mkMapped of
                     true => EXeneWin.mapWin
                   | false => EXeneWin.unmapWin
          fun mapf (r : box_rep) = mf(#win r)
          in
            fn (item as (isMapped,box,repl)) =>
              if isMapped = mkMapped then item
              else (
                app mapf repl;
                (mkMapped, box, repl)
              )
          end

    fun anyVisible [] = false
      | anyVisible ((true,_,_)::_) = true
      | anyVisible (_::rest) = anyVisible rest

    fun makeCO cl = let
          fun f' ({win,co,...} : box_rep,l) = CML.wrap(co, fn evt => (win, evt))::l
          fun f ((_,_,repl),l) = List.foldl f' l repl
          in
            CML.choose(List.foldl f [] cl)
          end

    fun natRect w = mkRect(originPt,natSize w)

    fun updateRect (w as {rect, win, widget, co}, nrect) =
          if rect = nrect then w
          else (
(* DEBUG
            print (implode["update rect: ",rectToStr nrect,"\n"]);
   END DEBUG *)
            EXeneWin.moveAndResizeWin win nrect;
            {rect=nrect, win = win, widget = widget, co = co}
          )

    fun mkLayout root (w as WBox _) = mkLayout root (HzCenter [w])
      | mkLayout root (g as Glue _) = mkLayout root (HzCenter [g])
      | mkLayout root boxes = let
          open CML Interact
          val rqstChan = channel () and repChan = channel () and sizeChan = channel ()
          val (cvtFn, itemFn, clist) = genFns boxes
          val scr = screenOf root
          fun getvis l = cvtFn(I.find (fn (_,(true,v,_)) => SOME v | (_,(false,_,_)) => NONE) l)

          fun realizeBox {env=inenv as InEnv{co=myco,...}, win, sz} ctree = let
                val (my_inenv, my_outenv) = createWinEnv ()
                val router = Router.mkRouter (inenv, my_outenv, [])
                val InEnv{ci=myci,...} = ignoreInput my_inenv
                fun getVis (me : layout_rep) = getvis (#clist me)
                val rect = mkRect(originPt,sz)
                val (_,places) = compLayout (rect, getvis ctree)
        
                fun reposition (clist,rlist) = let
                      fun repos ([],rl,l) = (rev l,rl)
                        | repos (_,[],_) = raise LibBase.Impossible "Box.realizeBox"
                        | repos (w::wl,(_,r)::rl,l) = repos(wl,rl,(updateRect(w,r))::l)
                      fun doRepos ([],_,cl) = cl
                        | doRepos ((rep as (false,_,_))::rest,rl,cl) = doRepos(rest,rl,rep::cl)
                        | doRepos ((rep as (true,b,[]))::rest,rl,cl) = doRepos(rest,rl,rep::cl)
                        | doRepos ((true,b,wl)::rest,rl,cl) = let
                            val (repl,rl') = repos (wl,rl,[])
                            in doRepos(rest,rl',(true,b,repl)::cl) end
                      in
                        doRepos(rev clist, rlist, [])
                      end

                fun zombie (me : layout_rep) = let
                      val childco = makeCO (#clist me)
                      val bnds = fixBounds(1,1)
                      fun handleReq GetSize = send (sizeChan, bnds)
                        | handleReq _ = ()
                      fun loop () =
                            loop(select [
                              wrap(recvEvt rqstChan, handleReq),
                              wrap (myci, fn _ => ()),
                              wrap (childco, fn _ => ())
                            ])
                      in
                        loop()
                      end

                fun mkBoxRep rectfn w = let 
                      val rect = rectfn w
                      val sz = sizeOfRect rect
                      val (ienv, oenv as OutEnv{co,...}) = createWinEnv ()
                      val cwin = wrapCreate (win, rect,argsOf w)
                      val co = wrapQueue co
                      val rep = {
                          widget = w,
                          win = cwin,
                          rect = rect,
                          co = co
                        }
                      in
                        Router.addChild router (cwin, oenv);
                        realizeFn w {env=ienv, win=cwin, sz=sz};
                        rep
                      end

                fun initFn (clist,rlist) = let
                      fun mkBox ([],rl,l) = (rev l,rl)
                        | mkBox (_,[],_) = raise LibBase.Impossible "Box.initFn"
                        | mkBox (w::wl,(_,r)::rl,l) = mkBox(wl,rl,(mkBoxRep (fn _ => r) w)::l)
                      fun init ([],_,cl) = cl
                        | init ((ison,b,[])::rest,rl,cl) = init(rest,rl,(ison,b,[])::cl)
                        | init ((false,b,wl)::rest,rl,cl) = 
                            init(rest,rl,(false, b, map (mkBoxRep natRect) wl)::cl)
                        | init ((true,b,wl)::rest,rl,cl) = let
                            val (repl,rl') = mkBox (wl,rl,[])
                            in 
                              app (fn ({win,...} : box_rep) => EXeneWin.mapWin win) repl;
                              init(rest,rl',(true,b,repl)::cl) 
                            end
                      in
                        init(rev clist, rlist, [])
                      end
                       
                fun insertFn box = let
                      val (b,wl) = itemFn box
                      in
                        (false, b, map (mkBoxRep natRect) wl)
                      end

                fun resize (me : layout_rep) = let
                      val (fits,nlist) = compLayout (#rect me, getVis me)
(* DEBUG
                      val _ = print (implode["resize: rect = ",rectToStr (#rect me),"\n"])
                      val _ = print (F.format "resize: fits = %b\n        nlist =\n" [F.BOOL fits])
                      val _ = app (fn (_,r) => print (implode["        ",rectToStr r,"\n"])) nlist
   END DEBUG *)
                      val clist' = reposition (#clist me, nlist)
                      val me' = {rect = #rect me, clist = clist'}
                      in
                        if not fits then sync(myco CO_ResizeReq) else ();
                        me'
                      end

                fun handleCO (me, (_,CO_ResizeReq)) = resize me
                  | handleCO (me, (_,CO_KillReq)) = me (* FIX *)

                fun handleCI (me : layout_rep, CI_Resize r) = let
                      val nrect = mkRect(originPt, sizeOfRect r)
                      val (_,nlist) = compLayout (nrect, getVis me)
                      in
                        {rect=nrect, clist=reposition (#clist me, nlist)}
                      end
                  | handleCI (me, CI_ChildDeath child) =
                        (Router.delChild router child; me)
                  | handleCI (me, CI_OwnDeath) = zombie me
                  | handleCI (me, _) = me

                fun handleReq (me,rqst) = case rqst of
                      GetSize => (send (sizeChan, layoutSize (getVis me)); me)
                    | Insert (index,bl) => (let
                        val bl' = map insertFn bl 
                        val ct' = I.insert(#clist me,index,bl')
                        in 
                          send(repChan, Okay);
                          main {rect= #rect me, clist=ct'}
                        end handle e => (send(repChan, Error e); me))
                    | Delete indices => (let
                        val (ct',dl) = I.delete(#clist me, I.chkSort indices)
                        val me' = {rect= #rect me, clist=ct'}
                        in 
                          app (fn (_,_,repl) => app cleanup repl) dl;
                          send(repChan, Okay);
                          if anyVisible dl then main (resize me') else main me'
                        end handle e => (send(repChan, Error e); me))
                    | Map (mapped,indices) => (let
                        val ct' = I.doMap(#clist me, mapfn mapped, I.chkSort indices)
                        in 
                          send(repChan, Okay);
                          resize {rect= #rect me, clist=ct'}
                        end handle e => (send(repChan, Error e); me))
                    | DoRealize _ => me

                and main me = let
                      val childco = makeCO (#clist me)
                      fun loop me =
                            loop(select [
                              wrap(childco, fn msg => handleCO (me, msg)),
                              wrap(myci, fn evt => handleCI (me, msgBodyOf evt)),
                              wrap(recvEvt rqstChan, fn msg => handleReq (me,msg))
                            ])
                      in
                        loop me
                      end
                in
                  main{rect = rect, clist = initFn (ctree,places)}; ()
                end

          fun initItemFn vis b = let
                val (box,wl) = itemFn b
                in (vis,box,wl) end

          fun initLoop ct = 
                case (recv rqstChan) of 
                  GetSize => (send (sizeChan, layoutSize (getvis ct)); initLoop ct)
                | DoRealize arg => realizeBox arg ct
                | Insert (index,bl) => (let
                    val ct' = I.insert(ct,index,map (initItemFn false) bl)
                    in 
                      send(repChan, Okay);
                      initLoop ct'
                    end handle e => (send(repChan, Error e); initLoop ct))
                | Delete indices => (let
                    val (ct',_) = I.delete(ct, I.chkSort indices)
                    in 
                      send(repChan, Okay);
                      initLoop ct'
                    end handle e => (send(repChan, Error e); initLoop ct))
                | Map (mapped,indices) => (let
                    val ct' = I.doMap(ct, fn (_,b,wl) => (mapped,b,wl), I.chkSort indices)
                    in 
                      send(repChan, Okay);
                      initLoop ct'
                    end handle e => (send(repChan, Error e); initLoop ct))
          in
            spawn (fn () => initLoop (map (initItemFn true) clist));
            Box {
              widget =mkWidget{
                  root = root,
                  args = fn () => {background = NONE},
                  boundsOf = (fn () => (send (rqstChan, GetSize); recv sizeChan)),
                  realize = (fn arg => (send (rqstChan, DoRealize arg)))
                },
              reqChan = rqstChan,
              repChan = repChan
            }
          end (* mkLayout *)

    fun layout (root,view,_) box = let
          val Box{widget,reqChan,repChan} = mkLayout root box
          val widget' = Background.background (root,view,[]) widget
          in
            Box {
              widget = widget',
              reqChan = reqChan,
              repChan = repChan
            }
          end

    fun widgetOf (Box{widget,...}) = widget
    local
      fun command wrapfn (Box{reqChan,repChan,...}) =
            fn arg => 
              (CML.send(reqChan,wrapfn arg);
              case CML.recv repChan of Error e => raise e | Okay => ())
    in
    val insert = command Insert
    fun append box (i,bl) = insert box (i+1,bl)
    val delete = command Delete
    (* val replace = command Replace *)
    val mapBox = command (fn l => Map(true,l))
    val unmapBox = command (fn l => Map(false,l))
    end (* local *)

  end (* Box *)
