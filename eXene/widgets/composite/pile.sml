(* pile.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Pile widget, for managing a collection of widgets, one piled on top of 
 * another.
 *
 * Fix by David Benson concerning resizing
 *)

signature PILE =
sig
  structure W : WIDGET

  type pile

  exception NoWidgets
  exception BadIndex

  val pile : (W.root * W.view * W.arg list) -> W.widget list -> pile
  val mkPile : W.root -> W.widget list -> pile
  val widgetOf : pile -> W.widget

  val insert : pile -> (int * W.widget list) -> unit
  val append : pile -> (int * W.widget list) -> unit
  val delete : pile -> int list -> unit

  val mkVisible : pile -> int -> unit
  val visible : pile -> int
  val size : pile -> int
end;

structure Pile : PILE =
struct
  structure W = Widget
  structure I = Index
 
  exception NoWidgets
  exception BadIndex = I.BadIndex

  datatype request = Bounds
                   | DoRealize of
                       {
                        env : Interact.in_env,
                        win : W.EXB.window,
                        sz : W.G.size
                       }
                   | Visible of int option CML.chan
                   | Size of int CML.chan
                   | MakeVis of int
                   | Insert of int * W.widget list
                   | Delete of int list

  datatype reply = Okay | Error of exn
    
  datatype pile = Pile of
                    {
                     widget : W.widget, 
                     repChan : reply CML.chan,
                     reqChan : request CML.chan
                    }

  datatype item = W of
                    {
                     widget : W.widget, 
                     win : W.EXB.window,
                     co : Interact.cmd_out CML.event
                    }

  datatype 'a pile_rep = Empty
                       | P of
                           {  
                            top : int,
                            widget : 'a,
                            wlist : 'a list
                           }

  fun err s = print("Pile."^s^"\n")
  val DEBUG_ = false
  fun debug s = if DEBUG_ then err s else ()

  fun cloop co () = (CML.sync co; cloop co ())

  fun isValid (Empty,0) = true
    | isValid (Empty,_) = false
    | isValid (P{wlist,...},i) = Index.isValid(wlist,i)

  fun topIndex Empty = NONE
    | topIndex (P{top,...}) = SOME top

  fun topi Empty = raise LibBase.Impossible "Pile.topi"
    | topi (P{top,...}) = top

  fun topWidget Empty = raise LibBase.Impossible "Pile.topWidget"
    | topWidget (P{widget,...}) = widget

  fun topWin Empty = raise LibBase.Impossible "Pile.topWin"
    | topWin (P{widget=W{win,...},...}) = win

  fun size Empty = 0
    | size (P{wlist,...}) = length wlist

  val dfltDim = W.DIM {base=1, incr=1, min=0, nat=0, max=NONE}
  val dfltBounds = { x_dim = dfltDim, y_dim = dfltDim }
  fun bounds f Empty = dfltBounds
    | bounds f (P{widget,...}) = f widget

  fun deleteW (Empty,_) = raise BadIndex
    | deleteW (P{wlist,top,widget},indices) =
        let
          val indices = I.chkSort indices
        in
          case I.delete(wlist,indices)
            of ([],dlist) => (Empty,dlist)
             | (wlist',dlist) =>
                 (debug("deleteW: # wlist'="^(Int.toString(List.length wlist')));
                  (case I.preIndices (top,indices)
                     of NONE => (P{wlist=wlist',top=0,widget= hd wlist'},dlist)
                      | SOME j => (P{wlist=wlist',top= top-j,widget= widget},dlist)
                  (*esac*)))
        end handle _ => raise BadIndex

      (* insertW:
       * Assume wl <> []
       *)
  fun insertW (Empty,0,wl) = P{wlist=wl,top=0,widget= hd wl}
    | insertW (Empty,_,_) = raise BadIndex
    | insertW (P{wlist,top,widget},index,wl) =
        let
          val wlist' = I.insert(wlist,index,wl)
          val top' = if index <= top then top + (length wl) else top
        in
          P{wlist=wlist',top=top',widget=widget} 
        end handle _ => raise BadIndex

  fun makeVis (Empty,_) = raise BadIndex
    | makeVis (P{wlist,...},i) =
        let
          val w = List.nth(wlist,i)
        in 
          (P{wlist=wlist,top=i,widget=w}, w) 
        end handle _ => raise BadIndex

  fun makeReal (mkr, Empty) = Empty
    | makeReal (mkr, P{top,widget,wlist}) =
        let
          val wl = map mkr wlist
        in
          P{top = top,wlist = wl,widget = List.nth(wl,top)}
        end

  fun destroy (W{win,co,...}) = (W.EXW.destroyWin win; CML.spawn (cloop co); ())

  fun mkPile root widgets =
        let
          open CML Geometry Interact W.EXB W.EXW
          val repChan = channel ()
          val reqChan = channel ()
          val sizeChan = channel ()
          val reqEvt = recvEvt reqChan

          fun makeCOEvt Empty = choose []
            | makeCOEvt (P{wlist,...}) =
                let
                  fun mkEvt(W{co,...},i) = wrap(co, fn evt => (i,evt))
                  fun mkL ([],_) = []
                    | mkL (w::wl,i) = (mkEvt(w,i))::(mkL(wl,i+1))
                in
                  choose(mkL(wlist,0))
                end

          fun realize {env = inenv as InEnv{co=myco,...}, win, sz} widgets =
                let
                  val (my_inenv, my_outenv) = createWinEnv ()
                  val InEnv{ci=myci,...} = ignoreInput my_inenv
                  val router = Router.mkRouter (inenv, my_outenv, [])
                  val bounds = bounds (fn W{widget,...} => W.boundsOf widget)

                  fun mkReal sz =
                        let
                          val rect = mkRect(originPt, sz)
                        in
                          fn widget =>
                               let
                                 val cwin = W.wrapCreate (win, rect,W.argsOf widget) 
                                 val (cinenv, coutenv as OutEnv{co,...}) = createWinEnv ()
                               in
                                 Router.addChild router (cwin, coutenv);
                                 configureWin cwin [WC_StackMode Below];
                                 W.realizeFn widget {env=cinenv, win=cwin, sz=sz};
                               (*debug "mkReal widget: realizeFn finished.";*)
                               (*CML.sync(CML.timeOutEvt(Time.fromMilliseconds 800));*)
                               (*debug "mkReal widget: starting mapWin...";*)
                                 mapWin cwin;
                                 W{
                                   widget = widget,
                                   win = cwin,
                                   co = co
                                  }
                               end
                        end

                  fun zombie me =
                        let
                          val childco = makeCOEvt me
                          fun handleReq (Visible rc) = send(rc,topIndex me)
                            | handleReq (Size rc) = send(rc,size me)
                            | handleReq Bounds = send(sizeChan, bounds me)
                            | handleReq _ = ()
                          fun loop () =
                                loop(
                                  select [
                                    wrap(reqEvt, handleReq),
                                    wrap (myci, fn _ => ()),
                                    wrap (childco, fn _ => ())
                                         ])
                        in
                          loop()
                        end

                  (* FIX child requests own death *)
                  fun handleCO(me,i,CO_ResizeReq) =
                        (case topIndex me
                           of SOME j => if i = j then sync(myco CO_ResizeReq) else ()
                            | NONE => ()
                        (*esac*))
                    | handleCO(_,_,CO_KillReq) = ()

                  fun handleCI (me, CI_Resize (RECT{x,y,wid,ht})) =
                        let
                          val sz = SIZE{wid=wid,ht=ht}
                        in
                          let val win = topWin me in resizeWin win sz end handle _ => ();
                          main(sz,me)
                        end
                    | handleCI (_, CI_ChildDeath w) = Router.delChild router w
                    | handleCI (me, CI_OwnDeath) = zombie me
                    | handleCI _ = ()

                  and main (sz,me) =
                        let
                          val childco = makeCOEvt me

                          fun handleReq (Visible repc) = send(repc, topIndex me)
                            | handleReq (Size repc) = send(repc, size me)
                            | handleReq Bounds = send(sizeChan, bounds me)
                            | handleReq (MakeVis i) =
                                ((*debug("handleReq: MakeVis "^(Int.toString i));*)
                                 let
                                   val (me',W{win,widget,...}) = makeVis(me,i)
                                 in
                                   configureWin win [WC_StackMode Above, WC_Size sz];
                                   if W.okaySize(widget, sz) then () else sync(myco CO_ResizeReq);
                                   send(repChan,Okay);
                                   main (sz,me')
                                 end handle e => send(repChan, Error e))
                            | handleReq (Delete indices) =
                                (let
                                   val (me',dlist) = deleteW(me,indices)
                                   val W{win,...} = topWidget me
                                 in
                                   send(repChan,Okay);
                                   let
                                     val W{win=win', widget,...} = topWidget me'
                                   in
                                     if sameWindow(win,win') then ()
                                     else
                                     (configureWin win' [WC_StackMode Above, WC_Size sz];
                                      if W.okaySize(widget, sz) then () else sync(myco CO_ResizeReq))
                                   end handle _ => sync(myco CO_ResizeReq);
                                   app destroy dlist;
                                   main(sz,me')
                                 end handle e => send(repChan, Error e))
                            | handleReq (Insert (index,wl)) =
                                ((if isValid(me,index) then 
                                    case topIndex me
                                      of NONE =>
                                           let
                                             val sz' = W.natSize (hd wl)
                                             val me' = insertW(me,index,map (mkReal sz') wl)
                                           in
                                             send(repChan, Okay);
                                             sync(myco CO_ResizeReq);
                                             main(sz', me')
                                           end
                                       | _ =>
                                           let
                                             val me' = insertW(me,index,map (mkReal sz) wl)
                                           in 
                                             send(repChan, Okay);
                                             main(sz, me')
                                           end handle e => send(repChan, Error e)
                                  else send(repChan, Error BadIndex))
                                    handle e => send(repChan, Error e))
                            | handleReq _ = ()

                          fun loop () =
                                loop(
                                  select [
                                    wrap(reqEvt, handleReq),
                                    wrap (myci, fn evt => handleCI(me,msgBodyOf evt)),
                                    wrap (childco, fn (child,cevt) => handleCO(me,child,cevt))
                                         ])
                        in
                          loop ()
                        end
                  val me =  makeReal (mkReal sz, widgets)
                  val W{win,widget,...} = topWidget me
                in
                  configureWin win [WC_StackMode Above, WC_Size sz];
                  if W.okaySize(widget, sz) then () else sync(myco CO_ResizeReq);
                  main (sz, me)
                end

          val bounds = bounds (fn widget => W.boundsOf widget)
          fun initLoop me =
                (case (recv reqChan)
                   of Visible repc => send(repc, topIndex me)
                    | Size repc => send(repc, size me)
                    | Bounds => send(sizeChan, bounds me)
                    | DoRealize arg => realize arg me
                    | MakeVis i =>
                        ((*debug("initLoop: MakeVis "^(Int.toString i));*)
                         let
                           val (me',_) = makeVis(me,i)
                         in
                           send(repChan,Okay);
                           initLoop me'
                         end handle e => send(repChan, Error e))
                    | Insert (index,wl) =>
                        (let
                           val me' = insertW(me,index,wl)
                         in 
                           send(repChan, Okay);
                           initLoop me'
                         end handle e => send(repChan, Error e))
                    | Delete indices =>
                        (let
                           val (me',_) = deleteW(me, indices)
                         in 
                           send(repChan, Okay);
                           initLoop me'
                         end handle e => send(repChan, Error e));
                 initLoop me)
        in
          case widgets
            of [] => spawn (fn () => initLoop Empty)
             | w::_ => spawn (fn () => initLoop (P{top=0,widget=w,wlist=widgets}));
          Pile {
            widget=W.mkWidget
              {
               root=root,
               args= fn () => {background = NONE},
               boundsOf = (fn () => (send (reqChan, Bounds); recv sizeChan)),
               realize = (fn arg => (send (reqChan, DoRealize arg)))
              },
            repChan = repChan,
            reqChan = reqChan
               }
        end

  fun pile (root,view,_) widgets = mkPile root widgets

  fun widgetOf (Pile{widget,...}) = widget
  fun visible (Pile{reqChan,...}) =
        let
          val retc = CML.channel()
        in
          CML.send(reqChan, Visible retc);
          case CML.recv retc
            of NONE => raise NoWidgets
             | SOME i => i
        end
  fun size (Pile{reqChan,...}) =
        let
          val retc = CML.channel()
        in
          CML.send(reqChan, Size retc);
          CML.recv retc
        end
  local
    fun command wrapfn (Pile{reqChan,repChan,...}) =
          fn arg => 
               ((*debug "command: sending reqChan";*)
                CML.send(reqChan,wrapfn arg);
                case CML.recv repChan of Error e => raise e | Okay => ())
  in
    val mkVisible = command MakeVis
    val insert' = command Insert
    fun insert pile (i,[]) = ()
      | insert pile arg = insert' pile arg
    fun append pile (i,bl) = insert pile (i+1,bl)
    val delete' = command Delete
    fun delete pile [] = ()
      | delete pile arg = delete' pile arg
  end (* local *)

end (* structure Pile *);
