(* widget-set.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 *
 * Provides a protocol for a collection of
 * of widgets, some of which can be selected by the user.
 *
 *)

signature WIDGET_SET =
  sig

    structure W : WIDGET

    exception BadIndex
    exception MultipleChoices

    type widget_set

    type set_item (*  = {
        widget : W.widget,
        state : W.wstate,
        pick_fn : bool -> unit,
        active_fn : bool -> unit
      } *)
      
    val mkSetItem : {
           widget : W.widget,
           state : W.wstate,
           pick_fn : bool -> unit,
           active_fn : bool -> unit
         } -> set_item

    val mkMultiSet : 
          W.root -> set_item list -> (widget_set * W.widget list) 
    val mkSingleSet : 
          W.root -> set_item list -> (widget_set * W.widget list) 

    val insert : widget_set -> (int * set_item list) -> W.widget list
    val append : widget_set -> (int * set_item list) -> W.widget list

    val setChosen : widget_set -> (int * bool) list -> unit
    val setActive : widget_set -> (int * bool) list -> unit
    val getChosen : widget_set -> int list
    val getState : widget_set -> W.wstate list

  end (* WIDGET_SET *)

structure WidgetSet : WIDGET_SET = 
  struct

    structure W = Widget
    structure I = Index

    exception BadIndex = I.BadIndex
    exception MultipleChoices

    fun isChosen (W.Active v) = v
      | isChosen (W.Inactive v) = v
    fun flipState (W.Active v) = (W.Active(not v))
      | flipState (W.Inactive v) = (W.Inactive(not v))

    type set_item = {
        widget : W.widget,
        state : W.wstate,
        pick_fn : bool -> unit,
        active_fn : bool -> unit
      }
    fun mkSetItem x = x

    datatype reply = Okay | Error of exn
    datatype ireply = Widgets of W.widget list | IError of exn

    datatype rqst =
        Insert of ((int * set_item list) * ireply CML.chan)
      | SetChosen of (int * bool) list
      | SetActive of (int * bool) list
      | GetChosen of int list CML.chan
      | GetState of W.wstate list CML.chan


    datatype widget_set = WS of { 
        repChan : reply CML.chan,
        rqstChan : rqst CML.chan
      }

    datatype item_msg = 
        Remove 
      | Pick of unit SyncVar.ivar
      | Unpick of unit SyncVar.ivar

    datatype item_rep = IR of {
        state : W.wstate ref,
        pick_fn : bool -> unit,
        active_fn : bool -> unit,
        evt : item_msg CML.event
      }

    fun isActive (IR{state = ref (W.Active _),...}) = true
      | isActive _ = false

    datatype 'a result = Success of 'a * item_rep list * W.widget list | Failure of exn

    fun cloop co () = (CML.sync co; cloop co ())

    fun mkRep (state,pfn,afn,wchan) = 
          IR {state = ref state,pick_fn = pfn, 
               active_fn = afn, evt = CML.recvEvt wchan}

    fun wrapW (w, wchan) = let
          open CML Interact Widget
          fun realize {env=env as InEnv{m,ci,k,...},win,sz} = let
                val mchan = channel() and cchan = channel() and kchan = channel()
                val env' = 
                  replaceKey(replaceCI(replaceMouse(env,recvEvt mchan),recvEvt cchan),recvEvt kchan)

                fun check wf = let
                      val cv = SyncVar.iVar ()
                      in send(wchan, wf cv); SyncVar.iGet cv end

                fun handleM msg = (
                        case msgBodyOf msg of
                          MOUSE_FirstDown{but=MButton 1,...} => check Pick
                        | MOUSE_FirstDown{but=MButton 2,...} => check Unpick
                        | _ => ();
                        send(mchan,msg)
                      )

                fun handleCI msg = (
                        case msgBodyOf msg of
                          CI_OwnDeath =>
                            if toWindow(msg,win) then send(wchan,Remove) else ()
                        | _ => ();
                        send(cchan,msg)
                      )
      
                fun loop () =
                      loop(select[
                        wrap(k, fn msg => send(kchan,msg)),
                        wrap(m, handleM),
                        wrap(ci, handleCI)
                      ])
                in
                  spawn loop;
                  realizeFn w {env=env',win=win,sz=sz}
                end
          in
            mkWidget{root = rootOf w, args = argsFn w, boundsOf = boundsFn w, realize = realize}
          end

    fun doItem (arg, (sl,wl)) = let
          fun do_item {widget,state,pick_fn,active_fn} = let
                val wchan = CML.channel()
                val w' = wrapW(widget,wchan)
                in (mkRep(state,pick_fn,active_fn,wchan),w') end
          val (s,w) = do_item arg
          in (s::sl,w::wl) end

    fun mkWidgetEvt slist = let
          fun wf (item as IR{evt,...},(i,l)) = (i+1,CML.wrap(evt,fn e => (e,i,item))::l)
          in CML.choose(#2(List.foldl wf (0,[]) slist)) end

    fun getState (IR{state,...}) = !state
    fun setActive slist (i,onoff) =
          case (Index.findi(slist,i),onoff) of
            (IR{state = state as ref (W.Inactive w),active_fn,...}, true) => 
              (state := W.Active w; active_fn true)
          | (IR{state = state as ref (W.Active w),active_fn,...}, false) => 
              (state := W.Inactive w; active_fn false)
          | _ => ()

    fun mkWidgetSet (pick,setPick,getPick) (root : W.root) (items : set_item list) =
	  let
          open CML
          val (slist, wlist) = List.foldr doItem ([],[]) items
          val picked = setPick slist
          val repChan = channel() and rqstChan = channel()

          fun doInsert (picked, slist, index, ilist) = let
                val _ = if Index.isValid(slist,index) then () else raise BadIndex
                val (sl,wl) = List.foldr doItem ([],[]) ilist
                val slist' = Index.insert(slist,index,sl)
                val picked = setPick slist'
                in
                  Success(picked, slist', wl)
                end handle e => Failure e

          fun main (picked, slist) = let
                val wevt = mkWidgetEvt slist

                fun picki ((i,dopick),picked) = pick(dopick,i,Index.findi(slist,i),picked)

                fun handleRqst (SetChosen setl,picked) =
                      (((List.foldl picki picked setl) before (send(repChan, Okay)))
                        handle e => (send(repChan, Error e);picked))
                  | handleRqst (SetActive activel,picked) = (
                      (app (setActive slist) activel; send(repChan, Okay)) 
                        handle e => send(repChan, Error e); 
                      picked)
                  | handleRqst (GetChosen rc,picked) = (send(rc, getPick (picked,slist)); picked)
                  | handleRqst (GetState rc,picked) = (send(rc, map getState slist); picked)
                  | handleRqst (Insert ((index,ilist),rc), picked) = (
		      case doInsert(picked, slist, index, ilist)
		       of Success(p,s,wl) => (send(rc,Widgets wl); main(p,s))
                        | Failure e => (send(rc,IError e); picked)
		      (* end case *))

                fun handleEvt ((Remove, i, _), _) = let
                      val (slist',dl) = Index.delete(slist,[i])
                      val IR{evt,...} = hd dl
                      in
                        spawn (cloop evt);
                        main (setPick slist', slist')
                      end
                  | handleEvt ((Pick cv, i, item), picked) =
                      (if isActive item then pick(true,i,item,picked) else picked) 
                        before SyncVar.iPut (cv,())
                  | handleEvt ((Unpick cv, i, item), picked) =
                      (if isActive item then pick(false,i,item,picked) else picked) 
                        before SyncVar.iPut (cv,())

                fun loop picked =
                      loop(select[
                        wrap(recvEvt rqstChan, fn rqst => handleRqst(rqst,picked)),
                        wrap(wevt, fn msg => handleEvt(msg,picked))
                      ])
                in loop picked end
          in
            spawn (fn () => (main (picked,slist);()));
            (WS{repChan=repChan,rqstChan=rqstChan}, wlist)
          end

    fun setPick _ = ()
    fun getPick (_,slist) = 
          Index.find (fn (i,IR{state,...}) => if isChosen(!state) then SOME i else NONE) slist
    fun pick(doPick,index,IR{state,pick_fn,...},_) =
          if doPick <> isChosen (!state) 
            then (pick_fn doPick; state := flipState(!state); ())
            else ()
    val mkMultiSet = mkWidgetSet (pick, setPick, getPick)

    fun setPick1 slist = let
          fun setp (item as IR{state,...},(i,NONE)) =
               if isChosen(!state) then (i+1,SOME(i,item)) else (i+1,NONE)
            | setp (item as IR{state,...},(i,p)) =
               if isChosen(!state) then raise MultipleChoices else (i+1,p)
          in #2(List.foldl setp (0,NONE) slist) end
    fun getPick1 (NONE,_) = []
      | getPick1 (SOME(i,_),_) = [i]
    fun pick1(true,index,item as IR{state,pick_fn,...},NONE) = 
          (pick_fn true; state := flipState(!state); SOME(index,item))
      | pick1(false,index,IR{state,pick_fn,...},NONE) = NONE
      | pick1(true,index,item as IR{state,pick_fn,...},p as SOME(i,IR{state=s,pick_fn=pf,...})) =
          if i = index then p 
          else (
            pf false;
            pick_fn true;
            s := flipState(!s); 
            state := flipState(!state); 
            SOME(index,item)
          )
      | pick1(false,index,IR{state,pick_fn,...},p as SOME(i,_)) =
          if i <> index then p 
          else (pick_fn false; state := flipState(!state); NONE)
    val mkSingleSet = mkWidgetSet (pick1, setPick1, getPick1)

    local
      fun get rqst (WS{rqstChan,...}) = let
            val retc = CML.channel()
            in
              CML.send(rqstChan, rqst retc);
              CML.recv retc
            end
      fun command wrapfn (WS{rqstChan,repChan,...}) =
            fn arg => 
              (CML.send(rqstChan,wrapfn arg);
              case CML.recv repChan of Error e => raise e | Okay => ())
    in
    val getChosen = get GetChosen
    val getState = get GetState
    val setChosen = command SetChosen
    val setActive = command SetActive
    fun insert (WS{rqstChan,...}) arg = let
            val retc = CML.channel()
            in
              CML.send(rqstChan, Insert (arg,retc));
              case CML.recv retc of Widgets wl => wl | IError e => raise e
            end
    fun append wset (i,bl) = insert wset (i+1,bl)
    end (* local *)

end (* WidgetSet *)

