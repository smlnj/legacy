(* item-list.sml
 *
 * COPYRIGHT (c) 1991,1992 by AT&T Bell Laboratories
 *
 * Structure for maintaining lists of items with widget state.
 *)

structure ItemList :
  sig
    exception BadIndex

    type 'a items

    val items : {multiple : bool,
                 items : ('a * Widget.wstate) list,
                 pickfn : 'a * bool -> unit}  -> 'a items
    val numItems : 'a items -> int
    val getState : 'a items -> Widget.wstate list
    val getChosen : 'a items -> int list
    val item : ('a items * int) -> ('a * Widget.wstate)
    val listItems : ('a items * int * int) -> ('a * Widget.wstate) list

    val revfold : (('a * 'b) -> 'b) -> 'b -> 'a items -> 'b

    val setActive : 'a items * (int * bool) list -> 'a items
    val setChosen : 'a items * (int * bool) list -> ('a items * int option)
    val insert : 'a items * int * 'a list -> 'a items
    val delete : 'a items * int list -> 'a items
  end =
  struct

    structure W = Widget 
    structure I = Index 
    exception BadIndex = I.BadIndex

    type 'a item = 'a * W.wstate ref
    type 'a pick = (int * 'a item) option
    fun mki (i,s) = (i, ref s)

    type 'a pickfns = {
      pickfn : 'a * bool -> unit,
      setpickfn : (bool * int * 'a item * 'a pick) -> 'a pick,
      getpickfn : 'a pick * 'a item list -> int list
    }
    datatype 'a items = I of {
      multi : bool,
      items : 'a item list,
      cnt : int,
      pick : 'a pick,
      pickfns : 'a pickfns
    }

    fun isChosen (W.Active v) = v
      | isChosen (W.Inactive v) = v
    fun flipState (W.Active v) = (W.Active(not v))
      | flipState (W.Inactive v) = (W.Inactive(not v))
    fun set_active (W.Active v, false) = (W.Inactive v)
      | set_active (W.Inactive v, true) = (W.Active v)
      | set_active (s, _) = s

    fun getPick (_,slist) =
          Index.find (fn (i,(_,state)) => if isChosen(!state) then SOME i else NONE) slist
    fun pick pickfn (doPick,index,(item,state),_) =
          if doPick <> isChosen (!state)
            then (pickfn (item,doPick); state := flipState(!state); NONE)
            else NONE

    fun initPick1 slist = let
          fun setp (item as (_,state),(i,p)) =
               if isChosen(!state) then (i+1,SOME(i,item)) else (i+1,p)
          in #2(List.foldl setp (0,NONE) slist) end
    fun getPick1 (NONE,_) = []
      | getPick1 (SOME(i,_),_) = [i]
    fun pick1 pickfn (true,index,v as (item,state),NONE) =
          (pickfn (item,true); state := flipState(!state); SOME(index,v))
      | pick1 pickfn (false,index,_,NONE) = NONE
      | pick1 pickfn (true,index,(item',state'),p as SOME(i,(item,state))) =
          if i = index then p
          else (
            pickfn (item,false);
            pickfn (item',true);
            state' := flipState(!state');
            state := flipState(!state);
            SOME(index,(item',state'))
          )
      | pick1 pickfn (false,index,(item,state),p as SOME(i,_)) =
          if i <> index then p
          else (pickfn (item,false); state := flipState(!state); NONE)

    fun items {multiple,items=l,pickfn} = let
          val itemlist = map mki l
          val (pick, setpickfn, getpickfn) =
                 if multiple then (NONE,pick pickfn,getPick)
                 else (initPick1 itemlist,pick1 pickfn,getPick1)
          val pickfns = {pickfn=pickfn,setpickfn=setpickfn,getpickfn=getpickfn}
          in
            I {
              multi = multiple,
              items = itemlist,
              cnt = length itemlist,
              pick = pick,
              pickfns = pickfns
            }
          end
    fun numItems (I{cnt,...}) = cnt

    fun getChosen (I{pick,items,pickfns,...}) = 
          (#getpickfn pickfns) (pick,items)
    fun getState (I{items,...}) = let
          fun get_state (_,state) = !state
          in map get_state items end

    fun listItems (I{cnt,items,...},start,len) = let
          fun nthtail (0,l) = l
            | nthtail (_,[]) = []
            | nthtail (n,_::t) = nthtail(n-1,t)
          fun get ([],_,l) = rev l
            | get (_,0,l) = rev l
            | get ((i,s)::t,n,l) = get(t,n-1,(i,!s)::l)
          in
            if start < 0 then raise BadIndex
            else get(nthtail(start,items), Int.max(0,len),[])
          end

    fun item (i,start) = hd(listItems(i,start,1))

    fun revfold f b (I{items,...}) = 
          List.foldl (fn ((v,_),b) => f(v,b)) b items

    fun delete (I{multi,cnt,items,pick,pickfns}, indices) = let
          val indices = I.chkSort indices
          val (items',dl) = I.delete (items, indices)
          val pickfn = #pickfn pickfns
          fun unpick (i,state) = if isChosen (!state) then pickfn(i,false) 
                                 else ()
          val pick' = case pick of
                        NONE => NONE
                      | SOME (i,item) => 
                          (case I.preIndices (i,indices) of
                             NONE => NONE
                          | SOME j => SOME(i-j,item))
          in
            app unpick dl;
            I {
              multi = multi,
              items = items',
              cnt = cnt - length indices,
              pick = pick',
              pickfns = pickfns
            }
          end

    fun insert (I{multi,cnt,items,pick,pickfns},index, ilist) = let
          val _ = if index < 0 orelse index > cnt then raise BadIndex else ()
          val initstate = W.Active false
          val ilist' = map (fn i => (i,ref initstate)) ilist
          val cnt' = length ilist
          val pick' = case pick of
                        NONE => NONE
                      | SOME(i,item) => if index > i then pick
                                        else SOME(i+cnt',item)
          val items' = Index.insert(items,index,ilist')
          in
            I {
              multi = multi,
              items = items',
              cnt = cnt + cnt',
              pick = pick',
              pickfns = pickfns
            }
          end

    fun setActive (il as I{items,...}, ilist) = let
          fun seta (i,on_off) = let
                val state = #2(I.findi(items,i))
                in state := set_active(!state,on_off) end
          in app seta ilist; il end

    fun setChosen (I{multi,cnt,items,pick,pickfns},ilist) = let
          val optpick = case pick of NONE => NONE | SOME(i,_) => SOME i
          val setpickfn = #setpickfn pickfns
          fun picki ((i,on_off),pick) =
                      setpickfn (on_off,i,I.findi(items,i),pick)
          in
            (I {
              multi = multi,
              items = items,
              cnt = cnt,
              pick = List.foldl picki pick ilist,
              pickfns = pickfns
            },
            optpick)
          end

  end (* functor ItemList *) 

