(* index.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Utility functions for managing lists indexed by integers.
 *)

signature INDEX =
  sig

    exception BadIndex

    val find : (int * 'a -> 'b option) -> 'a list -> 'b list
    val findi : 'a list * int -> 'a
    val isValid : 'a list * int -> bool
    val chkSort : int list -> int list
    val chkUSort : int list -> int list
    val doMap : 'a list * ('a -> 'a) * int list -> 'a list
    val delete : 'a list * int list -> 'a list * 'a list
    val insert : 'a list * int * 'a list -> 'a list
    val preIndices : int * int list -> int option

  end (* INDEX *)

structure Index : INDEX =
  struct

    exception BadIndex

    fun find pred cl = let
          fun gv (_,[]) = []
            | gv (i,w::rest) = 
                case pred (i,w) of
                  NONE => gv (i+1,rest)
                | SOME v => v::(gv (i+1,rest))
          in gv (0,cl) end

    fun isValid (l, index) = let
          fun chk(j,[]) = j = index
            | chk(j,_::rest) = j = index orelse chk(j+1,rest)
          in
            if index < 0 then false else chk (0,l)
          end

    fun findi (l, i) = let
          fun f([], _) = raise BadIndex
            | f(a::rest, j) = if i = j then a else f(rest, j+1)
          in f(l,0) end

    fun cmp (i,j : int) = if i < j then LESS
                          else if i = j then EQUAL
                          else GREATER
    val sort = ListMergeSort.sort Int.>
    val sorted = ListMergeSort.sorted Int.>
    val usort = ListMergeSort.uniqueSort cmp

    fun usorted [] = true
      | usorted [_ : int] = true
      | usorted (x::(rest as (y::_))) = x < y andalso usorted rest

    fun chkSort [] = []
      | chkSort (l as [_]) = l
      | chkSort (l as [i,j]) = if i <= j then l else [j,i]
      | chkSort l = if sorted l then l else sort l

    fun chkUSort [] = []
      | chkUSort (l as [_]) = l
      | chkUSort (l as [i,j]) = if i < j then l 
                                else if i = j then [i]
                                else [j,i]
      | chkUSort l = if usorted l then l else usort l

      (* doMap: 'a list * ('a -> 'a) * int list -> 'a list
       * Applies mapfn to items whose index is in index list
       * Assume il is sorted in non-decreasing order
       *)
    fun doMap (cl, mapfn, il) = let
          fun domap(_, l, []) = l
            | domap(_, [], _) = raise BadIndex
            | domap(j, c::cl', il as i::il') = 
                if i < j then raise BadIndex
                else if i = j then (mapfn c)::(domap(j+1,cl',il'))
                else c::(domap(j+1,cl',il))
          in
            domap (0, cl, il)
          end

      (* delete: 'a list * int list -> 'a list * 'a list
       * Remove all items whose index appears in the
       * list of integers.
       *)
    fun delete (cl, il) = let
          fun del(_, l, []) = (l, [])
            | del(_, [], _) = raise BadIndex
            | del(j, c::cl', il as i::il') =
                if i < j then raise BadIndex
                else if i = j then let
                  val (l,d) = del(j+1,cl',il')
                  in
                    (l,c::d)
                  end
                else let
                  val (l,d) = del(j+1,cl',il)
                  in
                    (c::l,d)
                  end
          in
            del (0, cl, il)
          end

    fun insert (cl, index, boxel) = let
          fun ins (0, l) = boxel @ l
            | ins (i, x::r) = x::(ins(i-1,r))
            | ins (i, []) = raise BadIndex
          in 
            if index < 0 then raise BadIndex
            else ins(index, cl) 
          end

    fun preIndices (index : int, il) = let
          fun loop (cnt, []) = SOME cnt
            | loop (cnt, i::l) = 
                if i < index then loop(cnt+1,l)
                else if i = index then NONE
                else SOME cnt
          in loop(0,il) end
  end
