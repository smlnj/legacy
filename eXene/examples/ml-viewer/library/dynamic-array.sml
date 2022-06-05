(* dynamic-array.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Arrays of unbounded length
 *
 *)

functor DynamicArray (A : STATIC_ARRAY) : DYNAMIC_ARRAY =
  struct

    type elem = A.elem
    datatype array = BLOCK of A.array ref * elem
 
    exception Subscript = A.Subscript
    exception Size = A.Size

    fun array (sz, dflt) = BLOCK(ref (A.array (sz,dflt)),dflt)

    fun subArray (BLOCK(arr,dflt),lo,hi) = let
          val arrval = !arr
          fun copy i = (A.sub(arrval,i+lo)) handle _ => dflt
          in
            BLOCK(ref(A.tabulate(hi-lo,copy)),dflt)
          end

    fun arrayoflist (initlist, dflt) = BLOCK(ref (A.arrayoflist initlist),dflt)

    fun tabulate (sz,fillfn,dflt) = BLOCK(ref (A.tabulate (sz,fillfn)),dflt)

    fun default (BLOCK(_,dflt)) = dflt

    fun sub (BLOCK(arr,dflt),idx) = (A.sub(!arr,idx)) 
          handle Subscript => if idx < 0 then raise Subscript else dflt

    fun bound (BLOCK(arr,_)) = A.length (!arr)

    fun expand(arr,oldlen,newlen,dflt) = let
          fun fillfn i = if i < oldlen then A.sub(arr,i) else dflt
          in
            A.tabulate(newlen,fillfn)
          end

    fun update (b as BLOCK(arr,dflt),idx,v) = let 
          val len = bound b
          in
            if idx >= len 
              then arr := expand(!arr,len,max(len+len,idx+1),dflt) 
              else ();
            A.update(!arr,idx,v)
          end

  end (* DynamicArray *)

