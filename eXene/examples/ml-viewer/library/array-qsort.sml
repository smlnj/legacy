(* array-qsort.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Structure for in-place sorting of arrays.
 * Uses an engineered version of quicksort due to 
 * Bentley and McIlroy.
 *
 *)

structure ArrayQSort : ARRAY_SORT =
  struct

    open Array LibBase
    val sub = System.Unsafe.subscript
    val update = System.Unsafe.update

    fun isort (array, start, n, cmp) = let
          fun item i = sub(array,i)
          fun swap (i,j) = let 
                val tmp = sub(array,i)
                in update(array,i,sub(array,j)); update(array,j,tmp) end
          fun vecswap (i,j,0) = ()
            | vecswap (i,j,n) = (swap(i,j);vecswap(i+1,j+1,n-1))
          fun insertSort (start, n) = let
                val limit = start+n
                fun outer i =
                      if i >= limit then ()
                      else let
                        fun inner j =
                              if j = start then outer(i+1)
                              else let
                                val j' = j - 1
                                in
                                  if cmp(item j',item j) = Greater
                                    then (swap(j,j'); inner j')
                                    else outer(i+1)
                                end
                        in inner i end
                in
                  outer (start+1)
                end
          in insertSort (start, n); array end

    fun sortRange (array, start, n, cmp) = let
          fun item i = sub(array,i)
          fun swap (i,j) = let 
                val tmp = sub(array,i)
                in update(array,i,sub(array,j)); update(array,j,tmp) end
          fun vecswap (i,j,0) = ()
            | vecswap (i,j,n) = (swap(i,j);vecswap(i+1,j+1,n-1))
          fun insertSort (start, n) = let
                val limit = start+n
                fun outer i =
                      if i >= limit then ()
                      else let
                        fun inner j =
                              if j = start then outer(i+1)
                              else let
                                val j' = j - 1
                                in
                                  if cmp(item j',item j) = Greater
                                    then (swap(j,j'); inner j')
                                    else outer(i+1)
                                end
                        in inner i end
                in
                  outer (start+1)
                end

          fun med3(a,b,c) =
                case (cmp(item a,item b),cmp(item b,item c)) of
                  (Less,Less) => b
                | (Less, _  ) => (case cmp(item a,item c) of Less => c | _ => a)
                | (_,Greater) => b
                |  _          => (case cmp(item a,item c) of Less => a | _ => c)

          fun getPivot (a,n) = 
                if n <= 7 then a + n div 2
                else let
                  val p1 = a
                  val pm = a + n div 2
                  val pn = a + n - 1
                  in
                    if n <= 40 then med3(p1,pm,pn)
                    else let
                      val d = n div 8
                      val p1 = med3(p1,p1+d,p1+2*d)
                      val pm = med3(pm-d,pm,pm+d)
                      val pn = med3(pn-2*d,pn-d,pn)
                      in
                        med3(p1,pm,pn)
                      end
                  end
          
          fun quickSort (arg as (a, n)) = let
                fun bottom limit = let
                      fun loop (arg as (pa,pb)) =
                            if pb > limit then arg
                            else case cmp(item pb,item a) of
                              Greater => arg
                            | Less => loop (pa,pb+1)
                            | _ => (swap arg; loop (pa+1,pb+1))
                      in loop end
      
                fun top limit = let
                      fun loop (arg as (pc,pd)) =
                            if limit > pc then arg
                            else case cmp(item pc,item a) of
                              Less => arg
                            | Greater => loop (pc-1,pd)
                            | _ => (swap arg; loop (pc-1,pd-1))
                      in loop end

                fun split (pa,pb,pc,pd) = let
                      val (pa,pb) = bottom pc (pa,pb)
                      val (pc,pd) = top pb (pc,pd)
                      in
                        if pb > pc then (pa,pb,pc,pd)
                        else (swap(pb,pc); split(pa,pb+1,pc-1,pd))
                      end

                val pm = getPivot arg
                val _ = swap(a,pm)
                val pa = a + 1
                val pc = a + (n-1)
                val (pa,pb,pc,pd) = split(pa,pa,pc,pc)
                val pn = a + n
                val r = min(pa - a, pb - pa)
                val _ = vecswap(a, pb-r, r)
                val r = min(pd - pc, pn - pd - 1)
                val _ = vecswap(pb, pn-r, r)
                val n' = pb - pa
                val _ = if n' > 1 then sort(a,n') else ()
                val n' = pd - pc
                val _ = if n' > 1 then sort(pn-n',n') else ()
                in () end

          and sort (arg as (_, n)) = if n < 7 then insertSort arg 
                                     else quickSort arg
          in sort (start,n); array end

    fun sort cmp array = sortRange(array,0,length array, cmp)

    fun sorted cmp array = let
          val len = length array
          fun s (v,i) = let
                val v' = sub(array,i)
                in
                  case cmp(v,v') of
                    Greater => false
                  | _ => if i+1 = len then true else s(v',i+1)
                end
          in
            if len = 0 orelse len = 1 then true
            else s(sub(array,0),1)
          end

  end (* ArraySort *)

