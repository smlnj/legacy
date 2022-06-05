(* binary-set.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This code was adapted from Stephen Adams' binary tree implementation
 * of applicative integer sets.
 *
 *    Copyright 1992 Stephen Adams.
 *
 *    This software may be used freely provided that:
 *      1. This copyright notice is attached to any copy, derived work,
 *         or work including all or part of this software.
 *      2. Any derived work must contain a prominent notice stating that
 *         it has been altered from the original.
 *
 *   Name(s): Stephen Adams.
 *   Department, Institution: Electronics & Computer Science,
 *      University of Southampton
 *   Address:  Electronics & Computer Science
 *             University of Southampton
 *         Southampton  SO9 5NH
 *         Great Britian
 *   E-mail:   sra@ecs.soton.ac.uk
 *
 *   Comments:
 *
 *     1.  The implementation is based on Binary search trees of Bounded
 *         Balance, similar to Nievergelt & Reingold, SIAM J. Computing
 *         2(1), March 1973.  The main advantage of these trees is that
 *         they keep the size of the tree in the node, giving a constant
 *         time size operation.
 *
 *     2.  The bounded balance criterion is simpler than N&R's alpha.
 *         Simply, one subtree must not have more than `weight' times as
 *         many elements as the opposite subtree.  Rebalancing is
 *         guaranteed to reinstate the criterion for weight>2.23, but
 *         the occasional incorrect behaviour for weight=2 is not
 *         detrimental to performance.
 *
 *     3.  There are two implementations of union.  The default,
 *         hedge_union, is much more complex and usually 20% faster.  I
 *         am not sure that the performance increase warrants the
 *         complexity (and time it took to write), but I am leaving it
 *         in for the competition.  It is derived from the original
 *         union by replacing the split_lt(gt) operations with a lazy
 *         version. The `obvious' version is called old_union.
 *
 *     4.  Most time is spent in T', the rebalancing constructor.  If my
 *         understanding of the output of *<file> in the sml batch
 *         compiler is correct then the code produced by NJSML 0.75
 *         (sparc) for the final case is very disappointing.  Most
 *         invocations fall through to this case and most of these cases
 *         fall to the else part, i.e. the plain contructor,
 *         T(v,ln+rn+1,l,r).  The poor code allocates a 16 word vector
 *         and saves lots of registers into it.  In the common case it
 *         then retrieves a few of the registers and allocates the 5
 *         word T node.  The values that it retrieves were live in
 *         registers before the massive save.
 *
 *   Modified to functor to support general ordered values
 *)

functor BinarySet (K : ORD_KEY) : ORD_SET =
  struct

    open LibBase K
    type item = ord_key

    datatype set = 
      E 
    | T of {
        elt : item, 
        cnt : int, 
        left : set,
        right : set
      }

    fun numItems E = 0
      | numItems (T{cnt,...}) = cnt
        
    fun mkT(v,n,l,r) = T{elt=v,cnt=n,left=l,right=r}

      (* N(v,l,r) = T(v,1+numItems(l)+numItems(r),l,r) *)
    fun N(v,E,E) = mkT(v,1,E,E)
      | N(v,E,r as T{cnt=n,...}) = mkT(v,n+1,E,r)
      | N(v,l as T{cnt=n,...}, E) = mkT(v,n+1,l,E)
      | N(v,l as T{cnt=n,...}, r as T{cnt=m,...}) = mkT(v,n+m+1,l,r)

    fun single_L (a,x,T{elt=b,left=y,right=z,...}) = N(b,N(a,x,y),z)
      | single_L _ = raise Match
    fun single_R (b,T{elt=a,left=x,right=y,...},z) = N(a,x,N(b,y,z))
      | single_R _ = raise Match
    fun double_L (a,w,T{elt=c,left=T{elt=b,left=x,right=y,...},right=z,...}) =
          N(b,N(a,w,x),N(c,y,z))
      | double_L _ = raise Match
    fun double_R (c,T{elt=a,left=w,right=T{elt=b,left=x,right=y,...},...},z) =
          N(b,N(a,w,x),N(c,y,z))
      | double_R _ = raise Match

    val weight = 3

    fun T' (v,E,E) = mkT(v,1,E,E)
      | T' (v,E,r as T{left=E,right=E,...}) = mkT(v,2,E,r)
      | T' (v,l as T{left=E,right=E,...},E) = mkT(v,2,l,E)

      | T' (p as (_,E,T{left=T _,right=E,...})) = double_L p
      | T' (p as (_,T{left=E,right=T _,...},E)) = double_R p

        (* these cases almost never happen with small weight*)
      | T' (p as (_,E,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...})) =
            if ln<rn then single_L p else double_L p
      | T' (p as (_,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...},E)) =
            if ln>rn then single_R p else double_R p

      | T' (p as (_,E,T{left=E,...})) = single_L p
      | T' (p as (_,T{right=E,...},E)) = single_R p

      | T' (p as (v,l as T{elt=lv,cnt=ln,left=ll,right=lr},
              r as T{elt=rv,cnt=rn,left=rl,right=rr})) =
          if rn >= weight*ln (*right is too big*)
            then
              let val rln = numItems rl
                  val rrn = numItems rr
              in
                if rln < rrn then single_L p else double_L p
              end
          else if ln >= weight*rn (*left is too big*)
            then
              let val lln = numItems ll
                  val lrn = numItems lr
              in
                if lrn < lln then single_R p else double_R p
              end
          else mkT(v,ln+rn+1,l,r)

    fun add (E,x) = mkT(x,1,E,E)
      | add (set as T{elt=v,left=l,right=r,cnt},x) =
          case cmpKey(x,v) of
            Less => T'(v,add(l,x),r)
          | Greater => T'(v,l,add(r,x))
          | Equal => mkT(x,cnt,l,r)

    fun concat3 (E,v,r) = add(r,v)
      | concat3 (l,v,E) = add(l,v)
      | concat3 (l as T{elt=v1,cnt=n1,left=l1,right=r1}, v, 
                  r as T{elt=v2,cnt=n2,left=l2,right=r2}) =
        if weight*n1 < n2 then T'(v2,concat3(l,v,l2),r2)
        else if weight*n2 < n1 then T'(v1,l1,concat3(r1,v,r))
        else N(v,l,r)

    fun split_lt (E,x) = E
      | split_lt (T{elt=v,left=l,right=r,...},x) =
          case cmpKey(v,x) of
            Greater => split_lt(l,x)
          | Less => concat3(l,v,split_lt(r,x))
          | _ => l

    fun split_gt (E,x) = E
      | split_gt (T{elt=v,left=l,right=r,...},x) =
          case cmpKey(v,x) of
            Less => split_gt(r,x)
          | Greater => concat3(split_gt(l,x),v,r)
          | _ => r

    fun min (T{elt=v,left=E,...}) = v
      | min (T{left=l,...}) = min l
      | min _ = raise Match
        
    fun delmin (T{left=E,right=r,...}) = r
      | delmin (T{elt=v,left=l,right=r,...}) = T'(v,delmin l,r)
      | delmin _ = raise Match

    fun delete' (E,r) = r
      | delete' (l,E) = l
      | delete' (l,r) = T'(min r,l,delmin r)

    fun concat (E, s) = s
      | concat (s, E) = s
      | concat (t1 as T{elt=v1,cnt=n1,left=l1,right=r1}, 
                  t2 as T{elt=v2,cnt=n2,left=l2,right=r2}) =
          if weight*n1 < n2 then T'(v2,concat(t1,l2),r2)
          else if weight*n2 < n1 then T'(v1,l1,concat(r1,t2))
          else T'(min t2,t1, delmin t2)


    local
      fun trim (lo,hi,E) = E
        | trim (lo,hi,s as T{elt=v,left=l,right=r,...}) =
            if cmpKey(v,lo) = Greater
              then if cmpKey(v,hi) = Less then s else trim(lo,hi,r)
              else trim(lo,hi,r)
                
      fun uni_bd (s,E,_,_) = s
        | uni_bd (E,T{elt=v,left=l,right=r,...},lo,hi) = 
             concat3(split_gt(l,lo),v,split_lt(r,hi))
        | uni_bd (T{elt=v,left=l1,right=r1,...}, 
                   s2 as T{elt=v2,left=l2,right=r2,...},lo,hi) =
            concat3(uni_bd(l1,trim(lo,v,s2),lo,v),
                v, 
                uni_bd(r1,trim(v,hi,s2),v,hi))
              (* inv:  lo < v < hi *)

        (* all the other versions of uni and trim are
         * specializations of the above two functions with
         *     lo=-infinity and/or hi=+infinity 
         *)

      fun trim_lo (_, E) = E
        | trim_lo (lo,s as T{elt=v,right=r,...}) =
            case cmpKey(v,lo) of
              Greater => s
            | _ => trim_lo(lo,r)

      fun trim_hi (_, E) = E
        | trim_hi (hi,s as T{elt=v,left=l,...}) =
            case cmpKey(v,hi) of
              Less => s
            | _ => trim_hi(hi,l)
                
      fun uni_hi (s,E,_) = s
        | uni_hi (E,T{elt=v,left=l,right=r,...},hi) = 
             concat3(l,v,split_lt(r,hi))
        | uni_hi (T{elt=v,left=l1,right=r1,...}, 
                   s2 as T{elt=v2,left=l2,right=r2,...},hi) =
            concat3(uni_hi(l1,trim_hi(v,s2),v),v,uni_bd(r1,trim(v,hi,s2),v,hi))

      fun uni_lo (s,E,_) = s
        | uni_lo (E,T{elt=v,left=l,right=r,...},lo) = 
             concat3(split_gt(l,lo),v,r)
        | uni_lo (T{elt=v,left=l1,right=r1,...}, 
                   s2 as T{elt=v2,left=l2,right=r2,...},lo) =
            concat3(uni_bd(l1,trim(lo,v,s2),lo,v),v,uni_lo(r1,trim_lo(v,s2),v))

      fun uni (s,E) = s
        | uni (E,s) = s
        | uni (T{elt=v,left=l1,right=r1,...}, 
                s2 as T{elt=v2,left=l2,right=r2,...}) =
            concat3(uni_hi(l1,trim_hi(v,s2),v), v, uni_lo(r1,trim_lo(v,s2),v))

    in
      val hedge_union = uni
    end

      (* The old_union version is about 20% slower than
       *  hedge_union in most cases 
       *)
    fun old_union (E,s2)  = s2
      | old_union (s1,E)  = s1
      | old_union (T{elt=v,left=l,right=r,...},s2) = 
          let val l2 = split_lt(s2,v)
              val r2 = split_gt(s2,v)
          in
            concat3(old_union(l,l2),v,old_union(r,r2))
          end

    exception NotFound

    val empty = E
    fun singleton x = T{elt=x,cnt=1,left=E,right=E}

    val add = add

    fun peek (set,x) =
          let fun pk E = NONE
                | pk (T{elt=v,left=l,right=r,...}) =
                    case cmpKey(x,v) of
                      Less => pk l
                    | Greater => pk r
                    | _ => SOME v
          in pk set end

    fun member arg = case peek arg of NONE => false | _ => true

    fun find arg = case peek arg of NONE => raise NotFound | SOME v => v

    fun delete (E,x) = raise NotFound
      | delete (set as T{elt=v,left=l,right=r,...},x) =
          case cmpKey(x,v) of
            Less => T'(v,delete(l,x),r)
          | Greater => T'(v,l,delete(r,x))
          | _ => delete'(l,r)

    val union = hedge_union

    fun intersection (E,_) = E
      | intersection (_,E) = E
      | intersection (s, T{elt=v,left=l,right=r,...}) =
          let val l2 = split_lt(s,v)
              val r2 = split_gt(s,v)
          in
            case peek(s,v) of
              NONE => concat(intersection(l2,l),intersection(r2,r))
            | _ => concat3(intersection(l2,l),v,intersection(r2,r))
          end

    fun difference (E,s) = E
      | difference (s,E)  = s
      | difference (s, T{elt=v,left=l,right=r,...}) =
          let val l2 = split_lt(s,v)
              val r2 = split_gt(s,v)
          in
            concat(difference(l2,l),difference(r2,r))
          end

    fun fold f set b =
          let fun foldf (E,b) = b
                | foldf (T{elt,left,right,...},b) = 
                    foldf(left,(f(elt,foldf(right,b))))
          in
            foldf(set,b)
          end

    fun revfold f set b =
          let fun foldf (E,b) = b
                | foldf (T{elt,left,right,...},b) = 
                    foldf(right,(f(elt,foldf(left,b))))
          in
            foldf(set,b)
          end

    fun listItems set = fold (op::) set []

    fun revapp apf =
         let fun apply E = ()
               | apply (T{elt,left,right,...}) = 
                   (apply right;apf elt; apply left)
         in
           apply
         end

    fun app apf =
         let fun apply E = ()
               | apply (T{elt,left,right,...}) = 
                   (apply left;apf elt; apply right)
         in
           apply
         end

  end (* BinarySet *)
