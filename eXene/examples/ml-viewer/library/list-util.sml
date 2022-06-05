(* list-util.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * General list utilities
 *)

structure ListUtil : LIST_UTIL = 
  struct

    exception Zip
    exception Split

    fun find pred [] = []
      | find pred (a::rest) = if pred a then a::(find pred rest) else (find pred rest)

    fun findOne pred [] = NONE
      | findOne pred (a::rest) = if pred a then SOME a else (findOne pred rest)

    fun splitp pred = let
          fun spl (l as a::rest) = 
                if pred a then ([],l)
                else let
                  val (p,s) = spl rest
                  in (a::p,s) end
            | spl [] = ([], []) 
          in spl end

    fun prefix pred l = #1(splitp pred l)
    fun suffix pred [] = []
      | suffix pred (a::rest) = if pred a then rest else suffix pred rest

    fun remove pred = let
          fun rm [] = []
            | rm (a::l) = if pred a then rm l else a::(rm l)
          in rm end

    fun removeOne pred = let
          fun rm [] = []
            | rm (a::l) = if pred a then l else a::(rm l)
          in rm end

    fun flatten [] = []
      | flatten [l] = l
      | flatten ll = let
	  fun flat ([], l) = rev l
	    | flat (ll::r, l) = flat(r, flat2(ll, l))
	  and flat2 ([], l) = l
	    | flat2 (x::r, l) = flat2(r, x::l)
	  in
	    flat (ll, [])
	  end

    fun zip ([],[]) = []
      | zip ((a :: x),(b :: y)) = (a, b) :: zip (x, y)
      | zip (_,_) = raise Zip

    fun unzip [] = ([], [])
      | unzip ((a, b) :: z) = let
          val (x, y) = unzip z
          in
            (a :: x, b :: y)
          end

    fun filter pred l = let
	  fun filterP ([], l) = rev l
	    | filterP (x::r, l) = (case (pred x)
		 of SOME y => filterP(r, y::l)
		  | NONE => filterP(r, l)
		(* end case *))
	  in
	    filterP (l, [])
	  end

    fun fromTo (lo, hi) = 
          if hi < lo then LibBase.badArg{module="ListUtil", func="fromTo", msg="hi < lo"}
          else let
            fun loop (i, l) = if i < lo then l else loop(i-1,i::l)
            in
              loop (hi,[])
            end

    fun genList (len, genfn) = if (len < 0)
	  then LibBase.badArg{module="ListUtil", func="list", msg="len < 0"}
          else let
            fun loop (i, l) = if i < 0 then l else loop(i-1,(genfn i)::l)
            in
              loop (len-1,[])
            end


    fun split n l =
          if n < 0 then LibBase.badArg{module="ListUtil", func="split", msg="len < 0"}
          else let
            fun spl (0,l) = ([],l)
              | spl (n,a::rest) = let
                  val (p,s) = spl(n-1,rest)
                  in (a::p, s) end
              | spl _ = raise Split
            in spl (n,l) end
       
  end (* ListUtil *)

