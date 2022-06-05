(* band.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories
 *
 * Code for band data structure.
 *
 * A band is a list non-continguous rectangles listed from left
 * to right (increasing x) that all have the same upper and lower
 * y coordinates. Regions (see region-sig.sml and region.sml)
 * are essentially ordered lists of bands.
 * 
 *
 *)
signature BAND =
  sig
    structure G : GEOMETRY

    datatype rect_overlap = RectangleOut | RectangleIn | RectanglePart

    datatype band = BAND of {
                      y1 : int,         (* top y value *)
                      y2 : int,         (* bottom y value *)
                      xs : (int * int) list   (* list of (left,right) values *)
                    }

          (* Return y1(y2) of band *)
    val y1Of : band -> int
    val y2Of : band -> int

          (* Return number of intervals. Always > 0 *)
    val sizeOf : band -> int

          (* concat list of rectangles of band on accumlator list
           * The leftmost rectangle in the band will be the head of
           * the resulting list.
           *)
    val rectsOfBand : band * G.rect list -> G.rect list

          (* True if point is in band *)
    val inBand : band * G.point -> bool

          (* Return left and right extent of band *)
    val bandExtent : band -> (int * int)

          (* Compares argument interval with x intervals of band *)
    val rectInBand : band * int * int -> rect_overlap

          (* Returns true if any two x intervals of the bands intersect *)
    val overlap : band * band -> bool

          (* Translate band by given vector *)
    val offsetBand : G.point -> band -> band

          (* Translate band horizontally(vertically) *)
    val xOffsetBand : int -> band -> band
    val yOffsetBand : int -> band -> band

          (* Coalesce lower band below upper band.
           * Return SOME of new band if successful.
           * Assumes y values are compatible.
           *)
    val coalesce : {lower : band, upper : band} -> band option

          (* Create a new band that is the union(intersection,difference)
           * of the two argument bands. The integer return value is
           * the number of intervals in the band.
           * The integer arguments provide the upper
           * and lower y coordinates for the resulting band. The operation
           * only involves the x intervals; it is assumed that y overlap
           * has already been checked.
           *)
    val union : band * band * int * int -> (band * int)
    val intersect : band * band * int * int -> (band * int)
    val subtract : band * band * int * int -> (band * int)

          (* Return a new band that has the same x intervals as the
           * argument band, but with the new upper and lower y values.
           *)
    val squeeze : band * int * int -> (band * int)
  end

structure Band : BAND =
  struct
    structure G = Geometry

    datatype rect_overlap = RectangleOut | RectangleIn | RectanglePart

      (* It might be worthwhile to maintain the length of xs in the band *)
    datatype band = BAND of {
                      y1 : int,
                      y2 : int,
                      xs : (int * int) list
                    }

    fun isIn (x : int) (x1,x2) = x1 <= x andalso x < x2
    fun xoff (x : int) (x1,x2) = (x1+x,x2+x)
    fun ontop ([],l,n) = (l,n)
      | ontop (a::t,l,n) = ontop(t,a::l,n+1)
    fun mkr (y1,y2) = let
          val ht = y2 - y1
          in fn ((x1,x2),l) => ((G.RECT{x=x1,y=y1,wid=x2 - x1,ht = ht})::l) end

    fun rectsOfBand (BAND{xs,y1,y2},l) = foldr (mkr (y1,y2)) l xs
    fun squeeze (BAND{xs,...},top,bot) = (BAND{xs=xs,y1=top,y2=bot},length xs)

    fun y1Of (BAND{y1,...}) = y1
    fun y2Of (BAND{y2,...}) = y2
    fun sizeOf (BAND{xs,...}) = length xs

    fun inBand (BAND{y1,y2,xs},G.PT{x=px,y=py}) =
          y1 <= py andalso py < y2 andalso List.exists (isIn px) xs

    fun bandExtent (BAND{xs = xs as ((x1,_)::_),...}) = let
          fun right ([(l,r)]) = r
            | right (_::t) = right t
            | right _ = raise LibBase.Impossible "Band.bandExtent.right"
          in (x1,right xs) end
      | bandExtent _ = raise LibBase.Impossible "Band.bandExtent"

    fun rectInBand (BAND{y1,y2,xs},x1,x2) = let
          fun rib [] = RectangleOut 
            | rib ((l,r)::rest) =
                if r <= x1 then rib rest
                else if x2 <= l then RectangleOut
                else if l <= x1 andalso x2 <= r then RectangleIn
                else RectanglePart
          in rib xs end

      (* Only check overlap of x intervals *)
    fun overlap (BAND{xs,...},BAND{xs=xs',...}) = let
          fun loop([],_) = false
            | loop(_,[]) = false
            | loop(x as ((x1,x2)::xs),x' as ((x1',x2')::xs')) =
                if x2 <= x1' then loop(xs,x')
                else if x2' <= x1 then loop(x,xs')
                else true
          in loop (xs,xs') end

    fun xOffsetBand dx (BAND{y1,y2,xs}) = BAND{y1=y1,y2=y2, xs = map (xoff dx) xs}

    fun yOffsetBand dy (BAND{y1,y2,xs}) = BAND{y1=y1+dy,y2=y2+dy, xs = xs}

    fun offsetBand (G.PT{x=dx,y=dy}) (BAND{y1,y2,xs}) =
          BAND{y1=y1+dy,y2=y2+dy, xs = map (xoff dx) xs}

      (* coalesces two bands into one, if possible.
       * assume y1 of lower band = y2 of upper band
       * Check that each contain same horizontal intervals.
       * If so, combine and return SOME of resulting band.
       * Else return NONE.
       *)
    fun coalesce {lower = BAND{y2,xs,...}, upper = BAND{y1=y1',xs=xs',...}} =
          if xs = xs' then SOME(BAND{y1=y1',y2=y2,xs=xs}) else NONE

    fun union (BAND{xs,...},BAND{xs=xs',...},top,bot) = let
          val h = hd xs
          val h' = hd xs'
          fun finalmerge([],ci,xs) = ontop(xs,[ci],1)
            | finalmerge((i as (l,r))::t ,i' as (l',r'),xs) =
                if r' < l then ontop(xs,i'::i::t,2 + length t)
                else if r <= r' then finalmerge(t,i',xs) 
                else ontop(xs,(l',r)::t,1 + length t)
          fun loop ([],[],ci,xs) = ontop(xs,[ci],1)
            | loop (x,[],ci,xs) = finalmerge(x,ci,xs)
            | loop ([],x,ci,xs) = finalmerge(x,ci,xs)
            | loop (x as ((i as (x1,x2))::t),x' as ((i' as (x1',x2'))::t'),ci,xs) =
                if x1 < x1' then merge(t,x',i,ci,xs) else merge(x,t',i',ci,xs)
          and merge(t,t',i as (l,r),i' as (l',r'),xs) =
                if r' < l then loop(t,t',i,i'::xs) 
                else if r <= r' then loop(t,t',i',xs) 
                else loop(t,t',(l',r),xs) 
          val (xs'',n) = if #1 h < #1 h' then loop(tl xs,xs',h,[])
                         else loop(xs,tl xs',h',[])
          in
            (BAND{y1=top,y2=bot,xs= xs''},n)
          end

    fun intersect (BAND{xs,...},BAND{xs=xs',...},top,bot) = let
          fun loop ([],_,xs) = ontop(xs,[],0)
            | loop (_,[],xs) = ontop(xs,[],0)
            | loop (x as ((x1,x2)::t),x' as ((x1',x2')::t'),xs) = let
	        val l = Int.max(x1,x1')
	        val r = Int.min(x2,x2')
                val xs' = if l < r then (l,r)::xs else xs
                in
	          if x2 < x2' then loop(t,x',xs')
	          else if x2 > x2' then loop(x,t',xs')
                  else loop(t,t',xs')
                end
          in
            case loop(xs,xs',[]) of
              (xs'',n) => (BAND{y1=top,y2=bot,xs= xs''},n)
          end

    fun subtract (BAND{xs,...},BAND{xs=xs',...},top,bot) = let
          fun loop ([],_,xs) = ontop(xs,[],0)
            | loop (x,[],xs) = ontop(xs,x,length x)
            | loop (x as ((x1,x2)::t),x' as ((x1',x2')::t'),xs) =
                if x2' <= x1 then loop(x,t',xs)
                else if x2 <= x1' then loop(t,x',(x1,x2)::xs)
                else if x1' <= x1 then
                  if x2' < x2 then loop((x2',x2)::t,t',xs)
                  else if x2' = x2 then loop(t,t',xs)
                  else loop(t,x',xs)
                else
                  if x2' < x2 then loop((x2',x2)::t,t',(x1,x1')::xs)
                  else if x2' = x2 then loop(t,t',(x1,x1')::xs)
                  else loop(t,x',(x1,x1')::xs)
          in
            case loop(xs,xs',[]) of
              (xs'',n) => (BAND{y1=top,y2=bot,xs=xs''},n)
          end

  end
