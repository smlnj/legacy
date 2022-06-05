(* region.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories
 *
 * Code for maintaining regions.
 *
 * The interface and algorithms are roughly based on those
 * found in the sample X library.
 *
 * Regions correspond to sets of points. 
 * Regions are implemented as YX banded lists of rectangles.
 * Specifically, a region is a list of bands listed by increasing y 
 * coordinates. A band is a list of rectangles listed by increasing x
 * coordinates. Within a band, the rectangles are non-contiguous
 * and all have the same upper and lower y coordinate. In addition,
 * the vertical intervals determined by two bands are disjoint.
 * (Note that if a band has upper and lower limits y1 and y2, this
 * corresponds to the half-open interval [y1,y2).)
 *
 * Thus, in a region, the rectangles lie in non-overlapping
 * bands. Within a band, the rectangles are as wide as possible.
 * Some effort is also taken to coalesce compatible bands, i.e.,
 * those that have the same x intervals and whose y intervals abut.
 *)

structure Region : REGION =
  struct
    structure G = Geometry

    val min = Int.min
    val max = Int.max

    fun impossible msg = raise LibBase.Impossible ("Region"^msg)

    open Box Band ScanConvert

    datatype region = REGION of {
        numRects : int,
        bands : band list,
        extents : box
      }

    val empty = REGION {
          numRects = 0,
          extents = zeroBox,
          bands = []
        }

    fun rectsOf (REGION{bands,...}) = foldr rectsOfBand [] bands

      (* Calculate the bounding box of a region *)
    fun setExtents (REGION{numRects,bands=[],...}) = 
          REGION{numRects=0,bands=[],extents=zeroBox}
      | setExtents (REGION{numRects,
                       bands = bands as ((b as BAND{y1,y2,...})::rs),...}) = let
          val (x1,x2) = bandExtent b
          fun bnds ([b as BAND{y2,...}],l,r) = let
                val (x1,x2) = bandExtent b
                in BOX{x1=min(x1,l),y1=y1,x2=min(x2,r),y2=y2} end
            | bnds (b::rs,l,r) = let
                val (x1,x2) = bandExtent b
                in bnds(rs,min(x1,l),max(x2,r)) end
            | bnds _ = impossible "setExtents"
          in 
            case rs of 
              [] => REGION{numRects=numRects,bands=bands,
                        extents= BOX{x1=x1,y1=y1,x2=x2,y2=y2}}
            | _  => REGION{numRects=numRects,bands=bands,
                        extents= bnds (rs,x1,x2)}
          end

    fun clipBox (REGION{extents=BOX{x1,y1,x2,y2},...}) =
          G.RECT{x = x1,y = y2, wid = x2 - x1, ht = y2 - y1}

    fun polyRegion arg = let
          open Geometry
          fun coalesce (b' as BAND{y2,...},n',b as BAND{y1,...}) =
                if y1 = y2 andalso n' = sizeOf b 
                  then Band.coalesce{upper=b',lower=b} else NONE

          fun skip (ps as (PT{x,y}::PT{x=x',...}::pts)) = 
                if x = x' then skip pts else ps
            | skip ps = ps

            (* Assume at least two points and the first two satisfy x1 < x2.
             * This guarantees the band in non-empty.
             *)
          fun getBand (PT{x=x2,y=y2}::PT{x=x1,y=y1}::ps) = let
                fun loop ([],x1,xs,n) = ([],n,x1,x2,BAND{y1=y1,y2=y2+1,xs=xs})
                  | loop(ps as (PT{x,y}::PT{x=x',y=y'}::pts),x1,xs,n) =
                      if y' = y1 then
                        if x = x' then loop(pts,x1,xs,n) 
                        else case xs of
                          (l,r)::xs' =>
                            if x >= l then loop(pts,x',(x',r)::xs',n)
                            else loop(pts,x',(x',x)::xs,n+1)
                        | _ => impossible "polygonRegion.getBand.loop"
                      else (skip ps,n,x1,x2,BAND{y1=y1,y2=y2+1,xs=xs})
                  | loop _ = impossible "polygonRegion: odd number of points"
                in loop (ps,x1,[(x1,x2)],1) end
            | getBand _ = impossible "polygonRegion.getBand"

          fun poly([],n,x1,x2,bands) = (n,x1,x2,bands)
            | poly(pts,_,_,_,[]) = let
                val (pts',n,x1,x2,b) = getBand pts
                in poly(pts',n,x1,x2,[b]) end
            | poly(pts,n,x1,x2,bs as b::rb) = let
                val (pts',dn,x1',x2',b') = getBand pts
                val (bs',n') = case coalesce(b',dn,b) of 
                                 NONE => (b'::bs,n+dn) 
                               | SOME b'' => (b''::rb, n)
                in poly(pts',n',min(x1,x1'),max(x2,x2'),bs') end

          val (numRects,x1,x2,bands) = poly (skip(scanConvert arg),0,0,0,[])
          in
            if numRects = 0
	      then empty
              else REGION{
		  numRects=numRects,
		  bands=bands,
                  extents=BOX{
		      x1=x1,y1= y1Of(hd bands),
		      x2=x2,y2= y2Of(List.last bands)
		    }
		}
          end

      (* Create a rectangular region given two opposing corners. *)
    fun rectR (ax,cx,ay,cy) = let
          val x1 = min(ax, cx)
          val y1 = min(ay, cy)
          val x2 = max(ax, cx)
          val y2 = max(ay, cy)
          in 
            if x1 = x2 orelse y1 = y2 then empty
            else REGION{numRects=1,extents=BOX{x1=x1,y1=y1,x2=x2,y2=y2},
                      bands=[BAND{y1=y1,y2=y2,xs=[(x1,x2)]}]}
          end

      (* If the points correspond to a rectangle, create the rectangular
       * region. Else return NONE.
       *)
    fun rectRegion(G.PT{x=ax,y=ay},G.PT{x=bx,y=by},
                   G.PT{x=cx,y=cy},G.PT{x=dx,y=dy}) = 
          if ((ay = by andalso bx = cx andalso cy = dy andalso dx = ax) orelse
              (ax = bx andalso by = cy andalso cx = dx andalso dy = ay)) 
            then SOME(rectR(ax,cx,ay,cy))
            else NONE

      (* Create a region from a rectangle. 
       * If the rectangle is degenerate, returns empty.
       * Canonicalizes the rectangle, so this works even
       * for negative width and height.
       *)
    fun rectangle (G.RECT{x,y,wid,ht}) = rectR(x,x+wid,y,y+ht)

      (* Create a region given a list of points describing a
       * polygon and a fill rule. Try to catch the simple case
       * of rectangles.
       *)
    fun polygon (arg as ([_,_,_], _)) = polyRegion arg
      | polygon (arg as ([a,b,c,d], _)) =
          (case rectRegion (a,b,c,d) of
            NONE => polyRegion arg
          | SOME r => r)
      | polygon (arg as ([a,b,c,d,e], _)) =
          if a = e then case rectRegion (a,b,c,d) of
              NONE => polyRegion arg
            | SOME r => r
          else polyRegion arg
      | polygon (arg as ((_::_::_::_), _)) = polyRegion arg
      | polygon _ = empty


    fun offsetf (REGION{bands,extents,numRects},offsetbox,offsetband) =
          REGION{numRects=numRects,extents = offsetbox extents, 
                  bands = map offsetband bands}
    fun offset (r,p) = offsetf (r,offsetBox p, offsetBand p)
    fun xOffset (r,x) = offsetf (r,xOffsetBox x, xOffsetBand x)
    fun yOffset (r,y) = offsetf (r,yOffsetBox y, yOffsetBand y)
          
(*
 * regionOp --
 *      Apply an operation to two regions.
 *      The function recurses down the two lists of bands.
 *      If the y intervals of two bands intersect in (y1,y2), a new band is
 *      created with bounds (y1,y2) and horizontal intervals determined
 *      by ofn applied to the two bands. For those y intervals in which
 *      one band is disjoint from all others, a new band is created by
 *      clipping the band to the y interval and applying nofn1 or nofn2. 
 *      The function attempts to coalesce each band with the previous.
 *)
    fun regionOp(REGION{bands=b1,extents=e1,...}, 
                 REGION{bands=b2,extents=e2,...}, ofn,  nofn1, nofn2) = let
          fun coalesce (b' as BAND{y1,...},n',bs as ((b as BAND{y2,...})::rest),n) =
                if y1 = y2 andalso n' = sizeOf b
                  then case Band.coalesce {lower=b',upper=b} of
                    NONE => (b'::bs,n+n')
                  | SOME b'' => (b''::rest,n)
                  else (b'::bs,n+n')
            | coalesce(b',n',[],_) = ([b'],n')
          fun wrapup(bs,n) = REGION{bands = rev bs, extents = zeroBox, numRects = n}
          fun tail(b as BAND{y1,y2,...},rest,f,ybot,bs,n) = let
                fun loop ([],a) = wrapup a
                  | loop ((b as BAND{y1,y2,...})::rest,(bs,n)) =
                      case f(b,max(y1,ybot),y2) of
                        (_,0) => loop(rest,(bs,n))
                      | (b',n') => loop(rest,(b'::bs,n'+n))
                in
                  case f(b,max(y1,ybot),y2) of
                    (_,0) => loop(rest,(bs,n))
                  | (b',n') => loop(rest,coalesce(b',n',bs,n))
                end
          fun inter(b,b',top,bot,bs,n) =
                case ofn(b,b',top,bot) of
                  (_,0) => (bs,n)
                | (b',n') => coalesce(b',n',bs,n)
          fun noninter(_,_,NONE,_,bs,n) = (bs,n)
            | noninter(top,bot,SOME f,b,bs,n) =
                if top = bot then (bs,n)
                else case f(b,top,bot) of
                  (_,0) => (bs,n)
                | (b',n') => coalesce(b',n',bs,n)
          fun loop ([],[],_,bs,n) = wrapup(bs,n)
            | loop (b::b1,[],ybot,bs,n) = 
                (case nofn1 of NONE => wrapup(bs,n) | SOME f => tail (b,b1,f,ybot,bs,n))
            | loop ([],b::b2,ybot,bs,n) =
                (case nofn2 of NONE => wrapup(bs,n) | SOME f => tail (b,b2,f,ybot,bs,n))
            | loop (bl as ((b as BAND{y1,y2,...})::next),
                    bl' as ((b' as BAND{y1=y1',y2=y2',...})::next'),ybot,bs,n) = let
                val (ytop,(bs',n')) = 
                      if y1 < y1' then (y1',noninter(max(y1,ybot),min(y2,y1'),nofn1,b,bs,n))
                      else if y1' < y1 then (y1,noninter(max(y1',ybot),min(y2',y1),nofn2,b',bs,n))
                      else (y1,(bs,n))
                val ybot = min(y2, y2')
                val (bs'',n'') = if ybot > ytop then inter(b,b',ytop,ybot,bs',n') else (bs',n')
                val nb = if y2 = ybot then next else bl
                val nb' = if y2' = ybot then next' else bl'
                in
                  loop(nb,nb',ybot,bs'',n'')
                end
          in
            loop(b1,b2,min(miny e1,miny e2),[],0)
          end

    fun intersect (
            reg1 as REGION{numRects,extents,...},
            reg2 as REGION{numRects=numRects',extents=extents',...}) =
          if numRects = 0 orelse numRects' = 0 (* check for trivial cases *)
              orelse not (Box.overlap(extents,extents'))
            then empty
            else setExtents (regionOp (reg1, reg2, Band.intersect, NONE, NONE))

    fun union (
            reg1 as REGION{numRects,extents,...},
            reg2 as REGION{numRects=numRects',extents=extents',...}) =
          if numRects = 0 then reg2
          else if numRects' = 0 then reg1
          else if numRects = 1 andalso inside(extents',extents) then reg1
          else if numRects' = 1 andalso inside(extents,extents') then reg2
          else let
            val REGION{bands,numRects,...} = 
              regionOp (reg1, reg2, Band.union, SOME squeeze, SOME squeeze)
            in 
              REGION{bands=bands,numRects=numRects,
                     extents = boundBox(extents,extents')} 
            end

    fun subtract (
            regM as REGION{numRects,extents,...},
            regS as REGION{numRects=numRects',extents=extents',...}) =
          if numRects = 0 orelse numRects' = 0 (* check for trivial reject *)
              orelse not (Box.overlap(extents,extents'))
            then regM
            else setExtents (regionOp (regM, regS, Band.subtract, SOME squeeze, NONE))

(* 
 * adjust:
 *   Returns region r' where (x,y) is in r' iff:
 *     (x+m,y) in r for some m <= dx   : shiftfn=xOffset,opfn=union
 *     (x+m,y) in r for all m <= dx    : shiftfn=xOffset,opfn=intersection
 *     (x,y+m) in r for some m <= dx   : shiftfn=yOffset,opfn=union
 *     (x,y+m) in r for all m <= dx    : shiftfn=yOffset,opfn=intersection
 *)

(*** NOTE: this code should be checked ***)
    fun adjust(r, dx, shiftfn, opfn) = let
          fun comp (0,_,_,r) = r
            | comp (arg as (dx,shift,s,r)) =
                if Word.andb(Word.fromInt dx, Word.fromInt shift) <> 0w0
		  then let
                    val r' = opfn(shiftfn(r,~shift),s)
                    val dx' = dx - shift
                    in if dx' = 0 then r' else c(dx',shift,s,r') end
                  else c arg
          and c (dx,shift,s,r) =
		comp(dx, Word.toIntX(Word.<<(Word.fromInt shift, 0w1)), opfn(shiftfn(s,~shift),s),r)
          in comp (dx,1,r,r) end

    fun shrink (r,G.PT{x=0,y=0}) = r
      | shrink (r,p as G.PT{x=dx,y=dy}) = let
          val xr = if dx = 0 then r
                   else if dx < 0 then adjust(r,2*(~dx),xOffset,union)
                   else adjust(r,2*dx,xOffset,intersect)
          val yr = if dy = 0 then xr
                   else if dy < 0 then adjust(xr,2*(~dy),yOffset,union)
                   else adjust(xr,2*dy,yOffset,intersect)
          in offset(yr, p) end

    fun xor (r1,r2) = union(subtract(r1,r2),subtract(r2,r1))

    fun isEmpty (REGION{numRects,...}) = numRects = 0

    fun equal (REGION{numRects,extents,bands,...},
              REGION{numRects=numRects',extents=extents',bands=bands',...}) =
          (numRects = numRects') andalso
            ((numRects = 0) orelse (extents = extents' andalso bands = bands'))

    fun overlap (REGION{numRects,extents,bands},
                 REGION{numRects=numRects',extents=extents',bands=bands'}) = let
          fun overl([],_) = false
            | overl(_,[]) = false
            | overl(bl as ((b as BAND{y1,y2,xs})::bs),
                    bl' as ((b' as BAND{y1=y1',y2=y2',xs=xs'})::bs')) =
                if y2 <= y1' then overl(bs,bl')
                else if y2' <= y1 then overl(bl,bs')
                else if Band.overlap (b,b') then true
                else if y2 < y2' then overl(bs,bl')
                else if y2' < y2 then overl(bl,bs')
                else overl(bs,bs')
          in
            numRects <> 0 andalso numRects' <> 0 andalso 
            Box.overlap(extents,extents') andalso overl(bands,bands')
          end

    fun pointIn (REGION{numRects=0,...},_) = false
      | pointIn (REGION{extents,bands,...},p) =
         inBox(extents,p) andalso List.exists (fn b => inBand(b,p)) bands

    fun rectIn (REGION{numRects = 0,...},_) = RectangleOut
      | rectIn (REGION{numRects,extents,bands},G.RECT{x,y,wid,ht}) = let
          val b  as BOX{x2=rx2,y2=ry2,...} = BOX{x1=x,y1=y,x2 = x+wid, y2 = y+ht}
          fun endChk (false,_) = RectangleOut
            | endChk (_,ry) = if ry < ry2 then RectanglePart else RectangleIn
          fun check ([],ry,partIn,partOut) = endChk(partIn,ry)
            | check ((b as BAND{y1,y2,...})::rest,ry,partIn,partOut) =
                if y2 <= ry then check(rest,ry,partIn,partOut)
                else if y1 >= ry2 then endChk(partIn,ry)
                else if y1 > ry then
                  if partIn then RectanglePart
                  else case rectInBand (b,x,rx2) of
                         RectangleOut => check(rest,ry,false,true)
                       | _ => RectanglePart
                else case rectInBand (b,x,rx2) of
                       RectanglePart => RectanglePart
                     | RectangleOut => if partIn then RectanglePart
                                       else check(rest,ry,false,true)
                     | RectangleIn => if partOut then RectanglePart
                                      else check(rest,y2,true,false)
          in
            if Box.overlap(extents,b) then check(bands,y,false,false)
            else RectangleOut
          end
  end
