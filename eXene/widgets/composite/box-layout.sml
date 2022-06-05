(* box-layout.sml
 *
 * COPYRIGHT (c) 1991, 1992 by AT&T Bell Laboratories
 *
 * Code for laying out box widgets.
 *)

signature BOX_LAYOUT = 
  sig

    structure G : GEOMETRY
    structure W : WIDGET

    datatype box_item = 
      G of { x_dim : W.dim, y_dim : W.dim }   (* should be G of W.bounds *)
    | W of W.widget
    | HB of (W.valign * box_item list)
    | VB of (W.valign * box_item list)

    val compLayout : (G.rect * box_item) -> (bool * (W.widget * G.rect) list)

    val compSize : box_item -> W.bounds

  end (* BOX_LAYOUT *)

structure BoxLayout : BOX_LAYOUT = struct

  structure G = Geometry
  structure W = Widget

  open Geometry Widget

    val min = Int.min
    val max = Int.max

  datatype box_item = 
    G of bounds
  | W of widget
  | HB of (valign * box_item list)
  | VB of (valign * box_item list)

  datatype bnds_tree = 
    BT_G of bounds
  | BT_W of (bounds * widget)
  | BT_HB of (bounds * valign * bnds_tree list)
  | BT_VB of (bounds * valign * bnds_tree list)

  val MAXX = 65535       (* Maximum dimension of an X window. *)

  fun flipBnds ({x_dim,y_dim} : bounds) = {x_dim=y_dim,y_dim=x_dim}

  fun bndsOf (BT_G b) = b
    | bndsOf (BT_W (b,_)) = b
    | bndsOf (BT_HB (b,_,_)) = b
    | bndsOf (BT_VB (b,_,_)) = b

  fun flipBT (BT_G b) = BT_G (flipBnds b)
    | flipBT (BT_W (b,tw)) = BT_W (flipBnds b,tw)
    | flipBT (BT_HB (b,a,l)) = BT_HB (flipBnds b,a,l)
    | flipBT (BT_VB (b,a,l)) = BT_VB (flipBnds b,a,l)

  fun getBnds (DIM{base,incr,min,nat,max=NONE}) =
        (base+incr*nat,base+incr*min,NONE,incr)
    | getBnds (DIM{base,incr,min,nat,max=SOME max}) =
        (base+incr*nat,base+incr*min,SOME(base+incr*max),incr)
  fun xBnds ({x_dim,...} : bounds) = getBnds x_dim
  fun yBnds ({y_dim,...} : bounds) = getBnds y_dim

  fun comp_size cl = let
        fun doX (NONE, _) = NONE
          | doX (_, NONE) = NONE
          | doX (SOME cx, SOME sx) = SOME(cx + sx)
        fun doY (cy, NONE) = cy
          | doY (NONE, SOME sy) = SOME sy
          | doY (SOME cy, SOME sy) = SOME(max(cy,sy))
        fun tight (_, NONE) = false
          | tight (mn, SOME mx) = (mn = mx)
        fun maxDim (DIM{base,incr,max=NONE,...}) = NONE
          | maxDim (DIM{base,incr,max=SOME max,...}) = SOME(base + incr*max)

        fun accBnds ({x_dim,y_dim}, (nx,ny,mnx,mny,mxx,mxy,ix,iy)) = let
              val DIM{base=basex,incr=incx,min=minx,nat=natx,max=maxx} = x_dim
              val DIM{base=basey,incr=incy,min=miny,nat=naty,max=maxy} = y_dim
              in
                (
                  nx + basex + incx*natx,
                  max(ny, basey + incy*naty),
                  mnx + basex + incx*minx,
                  max(mny, basey + incy*miny),
                  doX (mxx, maxDim x_dim), 
                  doY (mxy, maxDim y_dim),
                  if tight (minx,maxx) then ix else min(ix, incx), 
                  if tight (miny,maxy) orelse incy = 1 then iy 
                  else min(iy, incy)
                )
              end

        val (natx,naty,minx,miny,maxx,maxy,incx,incy) = 
              List.foldl accBnds (0,0,0,0,SOME 0,NONE,MAXX,MAXX) cl

      (* Guarantee increment > 0 *)
        fun adjustIncr i = if i = MAXX orelse i <= 0 then 1 else i
        val incx = adjustIncr incx
        val incy = adjustIncr incy

      (* Guarantee maxy >= naty *)
        val maxy = case maxy of NONE => NONE | SOME my => SOME(max(my,naty))

      (* Return least f such that min + f*inc >= v *)
        fun factor (min,1) v = v - min
          | factor (min,inc) v = ((v - min + inc - 1) div inc)
        val xfact = factor (minx,incx)
        val yfact = factor (miny,incy)
        in
          { 
            x_dim = DIM{
              base=minx,
              incr=incx,
              min=0,
              nat=xfact natx,
              max=case maxx of NONE => NONE | SOME v => SOME(xfact v)
            },
            y_dim = DIM{
              base=miny,
              incr=incy,
              min=0,
              nat=yfact naty,
              max=case maxy of NONE => NONE | SOME v => SOME(yfact v)
            }
          }
        end

  fun compSize (G bnds) = bnds
    | compSize (W widget) = boundsOf widget
    | compSize (HB(_,boxes)) = comp_size (map compSize boxes)
    | compSize (VB(_,boxes)) = 
        flipBnds (comp_size (map (flipBnds o compSize) boxes))

  fun flr (v : int, base, inc) =
        (if v = base then v else base + ((v - base) div inc)*inc) 
          handle Div => raise BadIncrement
  fun ceil (v : int, base, inc) =
        (if v = base then v else base + ((v - base + inc - 1) div inc)*inc) 
          handle Div => raise BadIncrement

  fun setMinors (yo, ys, bndl, align) = let
        fun setM bnd = let
              val sz = (case yBnds (bndsOf bnd) of
                (nat, mn, NONE, 1) => max (ys, mn)
              | (nat, mn, SOME mx, 1) => min (mx, max(ys,mn))
              | (nat, mn, NONE, incy) => max (flr(ys,nat,incy), ceil(mn,nat,incy))
              | (nat, mn, SOME mx, incy) => 
                  min (flr(mx,nat,incy), max (flr(ys,nat,incy), ceil(mn,nat,incy)))
              )
              in
                case align of
                  VCenter => (yo + ((ys - sz) div 2), sz)
                | VTop => (yo, sz)
                | VBottom => (yo + ys - sz, sz)
              end
        in
          map setM bndl
        end

  fun setMajors (xo, xs, bndl) = let
        fun mkQuad (BT_G b) =  
              (case xBnds b of
                (nat, mn, NONE, inc) => (nat, nat-mn, MAXX-nat, inc)
              | (nat, mn, SOME mx, inc) => (nat, nat-mn, mx-nat, inc)
              )
          | mkQuad bnd = 
              (case xBnds (bndsOf bnd) of
                (nat, mn, NONE, inc) => (nat, nat-max(1,mn), MAXX-nat, inc)
              | (nat, mn, SOME mx, inc) => (nat, nat-max(1,mn), mx-nat, inc)
              )

        val szList = map mkQuad bndl

        fun addCnt ((s:int,0,0,_),(cs,sh_cnt,st_cnt)) = (cs+s,sh_cnt,st_cnt)
          | addCnt ((s,0,_,_),(cs,sh_cnt,st_cnt)) = (cs+s,sh_cnt,st_cnt+1)
          | addCnt ((s,_,0,_),(cs,sh_cnt,st_cnt)) = (cs+s,sh_cnt+1,st_cnt)
          | addCnt ((s,_,_,_),(cs,sh_cnt,st_cnt)) = (cs+s,sh_cnt+1,st_cnt+1)
        val (sz, shr_cnt, str_cnt) = List.foldl addCnt (0,0,0) szList

        fun addWd (l, amt, cnt) = let
              fun dst ([], amt, _, cnt, l) = (rev l, amt, cnt)
                | dst ((v as (s,_,0,_))::tl, amt, per, cnt, l) = dst(tl, amt, per, cnt, v::l)
                | dst ((s,sh,st,inc)::tl, amt, per, cnt, l) = let
                    val delta = 
                          if inc = 1 then min(amt,min(per,st))
                          else inc*(min(amt,min(per,st)) div inc)
                    in
                      if delta = amt then ((rev l)@((s+delta,sh,st-delta,inc)::tl), 0, 0)
                      else if delta = st orelse delta = 0 then 
                        dst (tl, amt-delta, per, cnt, (s+delta,sh,0,inc)::l)
                      else dst (tl, amt-delta, per, cnt+1, (s+delta,sh,st-delta,inc)::l)
                    end
              in
                if amt <= 0 orelse cnt = 0 then l
                else addWd (dst (l, amt, max(1,amt div cnt), 0, []))
              end

        fun subWd (l, amt, cnt) = let
              fun dst ([], amt, _, cnt, l) = (rev l, amt, cnt)
                | dst ((v as (s,0,_,_))::tl, amt, per, cnt, l) = dst(tl, amt, per, cnt, v::l)
                | dst ((s,sh,st,inc)::tl, amt, per, cnt, l) = let
                    val delta = 
                          if inc = 1 then min(amt,min(per,sh))
                          else inc*(min(amt,min(per,sh)) div inc)
                    in
                      if delta = amt then ((rev l)@((s-delta,sh-delta,st,inc)::tl), 0, 0)
                      else if delta = sh orelse delta = 0 then 
                        dst (tl, amt-delta, per, cnt, (s-delta,0,st,inc)::l)
                      else dst (tl, amt-delta, per, cnt+1, (s-delta,sh-delta,st,inc)::l)
                    end
              in
                if amt <= 0 orelse cnt = 0 then l
                else subWd (dst (l, amt, max(1,amt div cnt), 0, []))
              end

        fun distrib () =
              if sz = xs then szList
              else if sz < xs then addWd (szList, xs-sz, str_cnt)
              else subWd (szList, sz-xs, shr_cnt)

        fun addOr (curo, ((wd : int,_,_,_)::tl)) = (curo,wd)::(addOr (curo+wd,tl))
          | addOr (curo, []) = []

        in
          addOr(xo, distrib ())
        end

  fun bndsTree (G bnds) = BT_G bnds
    | bndsTree (W tw) = BT_W(boundsOf tw,tw)
    | bndsTree (HB(a,boxes)) = let
        val tree = map bndsTree boxes
        in
          BT_HB(comp_size (map bndsOf tree),a,tree)
        end
    | bndsTree (VB(a,boxes)) = let
        val tree = map (flipBT o bndsTree) boxes
        in
          BT_VB(flipBnds (comp_size (map bndsOf tree)),a,tree)
        end

    (* Given a rectangle and the bounds tree for the layout,
     * compute the layout, which consists of a
     * list of widgets and their new rectangles.
     *)
  local
    fun merge ([],[],[]) = []
      | merge ((x,w)::xs,(y,h)::ys,b::bs) =
          (RECT{x=x,y=y,wid=w,ht=h},b)::(merge(xs,ys,bs))
      | merge _ = raise LibBase.Impossible "BoxLayout.HB"

  in
  fun comp_layout (_, BT_G _) = []
    | comp_layout (r, BT_W (_,w)) = [(w,r)]
    | comp_layout (RECT{x,y,wid,ht}, BT_HB(_,a,bl)) = let
        val l = merge(setMajors(x,wid,bl),setMinors(y,ht,bl,a),bl)
        in
          List.foldl (fn(bx,bl) => (comp_layout bx)@bl) [] l
        end
    | comp_layout (r as RECT{x,y,wid,ht}, BT_VB(_,a,bl)) = let
        val l = merge(setMinors(x,wid,bl,a),setMajors(y,ht,bl),bl)
        in
          List.foldl (fn(bx,bl) => (comp_layout bx)@bl) [] l
        end
  end (* local *)

  fun compLayout (rect, boxes) = let
        val bndsT = bndsTree boxes
        val fits = compatibleSize(bndsOf bndsT,sizeOfRect rect)
        val l = case bndsT of
                  BT_G _ => []
                | (v as BT_W (bnds,w)) => 
                    comp_layout(rect,BT_HB(bnds,VCenter,[v]))
                | bt => comp_layout(rect,bt)
        in
          (fits,l)
        end

end (* BoxLayout *)
