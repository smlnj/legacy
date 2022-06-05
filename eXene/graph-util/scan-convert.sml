(* scan-convert.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories
 *
 * Code for scan converting a polygon specified as a list of
 * points and a fill rule into even length list of points
 * corresponding to scan line segments.
 *
 * The resulting list of points is ordered from bottom to top and
 * from left to right.
 *
 * The algorithms are roughly based on those found in the sample X library.
 *)

structure ScanConvert :
  sig
    structure G : GEOMETRY

    datatype fill_rule = EvenOdd | Winding

    val scanConvert : G.point list * fill_rule -> G.point list

  end = struct

    structure G = Geometry

    open G

    datatype fill_rule = EvenOdd | Winding

    structure Bres = 
      struct
        type bres_info = {
          x : int,                (* minor axis        *)
          d : int                 (* decision variable *)
        }

(*
 *  In scan converting polygons, we want to choose those pixels
 *  which are inside the polygon.  Thus, we add .5 to the starting
 *  x coordinate for both left and right edges.  Now we choose the
 *  first pixel which is inside the pgon for the left edge and the
 *  first pixel which is outside the pgon for the right edge.
 *  Draw the left pixel, but not the right.
 *
 *  How to add .5 to the starting x coordinate:
 *      If the edge is moving to the right, then subtract dy from the
 *  error term from the general form of the algorithm.
 *      If the edge is moving to the left, then add dy to the error term.
 *
 *  The reason for the difference between edges moving to the left
 *  and edges moving to the right is simple:  If an edge is moving
 *  to the right, then we want the algorithm to flip immediately.
 *  If it is moving to the left, then we don't want it to flip until
 *  we traverse an entire pixel.
 *)
           
        fun incr (m,m1,incr1,incr2) =
              if m1 > 0 
                then fn {x,d} => if d > 0 then {x=x+m1,d=d+incr1} else {x=x+m,d=d+incr2}
                else fn {x,d} => if d >= 0 then {x=x+m1,d=d+incr1} else {x=x+m,d=d+incr2}

        fun mkBresInfo (dy, x1, x2) = let    (* assume dy > 0 *)
              val dx = x2 - x1
              val m = Int.quot(dx, dy)
              in 
                if dx < 0 then 
                  let val m1 = m - 1
                      val ix = ~(dx + dx)
                      val iy = dy + dy
                      val incr1 = ix + iy * m1
                      val incr2 = ix + iy * m
                      in 
                        ({x = x1,d = m1 * iy + ix}, incr (m,m1,incr1,incr2))
                      end
                else
                  let val m1 = m + 1
                      val ix = dx + dx
                      val iy = ~(dy + dy)
                      val incr1 = ix + iy * m1
                      val incr2 = ix + iy * m
                      in 
                        ({x = x1, d = m * iy + ix}, incr (m,m1,incr1,incr2))
                      end
              end
      end (* structure Bres *)

    val largeCoord = 1000000
    val smallCoord = ~largeCoord

    datatype edge = E of {
      adv : Bres.bres_info -> Bres.bres_info, (* function to update Bresenham info *)
      bres : Bres.bres_info ref,   (* Bresenham info to run the edge     *)
      clockwise : bool,            (* flag for winding number rule       *)
      ymax : int                   (* ycoord at which we exit this edge. *)
    }


    type scanline = int * edge list

    datatype edge_table = ET of {
      ymax : int,                 (* ymax for the polygon     *)
      ymin : int,                 (* ymin for the polygon     *)
      scanlines : scanline list   (* scanlines                *)
    }

    fun insertEdge (scanlines,miny : int,edge as E{bres= ref {x=minx,...},...}) = let
          fun ine [] = [edge]
            | ine (el as ((e as E{bres= ref {x,...},...})::rest)) =
                if x < minx then e::(ine rest) else edge::el
          fun ins [] = [(miny,[edge])]
            | ins (sl as ((s as (y,edges))::rest)) =
                if y < miny then s::(ins rest)
                else if y = miny then (y,ine edges)::rest
                else (miny,[edge])::sl
          in ins scanlines end

    fun mkEdgeTable pts = let
(*
open Format
val fmt = formatf "make edge: topx %d topy %d botx %d boty %d cw %b\n"
            (outputc std_out)
val fmt1 = formatf "number of scanlines = %d\n" (outputc std_out)
*)
          fun mkEdge (ymax,clockwise,dy,topx,botx) = let
                val (info,f) = Bres.mkBresInfo (dy,topx,botx)
                in E {ymax=ymax,clockwise=clockwise,bres = ref info,adv=f} end
          fun loop ([],prevpt,ymax,ymin,slines) =
                ET{ymax=ymax,ymin=ymin,scanlines=slines}
            | loop ((cp as PT{x,y})::rest,PT{x=x',y=y'},ymax,ymin,slines) = let
(* val _ = fmt1 [INT (length slines)] *)
                val (botx,boty,topx,topy,clockwise) = 
                      if y' > y then (x',y',x,y,false)
                      else (x,y,x',y',true)
                in
                  if boty = topy then loop(rest,cp,ymax,ymin,slines)
                  else let
                    val dy = boty - topy
                    val e = mkEdge(boty-1,clockwise,boty-topy,topx,botx)
(* val _ = fmt [INT topx,INT topy,INT botx,INT boty,BOOL clockwise] *)
                    in
                      loop (
			rest,
			cp,
			Int.max(y',ymax),
			Int.min(y',ymin),
			insertEdge(slines,topy,e))
                    end
                end
          in loop (pts, List.last pts, smallCoord, largeCoord, []) end

    fun getWinding edges = let
          fun loop ([],_,_) = []
            | loop ((e as E{clockwise,...})::es,isInside,inside) = let
                val isInside' = if clockwise then isInside+1 else isInside-1
                in
                  if inside = (isInside' <> 0) then e::(loop(es,isInside',not inside))
                  else loop(es,isInside',inside)
                end
          in loop (edges,0,true) end

    fun gtr(E{bres = ref {x,...},...},E{bres = ref {x=x',...},...}) = x > x'
    val sorted = ListMergeSort.sorted gtr
    val sort' = ListMergeSort.sort gtr
    fun sort edges = if sorted edges then (edges,false) else (sort' edges, true)

    fun addActive ([],acs) = acs
      | addActive (es,[]) = es
      | addActive (el as (e as E{bres = ref {x,...},...})::es,
                   al as (a as E{bres = ref {x=ax,...},...})::acs) =
          if x <= ax then e::(addActive(es,al)) else a::(addActive(el,acs))

    fun evenOdd (ET{ymin,ymax,scanlines}) = let
          fun doEdges (y,edges,pts) = let
                fun loop ([],es,pts) = (rev es,pts)
                  | loop ((e as E{ymax,adv,bres,...})::rest,es,pts) = let
                      val bi as {x,...} = !bres
                      in
                        if ymax = y then loop(rest,es,PT{x=x,y=y}::pts)
                        else (
                          bres := adv bi;
                          loop(rest,e::es,PT{x=x,y=y}::pts)
                        )
                      end
                in loop (edges,[],pts) end
          fun chkActive(y,[],active) = ([],active)
            | chkActive(y,sl as ((y',edges)::rest),active) =
                if y = y' then (rest,addActive(edges,active)) else (sl,active)
          fun loop(y,scanlines,active,pts) =
                if y = ymax then pts
                else let
                  val (scanlines',active') = chkActive(y,scanlines,active) 
                  val (active'',pts') = doEdges (y,active',pts)
                  in loop(y+1,scanlines',#1(sort active''), pts') end
          in loop(ymin,scanlines,[],[]) end

    fun winding (ET{ymin,ymax,scanlines}) = let
          fun doEdges (y,edges,ws,pts) = let
                fun update (e as E{ymax,adv,bres,...},(es,fix)) =
                      if ymax = y then (es,true)
                      else (bres := adv (!bres); (e::es,fix))
                fun finish (edges,es,pts) = let
                      fun f ([],(es,fix)) = (rev es,pts,fix)
                        | f (e::rest,es) = f (rest,update(e,es))
                      in f (edges,es) end
                fun loop (edges,[],es,pts) = finish (edges,es,pts)
                  | loop (e::rest,wl as (E{bres = b',...}::ws),es,pts) = let
                      val E{bres = b as ref{x,...},...} = e
                      in
                        if b = b' then loop(rest,ws,update(e,es),PT{x=x,y=y}::pts)
                        else loop(rest,wl,update(e,es),pts)
                      end
                  | loop _ = raise LibBase.Impossible "ScanConvert.winding"
                in loop (edges,ws,([],false),pts) end
          fun chkActive(y,[],active,ws) = ([],active,ws)
            | chkActive(y,sl as ((y',edges)::rest),active,ws) =
                if y = y' then let
                  val acs = addActive(edges,active)
                  in (rest,acs,getWinding acs) end
                else (sl,active,ws)
          fun loop(y,scanlines,active,ws,pts) =
                if y = ymax then pts
                else let
                  val (scanlines',active',ws') = chkActive(y,scanlines,active,ws) 
                  val (active'',pts',fix) = doEdges (y,active',ws',pts)
                  val (active''',changed) = sort active''
                  val ws'' = if fix orelse changed then getWinding active''' else active'''
                  in loop(y+1,scanlines',active''', ws'', pts') end
          in loop(ymin,scanlines,[],[],[]) end
 
    fun scanConvert (pts, EvenOdd) = evenOdd(mkEdgeTable pts) 
      | scanConvert (pts, Winding) = winding(mkEdgeTable pts)
 
  end
