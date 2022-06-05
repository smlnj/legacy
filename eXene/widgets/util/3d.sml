(* 3d.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

structure ThreeD : THREE_D =
  struct
    structure G = Geometry
    structure D = Drawing

    open Geometry Drawing

    datatype relief = Flat | Raised | Sunken | Groove | Ridge
        
    fun draw3DRect drawable (RECT {x,y,wid,ht},width) = let
          val point_list =
              [PT {x=x,y=y+ht},
               PT {x=x,y=y},
               PT {x=x+wid,y=y},
               PT {x=x+wid-width,y=y+width},
               PT {x=x+width,y=y+width},
               PT {x=x+width,y=y+ht-width},
               PT {x=x,y=y+ht}] 
          val r1 = RECT {x=x,y=y+ht-width, wid=wid,ht=width}
          val r2 = RECT {x=x+wid-width,y=y, wid=width,ht=ht}
          val dblw = width + width
          in 
            if wid < dblw orelse ht < dblw then fn _ => ()
            else fn {top,bottom} => (
              fillRect drawable bottom r1;
              fillRect drawable bottom r2;
              fillPolygon drawable top {verts=point_list, shape=NonconvexShape})
          end

    fun draw3DRect2 drawable (rect as (RECT {x,y,wid,ht}),width) = let
          val halfWidth = width div 2
          val halfWidth' = width - halfWidth
          val outer = draw3DRect drawable (rect,halfWidth')
          val r' = RECT {x=x+halfWidth',
                         y=y+halfWidth',
                         wid=wid-2*halfWidth',
                         ht=ht-2*halfWidth'}
          val inner = draw3DRect drawable (r', halfWidth)
          in
            fn pens => (outer pens; inner {top= #bottom pens,bottom= #top pens})
          end

    fun drawRect drawable {rect,width,relief} =
          case relief of
            Flat   => let val f = draw3DRect drawable (rect,width)
                      in fn ({base,...} : WidgetBase.shades) => f {top=base,bottom=base} end
          | Raised => let val f = draw3DRect drawable (rect,width)
                      in fn {light,dark,...} => f {top=light,bottom=dark} end
          | Sunken => let val f = draw3DRect drawable (rect,width)
                      in fn {light,dark,...} => f {top=dark,bottom=light} end
          | Ridge  => let val f = draw3DRect2 drawable (rect,width)
                      in fn {light,dark,...} => f {top=light,bottom=dark} end
          | Groove => let val f = draw3DRect2 drawable (rect,width)
                      in fn {light,dark,...} => f {top=dark,bottom=light} end

    fun drawFilledRect dr {rect,relief=Flat,width} shades =
          fillRect dr (#base shades) rect 
      | drawFilledRect dr {rect,width=0,relief=_} shades =
          fillRect dr (#base shades) rect 
      | drawFilledRect dr (a as {rect=RECT{x,y,wid,ht},width,...}) shades = let
          val delta = width + width
          val rect' = RECT{x=x+width,y=y+width,wid=wid - delta,ht=ht - delta}
          in
            fillRect dr (#base shades) rect';
            drawRect dr a shades
          end

    fun draw3DRoundRect drawable {rect, width, c_wid, c_ht} = let
          val RECT{x, y, wid, ht} = rect
          val halfwidth = width div 2
          val x = x + halfwidth
          val y = y + halfwidth
          val w = wid - 2*halfwidth
          val h = ht - 2*halfwidth
          val w2 = c_wid+c_wid
          val h2 = c_ht+c_ht
          val (ew, ew2) = if (w2 > w) then (0, 0) else (c_wid, w2)
          val (eh, eh2) = if (h2 > h) then (0, 0) else (c_ht, h2)
          in
            fn {top,bottom} => let
              val top = Drawing.updatePen(top,[Drawing.PV_LineWidth width])
              val bottom = Drawing.updatePen(bottom,[Drawing.PV_LineWidth width])
              in
                Drawing.drawArcs drawable top [
                   ARC{x= x, y= y, wid= ew2, ht= eh2, angle1= 180*64,
                            angle2= ~90*64},
                   ARC{x= x+ew, y= y, wid= w - ew2, ht= 0, angle1= 180*64,
                        angle2= ~180*64},
    
                   ARC{x= x, y= y+eh, wid= 0, ht= h - eh2, angle1= 270*64,
                        angle2= ~180*64},
                   ARC{x= x+w - ew2, y= y, wid= ew2, ht= eh2,
                            angle1= 45*64, angle2= 45*64},
                   ARC{x= x, y= y+h - eh2, wid= ew2, ht= eh2, angle1= 225*64,
                       angle2= ~45*64}
                   ];
    
                Drawing.drawArcs drawable bottom [
                   ARC{x= x+w - ew2, y= y, wid= ew2, ht= eh2,
                            angle1= 45*64, angle2= ~45*64},
                   ARC{x= x+w, y= y+eh, wid= 0, ht= h - eh2, angle1= 90*64,
                       angle2= ~180*64},
                   ARC{x= x+w - ew2, y= y+h - eh2, wid= ew2, ht= eh2,
                       angle1= 0,angle2= ~90*64},
                   ARC{x= x+ew, y= y+h, wid= w - ew2, ht= 0, angle1= 0,
                       angle2= ~180*64},
                   ARC{x= x, y= y+h - eh2, wid= ew2, ht= eh2, angle1= 270*64,
                       angle2= ~45*64}]
              end
          end

    fun draw3DRoundRect2 drawable {rect as RECT{x,y,wid,ht}, width, c_wid, c_ht} = let
          val halfWidth = width div 2
          val halfWidth' = width - halfWidth
          val outer = draw3DRoundRect drawable 
                        {rect=rect,width=halfWidth',c_wid=c_wid,c_ht=c_ht}
          val r' = RECT {x=x+halfWidth',
                         y=y+halfWidth',
                         wid=wid-2*halfWidth',
                         ht=ht-2*halfWidth'}
          val inner = draw3DRoundRect drawable
                        {rect=r',width=halfWidth,c_wid=c_wid,c_ht=c_ht}
          in
            fn pens => (outer pens; inner {top= #bottom pens,bottom= #top pens})
          end


    fun drawRoundRect drawable {rect, width, c_wid, c_ht, relief} =
          case relief of
            Flat => let val f = draw3DRoundRect drawable 
                                    {rect=rect,width=width,c_wid=c_wid,c_ht=c_ht}
                      in fn ({base,...} : WidgetBase.shades) => f {top=base,bottom=base} end
          | Raised => let val f = draw3DRoundRect drawable 
                                    {rect=rect,width=width,c_wid=c_wid,c_ht=c_ht}
                      in fn {light,dark,...} => f {top=light,bottom=dark} end
          | Sunken => let val f = draw3DRoundRect drawable
                                    {rect=rect,width=width,c_wid=c_wid,c_ht=c_ht}
                      in fn {light,dark,...} => f {top=dark,bottom=light} end
          | Ridge  => let val f = draw3DRoundRect2 drawable
                                    {rect=rect,width=width,c_wid=c_wid,c_ht=c_ht}
                      in fn {light,dark,...} => f {top=light,bottom=dark} end
          | Groove => let val f = draw3DRoundRect2 drawable
                                    {rect=rect,width=width,c_wid=c_wid,c_ht=c_ht}
                      in fn {light,dark,...} => f {top=dark,bottom=light} end

    

        (*
         * The table below is used for a quick approximation in
         * computing a new point parallel to a given line
         * An index into the table is 128 times the slope of the
         * original line (the slope must always be between 0.0
         * and 1.0).  The value of the table entry is 128 times
         * the  amount to displace the new line in y for each unit
         * of perpendicular distance. In other words, the table 
         * maps from the tangent of an angle to the inverse of 
         * its cosine.  If the slope of the original line is greater 
         * than 1, then the displacement is done in x rather than in y.
         *) 

    val shiftTable = let
          fun compute i = let
            val tangent = (real i) / 128.0
            in Real.trunc ((128.0 / Math.cos(Math.atan tangent)) + 0.5) end
          val v = Vector.tabulate(129,compute)
          in fn i => Vector.sub(v,i) end
        
    (* Given two points on a line, compute a point on a
     * new line that is parallel to the given line and
     * a given distance away from it.
     *)
    fun shiftLine (p1 as PT{x,y},p2,distance) = let
        fun << (w,i) = Word.toInt (Word.<< (Word.fromInt w, i))
        fun >> (w,i) = Word.toInt (Word.>> (Word.fromInt w, i))
        infix << >>
        val (PT{x=dx,y=dy}) = subPt(p2,p1)
        val (dy,dyNeg) = if dy < 0 then (~dy,true) else (dy,false)
        val (dx,dxNeg) = if dx < 0 then (~dx,true) else (dx,false)
        fun adjust(dy,dx) = 
              ((distance * shiftTable((dy << 0w7) div dx)) + 64) >> 0w7
        in
          if dy <= dx then let
            val dy = adjust(dy,dx)
            in PT{x=x,y= y + (if dxNeg then dy else ~dy)} end
          else let
            val dx = adjust(dx,dy)
            in PT{x= x + (if dyNeg then ~dx else dx),y=y} end
        end
    
        (* Find the intersection of two lines with the given endpoints.
         * Return NONE if lines are parallel
         *)
        
    fun intersect (a1 as (PT{x=a1x,y=a1y}),a2, b1 as (PT{x=b1x,y=b1y}),b2) = let
          val PT {x=ax,y=ay} = subPt(a2,a1)
          val PT {x=bx,y=by} = subPt(b2,b1)
          val axby = ax * by
          val bxay = bx * ay
          val axbx = ax * bx
          val ayby = ay * by
          fun solve (p,q) = let
                val (p,q) = if q < 0 then (~p,~q) else (p,q)
                in
                  if p < 0 then ~(((~p) + q div 2) div q)
                  else (p + (q div 2)) div q
                end
          in
            if axby = bxay then NONE
            else let
              val x = solve (a1x*bxay - b1x*axby + (b1y - a1y)*axbx,bxay - axby)
              val y = solve (a1y*axby - b1y*bxay + (b1x - a1x)*ayby,axby - bxay)
            in (SOME (PT{x=x,y=y})) end
          end
    
    fun makePerp (PT{x,y},PT{x=x',y=y'}) = PT{x=x+(y'-y),y=y-(x'-x)}
        
    fun last2Pts [] = raise LibBase.Impossible "ThreeD.last2Pts"
      | last2Pts [v1,v2] = (v1,v2)
      | last2Pts (v::vs) = last2Pts vs

    (*
     * draw3DPoly draws a polygon of given width. The widening occurs
     * on the left of the polygon as it is traversed. If the width
     * is negative, the widening occurs on the right. Duplicate points
     * are ignored. If there are less than two distinct points, nothing
     * is drawn.
     * 
     * The main loop below (loop2) is executed once for each vertex in 
     * the polgon.  At the beginning of each iteration things look like this:
     *
     *          poly1       /
     *             *       /
     *             |      /
     *             b1   * poly0
     *             |    |
     *             |    |
     *             |    |
     *             |    |
     *             |    |
     *             |    | p1                 p2
     *             b2   *--------------------*
     *             |
     *             |
     *             *----*--------------------*
     *          poly2   newb1               newb2
     *
     * For each interation, we:
     * (a) Compute poly2 (the border corner corresponding to p1)
     *     As part of this process, compute a new b1 and b2 value 
     *     for the next side (p1-p2) of the polygon.
     * (b) Draw the polygon (poly0,poly1,poly2,p1)
     *
     * The above situation doesn't exist until two points have 
     * been processed. We start with the last two points in the list
     * (in loop0) to get an initial b1 and b2. Then, in loop1, we
     * use the first point to get a new b1 and b2, with which we
     * can calculate an initial poly1 (poly0 is the last point in
     * the list). At this point, we can start the main loop.
     *
     * If two consecutive segments of the polygon are parallel,
     * then things get more complex. (See findIntersect).
     * Consider the following diagram:
     *
     * poly1
     *    *----b1-----------b2------a
     *                                \
     *                                  \
     *         *---------*----------*    b
     *        poly0     p2         p1   /
     *                                /
     *              --*--------*----c
     *              newB1    newB2
     *
     * Instead of using the intersection and p1 as the last two points 
     * in the polygon and as poly1 and poly0 in the next iteration, we 
     * use a and b, and b and c, respectively.
     *
     * Do the computation in three stages:
     * 1. Compute a point "perp" such that the line p1-perp
     *    is perpendicular to p1-p2.
     * 2. Compute the points a and c by intersecting the lines
     *    b1-b2 and newb1-newb2 with p1-perp.
     * 3. Compute b by shifting p1-perp to the right and
     *    intersecting it with p1-p2.
     *)

    fun draw3DPoly _ ([],_) _ = ()
      | draw3DPoly _ ([_],_) _ = ()
      | draw3DPoly drawable (ps as (iP::_),width) {top,bottom} = let
          val (p1,p2) = last2Pts ps
          fun calcOffPoints (v1,v2) = let
              val b1 = shiftLine (v1,v2,width)
              in (b1,addPt(b1,subPt(v2,v1))) end
          fun findIntersect (p1,p2,newb1,newb2,b1,b2) =
                case intersect(newb1,newb2,b1,b2) of
                  SOME x => (x,p1,x)
                | NONE => let
                    val perp = makePerp (p1,p2)
                    val SOME poly2 = intersect (p1,perp,b1,b2)
                    val SOME c = intersect (p1,perp,newb1,newb2)
                    val shift1 = shiftLine (p1,perp,width)
                    val shift2 = addPt(shift1,subPt(perp,p1))
                    val SOME poly3 = intersect (p1,p2,shift1,shift2)
                    in (poly2,poly3,c) end
          fun draw (p0,p1,p2,p3) = let
                val PT{x=dx,y=dy} = subPt(p3,p0)
                val pen =
                    if dx > 0 then if dy <= dx then bottom else top
                    else if dy < dx then bottom else top
                in
                  fillPolygon drawable pen {verts=[p0,p1,p2,p3],
                              shape=ConvexShape}
                end

          fun loop2(p1,[],b1,b2,poly0,poly1) = 
                if p1 = iP then ()
                else let
                  val (newb1,newb2) = calcOffPoints (p1,iP)
                  val (poly2,poly3,_) = findIntersect (p1,iP,newb1,newb2,b1,b2)
                  in draw (poly0,poly1,poly2,poly3) end
            | loop2(p1,p2::ps,b1,b2,poly0,poly1) =
                if p1 = p2 then loop2(p1,ps,b1,b2,poly0,poly1)
                else let
                  val (newb1,newb2) = calcOffPoints (p1,p2)
                  val (poly2,poly3,c) = findIntersect (p1,p2,newb1,newb2,b1,b2)
                  in
                    draw (poly0,poly1,poly2,poly3);
                    loop2(p2,ps,newb1,newb2,poly3,c)
                  end
          fun loop1(p1,[],_,_) = ()
            | loop1(p1,p2::ps,b1,b2) =
                if p1 = p2 then loop1(p1,ps,b1,b2)
                else let
                  val (newb1,newb2) = calcOffPoints (p1,p2)
                  val (poly2,poly3,c) = findIntersect (p1,p2,newb1,newb2,b1,b2)
                  in loop2(p2,ps,newb1,newb2,poly3,c) end
          fun loop0(_,[]) = ()
            | loop0(p1,p2::ps) =
                if p1 = p2 then loop0(p2,ps)
                else let
                  val (b1,b2) = calcOffPoints (p1,p2)
                  in loop1(p2,ps,b1,b2) end
          in
            loop0(p1,p2::ps)
          end

    fun draw3DPoly2 drawable (pts,width) = let
          val halfWidth = width div 2
          val outer = draw3DPoly drawable (pts,halfWidth)
          val inner = draw3DPoly drawable (pts,~halfWidth)
          in
            fn pens => (outer pens; inner {top= #bottom pens,bottom= #top pens})
          end

    fun drawPoly drawable {pts,width,relief} =
          case relief of
            Flat => let val f = draw3DPoly drawable (pts,width)
                      in fn ({base,...} : WidgetBase.shades) => f {top=base,bottom=base} end
          | Raised => let val f = draw3DPoly drawable (pts,width)
                      in fn {light,dark,...} => f {top=light,bottom=dark} end
          | Sunken => let val f = draw3DPoly drawable (pts,width)
                      in fn {light,dark,...} => f {top=dark,bottom=light} end
          | Ridge =>  let val f = draw3DPoly2 drawable (pts,width)
                      in fn {light,dark,...} => f {top=light,bottom=dark} end
          | Groove => let val f = draw3DPoly2 drawable (pts,width)
                      in fn {light,dark,...} => f {top=dark,bottom=light} end

  end


