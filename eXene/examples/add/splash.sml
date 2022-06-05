(* splash.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure Splash = 
  struct
    local 
      open Geometry EXeneBase Drawing Widget 

      fun bbox [] = RECT{x=0,y=0,wid=0,ht=0}
        | bbox (PT{x,y}::pts) = let
        fun bb (minx,miny,maxx,maxy,[]) = 
               RECT{x=minx,y=miny,wid=maxx-minx+1,ht=maxy-miny+1}
          | bb (minx,miny,maxx,maxy,PT{x,y}::pts) = 
               bb(min(minx,x),min(miny,y),max(maxx,x),max(maxy,y),pts)
        in
          bb(x,y,x,y,pts)
        end

      fun mkSplash (root, waterTile) = let
        val scr = screenOf root
        val waterPen = newPen [
          PV_FillStyle_Stippled, 
          PV_Stipple waterTile,
          PV_Foreground color1]

        fun mks pts = let
          val RECT{x,y,wid,ht} = bbox pts
          val delPt = PT{x=x,y=y}
          val pts' = map (fn p => subPt(p,delPt)) pts
          val pixmap = createPixmap scr (SIZE{wid=wid,ht=ht},1)
          val draww = drawableOfPM pixmap
          val _ = clearDrawable draww
          val _ = fillPolygon draww waterPen {shape=NonconvexShape, verts=pts'}
          val tile = createTileFromPixmap pixmap
          in
            destroyPixmap pixmap;
            ({origin = hd pts', data = tile} : Images.image)
          end
        in
          mks
        end

      fun mkPt (x,y) = PT{x=x,y= ~y}

      val littleData = [
          (0,0), (10,18), (6,15), (4,20), (0,12), (~4,18), (~7,15), (~15,20)
        ]
      val mediumData = [
          (0,0), (20,30), (14,25), (10,20), (8,24), (5,21), (3,27), (0,22), 
          (~3,18), (~5,23), (~7,25), (~11,20), (~14,24), (~18,21), (~20,25) 
        ]
      val bigData = [
          (0,0), (30,35), (28,38), (25,36), (23,32), (20,36), (18,34), 
          (15,31), (13,37), (10,32), (8,34), (5,31), (3,37), (0,32), 
          (~3,28), (~5,33), (~7,35), (~11,30), (~14,34), (~18,31), (~20,35),
          (~21,30), (~24,34), (~29,31), (~32,37) 
        ]
      val splashList = map (fn pts => map mkPt pts) 
          [littleData, mediumData, bigData]
    in

      fun mkSplashes arg = map (mkSplash arg) splashList

    end (* local *)
  end
