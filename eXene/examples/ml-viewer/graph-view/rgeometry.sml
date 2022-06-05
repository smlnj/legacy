(*
signature RGEOMETRY =
  sig

    datatype rpoint = RPT of {x : real, y : real}
    datatype rsize = RSIZE of {wid : real, ht : real}
    datatype rrect = RRECT of {x : real, y : real, wid : real, ht : real}
    val roriginPt : rpoint
    val originOfRect : rrect -> rpoint
    val cornerOfRect : rrect -> rpoint
    val rrect : Geometry.rect -> rrect
    val rect : rrect -> Geometry.rect
    val boundBox : rpoint list -> rrect
    val intersect : rrect * rrect -> bool
  end
*)
structure RGeometry =
  struct

    datatype rpoint = RPT of {x : real, y : real}
    datatype rsize = RSIZE of {wid : real, ht : real}
    datatype rrect = RRECT of {x : real, y : real, wid : real, ht : real}
    val roriginPt = RPT{x=0.0,y=0.0}
    fun originOfRect (RRECT{x,y,...}) = RPT{x=x,y=y}
    fun cornerOfRect (RRECT{x,y,wid,ht}) = RPT{x=x+wid,y=y+ht}
    fun rrect (Geometry.RECT{x,y,wid,ht}) = 
          RRECT {x = real x, y = real y, wid = real wid, ht = real ht}
    fun rect (RRECT{x,y,wid,ht}) = 
          Geometry.RECT {x = truncate x, y = truncate y, wid = truncate wid, ht = truncate ht}
    fun boundBox [] = RRECT{x=0.0, y=0.0, wid=0.0, ht=0.0}
      | boundBox ((RPT{x, y}) :: pts) = let
          fun min (a : real, b) = if a < b then a else b
          fun max (a : real, b) = if a > b then a else b
          fun bb (minx, miny, maxx, maxy, []) =
               RRECT{x = minx, y = miny, wid = maxx-minx+1.0, ht = maxy-miny+1.0}
            | bb (minx, miny, maxx, maxy, (RPT{x, y}) :: pts) =
               bb (min(minx, x), min(miny, y), max(maxx, x), max(maxy, y), pts)          in
            bb (x, y, x, y, pts)
          end
    fun intersect (RRECT{x=x1, y=y1, wid=w1, ht=h1},
            RRECT{x=x2, y=y2, wid=w2, ht=h2}) =
          ((x1 < (x2+w2)) andalso (y1 < (y2+h2))
          andalso (x2 < (x1+w1)) andalso (y2 < (y1+h1)))

  end
