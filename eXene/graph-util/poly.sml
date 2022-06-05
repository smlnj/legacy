    fun polygonRegion (l as [_,_,_], fill_rule) = poly(l,fill_rule)
      | polygonRegion (l as [a,b,c,d], fill_rule) =
          if isRect(a,b,c,d) then mkRect(a,b,c,d) else poly(l,fill_rule)
      | polygonRegion (l as [a,b,c,d,e], fill_rule) = poly(l,fill_rule)
          if a = e andalso isRect(a,b,c,d) then mkRect(a,b,c,d) else poly(l,fill_rule)
      | polygonRegion (l as (_::_::_::_), fill_rule) = poly(l,fill_rule)
      | polygonRegion _ = zeroRegion


    fun isRect(PT{x=ax,y=ay},PT{x=bx,y=by},PT{x=cx,y=cy},PT{x=dx,y=dy}) = 
          ((ay = by andalso bx = cx andalso cy = dy andalso dx = ax) orelse
          (ax = bx andalso by = cy andalso cx = dx andalso dy = ay)) andalso
          let
            val x1 = min(ax, cx)
            val y1 = min(ay, cy)
            val x2 = max(ax, cx)
            val y2 = max(ay, cy)
            in x1 <> x2 andalso y1 <> yw end

