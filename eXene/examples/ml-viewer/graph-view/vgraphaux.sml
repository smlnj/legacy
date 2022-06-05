signature VGRAPHAUX = 
  sig

    structure Graph: MODGRAPH
    structure VGraph: ATTR_GRAPH

    val mkVGraphOn : Graph.graph -> VGraph.graph

  end (* VGRAPHAUX *)

structure VGraphAux : VGRAPHAUX = 
  struct

    structure VGraph = VGraph
    structure Graph = ModGraph
    open VGraph Geometry RGeometry
  
    fun divscale (n,SIZE{wid,ht}) = PT{x=wid div n,y=ht div n}
  
    fun bindViewNode (ppf,prf,VG) n = let
          val name = Graph.nodeName n
          in
            case findNode (VG, name) of
              NONE => let
                val {center,size,label,shape,...} = !(Graph.infoOfNode n)
                val bbox = mkRect(subPt(center,divscale(2,size)),size)
                val attr = {
                    pos = ppf center,
                    bbox = prf bbox,
                    shape = shape,
                    base = n,
                    label = label
                  }
                in
                  addNode (VG, name, SOME attr)
                end
            | SOME nn => nn
          end
  
    local
      val ARROWL = 10
      val ARROWW = 7
      val PIo2   =  1.57079632679489661923
      val PI     =  3.14159265358979323846
  
      fun atan2(y,0.0) = if y = 0.0 then 0.0 else if y > 0.0 then PIo2 else ~PIo2
        | atan2(y,x) = if x > 0.0 then arctan(y/x)
                       else if y >= 0.0 then arctan(y/x) + PI
                       else arctan(y/x) - PI
  
    in
    fun mkArrow (RPT{x=x',y=y'}, p as RPT{x,y},arrowl,arroww) = let
          val delx = x'-x
          val dely = y'-y
          val theta = atan2(dely, delx)
          val costh = cos theta
          val sinth = sin theta
          val sp = RPT{
                     x = x + (arrowl*costh + arroww*sinth),
                     y = y + (arrowl*sinth - arroww*costh)
                   }
          val ep = RPT{
                     x = x + (arrowl*costh - arroww*sinth),
                     y = y + (arrowl*sinth + arroww*costh)
                   }
          in
            [sp,p,ep]
          end
  
    fun mkViewNode (scf,ppf,prf,G,VG) = let
          val bindVNode = bindViewNode (ppf,prf,VG)
          fun last [a] = a
            | last (_::t) = last t
            | last [] = raise LibBase.Impossible "VGraphAux.mkViewNode"
          val arrowl = scf ARROWL
          val arroww = (scf ARROWW) / 2.0
  
          fun mkEdge tnode e = let
                val hnode = bindVNode (Graph.head e)
                val {pts,arrow} = !(Graph.infoOfEdge e)
                val info_pts = map ppf (pts@[arrow])
                val arrows = mkArrow (ppf(last pts), ppf arrow, arrowl, arroww)
                val info = {
                      bbox = RGeometry.boundBox(arrows@info_pts),
                      pts = info_pts,
                      arrows = arrows
                    }
                in
                  addEdge {graph=VG,tl=tnode, hd=hnode, info=SOME info}
                end
          in
            fn n => Graph.appOutEdges (mkEdge( bindVNode n)) (G, n)
          end
    end
  
    fun mkVGraphOn G = let
          val {bbox=SIZE{wid=gwid,ht=ght},scale,...} = !(Graph.infoOfGraph G)
          fun sc x = scale*(real x)
          fun projPt (PT{x,y}) = RPT{x=sc x,y=sc(ght-y)}
          fun projRt (RECT{x,y,wid,ht}) =
                RRECT{x = sc x, y = sc(ght-(y+ht)), wid = sc wid, ht = sc ht}
          fun pickNode G = let
                exception Done
                val nl : Graph.node list ref = ref []
                fun pick n = (nl := [n]; raise Done)
                in
                  (Graph.appNodes pick G) handle Done => ();
                  hd(!nl)
                end
          val info = {
                graph = G,
                bbox = RRECT{x=0.0,y=0.0,wid=sc gwid,ht=sc ght},
                fontsize = truncate(sc ViewFont.dfltFontSz)
              }
          val picknode = pickNode G
          val dfltNodeInfo = {
                pos = RGeometry.roriginPt,
                bbox = RGeometry.RRECT{x=0.0,y=0.0,wid=0.0,ht=0.0},
                shape = Attr.Box,
                base = picknode,
                label = ""
              } 
          val dfltEdgeInfo = {
                bbox = RRECT{x=0.0,y=0.0,wid=sc gwid,ht=sc ght},
                pts = [],
                arrows = []
              }
          val VG = mkGraph {
                     name = Graph.graphName G, 
                     info = SOME info,
                     graph_info = fn () => info,
                     node_info = fn () => dfltNodeInfo,
                     edge_info = fn () => dfltEdgeInfo
                   }
        in
          Graph.appNodes (mkViewNode (sc,projPt,projRt,G,VG)) G;
          VG
        end
  
  end (* VGraphAux *)
