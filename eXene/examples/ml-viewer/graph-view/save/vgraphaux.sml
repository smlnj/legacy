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
    open VGraph Geometry
  
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
      val ARROWL = 10.0
      val ARROWW = 7.0
      val ARROWW_2 = ARROWW / 2.0
      val PIo2   =  1.5707963267948965580E0
      val PI     =  3.1415926535897931160E0
  
      fun atan2(y,0.0) = if y = 0.0 then 0.0 else if y > 0.0 then PIo2 else ~PIo2
        | atan2(y,x) = if x > 0.0 then arctan(y/x)
                       else if y >= 0.0 then arctan(y/x) + PI
                       else arctan(y/x) - PI
  
    in
    fun mkArrow(PT{x=x',y=y'},p as PT{x,y}) = let
          val delx = real(x'-x)
          val dely = real(y'-y)
          val theta = atan2(dely, delx)
          val costh = cos theta
          val sinth = sin theta
          val sp = PT{
                     x = x + truncate(ARROWL*costh + (ARROWW_2)*sinth),
                     y = y + truncate(ARROWL*sinth - (ARROWW_2)*costh)
                   }
          val ep = PT{
                     x = x + truncate(ARROWL*costh - (ARROWW_2)*sinth),
                     y = y + truncate(ARROWL*sinth + (ARROWW_2)*costh)
                   }
          in
            [sp,p,ep]
          end
    end
  
    fun mkViewNode (ppf,prf,G,VG) n = let
          val bindVNode = bindViewNode (ppf,prf,VG)
          val tnode = bindVNode n
          fun last [a] = a
            | last (_::t) = last t
            | last [] = raise LibBase.Impossible "VGraphAux.mkViewNode"
  
          fun mkEdge e = let
                val hnode = bindVNode (Graph.head e)
                val {pts,arrow} = !(Graph.infoOfEdge e)
                val info = {
                      pts = map ppf (pts@[arrow]),
                      arrows = map ppf (mkArrow(last pts, arrow))
                    }
                in
                  addEdge {graph=VG,tl=tnode, hd=hnode, info=SOME info}
                end
          in
            Graph.appOutEdges mkEdge (G, n)
          end
  
    fun mkVGraphOn G = let
          val {bbox=SIZE{wid=gwid,ht=ght},scale,...} = !(Graph.infoOfGraph G)
          fun sc x = truncate(scale*(real x))
          fun projPt (PT{x,y}) = PT{x=sc x,y=sc(ght-y)}
          fun projRt (RECT{x,y,wid,ht}) =
                RECT{x = sc x, y = sc(ght-(y+ht)), wid = sc wid, ht = sc ht}
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
                bbox = RECT{x=0,y=0,wid=sc gwid,ht=sc ght},
                fontsize = sc ViewFont.dfltFontSz
              }
          val dfltNodeInfo = {
                pos = Geometry.originPt,
                bbox = Geometry.RECT{x=0,y=0,wid=0,ht=0},
                shape = Attr.Box,
                base = pickNode G,
                label = ""
              } 
          val dfltEdgeInfo = {
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
          Graph.appNodes (mkViewNode (projPt,projRt,G,VG)) G;
          VG
        end
  
  end (* VGraphAux *)
