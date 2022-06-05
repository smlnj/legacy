structure ModGraph : MODGRAPH =
  struct
    structure Attr = Attr
    structure G = Geometry
    structure AG = AttrGraph (type graph_info = Attr.graph_info ref
                              and  edge_info = Attr.edge_info ref
                              and  node_info = Attr.node_info ref)
    structure GIO = GraphIO (structure G = AG
                             val graph_info = fn () => ref Attr.dfltGraphInfo
                             val node_info = fn () => ref Attr.dfltNodeInfo
                             val edge_info = fn () => ref Attr.dfltEdgeInfo)

    open AG 
    open Format

    val scanBBox = scan "%d,%d,%d,%d"
    val scanReal = StringCvt.atof
    val scanSize = scan "%f,%f"
    val scanPt = scani "%d,%d"
    val scanArrow = scani "e,%d,%d"

    val offset = 18 
    fun shift (G.PT{x,y}) = G.PT{x=x+offset,y=y+offset}

    fun parseOpt pfn (NONE,dflt) = pfn dflt
      | parseOpt pfn (SOME s,_) = pfn s

    fun setAttrs g = let
          fun min(r : real, r') = if r <= r' then r else r'
          fun update (r,v) = r := v
          fun inch2ps r = truncate(72.0*r)
          fun ps2inch i = (real i)/72.0
          fun parseLabel n "\\N" = n
            | parseLabel n s = s
          fun parsePoint (s,i) = let
                val ([INT x, INT y],i') = scanPt (s,i)
                in (G.PT{x=x,y=y},i') end
          fun parseEdge NONE = raise Graph "setAttrs: no points on edge"
            | parseEdge (SOME e) = let
                val ([INT x, INT y],i) = scanArrow (e,0)
                fun rdPts (l,i) =
                      (case scanPt(e,i) of
                        ([INT x, INT y],i') => rdPts(G.PT{x=x,y=y}::l,i'))
                          handle _ => rev l
                in
                  (G.PT{x=x,y=y}, rdPts([],i))
                end 
          fun parseShape "ellipse" = Attr.Ellipse
            | parseShape "diamond" = Attr.Diamond
            | parseShape _ = Attr.Box
          fun parseBBox NONE = raise Graph "setAttrs: no bounding box"
            | parseBBox (SOME r) = let
                val [_,_,INT w, INT h] = scanBBox r
                in
                  G.SIZE{wid=w+2*offset, ht=h+2*offset}
                end
          fun parseSize (SOME w,SOME h) =
                G.SIZE{wid = inch2ps(scanReal w), ht = inch2ps(scanReal h)}
            | parseSize _ = raise Graph "setAttrs: no node width/height"
          fun parseScale (NONE,_) = 1.0
            | parseScale (SOME s, G.SIZE{wid,ht}) = let
                val [REAL rw, REAL rh] = scanSize s
                val rwid = ps2inch wid
                val rht = ps2inch ht
                in
                  if rwid <= rw andalso rht <= rh then 1.0
                  else min(rw/rwid,rh/rht)
                end
          fun setGraph g = let
                val get = getAttr (GRAPH g)
                val bbox = parseBBox(get "bb")
                val info = {
                      name = graphName g,
                      bbox = bbox,
                      scale = parseScale (get "size", bbox)
                    }
                in
                  update(infoOfGraph g, info)
                end
          fun setNode n = let
                val get = getAttr (NODE n)
                val name = nodeName n
                val info = {
                      center = shift(#1(parseOpt (fn s => parsePoint(s,0)) (get "pos","0,0"))),
                      size = parseSize(get "width", get "height"),
                      shape = parseOpt parseShape (get "shape", "box"),
                      label = parseOpt (parseLabel name) (get "label", name)
                    }
                in
                  update(infoOfNode n, info)
                end
          fun setEdge e = let
                val get = getAttr (EDGE e)
                val (arrow, pts) = parseEdge(get "pos")
                val info = {
                      pts = map shift pts,
                      arrow = shift arrow
                    }
                in
                  update(infoOfEdge e, info)
                end
          in
            setGraph g;
            appNodes setNode g;
            appNodes (fn n => appOutEdges setEdge (g,n)) g
          end

    fun readGraph name = let
          val ins = open_in name
          val g = (GIO.readGraph ins)
                  handle (e as AG.Graph msg) =>
                   (output(std_err,"Exn Graph "^msg^"\n"); raise e)
                  | e => 
                   (output(std_err,"Exn "^(System.exn_name e)^"\n"); raise e)
          in
            close_in ins;
            setAttrs g;
            g
          end handle (e as Io msg) =>
                   (output(std_err,"ModGraph.readGraph: "^msg^"\n"); raise e)
  end
