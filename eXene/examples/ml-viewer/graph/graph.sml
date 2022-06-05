(* graph.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

functor Graph (type graph_info and edge_info and node_info) : GRAPH =
  struct

    exception Graph of string

    type graph_info = graph_info
    type edge_info = edge_info
    type node_info = node_info

    structure IntKey =
      struct
        type ord_key = int
        fun cmpKey (i : int,j) =
              if i < j then LibBase.Less
              else if i = j then LibBase.Equal
              else LibBase.Greater
      end
    structure Dict = SplayDict(IntKey)
    fun insert (d,k,v) = d := Dict.insert(!d,k,v)

    type univ_data = { id_nodes : int ref, id_edges : int ref}

    datatype graph = G of {
        root : graph option,
        univ : univ_data,
        subgraphs : graph list ref,
        supgraphs : graph list ref,
        nodes : node Dict.dict ref,
        inedges : edge list Dict.dict ref,
        outedges : edge list Dict.dict ref,
        info : graph_info
      }
    and edge = E of {
        id : int,
        hd : node,
        tl : node,
        info : edge_info
      }
    and node = N of {
        graph : graph,
        id : int,
        info : node_info
      }
    
    fun eqGraph (G{nodes=n,...}, G{nodes=n',...}) = n = n'
    fun eqNode (N{graph=g,id,...}, N{graph=g',id=id',...}) = 
          (id = id') andalso eqGraph(g,g')
    fun eqEdge (E{hd=N{graph=g,...},id,...},E{hd=N{graph=g',...},id=id',...}) = 
          (id = id') andalso eqGraph(g,g')

    fun rootOfNode (N{graph,...}) = graph
    fun rootOfEdge (E{hd=N{graph,...},...}) = graph
    fun rootOfGraph (g as G{root=NONE,...}) = g
      | rootOfGraph (G{root=SOME g,...}) = g
    fun isRoot (G{root=NONE,...}) = true
      | isRoot _ = false

    fun infoOfGraph (G{info,...}) = info
    fun infoOfNode (N{info,...}) = info
    fun infoOfEdge (E{info,...}) = info

    fun mkGraph info = 
          G {
            root = NONE,
            univ = {id_nodes = ref 0, id_edges = ref 0},
            info = info,
            subgraphs = ref [],
            supgraphs = ref [],
            nodes = ref (Dict.mkDict()),
            inedges = ref (Dict.mkDict()),
            outedges = ref (Dict.mkDict())
          }
    fun mkSubgraph (g as G{univ,subgraphs,...}, info) = let
          val subg = G {
                       root = SOME(rootOfGraph g),
                       univ = univ,
                       info = info,
                       subgraphs = ref [],
                       supgraphs = ref [g],
                       nodes = ref (Dict.mkDict()),
                       inedges = ref (Dict.mkDict()),
                       outedges = ref (Dict.mkDict())
                     }
          in
            subgraphs := subg::(!subgraphs);
            subg
          end

    fun numNodes (G{nodes,...}) = Dict.numItems(!nodes)
    fun numEdges (G{inedges,...}) = 
          Dict.revfold (fn (_,l,a) => a+(length l)) (!inedges) 0

    fun insert_node (i as (id, n)) (G{nodes, supgraphs,...}) = 
          (insert(nodes, id, n); app (insert_node i) (!supgraphs))

    fun insertNode (g, n as N{id,graph,...}) = let
          fun ins (g as G{nodes,supgraphs,...}) =
                case Dict.peek(!nodes,id) of 
                  NONE =>
                    (insert(nodes, id, n); app ins (!supgraphs))
                | _ => ()
          in
            if eqGraph(rootOfGraph g, graph) then ins g
            else raise Graph "Graph.insertNode"
          end

    fun addNode (g as G{univ={id_nodes,...},nodes,...}, info) = let
          val id = !id_nodes
          val n = N{graph = rootOfGraph g,info = info,id = id}
          in
            insert_node (id,n) g;
            inc id_nodes;
            n
          end

    fun delNode (g, N{graph,id,...}) = let
          open ListUtil
          fun pred id (E{id=eid,...}) = eid = id
          fun foldout (E{hd=N{id=hid,...},tl=N{id=tid,...},id,...},d) =
                if hid = tid then d
                else Dict.insert(d,hid,removeOne (pred id) (Dict.find (d,hid)))
          fun foldin (E{hd=N{id=hid,...},tl=N{id=tid,...},id,...},d) =
                if hid = tid then d
                else Dict.insert(d,tid,removeOne (pred id) (Dict.find (d,tid)))

          fun rmEdges (el1, el2, foldfn) = let
                val (el1', elist) = Dict.remove(el1,id)
                in
                  (el1',  List.revfold foldfn elist el2)
                end handle Dict.NotFound => (el1, el2)
          fun rmNode (g as G{nodes,inedges,outedges,...}) = let
                val nodes' = #1(Dict.remove(!nodes,id))
                val (oe,ie) = rmEdges (!outedges, !inedges, foldout)
                val (ie,oe) = rmEdges (ie, oe, foldin)
                in
                  inedges := ie;
                  outedges := oe;
                  nodes := nodes';
                  true
                end handle Dict.NotFound => false
          fun recRmNode (g as G{subgraphs,...}) =
                if rmNode g then app recRmNode (!subgraphs) else ()
          in
            if eqGraph(rootOfGraph g,graph) then recRmNode g 
            else raise Graph "Graph.delNode"
          end

    fun nodes (G{nodes,...}) = Dict.revfold (fn (_,n,l) => n::l) (!nodes) []
    fun foldNodes fldf (G{nodes,...}) seed =
          Dict.revfold (fn (_,n,b) => fldf(n,b)) (!nodes) seed
    fun appNodes f (G{nodes,...}) = Dict.app (fn (_,n) => f n) (!nodes)

    fun insertEdge (i as (e,hid,tid)) (G{inedges,outedges,supgraphs,...}) = let
          val il = (Dict.find (!inedges,hid)) handle _ => []
          val ol = (Dict.find (!outedges,tid)) handle _ => []
          in
            inedges := Dict.insert(!inedges,hid,e::il);
            outedges := Dict.insert(!outedges,tid,e::ol);
            app (insertEdge i) (!supgraphs)
          end

    fun addEdge {graph, tl = tl as N{graph=tg,id=tid,...},
                 hd = hd as N{graph=hg,id=hid,...},
                 info} =
          if eqGraph(rootOfGraph graph,hg) andalso eqGraph(hg,tg) then let
              val G{univ={id_edges,...},...} = graph
              val id = !id_edges
              val e = E{info = info,id = id, hd = hd, tl = tl}
              in
                insertEdge (e,hid,tid) graph;
                inc id_edges;
                e
              end
          else raise Graph "Graph.addEdge"

    exception NotFound
    fun delEdge (g, E{hd=N{graph,id=hid,...},tl=N{id=tid,...},id,...}) = let
          fun remove [] = raise NotFound
            | remove ((e as E{id=eid,...})::rest) =
                if eid = id then rest else e::(remove rest)
          fun update (edgeDict, id) = 
                case Dict.peek (!edgeDict,id) of
                  NONE => false
                | SOME l => (
                    edgeDict := Dict.insert(!edgeDict,id,remove l);
                    true
                  ) handle NotFound => false
          fun rmEdge (G{outedges,inedges,...}) =
                update(inedges,hid) andalso update(outedges,tid)
          fun recRmEdge (g as G{subgraphs,...}) =
                if rmEdge g then app recRmEdge (!subgraphs) else ()
          in
            if eqGraph(rootOfGraph g,graph) then recRmEdge g 
            else raise Graph "Graph.delEdge"
          end

    fun inEdges (g as G{inedges,...}, N{graph,id,...}) = 
          if eqGraph(rootOfGraph g,graph) 
            then (Dict.find(!inedges,id)) handle _ => []
            else raise Graph "Graph.inEdges"
    fun outEdges (g as G{outedges,...}, N{graph,id,...}) = 
          if eqGraph(rootOfGraph g,graph) 
            then (Dict.find(!outedges,id)) handle _ => []
            else raise Graph "Graph.outEdges"
    fun appInEdges f (g as G{inedges,...}, N{graph,id,...}) = 
          if eqGraph(rootOfGraph g,graph) 
            then app f ((Dict.find(!inedges,id)) handle _ => [])
            else raise Graph "Graph.appInEdges"
    fun appOutEdges f (g as G{outedges,...}, N{graph,id,...}) = 
          if eqGraph(rootOfGraph g,graph) 
            then app f ((Dict.find(!outedges,id)) handle _ => [])
            else raise Graph "Graph.appOutEdges"
    fun edges g = foldNodes (fn (n,l) => (outEdges (g,n))@l) g []

    fun head (E{hd,...}) = hd
    fun tail (E{tl,...}) = tl
    fun nodesOf (E{tl,hd,...}) = {hd=hd,tl=tl}

    fun hasNode (g as G{nodes,...},N{graph,id,...}) =
          eqGraph(rootOfGraph g,graph) andalso
          case Dict.peek(!nodes,id) of NONE => false | _ => true

    fun hasEdge (g as G{nodes,inedges,...},
                  E{id,hd = N{graph,id=hid,...},tl,...}) = let
          fun pred (E{id=eid,...}) = eid = id
          in
            eqGraph(rootOfGraph g,graph) andalso
            case Dict.peek(!inedges,hid) of 
              NONE => false 
            | SOME el => case ListUtil.findOne pred el of 
                           NONE => false 
                         | _    => true
          end

  end (* Graph *)

