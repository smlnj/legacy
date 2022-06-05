(* attr-graph.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

functor AttrGraph (type graph_info and edge_info and node_info) : ATTR_GRAPH =
  struct
   
    structure StrKey =
      struct
        type ord_key = string
        val cmpKey = StringUtil.strcmp
      end
    structure Dict = SplayDict(StrKey)

    datatype kind = A_GRAPH of bool |  A_DIGRAPH of bool

    fun optInfo (SOME i, _) = i
      | optInfo (NONE, f) = f()

    type 'a dict = 'a Dict.dict ref
    fun delete (d,k) = (d := #1(Dict.remove(!d,k))) handle _ => ()
    fun peek (d,k) = Dict.peek(!d,k)
    fun insert (d,k,v) = d := Dict.insert(!d,k,v)
    fun rmNodeName (d,n) = d := #1(Dict.remove(!d,n))

    type user_node_info = node_info
    type user_edge_info = edge_info
    type user_graph_info = graph_info
    type node_info = {
           name : string, 
           attrs : string dict, 
           info : user_node_info
         }
    type edge_info = {
           attrs : string dict, 
           info : user_edge_info
         }
    type graph_info = {
           info : user_graph_info,
           kind : kind,
           name : string,
           dflt_node_attrs : string dict,
           dflt_edge_attrs : string dict,
           attrs : string dict
         } 

    structure G = Graph(type edge_info = edge_info and node_info = node_info and
      graph_info = graph_info) 

    open G

    type node = node
    type edge = edge
    datatype graph = G of {graph : G.graph, univ : univ_info}
    and univ_info = INFO of {
           graph_info : unit -> user_graph_info,
           edge_info : unit -> user_edge_info,
           node_info : unit -> user_node_info,
           nodes : node Dict.dict ref,
           graphs : graph Dict.dict ref
         }

    datatype graph_obj =
        GRAPH of graph
      | EDGE of edge
      | NODE of node
      | PROTONODE of graph
      | PROTOEDGE of graph

    fun nodeName n = #name(G.infoOfNode n)
    fun graphName (G{graph,...}) = #name(G.infoOfGraph graph)
    fun numNodes (G{graph,...}) = G.numNodes graph
    fun numEdges (G{graph,...}) = G.numEdges graph

    fun mkGraph {name, info, graph_info, edge_info, node_info} = let
          val univ = INFO {
                graph_info = graph_info,
                edge_info = edge_info,
                node_info = node_info,
                nodes = ref (Dict.mkDict ()),
                graphs = ref (Dict.mkDict ())
              }
          val info = {
                name = name, 
                info = optInfo(info,graph_info), 
                kind = A_GRAPH false, 
                attrs = ref (Dict.mkDict ()),
                dflt_node_attrs = ref (Dict.mkDict ()),
                dflt_edge_attrs = ref (Dict.mkDict ())
              }
          val g = G{graph = G.mkGraph info, univ = univ}
          val INFO {graphs, ...} = univ
          in
            insert(graphs, name, g);
            g
          end

    fun findSubgraph (G{univ=INFO{graphs,...},...}, name) = peek(graphs, name)

    fun addSubgraph (g as G{graph,univ}, name, opt_info) =
          case findSubgraph (g, name) of
            NONE => let
              val info = infoOfGraph graph
              val INFO {graphs, graph_info, ...} = univ
              val info' = {
                    name = name, 
                    info = optInfo (opt_info, graph_info),
                    kind = #kind info, 
                    attrs = ref (!(#attrs info)),
                    dflt_node_attrs = ref (!(#dflt_node_attrs info)),
                    dflt_edge_attrs = ref (!(#dflt_edge_attrs info))
                  }
              val sg = G{graph=G.mkSubgraph (graph, info'), univ=univ}
              in
                insert(graphs, name, sg);
                sg
              end
          | _ => raise Graph "AttrGraph.addSubgraph"

    fun hasNode (G{graph,...}, n) = G.hasNode(graph, n)
    fun delNode (g as G{graph,univ}, node) = (
          G.delNode(graph,node);
          if G.isRoot graph then let
            val INFO{nodes,...} = univ
            in
              rmNodeName(nodes, #name(infoOfNode node))
            end
          else ()
        )

    fun addNode (g as G{graph, univ}, name, opt_info) = let
          val INFO {nodes,node_info, ...} = univ
          val attrs = !(#dflt_node_attrs (infoOfGraph graph))
          val info = {name=name,
                      attrs=ref attrs,
                      info = optInfo(opt_info,node_info)}
          val node = G.addNode(graph,info)
          in
            (* print(implode[graphName g,": addNode ",name,"\n"]); *)
            insert(nodes,name,node);
            node
          end

    fun getNode (arg as (g as G{graph, univ}, name, _)) = let
          val INFO{nodes,...} = univ
          in
            case peek(nodes, name) of
              SOME n => (
                if G.hasNode(graph, n) 
                  then () 
                  else (
                    (* print(implode[graphName g,": insertNode ",name,"\n"]); *)
                    insertNode(graph, n)
                  );
                n
              )
            | NONE => addNode arg
          end

    fun findNode (g as G{graph, univ=INFO{nodes,...}}, name) =
          case peek(nodes, name) of
            NONE => NONE
          | N as (SOME n) => if G.isRoot graph then N
                             else if G.hasNode (graph,n) then N
                             else NONE

    fun hasEdge (G{graph,...}, e) = G.hasEdge(graph, e)
    fun delEdge (G{graph,...}, e) = G.delEdge(graph, e)
    fun addEdge {graph=g as G{graph,univ,...}, info, hd, tl} = let
          val attrs = !(#dflt_edge_attrs(infoOfGraph graph))
          val INFO {edge_info, ...} = univ
          val info = {attrs = ref attrs, info = optInfo(info,edge_info)}
          in
            (* print(implode[graphName g,": adding edge ",nodeName tl," -> ",nodeName hd,"\n"]); *)
            G.addEdge{graph=graph,hd=hd,tl=tl,info=info}
          end
    
    fun nodes (G{graph,...}) = G.nodes graph
    fun foldNodes f (G{graph,...}) a = G.foldNodes f graph a
    fun appNodes f (G{graph,...}) = G.appNodes f graph
    fun edges (G{graph,...}) = G.edges graph
    fun inEdges (G{graph,...},n) = G.inEdges (graph,n)
    fun outEdges (G{graph,...},n) = G.outEdges (graph,n)
    fun appInEdges f (G{graph,...},n) = G.appInEdges f (graph,n)
    fun appOutEdges f (G{graph,...},n) = G.appOutEdges f (graph,n)

    local
      fun look d k = peek(d,k)
      fun del d k = delete(d,k)
      fun ins d (k,v) = insert(d,k,v)
      fun app d f = Dict.app f (!d)
      fun cnt d = Dict.numItems (!d)
      fun doObj f (GRAPH (G{graph,...})) = f (#attrs(infoOfGraph graph))
        | doObj f (EDGE edge) = f (#attrs(infoOfEdge edge))
        | doObj f (NODE node) = f (#attrs(infoOfNode node))
        | doObj f (PROTONODE (G{graph,...})) =
            f (#dflt_node_attrs(infoOfGraph graph))
        | doObj f (PROTOEDGE (G{graph,...})) =
            f (#dflt_edge_attrs(infoOfGraph graph))
    in
    val getAttr = doObj look
    val setAttr = doObj ins
    val delAttr = doObj del
    val appAttr = doObj app
    val cntAttr = doObj cnt
    end

    fun infoOfNode n = #info(G.infoOfNode n)
    fun infoOfEdge e = #info(G.infoOfEdge e)
    fun infoOfGraph (G{graph,...}) = #info(G.infoOfGraph graph)

    type node_info = user_node_info
    type edge_info = user_edge_info
    type graph_info = user_graph_info

    fun eqGraph (G{graph=g,...},G{graph=g',...}) = G.eqGraph (g,g')
  end
