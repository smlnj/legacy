(* graph-sig.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

signature GRAPH =
  sig

    type graph
    type edge
    type node

    type graph_info
    type edge_info
    type node_info

    exception Graph of string

    val mkGraph : graph_info -> graph
    val mkSubgraph : (graph * graph_info) -> graph
    val numNodes : graph -> int
    val numEdges : graph -> int

    val addNode : graph * node_info -> node
    val insertNode : graph * node -> unit
    val delNode : graph * node -> unit
    val nodes : graph -> node list
    val appNodes : (node -> uni) -> graph -> unit
    val foldNodes : (node * 'a -> 'a) -> graph -> 'a -> 'a

    val addEdge : {graph : graph, hd:node, info:edge_info, tl:node} -> edge
    val delEdge : (graph * edge) -> unit
    val edges : graph -> edge list
    val inEdges : graph * node -> edge list
    val outEdges : graph * node -> edge list
    val appInEdges : (edge -> unit) -> (graph * node) -> unit
    val appOutEdges : (edge -> unit) -> (graph * node) -> unit

    val head : edge -> node
    val tail : edge -> node
    val nodesOf : edge -> {hd : node, tl : node}

    val rootOfNode : node -> graph
    val rootOfEdge : edge -> graph
    val rootOfGraph : graph -> graph
    val isRoot : graph -> bool

    val hasNode : (graph * node) -> bool
    val hasEdge : (graph * edge) -> bool

    val eqGraph : graph * graph -> bool
    val eqNode  : node * node -> bool
    val eqEdge  : edge * edge -> bool

    val infoOfEdge : edge -> edge_info
    val infoOfGraph : graph -> graph_info
    val infoOfNode : node -> node_info

  end (* GRAPH *)

