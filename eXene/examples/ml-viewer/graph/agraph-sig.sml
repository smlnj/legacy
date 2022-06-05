(* attr-graph-sig.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

signature ATTR_GRAPH =
  sig
    type graph
    type node
    type edge

    type graph_info
    type node_info
    type edge_info

    exception Graph of string

    datatype graph_obj =
        GRAPH of graph
      | EDGE of edge
      | NODE of node
      | PROTONODE of graph
      | PROTOEDGE of graph

    val mkGraph : {name : string, 
                   info : graph_info option, 
                   graph_info : unit -> graph_info,
                   node_info : unit -> node_info,
                   edge_info : unit -> edge_info} -> graph

    val graphName : graph -> string
    val nodeName : node -> string
    val numNodes : graph -> int
    val numEdges : graph -> int

    val hasNode : (graph * node) -> bool
    val hasEdge : (graph * edge) -> bool
    val delNode : (graph * node) -> unit
    val delEdge : (graph * edge) -> unit

    val addNode : (graph * string * node_info option) -> node
    val getNode : (graph * string * node_info option) -> node
    val findNode : (graph * string) -> node option
    val nodes : graph -> node list
    val appNodes : (node -> unit) -> graph -> unit
    val foldNodes : (node * 'a -> 'a) -> graph -> 'a -> 'a

    val addEdge : {graph : graph, 
                   hd : node, 
                   tl : node, 
                   info : edge_info option} -> edge
    val edges : graph -> edge list
    val inEdges : (graph * node) -> edge list
    val outEdges : (graph * node) -> edge list
    val appInEdges : (edge -> unit) -> (graph * node) -> unit
    val appOutEdges : (edge -> unit) -> (graph * node) -> unit
    val head : edge -> node
    val tail : edge -> node
    val nodesOf : edge -> {hd : node, tl : node}
    
    val addSubgraph : (graph * string * graph_info option) -> graph
    val findSubgraph : (graph * string) -> graph option

    val getAttr : graph_obj -> string -> string option
    val setAttr : graph_obj -> (string * string) -> unit
    val delAttr : graph_obj -> string -> unit
    val appAttr : graph_obj -> ((string * string) -> 'a) -> unit
    val cntAttr : graph_obj -> int

    val infoOfEdge : edge -> edge_info
    val infoOfGraph : graph -> graph_info
    val infoOfNode : node -> node_info

    val eqGraph : graph * graph -> bool
    val eqNode : node * node -> bool
    val eqEdge : edge * edge -> bool

  end (* ATTR_GRAPH *)

