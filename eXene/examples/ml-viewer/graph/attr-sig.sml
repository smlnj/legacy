signature ATTR =
  sig
    datatype shape = Ellipse | Box | Diamond

    type graph_info
    type node_info
    type edge_info

    val dfltGraphInfo : graph_info
    val dfltNodeInfo : node_info
    val dfltEdgeInfo : edge_info

  end
