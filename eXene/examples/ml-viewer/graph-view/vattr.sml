structure VAttr = 
  struct

    type graph_info = {
      graph : ModGraph.graph,
      bbox : RGeometry.rrect,
      fontsize : int
    }
      
    type node_info = {
      pos : RGeometry.rpoint,
      shape : Attr.shape,
      bbox : RGeometry.rrect,
      base : ModGraph.node,
      label : string
    }

    type edge_info = {
      bbox : RGeometry.rrect,              (* Bounding box of spline *)
      pts : RGeometry.rpoint list,         (* Spline control points *)
      arrows : RGeometry.rpoint list       (* Arrow head *)
    }

end (* VAttr *)
