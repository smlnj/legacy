structure VAttr = 
  struct

    type graph_info = {
      graph : ModGraph.graph,
      bbox : Geometry.rect,
      fontsize : int
    }
      
    type node_info = {
      pos : Geometry.point,
      shape : Attr.shape,
      bbox : Geometry.rect,
      base : ModGraph.node,
      label : string
    }

    type edge_info = {
      pts : Geometry.point list,         (* Spline control points *)
      arrows : Geometry.point list       (* Arrow head *)
    }

end (* VAttr *)
