
structure Attr : ATTR =
  struct
    structure G = Geometry

    datatype shape = Ellipse | Box | Diamond

    type graph_info = {
        name : string,
        scale : real,
        bbox : G.size
      }
    type node_info = {
        center : G.point,
        size : G.size,
        shape : shape,
        label : string
      } 
    type edge_info = {
        pts : G.point list,
        arrow : G.point
      }

    val dfltGraphInfo = {
        name = "",
        scale = 1.0,
        bbox = G.SIZE{wid=0,ht=0}
      }
    val dfltNodeInfo = {
        center = G.originPt,
        size = G.SIZE{wid=0,ht=0},
        shape = Ellipse,
        label = ""
      } 
    val dfltEdgeInfo = {
        pts = [],
        arrow = G.originPt
      }

  end
