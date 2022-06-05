structure VGraph : ATTR_GRAPH =
  struct
    structure Attr = VAttr
    structure AG = AttrGraph (type graph_info = Attr.graph_info
                              and  edge_info = Attr.edge_info
                              and  node_info = Attr.node_info)

    open AG 
  end

