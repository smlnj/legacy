signature MODGRAPH =
  sig
    include ATTR_GRAPH 

    val readGraph : string -> graph
  end
