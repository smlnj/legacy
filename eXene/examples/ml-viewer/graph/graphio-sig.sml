(* graph-io.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * I/O of graphs using the "dot" syntax.
 *)

signature GRAPH_IO =
  sig
    structure G : ATTR_GRAPH
    structure IO : IO

    val readGraph : IO.instream -> G.graph
    val writeGraph : (IO.outstream * G.graph) -> unit

  end
