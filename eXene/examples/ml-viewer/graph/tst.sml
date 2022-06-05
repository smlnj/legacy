  structure AG = AttrGraph (type graph_info = unit 
                            and  edge_info = unit
                            and  node_info = unit)
structure Test =
  struct
local
  exception Fail of string
  fun dummy () = ()
  structure GIO = GraphIO (structure IO = IO
                           structure G = AG
                           val mkGraphInfo = dummy
                           val mkNodeInfo = dummy
                           val mkEdgeInfo = dummy)
in
fun rg name = let
      val ins = (open_in name) handle Io msg => raise Fail msg
      val g = 
        (GIO.readGraph ins)
          handle (AG.Graph msg) => (close_in ins;raise Fail ("Graph "^msg))
            | e => (close_in ins; raise Fail ("Exn "^(System.exn_name e)))
      in
        close_in ins;
        g
      end
fun wg (g,name) = let
      val outs = open_out name
      in
        (GIO.writeGraph(outs,g)) handle e => (close_out outs; raise e);
        close_out outs
      end
fun doit fname = (output(std_out,fname^"\n");wg(rg fname, "_"^fname)) 
           handle Fail msg => output(std_err,fname^": "^msg^"\n")
                | e => output(std_err,"Exn "^(System.exn_name e)^"\n")
    
fun main (argv,_) = app doit (tl argv)
end
end
