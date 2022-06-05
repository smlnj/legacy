(* graphio.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * I/O of graphs using the "dot" syntax.
 *
 * NOTE: the mk*Info functions should take a "string -> string" dictionary
 * and build the node info from that, but this will require wholesale changes.
 *)

functor GraphIO (
    structure IO : IO
    structure G : ATTR_GRAPH 
  (* functions to make the default graph info *)
    val mkGraphInfo : unit -> G.graph_info
    val mkNodeInfo : unit -> G.node_info
    val mkEdgeInfo : unit -> G.edge_info
  ) : GRAPH_IO = struct

    structure IO = IO
    structure G = G

    structure GraphLrVals = 
       GraphLrValsFun (structure Token = LrParser.Token
                       structure G = G
                       val graph_info = mkGraphInfo
                       val node_info = mkNodeInfo
                       val edge_info = mkEdgeInfo)
    structure GraphLex = GraphLexFun (structure Tokens = GraphLrVals.Tokens)
    structure GraphParser = 
                JoinWithArg(structure ParserData = GraphLrVals.ParserData
                     structure Lex = GraphLex
                     structure LrParser = LrParser)

    fun readGraph ins = let
          fun complain msg =
                IO.output(IO.std_err, String.concat["lexer: ",msg,"\n"])
          val lexstate : GraphLex.UserDeclarations.lexstate = {
                lineNum = ref 1,
                stringstart = ref 0,
                commentState = ref NONE,
                charlist = ref [],
                complain = complain
              }
          val lexer = GraphParser.makeLexer (IO.inputc ins) lexstate
          val lookahead = 30
          fun errfn(msg,_,_) = 
            output(std_err,"Error (line "^(makestring (!(#lineNum lexstate)))^": "^msg^")\n")
          in
            case #1(GraphParser.parse (lookahead,lexer,errfn,())) of
              SOME g => g
            | NONE => (errfn("Empty graph",1,1); raise G.Graph "Empty graph")
          end

    local
      fun recognize s = let
            val sz = size s
            val cl = explode s
            in
              fn (s,i) => let
                  fun mk(i,[]) = true
                    | mk(i,c::rest) = String.sub(s,i) = c andalso mk(i+1,rest)
                  in
                    sz = (size s)-i andalso mk (i,cl)
                  end
            end
      val recEdge = recognize "dge"
      val recNode = recognize "ode"
      val recStrict = recognize "rict"
      val recDigraph = recognize "igraph"
      val recGraph = recognize "raph"
      val recSubgraph = recognize "bgraph"
      val minlen = 4
    in
    fun isKeyword s = if size s < minlen
	  then false
          else (case String.sub(s, 0)
	     of #"d" => recDigraph(s,1)
	      | #"e" => recEdge(s,1)
	      | #"g" => recGraph(s,1)
	      | #"n" => recNode(s,1)
	      | #"s" => (case String.sub(s,1)
		   of #"t" => recStrict(s,2)
		    | #"u" => recSubgraph(s,2)
		    | _ => false
		  (* end case *))
	      | _ => false
	    (* end case *))
    end

    fun canon "" = "\"\""
      | canon str = let
          open CType
          val maybe_num = let
		val c = String.sub(str, 0)
		in isDigitChr c orelse (c = #".") end
          fun run ([], l, special) = (#"\""::l, special)
            | run (#"\""::rest, l, special) = run(rest, #"\"" :: #"\\"::l, true)
            | run (c::rest, l, special) =
                if not (isAlphaNumChr c) andalso (c <> #"_")
                  then run(rest, c::l, true)
                  else if maybe_num andalso not(isDigitChr c) andalso (c <> #".")
                    then run(rest, c::l, true)
                    else run(rest, c::l, special)
          in
            case run (explode str, [#"\""], false)
	     of (cl, true) => implode(rev cl)
              | (cl, false) => if isKeyword str then implode(rev cl) else str
	    (* end case *)
          end

    fun mkAttr(n,v) = concat[n," = ",canon v]
    val attrList = ListFormat.formatList{init=" [",sep=", ",final="]",fmt=mkAttr}

    fun writeGraph (outs, graph) = let
          val puts = app (IO.outputc outs)
          fun tab () = puts ["  "]
          fun nl () = puts [";\n"]
          val (graphType, edgeOp) = 
                case G.getAttr (G.GRAPH graph) "graph_type" of
                  NONE => ("digraph"," -> ")
                | SOME gt => (
                    G.delAttr (G.GRAPH graph) "graph_type";
                    case gt of
                      "g" => ("graph"," -- ")
                    | "sg" => ("strict graph"," -- ")
                    | "dg" => ("digraph"," -> ")
                    | "sdg" => ("strict digraph"," -> ")
                    | _ => ("digraph"," -> ")
                    )
          val getProtoNode = G.getAttr (G.PROTONODE graph)
          val getProtoEdge = G.getAttr (G.PROTOEDGE graph)
          fun getDiffAttr (obj, lookup) = let
                val l : (string * string) list ref = ref []
                fun chk (nv as (n,v)) =
                      case lookup n of
                        NONE => l := nv::(!l)
                      | SOME v' => if v' = v then () else l := nv::(!l)
	              (* end case *)
                in
                  if G.cntAttr obj = 0 then []
                  else (G.appAttr obj chk; !l)
                end
          fun getAndDel (obj,name) =
                case G.getAttr obj name of
                  NONE => ""
                | SOME v => (G.delAttr obj name; v)
          fun writeAttrs [] = ()
            | writeAttrs al = puts[attrList al]
          fun writeEdge e = let
                val {hd,tl} = G.nodesOf e
                val tp = getAndDel (G.EDGE e,"tailport")
                val hp = getAndDel (G.EDGE e,"headport")
                in
                  tab();
                  puts [canon(G.nodeName tl),tp,edgeOp,canon(G.nodeName hd),hp];
                  writeAttrs(getDiffAttr (G.EDGE e, getProtoEdge));
                  nl()
                end
          fun writeNode n = (
                tab();
                puts [canon(G.nodeName n)];
                writeAttrs(getDiffAttr (G.NODE n, getProtoNode));
                nl()
              )
          fun writeDict (label, obj) =
                if G.cntAttr obj = 0 then ()
                else (
                  tab();
                  puts [label];
                  writeAttrs(getDiffAttr (obj, fn _ => NONE));
                  nl()
                )
          in
            puts [graphType, " ",canon(G.graphName graph),"{\n"];
            writeDict ("graph", G.GRAPH graph);
            writeDict ("node", G.PROTONODE graph);
            writeDict ("edge", G.PROTOEDGE graph);
            G.appNodes writeNode graph;
            G.appNodes (fn n => app writeEdge (rev(G.outEdges (graph,n)))) graph;
            puts ["}\n"]
          end

  end (* functor GraphIO *)
