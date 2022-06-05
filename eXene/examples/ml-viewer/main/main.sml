local

  fun viewGraph (fs, root, graph) = let
        val title = "ML-View: "^(ModGraph.graphName graph)
        val newvg = VGraphAux.mkVGraphOn graph
        val view = View.mkView (fs, root) newvg
        val shell = Shell.mkShell (View.widgetOf view, NONE,
            { win_name = SOME title, icon_name = SOME title })
        in
          Shell.init shell
        end

fun h (m, s) = (
      CIO.print (implode["uncaught exception ", m, " \"", s, "\"\n"]);
      RunCML.shutdown ())

  fun grapho (graphfile,server) = let
        val root = Widget.mkRoot server
        val fontserver = ViewFont.mkFontServer root

        val graph = ModGraph.readGraph graphfile
        val viewGraph = viewGraph (fontserver, root, graph)
    
        fun execute (outs, cmds) = let
              fun puts s = CIO.output (outs, s)

              fun exe ["quit"] = RunCML.shutdown ()
                | exe [] = ()
                | exe _ = puts "???\n"
              in
                exe cmds
              end


        val tokenize = StringUtil.tokenize " \t\n"
        fun mainloop () = (
          CIO.output (CIO.std_out, ">> ");
          CIO.flush_out CIO.std_out;
          execute (CIO.std_out, tokenize (CIO.input_line CIO.std_in,0));
          mainloop ()
        )
        in mainloop () end
          handle (ModGraph.Graph s) => h ("ModGraph.Graph", s)
               | (VGraph.Graph s) => h ("VGraph.Graph", s)
               | (EXeneBase.BadAddr s) => h ("EXeneBase.BadAddr", s)
               | e => RunCML.shutdown ()

  fun standalone dotFile _ = let
	val root = Widget.mkRoot ""
	val fontserver = ViewFont.mkFontServer root
	val graph = ModGraph.readGraph dotFile
	in
	  viewGraph (fontserver, root, graph);
	  ()
	end (* standalone *)
	  handle (ModGraph.Graph s) => h ("ModGraph.Graph", s)
               | (VGraph.Graph s) => h ("VGraph.Graph", s)
               | (EXeneBase.BadAddr s) => h ("EXeneBase.BadAddr", s)
               | e => RunCML.shutdown ()
in

  fun doit (graphfile,server) =
	RunCML.doit (fn () => grapho (graphfile,server), SOME 20)

  fun demo server = (
	System.Directory.cd "../data";
	doit ("nodes.dot", server))

  fun export name = RunCML.exportFn(name, standalone "nodes.dot", SOME 20)

end (* local *)

