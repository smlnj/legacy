(* test the vtty widget *)

structure TstVtty :
  sig
    val doit' : string list * string -> unit
    val doit : unit -> unit
    val main : string list * 'a -> unit
  end = 
  struct
    structure W = Widget
    structure A = Attrs

    val resources = [
        "*background: forestgreen"
      ]
  
    fun tester root = let
          fun quit () = (W.delRoot root; RunCML.shutdown())
          val style = W.styleFromStrings (root, resources)
          val name = Styles.mkView {name = Styles.styleName [],
                                    aliases = [Styles.styleName []]}
          val view = (name,style)
  
          val vtty = Vtty.mkVtty root {rows = 24, cols = 80}
          val (ins, outs) = Vtty.openVtty vtty
          val shellArgs = [(A.attr_title, A.AV_Str "test"),
                           (A.attr_iconName, A.AV_Str "test")]
          val shell = Shell.shell (root,view,shellArgs) (Vtty.widgetOf vtty)
  
  	  fun catFile fname = let
  	        val inf = CIO.open_in fname
                fun outF () = 
                     case (CIO.inputc inf 1024) of 
                       "" => ()
  		     | s => (CIO.output(outs,s); outF ())
  	        in
  		  outF ();
  		  CIO.close_in inf
  	        end handle (CIO.Io msg) => CIO.output (outs, (msg^"\n"))

  	  fun cat [] = CIO.output (outs,"cat: missing file name\n")
  	    | cat files = app catFile files
  	  fun loop () = let
  	        val _ = (CIO.output (outs,"> "); CIO.flush_out outs)
  	        val line = CIO.input_line ins
  	        fun doCmd ("cat"::t) = cat t
  		  | doCmd ("quit"::_) = quit ()
  		  | doCmd ("help"::_) = 
  		      CIO.output (outs,"Commands: cat <files>,quit,help\n")
  		  | doCmd (s::_) = 
  		      CIO.output (outs,"Unknown command: " ^ s ^ "\n")
  		  | doCmd [] = ()
  	        in
  		  doCmd (StringUtil.tokenize " \t\n" (line,0));
  		  loop ()
  	        end
  	  in
  	    Shell.init shell;
  	    loop ()
  	  end
  
    fun doit' (debugFlags, server) = (
          XDebug.init debugFlags;
          RunEXene.runWArgs tester {dpy= SOME server,timeq=NONE}
        )
  
    fun doit () = RunEXene.run tester
  
    fun main (prog::server::_,_) = doit'([], server)
      | main _ = doit ()
  
  end (* TstVtty *)
