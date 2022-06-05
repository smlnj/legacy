(* simple.sml
 *
 * COPYRIGHT (c) 1991,1995 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)
structure Simple : 
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

    fun goodbye root = let
          fun quit () = (W.delRoot root; RunCML.shutdown OS.Process.success)
          val style = W.styleFromStrings (root, resources)
          val name = Styles.mkView {name = Styles.styleName [],
                                    aliases = [Styles.styleName []]}
          val view = (name,style)
          val args = [(A.attr_label, A.AV_Str "Goodbye, Cruel World!")]
          val bttn = Button.textCmd (root,view,args) quit
          val layout = Box.layout (root,view,[]) (Box.VtCenter [
                  Box.Glue {nat=30, min=0, max=NONE},
                  Box.WBox (Button.widgetOf bttn),
                  Box.Glue {nat=30, min=0, max=NONE}
                ])
          val shell = Shell.shell (root,view,[]) (Box.widgetOf layout)
          fun loop () =
                if (TextIO.inputLine TextIO.stdIn) = "quit\n"
                  then quit ()
                  else loop ()
          in
            Shell.init shell;
            loop ()
          end
  
    fun doit' (debugFlags, server) = (
          XDebug.init debugFlags;
          RunEXene.runWArgs goodbye {dpy= SOME server,timeq=NONE}
        )
  
    fun doit () = RunEXene.run goodbye
  
    fun main (prog::server::_,_) = doit'([], server)
      | main _ = doit ()
  
  end (* Simple *)
