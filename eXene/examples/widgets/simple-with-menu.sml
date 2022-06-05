(* simple-with-menu.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Test the simple menu package.
 *)
structure SimpleWithMenu :
  sig
    val doit' : string list * string -> unit
    val doit : unit -> unit
    val main : string list * 'a -> unit
  end = 
  struct

    structure W = Widget
    structure A = Attrs
    structure SM = SimpleMenu

    val resources = [
        "*background: forestgreen"
      ]

    val menu1 = SM.MENU[
  	  SM.MenuItem("item-1", 1),
  	  SM.MenuItem("item-2", 2),
  	  SM.MenuItem("item-3", 3),
  	  SM.Submenu("submenu1", SM.MENU[
  	      SM.MenuItem("item-4", 4),
  	      SM.MenuItem("item-5", 5),
  	      SM.MenuItem("item-6", 6)		    
  	    ]),
  	  SM.MenuItem("item-7", 7)
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
  	  val (widget, evt) = SM.attachMenu (Box.widgetOf layout, 
                                   [Interact.MButton 3], menu1)
  	  fun monitor () = let
  	        val n = CML.sync evt
  	        in
  		  TextIO.output (TextIO.stdOut, "menu choice "^ Int.toString n ^ "\n");
  		  monitor ()
  	        end
          val shell = Shell.shell (root,view,[]) widget
          fun loop () =
                if (TextIO.inputLine TextIO.stdIn) = "quit\n"
                  then quit ()
                  else loop ()
          in
  	    CML.spawn monitor;
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
  
  
  end (* SimpleWithMenu *)
