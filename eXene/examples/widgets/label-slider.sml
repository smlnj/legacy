(* label-slider.sml
 *
 * COPYRIGHT (c) 1991,1995 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)
structure LabelSlider :
  sig
    val doit' : string list * string -> unit
    val doit : unit -> unit
    val main : string list * 'a -> unit
  end = 
  struct

    structure W = Widget
    structure Sl = Slider
    structure A = Attrs

    val resources = [
        "*relief: raised",
        "*background: forestgreen\n"
      ]

    fun mkLabelSlider (root,view) = let
          val lArgs = [(A.attr_label,A.AV_Str "0"),
                       (A.attr_width, A.AV_Int 4),
                       (A.attr_halign, A.AV_HAlign W.HRight)]
          val label = Label.label (root,view,lArgs)
          val sArgs = [(A.attr_width,A.AV_Int 20),
                       (A.attr_isVertical, A.AV_Bool false)]
          val slider = Sl.slider (root,view,sArgs)
          fun set l = Label.setLabel label (Label.Text l)
          val evt = Sl.evtOf slider
          fun loop () = loop (set (Int.toString (CML.sync evt)))
          in
            CML.spawn loop;
            Box.widgetOf(Box.layout (root,view,[]) (Box.HzCenter [
                (* Glue {nat=20, min=0, max=NONE}, *)
                Box.WBox (Label.widgetOf label), 
                Box.Glue {nat=20, min=20, max=SOME 20},
                Box.WBox (Sl.widgetOf slider)
              ]))
          end
  
    fun tester root = let
          fun quit () = (W.delRoot root; RunCML.shutdown OS.Process.success)
          val style = W.styleFromStrings (root, resources)
          val name = Styles.mkView {name = Styles.styleName [],
                                    aliases = [Styles.styleName []]}
          val view = (name,style)
          val lslider = mkLabelSlider (root,view)
  	  val layout = Box.layout (root,view,[]) (Box.VtCenter [
  		Box.WBox lslider,
  		Box.HzCenter [Box.Glue{nat=300, min=0, max=NONE}]
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
          RunEXene.runWArgs tester {dpy= SOME server,timeq=NONE}
        )
  
    fun doit () = RunEXene.run tester
  
    fun main (prog::server::_,_) = doit'([], server)
      | main _ = doit ()
  
  end (* LabelSlider *)
