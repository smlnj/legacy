(* main.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This is a test diriver for the calculator.
 *)

structure CalcTest =
  struct
  local
    structure W = Widget
    structure A = Attrs

    val resources = [
        "*background: gray"
      ]
  
    fun tester root = let
	  fun quit () = (W.delRoot root; RunCML.shutdown OS.Process.success)
          val style = W.styleFromStrings (root, resources)
          val name = Styles.mkView {name = Styles.styleName [],
                                    aliases = [Styles.styleName []]}
          val view = (name,style)
	  val calc = Calc.mkCalc (root,view,[])
          val shellArgs = [(A.attr_title, A.AV_Str "calc"),
                           (A.attr_iconName, A.AV_Str "calc")]
	  val shell = Shell.shell (root,view,shellArgs) calc
	  in
	    Shell.init shell
	  end
  in

    fun doit' (debugFlags, server) = (
          XDebug.init debugFlags;
          RunEXene.runWArgs tester {dpy= SOME server,timeq=NONE}
        )
  
    fun doit () = RunEXene.run tester
  
    fun main (prog,server::_) = doit'([], server)
      | main _ = doit ()
  
  end (* local *)
  end (* structure CalcTest *)
