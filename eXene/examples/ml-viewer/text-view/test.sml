(* test.sml
 *
 * This is the test driver for the ML source code viewer.
 *)

(**
val canvasTM = TraceCML.moduleOf("/eXene/widgets/TextCanvas");
val textTM = TraceCML.moduleOf("/eXene/widgets/TextDisplay");
val viewerTM = VDebug.tm;
**)

local
  structure EXB = EXeneBase
  structure V = Viewer
in
fun go dpy = let
      val root = Widget.mkRoot dpy
      fun openView fname =
	    MLViewer.openViewer root {file=fname, module="Foo", loc=0, range=NONE}
      fun quit () = (Widget.delRoot root; RunCML.shutdown())
      fun strip "" = ""
	| strip s = substring(s, 0, size s - 1)
      fun loop () = (case strip(CIO.input_line CIO.std_in)
	     of "" => quit()
	      | "quit" => quit()
	      | s => (openView s; loop())
	    (* end case *))
      in
	loop()
      end;
end; (* local *)

fun doit dpy = RunCML.doit (fn () => go dpy, SOME 10);

