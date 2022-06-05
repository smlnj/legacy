local
  fun main dpy = let
	val root = Widget.mkRoot dpy
	val tm = CMLTraceMenu.mkTraceMenu root ["/"]
	val shell = Shell.mkShell (CMLTraceMenu.widgetOf tm, NONE, {
		win_name = SOME "TraceMenu", icon_name = SOME "TM"
	      })
	in
	  Shell.init shell;
	  CIO.input_line CIO.std_in;
	  Shell.destroy shell;
	  RunCML.shutdown()
	end
in
  fun doit' (flags, dpy) = RunCML.doit (
	fn () => (XDebug.init flags; main dpy),
	SOME 10)
  fun doit dpy = doit'([], dpy)
end;
