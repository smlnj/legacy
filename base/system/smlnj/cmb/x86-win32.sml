(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure X86Win32CMB : CMB =
    BootstrapCompileFn (structure Backend = X86StdCallBackend
			val useStream = Backend.Interact.useStream
			val os = SMLofNJ.SysInfo.WIN32
			val load_plugin = CM0.load_plugin)
