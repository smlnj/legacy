(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure X86UnixCMB : CMB =
    BootstrapCompileFn (structure Backend = X86CCallBackend
			val useStream = Backend.Interact.useStream
			val os = SMLofNJ.SysInfo.UNIX
			val load_plugin = CM0.load_plugin)


