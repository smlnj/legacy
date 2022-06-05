(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure PPCUnixCMB : CMB =
    BootstrapCompileFn (structure Backend = PPCBackend
			val useStream = Backend.Interact.useStream
			val os = SMLofNJ.SysInfo.UNIX
			val load_plugin = CM0.load_plugin)
