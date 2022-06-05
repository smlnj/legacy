(* backend/ppc.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure PPCBackend = BackendFn (structure M = PPCMC
				  val cproto_conv = "ccall")
