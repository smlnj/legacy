(* backend/sparc.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure SparcBackend = BackendFn (structure M = SparcMC
				    val cproto_conv = "ccall")
