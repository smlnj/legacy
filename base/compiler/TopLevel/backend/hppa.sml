(* backend/hppa.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure HppaBackend = BackendFn (structure M = HppaMC
				   val cproto_conv = "unimplemented")
