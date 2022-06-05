(* backend/alpha32.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure Alpha32Backend = BackendFn (structure M = Alpha32MC
				      val cproto_conv = "unimplemented")
