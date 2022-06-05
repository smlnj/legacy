(* backend/x86.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
local
    (* turn on "fast-fp"... *)
    val _ = MLRiscControl.flag "x86-fast-fp" := true
in
structure X86StdCallBackend =
          BackendFn (structure M = X86MC (structure CCallParams = struct
					    val frameAlign = 4
					    val returnSmallStructsInRegs = false
					  end
					  val abi_variant = NONE)
                     val cproto_conv = "stdcall")
end
