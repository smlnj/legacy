(* x86-ccall.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)
local
  (* turn on "fast-fp"... *)
  val _ = MLRiscControl.flag "x86-fast-fp" := true
  (* The following is a GROSS HACK!
   *
   * Eventually we need to generate separate binaries for the
   * IntelMac platform.  This code figures out dynamically
   * whether it is running Darwin (i.e., Mac OS X on an Intel),
   * but this does not work correctly when cross-compiling.
   * In particular, once the compiler or any of the libraries
   * that get compiled by the cross-compiler starts using NLFFI,
   * then things will start to break.
   *
   * Also, the cross-compiler will not set the ABI_Darwin symbol
   * correctly for CM's conditional compilation mechanism, so the
   * compiler sources cannot rely on it!
   *)
  val (rssir, av) = (case SMLofNJ.SysInfo.getOSName ()
         of "Darwin" => (true, SOME "Darwin")
	  | _ => (false, NONE)
        (* end case *))
in

structure X86CCallBackend = BackendFn (
    structure M = X86MC (
        structure CCallParams = struct
            val frameAlign = 16
            val returnSmallStructsInRegs = rssir
          end
        val abi_variant = av)
    val cproto_conv = "ccall")

end (* local *)
