(* amd64-stdcall.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure AMD64StdCallBackend = BackendFn (
    structure M = AMD64MC (
	structure CCallParams = struct
	    val frameAlign = 16
	    val returnSmallStructsInRegs = false
	  end
	val abi_variant = NONE)
    val cproto_conv = "stdcall")
