(* invokegc.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is an alternative module for generating GC code.
 * There are a few improvements.
 *
 * All code to invoke GC is generated once at the end of the
 * compilation unit---with one exception. For each cluster, a
 * call to GC is a jump  to the end of the cluster  where there
 * is another jump.
 * Code to invoke GC for known functions is generated at the end of
 * the cluster. This is important as there may be spilling across
 * gc invocation calls.
 *)

signature INVOKE_GC =
  sig
    structure TS  : MLTREE_STREAM
    structure CFG : CONTROL_FLOW_GRAPH where P = TS.S.P

    type t = {
	maxAlloc : int,			(* maximum allocation in bytes *)
	regfmls  : TS.T.mlrisc list,
	regtys   : CPS.cty list,
	return   : TS.T.stm
      }
    type stream = (TS.T.stm, TS.T.mlrisc list, CFG.cfg) TS.stream

  (* List of registers which are used as the root of the GC *)
    val gcParamRegs : TS.T.rexp list

  (* initialize the state before compiling a module *)
    val init : unit -> unit

  (* generate a check limit for standard function *)
    val stdCheckLimit : stream -> t -> unit

  (* generate a check limit for known function *)
    val knwCheckLimit : stream -> t -> unit

  (* generate a check limit for optimized, known function *)
(* no longer used
    val optimizedKnwCheckLimit : stream -> t -> unit
*)

  (* generate a long jump to call gc *)
    val emitLongJumpsToGCInvocation : stream -> unit

  (* generate all GC invocation code in a module *)
    val emitModuleGC : stream -> unit

  (* generate the actual GC invocation code *)
    val callGC : stream -> {
		 regfmls : TS.T.mlrisc list,
		  regtys : CPS.cty list,
		  ret : TS.T.stm
		 }  -> unit

  end
