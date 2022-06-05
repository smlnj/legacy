(* machine-gen.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The interface the the machine-code generator.  See machine-gen.sml
 * for the implementation.
 *)

signature MACHINE_GEN =
  sig

    include MACHINE

    structure MLTreeComp : MLTREECOMP
		  where CFG = CFG
		    and I = CFG.I
    structure InvokeGC   : INVOKE_GC
		  where CFG=MLTreeComp.CFG
		    and TS = MLTreeComp.TS
    structure Shuffle    : SHUFFLE
		  where I = MLTreeComp.I
    structure MachSpec   : MACH_SPEC

    val abi_variant : string option (* to distinguish between different ABIs
				     * for same CPU/OSKind combination;
				     * prime example: intel-based macs which
				     * are x86/unix vs. intel-based linux
				     * boxen. *)

    val codegen : {
	  (* the source filename *)
	    source: string,
	  (* the functions to generate code for; the first function is the main
	   * entrypoint for the compilation unit.
	   *)
	    funcs: CPS.function list,
	  (* mapping from functions labels to the maximum number of words allocated
	   * on any execution path in the function.
	   *)
	    maxAlloc: CPS.lvar -> int
	  } -> (unit -> int)

  end (* MACHINE_GEN *)
