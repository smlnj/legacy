(* code-generator.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generation of code from the FLINT intermediate form (after all FLINT passes
 * have completed)
 *)

signature CODE_GENERATOR =
  sig
    structure Machine : MACHINE_GEN

    val architecture : string

    val abi_variant : string option (* to distinguish between, e.g., various
				   * intel-based unices, etc.*)

    val compile : {
	    source : string,
	    prog : FLINT.prog
	  } -> CodeObj.csegments

  end (* CODE_GENERATOR *)
