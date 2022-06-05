(* x86gen.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Machine-code generation for the x86 architecture.
 *)

functor X86MC (

    structure CCallParams : sig
	val frameAlign : int
	val returnSmallStructsInRegs : bool
      end

    val abi_variant: string option

  ) = CPSCompFn (
    structure Gen = X86CG (
	structure CCallParams = CCallParams
	val abi_variant = abi_variant)
    fun collect getEP = (
	  Gen.finish ();
	  CodeString.getCodeString (getEP ())))
