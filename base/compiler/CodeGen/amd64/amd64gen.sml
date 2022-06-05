(* amd64gen.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Machine-code generation for the AMD64 (aka x86-64) architecture.
 *)

functor AMD64MC (

    structure CCallParams : sig
	val frameAlign : int
	val returnSmallStructsInRegs : bool
      end

    val abi_variant: string option

  ) = CPSCompFn(
    structure Gen = AMD64CG (
	structure CCallParams = CCallParams
	val abi_variant = abi_variant)
    fun collect getEP = (
	  Gen.finish ();
	  CodeString.getCodeString (getEP ())))
