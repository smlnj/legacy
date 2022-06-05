(* hppagen.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Machine-code generation for the HP-PA architecture.
 *
 * NOTE: this code generator is no longer supported.
 *)

structure HppaMC = CPSCompFn (
    structure Gen = HppaCG
    fun collect getEP = (
	  HppaCG.finish ();
	  CodeString.getCodeString (getEP ())))
