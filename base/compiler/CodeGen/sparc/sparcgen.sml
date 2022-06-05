(* sparcgen.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Machine-code generation for the SPARC architecture (32-bt mode).
 *)

structure SparcMC = CPSCompFn (
    structure Gen = SparcCG
    fun collect getEP = (
	  SparcCG.finish ();
	  CodeString.getCodeString (getEP ())))
