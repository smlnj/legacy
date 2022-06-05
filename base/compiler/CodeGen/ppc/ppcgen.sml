(* ppcgen.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Machine-code generation for the PowerPC architecture (32-bt mode).
 *)

structure PPCMC = CPSCompFn (
    structure Gen = PPCCG
    fun collect getEP = (
	  PPCCG.finish ();
	  CodeString.getCodeString (getEP ())))
