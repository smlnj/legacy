(* alpha32gen.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Machine-code generation for the DEC Alpha architecture (32-bit mode).
 *
 * NOTE: this code generator is no longer supported.
 *)

structure Alpha32MC = CPSCompFn (
    structure Gen = Alpha32CG
    fun collect getEP = (
	  Alpha32CG.finish ();
	  CodeString.getCodeString(getEP ())))
