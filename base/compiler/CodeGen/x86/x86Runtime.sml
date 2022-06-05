(* x86Runtime.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure X86Runtime =
  struct

  (* number of virtual registers *)
    val numVregs = (* 24 *) 18

  (* stack offsets *)
    val vFpStart = 184			(* floating point registers  *)
    val vregStart = 72			(* virtual regs *)
    val regStart = 40			(* area for physcial registers *)
    val spillStart = X86Spec.initialSpillOffset (* spill area *)
    val spillAreaSz = X86Spec.spillAreaSz
(* QUESTION: fpTempMemOff does not appear to be used anywhere, but the constant 304
 * is used for a similar purpose (int->real conversion) in x86/x86CG.sml.  Why?
 *)
    val fpTempMemOff = 376 : Int32.int

  end
