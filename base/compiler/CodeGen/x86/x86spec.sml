(* x86spec.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure X86Spec : MACH_SPEC =
  struct

    structure DMS = DefaultMachSpecFn (
      struct
	val wordByteWidth = 4
	val addressByteWidth = 4
      end)
    open DMS

    val architecture = "x86"
    val bigEndian = false
    val addressWidth = 32
    val spillAreaSz = 8192
    val initialSpillOffset = 512
    val numRegs = 12 (* 18 *) (* can be |X86CpsRegs.availR| + |vregs| *)
    val numFloatRegs = 21   (* can be |8 upto 31| *)
    val numFloatCalleeSaves = 0
    val startgcOffset = 32
    val pseudoRegOffset = 136
    val constBaseRegOffset = 0
    val fixedArgPassing = false

    val ML_STATE_OFFSET = 176
    val VProcOffMSP = 4
    val InMLOffVSP = 8
    val LimitPtrMaskOffVSP = 200

  end
