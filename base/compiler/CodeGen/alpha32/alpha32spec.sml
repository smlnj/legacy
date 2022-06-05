(* alpha32spec.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Alpha32Spec : MACH_SPEC =
  struct

    structure DMS = DefaultMachSpecFn (
      struct
	val wordByteWidth = 4
	val addressByteWidth = 8
      end)
    open DMS

    val architecture = "alpha32"
    val bigEndian = false
    val spillAreaSz = 4000
    val initialSpillOffset = 128
    val numRegs = 17
    val numFloatRegs = 29
    val numFloatCalleeSaves = 0
    val startgcOffset = 8
    val constBaseRegOffset = 32768

    val ML_STATE_OFFSET = 0
    val VProcOffMSP = 8
    val InMLOffVSP = 16
    val LimitPtrMaskOffVSP = 216

  end
