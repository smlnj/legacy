(* ppcspec.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PPCSpec : MACH_SPEC =
  struct

    structure DMS = DefaultMachSpecFn (
      struct
	val wordByteWidth = 4
	val addressByteWidth = 4
      end)
    open DMS

    val framesize = 8192

    val architecture = "ppc"
    val bigEndian = true
    val spillAreaSz = 8192		(* really the end of the spill area! *)
    val initialSpillOffset = 4096+144
    val numRegs = 15
    val numFloatRegs = 30
    val numFloatCalleeSaves = 0
    val startgcOffset =	4096+4		(* from runtime *)
    val constBaseRegOffset = 32764

    val ML_STATE_OFFSET = 4096+0
    val VProcOffMSP = 4
    val InMLOffVSP = 8
    val LimitPtrMaskOffVSP = 200

    (* the pre-allocated space is 4k minus the linkage area (24 bytes) *)
    val ccall_prealloc_argspace = SOME (4096 - 24)

  end
