(* default-machspec-fn.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor DefaultMachSpecFn (Sizes : sig

(* 64BIT: since we no longer support the DEC Alpha, which was the only 32/64-bit
 * target, we can assume that wordByteWidth == addressByteWidth for all targets.
 *)
    val wordByteWidth : int		(* size of ML value (aka word) in bytes *)
    val addressByteWidth : int		(* native size of an address/pointer *)

  end) : MACH_SPEC = struct

    val architecture = ""

    val framesize = 1024 * Sizes.wordByteWidth

    val numRegs = 0
    val numFloatRegs = 0
    val bigEndian = false
    val spillAreaSz = 0
    val initialSpillOffset = 0
    val startgcOffset = 0
    val pseudoRegOffset = 0
    val constBaseRegOffset = 0

    val polling = false
    val unboxedFloats = true
    val newClosure = true
    val numArgRegs = 10
    val numFloatArgRegs = 0
    val numCalleeSaves = 3	(* miscreg0, miscreg1, miscreg2 *)
    val numFloatCalleeSaves = 0

    type value_tag = {
	tagbits : int,
	tagval : int
      }

    val intTag = {tagbits=1, tagval=1}
    val ptrTag = {tagbits=2, tagval=0}
    val descTag= {tagbits=2, tagval=2}

  (* representations of object descriptors *)
    structure ObjDesc = ObjectDesc

    val valueSize = Sizes.wordByteWidth
    val charSize  = 1
    val realSize  = 8
    val realAlign = true

    val floatRegParams = true

    val fixedArgPassing = false

    val spillRematerialization = false

    (* the following defaults happen to be the values for x86 *)
    val ML_STATE_OFFSET = 176
    val VProcOffMSP = 4
    val InMLOffVSP = 8
    val LimitPtrMaskOffVSP = 200

    val framePtrNeverVirtual = false

    (* x86 and sparc don't use pre-allocated arg space for c-calls *)
    val ccall_prealloc_argspace = NONE

  (* number of bits and bytes per ML word *)
    val wordByteWidth = Sizes.wordByteWidth
    val wordBitWidth = 8*wordByteWidth

  (* number of bits and bytes per C pointer *)
    val addressByteWidth = Sizes.addressByteWidth
    val addressBitWidth = 8 * addressByteWidth

  end (* DefaultMachSpecFn *)
