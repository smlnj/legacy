(* machspec.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This signature contains various machine and code-generator specific
 * parameters.
 *
 * When should a code-generator parameter be put in this signature?
 * Only when changing it will yield incompatible code.
 * Parameters that change optimization algorithms but yield compatible
 * code should not go here.       -- A. Appel
 *
 *)

signature MACH_SPEC =
  sig

    val architecture : string

    val framesize : int

  (* code generator flags *)
    val polling : bool
    val unboxedFloats : bool
    val newClosure : bool
    val numRegs : int		(* the number of registers used by ML *)
    val numFloatRegs : int	(* the number of registers used by ML *)
    val numArgRegs : int	(* the number of registers used to pass args. *)
    val numFloatArgRegs : int	(* the number of FP registers used for args. *)
    val numCalleeSaves : int
    val numFloatCalleeSaves : int

  (* machine representations *)
    type value_tag = {
	tagbits : int,		(* number of tag bits *)
	tagval : int		(* value of tag bits *)
      }

    val intTag : value_tag	(* tag for tagged integer values *)
    val ptrTag : value_tag	(* tag for pointers *)
    val descTag : value_tag	(* tag for object descriptors *)

  (* representations of object descriptors *)
    structure ObjDesc : OBJECT_DESC

    val valueSize : int		(* number of bytes for an ML value *)
    val charSize : int		(* number of bytes for a char *)
    val realSize : int		(* number of bytes of the default real type *)
    val realAlign : bool	(* if true, reals are realSize aligned *)

    val bigEndian : bool	(* true, if this is a big-endian machine *)

    val spillAreaSz : int	(* the size of the area for spilling registers *)
				(* in bytes *)
    val initialSpillOffset : int (* the offset of the first spill location *)

    val startgcOffset 	: int
    val constBaseRegOffset : int

    val floatRegParams : bool	(* for old-style codegen; default true *)

   (* get "conreps" into here eventually.
	Didn't want to do it now, because it would require
	functorizing the whole front end.  -- A. Appel*)

    val fixedArgPassing : bool
    (* Use fixed argument passing registers for known functions that
     * require garbage collection. Only an issue on  the x86 or machines
     * that have registers implemented as memory locations, i.e., at the
     * call to GC, there aren't enough registers to hold alll the roots.
     * The correct way to solve this problem is to create a record of
     * live variables inside the code that invokes the garbage collector
     * 							-- Lal George.
     *)

    val spillRematerialization : bool
    (* Whether rematerialization of spill locations is performed *)

    (* for accessing the in_ML flag etc.;
     * These values must be coordinated with their respective runtime
     * counterparts in ?.prim.asm and mlstate-offsets.h! *)
    val ML_STATE_OFFSET : int		(* within frame *)
    val VProcOffMSP : int		(* within ML state struct *)
    val InMLOffVSP : int		(* within VProc struct *)
    val LimitPtrMaskOffVSP : int	(* within VProc struct *)

    (* On machines with a real frame pointer, there is no point in
     * attempting to omit a (virtual) frame pointer.  Example: Sparc
     *)
    val framePtrNeverVirtual : bool	(* suppress omit-frame-ptr phase *)

    (* On machines where C arguments are allocated in the caller's frame
     * we pre-allocate a large chunk of stack space for this purpose.
     * Example: PPC
     *)
    val ccall_prealloc_argspace : int option

  (* number of bits and bytes per ML word *)
    val wordBitWidth	: int
    val wordByteWidth	: int

  (* number of bits and bytes per C pointer *)
    val addressByteWidth : int
    val addressBitWidth  : int

  end (* MACH_SPEC *)
