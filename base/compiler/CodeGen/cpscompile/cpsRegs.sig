(* cpsRegs.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The registers used for CPS compilation.
 *)

signature CPSREGS =
  sig
    structure T : MLTREE
    structure C : CELLS

    val vfp		: CellsBasis.cell
    val vfptr		: T.rexp

    val allocptr 	: T.rexp	(* must be a hardware register, - T.REG(r) *)

  (* The boolean argument in each case indicates the use of the virtual
   * frame pointer. Use virtual fp if true and physical fp if false.
   *
   * In principle a lot more of these should be functions over the boolean,
   * however, the x86 and amd64 are the only targets that implement CPS
   * registers in memory, so we will limit this to the set that it needs.
   *)
    val frameptr	: bool -> T.rexp	(* frame pointer *)
    val limitptr	: bool -> T.rexp	(* heap-limit pointer *)
    val stdlink		: bool -> T.rexp
    val stdclos		: bool -> T.rexp
    val stdarg 		: bool -> T.rexp
    val stdcont 	: bool -> T.rexp	(* holds return continuation *)
    val exnptr 		: bool -> T.rexp	(* holds exception-handler continuation *)
    val varptr  	: bool -> T.rexp
    val baseptr		: bool -> T.rexp	(* holds base address of current code object *)
    val storeptr 	: bool -> T.rexp	(* store-list pointer *)
    val gcLink		: bool -> T.rexp	(* return address for GC calls *)

    val calleesave	: T.rexp Array.array
    val exhausted 	: T.ccexp option
    val signedGCTest 	: bool

    val miscregs  	: T.rexp list
    val floatregs 	: T.fexp list
    val savedfpregs 	: T.fexp list

    val dedicatedR 	: T.reg list
    val availR     	: T.reg list
    val dedicatedF 	: T.reg list
    val availF     	: T.reg list

    val ccallCallerSaveR : T.reg list
    val ccallCallerSaveF : T.reg list

  end
