(* hppaCpsRegs.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * CPS registers used on the HPPA
 *)

structure HppaCpsRegs : CPSREGS =
  struct
    structure T = HppaMLTree
    structure C = HppaCells

  (* HPPA register conventions
     0     zero
     1	   caller-saves
     2     return-pointer and scratch
     3-18  callee-saves
     19-22 caller-saves
     23    arg3
     24    arg2
     25    arg1
     26    arg0
     27    reserved
     28    ret0
     29    ret1
     30    stack pointer
     31    millicode return and scratch.
   *)

    val GP = HppaCells.GPReg
    val FP = HppaCells.FPReg
    fun REG r = T.REG(32, GP r)
    fun FREG f = T.FREG(64, FP f)

    val vfp       = HppaCells.newReg()
    val vfptr     = T.REG(32, vfp)

    fun stdarg _		= REG(11)
    fun stdcont _		= REG(12)
    fun stdclos _		= REG(10)
    fun stdlink _		= REG(9)
    fun baseptr _		= REG(8)

    fun limitptr _		= REG(4)
    fun varptr _		= REG(7)
    val exhausted		= NONE
    fun storeptr _		= REG(5)
    val allocptr		= REG(3)
    fun exnptr _		= REG(6)

    val returnPtr		= GP 31
    fun gcLink _		= T.REG(32,returnPtr)
    val stackptr		= REG(30)

    fun frameptr _        	= stackptr

    val miscregs = map REG [1,13,14,15,16,17,18,19,20,21,22,23,24,25,26,28,2]
    val calleesave = Array.fromList miscregs

  (* Note: We need at least one register for shuffling purposes. *)
    fun fromto(n, m) = if n>m then [] else n :: fromto(n+1, m)
    val floatregs = map FREG (fromto(6, 30))
    val savedfpregs = []

    val availR = let
	  fun unREG (T.REG (_, r)) = r
	    | unREG _ = MLRiscErrorMsg.error ("HppaCpsRegs","availR")
	  in
	    map unREG ([
		stdlink(false), stdclos(false), stdarg(false),
		stdcont(false), gcLink(false)
	      ] @ miscregs)
	  end

    local
      structure SC = CellsBasis.SortedCells
      val -- = SC.difference
      infix --
    in
    val allRegs = map GP (fromto(0,31))
    val dedicatedR = SC.return (SC.uniq allRegs -- SC.uniq availR)

    val availFs = map FP (fromto(6, 30))
    val allFRegs = map FP (fromto(0, 31))
    val dedicatedF = SC.return (SC.uniq allFRegs -- SC.uniq availFs)
    val availF = availFs
    end

    val signedGCTest = false

  (* FIXME *)
    val ccallCallerSaveR = []		(* no c-calls implemented yet *)
    val ccallCallerSaveF = []		(* ... *)

  end
