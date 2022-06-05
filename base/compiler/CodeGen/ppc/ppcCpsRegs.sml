(* ppcCpsRegs.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * CPS registers used on the POWER PC (32-bit)
 *)

structure PPCCpsRegs : CPSREGS =
  struct
    structure T = PPCMLTree
    structure C = PPCCells
    fun upto (from,to) = if from>to then [] else from::(upto (from+1,to))
    infix upto

    val GP = PPCCells.GPReg
    val FP = PPCCells.FPReg
    val CC = PPCCells.Reg CellsBasis.CC

    fun REG r = T.REG(32, GP r)
    fun FREG f = T.FREG(64, FP f)

    val exhaustedR = CC 0
    val exhausted	= SOME(T.CC(T.GTU,exhaustedR))

    val vfp		= PPCCells.newReg()
    val vfptr		= T.REG(32, vfp)

    val stackptr	= REG(1)
    val allocptr	= REG(14)
    fun limitptr _ 	= REG(15)
    fun storeptr _	= REG(16)
    fun stdlink _	= REG(17)
    fun stdclos _	= REG(18)
    fun stdarg _	= REG(19)
    fun stdcont _  	= REG(20)
    fun exnptr _	= REG(21)
    fun varptr _	= REG(22)
    fun baseptr _	= REG(23)
    fun gcLink _	= T.REG(32,PPCCells.lr)

    fun frameptr _      = stackptr

    val miscregs =  map REG ([24,25,26,27,29,30,31] @ (3 upto 13))
    val calleesave = Array.fromList(miscregs)
    val floatregs = map FREG (1 upto 31)
    val savedfpregs = []

    val availR = let
	  fun unREG (T.REG (_, r)) = r
	    | unREG _ = MLRiscErrorMsg.error ("PPCCpsRegs", "availR")
	  in
	    map unREG ([
		stdlink(false), stdclos(false), stdarg(false),
		stdcont(false)
	      ] @ miscregs)
	  end

    local
      structure SC = CellsBasis.SortedCells
      val -- = SC.difference
      infix --
    in
    val allRegs = map GP (0 upto 31)
    val dedicatedR = SC.return (SC.uniq allRegs -- SC.uniq availR)
    end

    val availF = map FP (1 upto 31)
    val dedicatedF = [FP 0]

    val signedGCTest = false

    (* FIXME *)
    val ccallCallerSaveR = []		(* no c-calls implemented yet *)
    val ccallCallerSaveF = []		(* ... *)

  end
