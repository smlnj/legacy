(* alpha32CpsRegs.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * CPS registers used on the DEC Alpha (32-bit mode)
 *)

structure Alpha32CpsRegs : CPSREGS =
  struct
    structure T = Alpha32MLTree
    structure C = AlphaCells

    fun upto (from,to) = if from>to then [] else from::(upto (from+1,to))
    infix upto

    val GP = AlphaCells.GPReg
    val FP = AlphaCells.FPReg

    fun REG r = T.REG(32, GP r)
    fun FREG f = T.FREG(64, FP f)
    val vfp		= AlphaCells.newReg()
    val vfptr		= T.REG(32, vfp)
    fun stdarg _	= REG(0)
    fun stdcont _	= REG(1)
    fun stdclos _	= REG(2)
    fun stdlink _	= REG(3)
    fun baseptr _	= REG(4)

    fun limitptr _	= REG(9)
    fun varptr _	= REG(10)
    val exhaustedR	= GP 11
    val exhausted	= SOME(T.CC(T.GT,exhaustedR))
    fun storeptr _	= REG(12)
    fun exnptr _	= REG(14)
    fun gcLink _	= REG(26)

    val allocptr	= REG(13)
    val stackptr	= REG(30)

    fun frameptr _	= stackptr

    val miscregs =  map REG ((5 upto 8) @ (15 upto 25) @ [27])
    val calleesave = Array.fromList(miscregs)
    val floatregs = map FREG (0 upto 28)
    val savedfpregs = []

    val availR = let
	  fun get (T.REG (_, r)) = r
	    | get _ = MLRiscErrorMsg.error ("Alpha32CpsRegs","availR:get")
	  in
	    map get ([
		gcLink(false), T.REG(32, exhaustedR),
		stdlink(false), stdclos(false), stdarg(false), stdcont(false)
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

    val availF = map FP (0 upto 28)
    val dedicatedF = map FP [29, 30, 31]
    val signedGCTest = true

  (* FIXME *)
    val ccallCallerSaveR = []		(* no c-calls implemented yet *)
    val ccallCallerSaveF = []		(* ... *)

  end
