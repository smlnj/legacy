(* sparcCpsRegs.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * CPS registerS used on the Sparc (32-bit)
 *)

structure SparcCpsRegs : CPSREGS =
struct
    structure T = SparcMLTree
    structure C = SparcCells

    val GP = C.GPReg
    val FP = C.FPReg

    fun REG r = T.REG(32,GP r)
    fun FREG f = T.FREG(64,FP f)

    val returnPtr	= GP 15

    local
      val stdarg0	= REG(24) (* %i0 *)
      val stdcont0	= REG(25) (* %i1 *)
      val stdclos0	= REG(26) (* %i2 *)
      val stdlink0	= REG(1)  (* %g1 *)
      val baseptr0	= REG(27) (* %i3 *)
      val limitptr0	= REG(4)  (* %g4 *)
      val varptr0	= REG(29) (* %i5 *)
      val storeptr0	= REG(21)  (* %l5 *)
      val exnptr0	= REG(22)  (* %l6 *)
      val gcLink0	= T.REG(32,returnPtr)
      val frameptr0     = REG(30)
    in
      val vfp		= SparcCells.newReg()
      val vfptr		= T.REG(32, vfp)

      fun stdarg _	= stdarg0
      fun stdcont _	= stdcont0
      fun stdclos _	= stdclos0
      fun stdlink _	= stdlink0
      fun baseptr _	= baseptr0

      fun limitptr _	= limitptr0
      fun varptr _	= varptr0
      val exhausted	= SOME(T.CC(T.GTU,C.psr))  (* %psr *)
      fun storeptr _	= storeptr0
      val allocptr	= REG(23)  (* %l7 *)
      fun exnptr _	= exnptr0

      fun gcLink _	= gcLink0

      fun frameptr _    = frameptr0

      (* Warning %o2 is used as the asmTmp
       *)
      val miscregs =
	  map REG
	      [2, 3,				(* %g2-%g3 *)
	       8, 9,				(* %o0-%o1 *)
	       16, 17, 18, 19, 20,              (* %l0-%l4 *)
	       28, 31,				(* %i4, %i6, %i7 *)
	       11, 12, 13]			(* %o3-%o5 *)
      val calleesave = Array.fromList miscregs

      (* Note: We need at least one register for shuffling purposes. *)
      fun fromto(n, m, inc) = if n>m then [] else n :: fromto(n+inc, m, inc)
      val floatregs = map FREG (fromto(0,31,2))
      val savedfpregs = []

      local
	  fun unREG (T.REG (_, r)) = r
	    | unREG _ = raise Fail "sparcCpsRegs:unREG"
	  structure SC = CellsBasis.SortedCells
	  val -- = SC.difference
	  infix --
      in
	val availR =
	    map unREG ([stdlink0, stdclos0, stdarg0, stdcont0, gcLink0]
		       @ miscregs)

	val allRegs = map GP (fromto(0, 31, 1))
	val dedicatedR = SC.return (SC.uniq allRegs -- SC.uniq availR)

	val availF =  map FP (fromto(0, 30, 2))
	val dedicatedF = []

	val signedGCTest = false

	val ccallCallerSaveR = map unREG [limitptr0, storeptr0, exnptr0, allocptr]
	val ccallCallerSaveF = []
      end (*local*)
    end (* local *)

  end

