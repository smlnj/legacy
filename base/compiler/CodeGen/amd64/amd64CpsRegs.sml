(* amd64CpsRegs.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * CPS registers used on the AMD64.  See dev-notes/register-assignments.numbers
 * and dev-notes/amd64-stack-frame.numbers for more information.
 *)

structure AMD64CpsRegs : CPSREGS =
  struct
    structure T = AMD64MLTree
    structure C = AMD64Cells

    val GP = C.GPReg
    val FP = C.FPReg

    val rax = T.REG(64, C.rax)	val rsp = T.REG(64, C.rsp)
    val rcx = T.REG(64, C.rcx)	val rbp = T.REG(64, C.rbp)
    val rdx = T.REG(64, C.rdx)	val rsi = T.REG(64, C.rsi)
    val rbx = T.REG(64, C.rbx)	val rdi = T.REG(64, C.rdi)
    val r08 = T.REG(64, C.r8)   val r09 = T.REG(64, C.r9)
    val r10 = T.REG(64, C.r10)  val r11 = T.REG(64, C.r11)
    val r12 = T.REG(64, C.r12)  val r13 = T.REG(64, C.r13)
    val r14 = T.REG(64, C.r14)  val r15 = T.REG(64, C.r15)

  (* the virtual frame pointer; note that this will be equal to the top of the
   * stack (not %rbp), so the offsets are computed w.r.t. to %rsp!!
   * Also see amd64-omit-frameptr.sml
   *)
    val vfp = C.newDedicatedCell CellsBasis.GP ()
    val vfptr = T.REG(64, vfp)

    fun frameptr which = if which then vfptr else rsp

    fun regInMem (which, i) = let
          val fp = frameptr which
          in
	    T.LOAD(64, T.ADD(64, fp, T.LI(T.I.fromInt(32, i))), CPSRegions.memory)
          end

    val stackptr	= rsp
    val allocptr	= rdi
    fun limitptr _ 	= r14
    fun storeptr _ 	= r15
    fun exnptr useVFP	= regInMem(useVFP, AMD64FrameLayout.exnPtrOffset)
    fun stdarg _	= rbp
    fun stdcont _	= rsi
    fun stdlink  _	= r08
    fun stdclos  _	= r09
    fun baseptr useVFP	= regInMem(useVFP, AMD64FrameLayout.basePtrOffset)
    fun gcLink useVFP	= regInMem(useVFP, AMD64FrameLayout.gcLinkOffset)
    fun varptr useVFP 	= regInMem(useVFP, AMD64FrameLayout.varPtrOffset)

    fun mkRegList (base, cnt) = List.tabulate(cnt, fn i => T.REG(64, GP(base+i)))

  (* miscregs = {rbx,rcx,rdx,r10,r11,r12,r13} *)
    val miscregs = [rbx, rcx, rdx, r10, r11, r12, r13]

    val calleesave  = Array.fromList miscregs
    val exhausted   = NONE

    val floatregs   = List.tabulate(16, fn f => T.FREG(64,FP f))
    val savedfpregs = []

    local
      fun unREG (T.REG (_, r)) = r
	| unREG _ = raise Fail "amd64CpsRegs:unREG"
    in

    val availR = map unREG ([
	    rax, stdlink false, stdclos false, stdarg false, stdcont false
	  ] @ miscregs)

    val dedicatedR = map unREG [stackptr, allocptr, limitptr false, storeptr false, vfptr]

    val availF = List.tabulate(16, FP)
    val dedicatedF = []
    val signedGCTest = false

    val ccallCallerSaveR = [C.rax, C.rdi]
    val ccallCallerSaveF = []

    end (* local *)

  end
