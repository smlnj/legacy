(* sparcPseudoInstrs.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor SparcPseudoInstrs
   (Instr : SPARCINSTR where Region=CPSRegions) : SPARC_PSEUDO_INSTR =
struct
  structure I = Instr
  structure C = Instr.C

  type format1 =
       {r:CellsBasis.cell, i:I.operand, d:CellsBasis.cell} *
       (I.operand -> CellsBasis.cell) -> I.instruction list

  type format2 =
       {i:I.operand, d:CellsBasis.cell} *
       (I.operand -> CellsBasis.cell) -> I.instruction list

  fun error msg = MLRiscErrorMsg.impossible ("SparcPseudoInstrs."^msg)

  val delta = SparcSpec.framesize	(* initial value of %fp - %sp *)

  (* runtime system dependent; the numbers are relative to %sp but
   * we need offsets relative to %fp, hence the adjustment by delta *)
  val floatTmpOffset = I.IMMED (88 - delta)

  val stack = CPSRegions.stack

  fun umul_native({r, i, d}, reduceOpnd) =
        [I.arith{a=I.UMUL,r=r,i=i,d=d}]

  val TNE = I.ticc{t=I.BNE,cc=I.ICC,r=C.r0,i=I.IMMED 7}
  val TVS = I.ticc{t=I.BVS,cc=I.ICC,r=C.r0,i=I.IMMED 7}

      (* overflows iff Y != (d ~>> 31) *)
  fun smult_native({r, i, d}, reduceOpnd) =
      let val t1 = C.newReg()
          val t2 = C.newReg()
      in  [I.arith{a=I.SMUL,r=r,i=i,d=d},
           I.shift{s=I.SRA,r=d,i=I.IMMED 31,d=t1},
           I.rdy{d=t2},
           I.arith{a=I.SUBCC,r=t1,i=I.REG t2,d=C.r0},
           TNE
          ]
      end

  fun smul_native({r, i, d}, reduceOpnd) =
      [I.arith{a=I.SMUL,r=r,i=i,d=d}]

  fun udiv_native({r,i,d},reduceOpnd) =
      [I.wry{r=C.r0,i=I.REG C.r0},
       I.arith{a=I.UDIV,r=r,i=i,d=d}]

   (* May overflow if MININT div -1 *)
  fun sdivt_native({r,i,d},reduceOpnd) =
      let val t1 = C.newReg()
      in  [I.shift{s=I.SRA,r=r,i=I.IMMED 31,d=t1},
           I.wry{r=t1,i=I.REG C.r0},
           I.arith{a=I.SDIVCC,r=r,i=i,d=d},
           TVS
          ]
      end

  fun sdiv_native({r,i,d},reduceOpnd) =
      let val t1 = C.newReg()
      in  [I.shift{s=I.SRA,r=r,i=I.IMMED 31,d=t1},
           I.wry{r=t1,i=I.REG C.r0},
           I.arith{a=I.SDIV,r=r,i=i,d=d}
          ]
      end

  fun cvti2d({i, d}, reduceOpnd) =
      [I.store{s=I.ST,r=C.frameptrR,i=floatTmpOffset,d=reduceOpnd i,mem=stack},
       I.fload{l=I.LDF,r=C.frameptrR,i=floatTmpOffset,d=d,mem=stack},
       I.fpop1{a=I.FiTOd,r=d,d=d}
      ]
  fun cvti2s _ = error "cvti2s"
  fun cvti2q _ = error "cvti2q"

(* Generate native versions of the instructions *)
  val umul32 = umul_native
  val smul32 : format1 = smul_native
  val smul32trap = smult_native
  val udiv32 = udiv_native
  val sdiv32 : format1 = sdiv_native
  val sdiv32trap = sdivt_native

  val overflowtrap32 = (* tvs 0x7 *)
                       [I.ticc{t=I.BVS,cc=I.ICC,r=C.r0,i=I.IMMED 7}]
  val overflowtrap64 = [] (* not needed *)

end
