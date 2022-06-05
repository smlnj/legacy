(* alpha32PseudoInstrs.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor Alpha32PseudoInstrs
  (Instr : ALPHAINSTR where Region=CPSRegions) : ALPHA_PSEUDO_INSTR =
struct
  structure I = Instr
  structure T = I.T
  structure C = Instr.C
  structure CB = CellsBasis

  fun error msg = MLRiscErrorMsg.impossible("Alpha32PseudoInstrs."^msg)

  type reduceOpnd = I.operand -> CB.cell

  val floatTmpOffset = I.IMMop 96	(* runtime system dependent *)
  val floatTmpOffset8 = I.IMMop(96+8)		(* " *)
  val divlOffset = I.IMMop 120			(* " *)
  val divluOffset = I.IMMop 124			(* " *)

  val stack = CPSRegions.stack
  val sp = C.stackptrR
  val zeroR = 31

  val makeCellset = List.foldl C.addReg C.empty
  val defs = makeCellset (map C.GPReg [0, 23, 24, 25, 26, 28])
  val uses = makeCellset (map C.GPReg [16, 17])
  fun copyTmp() = SOME(I.Direct(C.newReg()))
  fun copy{dst, src, tmp} = I.COPY{k=CB.GP, sz=32, dst=dst, src=src, tmp=tmp}

  val r16 = C.GPReg 16
  val r17 = C.GPReg 17
  val r26 = C.GPReg 26
  val r27 = C.GPReg 27
  val r0  = C.GPReg 0

  fun divlv({ra, rb, rc}, reduceOpnd) =
    [copy{dst=[r16, r17], src=[ra, reduceOpnd rb], tmp=copyTmp()},
     I.load{ldOp=I.LDL, r=r27, b=sp, d=divlOffset, mem=stack},
     I.jsr{r=r26, b=r27, d=0, defs=defs, uses=uses, cutsTo=[], mem=stack},
     copy{dst=[rc], src=[r0], tmp=NONE}]

  val divl = divlv			(* FIXME!!!! *)

  fun divlu({ra, rb, rc}, reduceOpnd) =
    [copy{dst=[r16, r17], src=[ra, reduceOpnd rb],tmp=copyTmp()},
     I.load{ldOp=I.LDL, r=r27, b=sp, d=divluOffset, mem=stack},
     I.jsr{r=r26, b=r27, d=0, defs=defs, uses=uses, cutsTo=[], mem=stack},
     copy{dst=[rc], src=[r0], tmp=NONE}]

  fun unimplemented what = error ("unimplemented pseudo-instr: " ^ what)
  fun divqv _ = unimplemented "divqv"
  fun divq _  = unimplemented "divq"
  fun divqu _ = unimplemented "divqu"
  fun remlv _ = unimplemented "remlv"
  fun reml _  = unimplemented "reml"
  fun remlu _ = unimplemented "remlu"
  fun remqv _ = unimplemented "remqv"
  fun remq _  = unimplemented "remq"
  fun remqu _ = unimplemented "remqu"

  fun cvtlt({opnd, fd}, reduceOpnd) =
  let val ra = reduceOpnd opnd
  in  [I.store{stOp=I.STQ, r=ra, b=sp, d=floatTmpOffset, mem=stack},
       I.fload{ldOp=I.LDT, r=fd, b=sp, d=floatTmpOffset, mem=stack},
       I.funary{oper=I.CVTQT, fb=fd, fc=fd}]
  end

  fun cvtls _ = unimplemented "cvtls"
  fun cvtqt _ = unimplemented "cvtqt"
  fun cvtqs _ = unimplemented "cvtqs"
  fun cvtsl _ = unimplemented "cvtsl"
  fun cvttl _ = unimplemented "cvttl"
  fun cvtsq _ = unimplemented "cvtsq"
  fun cvttq _ = unimplemented "cvttq"
end
