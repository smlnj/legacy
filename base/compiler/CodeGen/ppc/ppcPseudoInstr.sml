(* ppcPseudoInstr.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor PPCPseudoInstr
  (structure Instr : PPCINSTR
     where Region = CPSRegions) : PPC_PSEUDO_INSTR =
struct
  structure I = Instr
  structure C = Instr.C

  val stack = CPSRegions.stack
  val cvti2dTmpOff = 4096+16		(* runtime system dependent *)
  val cvti2dConstOff = 4096+8		(*            ''             *)
  val sp = C.stackptrR

  (* Cute little trick -- go figure *)
  fun cvti2d {reg, fd} = let
	val tmpR = C.newReg()
	val tmpF = C.newFreg()
	in
	  map I.INSTR
	    [I.ARITHI{oper=I.XORIS, rt=tmpR, ra=reg, im=I.ImmedOp 32768},
	     I.ST{st=I.STW, rs=tmpR, ra=sp, d=I.ImmedOp(cvti2dTmpOff+4), mem=stack},
	     I.ARITHI{oper=I.ADDIS, rt=tmpR, ra=C.r0, im=I.ImmedOp(0x4330)},
	     I.ST{st=I.STW, rs=tmpR, ra=sp, d=I.ImmedOp(cvti2dTmpOff), mem=stack},
	     I.LF{ld=I.LFD, ft=fd, ra=sp, d=I.ImmedOp(cvti2dTmpOff), mem=stack},
	     I.LF{ld=I.LFD, ft=tmpF, ra=sp, d=I.ImmedOp(cvti2dConstOff), mem=stack},
	     I.FARITH{oper=I.FSUB, ft=fd, fa=fd, fb=tmpF, Rc=false}]
	end
end
