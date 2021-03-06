(*
 * WARNING: This file was automatically generated by MDLGen (v3.1)
 * from the machine description file "alpha/alpha.mdl".
 * DO NOT EDIT this file directly
 *)


functor AlphaMCEmitter(structure Instr : ALPHAINSTR
                       structure MLTreeEval : MLTREE_EVAL where T = Instr.T
                       structure Stream : INSTRUCTION_STREAM
                       structure CodeString : CODE_STRING
                      ) : INSTRUCTION_EMITTER =
struct
   structure I = Instr
   structure C = I.C
   structure Constant = I.Constant
   structure T = I.T
   structure S = Stream
   structure P = S.P
   structure W = Word32

   (* Alpha is little endian *)

   fun error msg = MLRiscErrorMsg.error("AlphaMC",msg)
   fun makeStream _ =
   let infix && || << >> ~>>
       val op << = W.<<
       val op >> = W.>>
       val op ~>> = W.~>>
       val op || = W.orb
       val op && = W.andb
       val itow = W.fromInt
       fun emit_bool false = 0w0 : W.word
         | emit_bool true = 0w1 : W.word
       val emit_int = itow
       fun emit_word w = w
       fun emit_label l = itow(Label.addrOf l)
       fun emit_labexp le = itow(MLTreeEval.valueOf le)
       fun emit_const c = itow(Constant.valueOf c)
       val w32ToByte = Word8.fromLarge o Word32.toLarge
       val loc = ref 0

       (* emit a byte *)
       fun eByte b =
       let val i = !loc in loc := i + 1; CodeString.update(i,b) end

       (* emit the low order byte of a word *)
       (* note: fromLargeWord strips the high order bits! *)
       fun eByteW w =
       let val i = !loc
       in loc := i + 1; CodeString.update(i, w32ToByte w) end

       fun doNothing _ = ()
       fun fail _ = raise Fail "MCEmitter"
       fun getAnnotations () = error "getAnnotations"

       fun pseudoOp pOp = P.emitValue{pOp=pOp, loc= !loc,emit=eByte}

       fun init n = (CodeString.init n; loc := 0)


   fun eWord32 w =
       let val b8 = w
           val w = w >> 0wx8
           val b16 = w
           val w = w >> 0wx8
           val b24 = w
           val w = w >> 0wx8
           val b32 = w
       in
          ( eByteW b8;
            eByteW b16;
            eByteW b24;
            eByteW b32 )
       end
   fun emit_GP r = itow (CellsBasis.physicalRegisterNum r)
   and emit_FP r = itow (CellsBasis.physicalRegisterNum r)
   and emit_CC r = itow (CellsBasis.physicalRegisterNum r)
   and emit_MEM r = itow (CellsBasis.physicalRegisterNum r)
   and emit_CTRL r = itow (CellsBasis.physicalRegisterNum r)
   and emit_CELLSET r = itow (CellsBasis.physicalRegisterNum r)
   fun emit_branch (I.BR) = (0wx30 : Word32.word)
     | emit_branch (I.BLBC) = (0wx38 : Word32.word)
     | emit_branch (I.BEQ) = (0wx39 : Word32.word)
     | emit_branch (I.BLT) = (0wx3A : Word32.word)
     | emit_branch (I.BLE) = (0wx3B : Word32.word)
     | emit_branch (I.BLBS) = (0wx3C : Word32.word)
     | emit_branch (I.BNE) = (0wx3D : Word32.word)
     | emit_branch (I.BGE) = (0wx3E : Word32.word)
     | emit_branch (I.BGT) = (0wx3F : Word32.word)
   and emit_fbranch (I.FBEQ) = (0wx31 : Word32.word)
     | emit_fbranch (I.FBLT) = (0wx32 : Word32.word)
     | emit_fbranch (I.FBLE) = (0wx33 : Word32.word)
     | emit_fbranch (I.FBNE) = (0wx35 : Word32.word)
     | emit_fbranch (I.FBGE) = (0wx36 : Word32.word)
     | emit_fbranch (I.FBGT) = (0wx37 : Word32.word)
   and emit_load (I.LDB) = error "LDB"
     | emit_load (I.LDW) = error "LDW"
     | emit_load (I.LDBU) = (0wx2 : Word32.word)
     | emit_load (I.LDWU) = (0wx4 : Word32.word)
     | emit_load (I.LDL) = (0wx28 : Word32.word)
     | emit_load (I.LDL_L) = (0wx2A : Word32.word)
     | emit_load (I.LDQ) = (0wx29 : Word32.word)
     | emit_load (I.LDQ_L) = (0wx2B : Word32.word)
     | emit_load (I.LDQ_U) = (0wxB : Word32.word)
   and emit_store (I.STB) = (0wxE : Word32.word)
     | emit_store (I.STW) = (0wxD : Word32.word)
     | emit_store (I.STL) = (0wx2C : Word32.word)
     | emit_store (I.STQ) = (0wx2D : Word32.word)
     | emit_store (I.STQ_U) = (0wxF : Word32.word)
   and emit_fload (I.LDF) = (0wx20 : Word32.word)
     | emit_fload (I.LDG) = (0wx21 : Word32.word)
     | emit_fload (I.LDS) = (0wx22 : Word32.word)
     | emit_fload (I.LDT) = (0wx23 : Word32.word)
   and emit_fstore (I.STF) = (0wx24 : Word32.word)
     | emit_fstore (I.STG) = (0wx25 : Word32.word)
     | emit_fstore (I.STS) = (0wx26 : Word32.word)
     | emit_fstore (I.STT) = (0wx27 : Word32.word)
   and emit_operate (I.ADDL) = (0wx10, 0wx0)
     | emit_operate (I.ADDQ) = (0wx10, 0wx20)
     | emit_operate (I.CMPBGE) = (0wx10, 0wxF)
     | emit_operate (I.CMPEQ) = (0wx10, 0wx2D)
     | emit_operate (I.CMPLE) = (0wx10, 0wx6D)
     | emit_operate (I.CMPLT) = (0wx10, 0wx4D)
     | emit_operate (I.CMPULE) = (0wx10, 0wx3D)
     | emit_operate (I.CMPULT) = (0wx10, 0wx1D)
     | emit_operate (I.SUBL) = (0wx10, 0wx9)
     | emit_operate (I.SUBQ) = (0wx10, 0wx29)
     | emit_operate (I.S4ADDL) = (0wx10, 0wx2)
     | emit_operate (I.S4ADDQ) = (0wx10, 0wx22)
     | emit_operate (I.S4SUBL) = (0wx10, 0wxB)
     | emit_operate (I.S4SUBQ) = (0wx10, 0wx2B)
     | emit_operate (I.S8ADDL) = (0wx10, 0wx12)
     | emit_operate (I.S8ADDQ) = (0wx10, 0wx32)
     | emit_operate (I.S8SUBL) = (0wx10, 0wx1B)
     | emit_operate (I.S8SUBQ) = (0wx10, 0wx3B)
     | emit_operate (I.AND) = (0wx11, 0wx0)
     | emit_operate (I.BIC) = (0wx11, 0wx8)
     | emit_operate (I.BIS) = (0wx11, 0wx20)
     | emit_operate (I.EQV) = (0wx11, 0wx48)
     | emit_operate (I.ORNOT) = (0wx11, 0wx28)
     | emit_operate (I.XOR) = (0wx11, 0wx40)
     | emit_operate (I.EXTBL) = (0wx12, 0wx6)
     | emit_operate (I.EXTLH) = (0wx12, 0wx6A)
     | emit_operate (I.EXTLL) = (0wx12, 0wx26)
     | emit_operate (I.EXTQH) = (0wx12, 0wx7A)
     | emit_operate (I.EXTQL) = (0wx12, 0wx36)
     | emit_operate (I.EXTWH) = (0wx12, 0wx5A)
     | emit_operate (I.EXTWL) = (0wx12, 0wx16)
     | emit_operate (I.INSBL) = (0wx12, 0wxB)
     | emit_operate (I.INSLH) = (0wx12, 0wx67)
     | emit_operate (I.INSLL) = (0wx12, 0wx2B)
     | emit_operate (I.INSQH) = (0wx12, 0wx77)
     | emit_operate (I.INSQL) = (0wx12, 0wx3B)
     | emit_operate (I.INSWH) = (0wx12, 0wx57)
     | emit_operate (I.INSWL) = (0wx12, 0wx1B)
     | emit_operate (I.MSKBL) = (0wx12, 0wx2)
     | emit_operate (I.MSKLH) = (0wx12, 0wx62)
     | emit_operate (I.MSKLL) = (0wx12, 0wx22)
     | emit_operate (I.MSKQH) = (0wx12, 0wx72)
     | emit_operate (I.MSKQL) = (0wx12, 0wx32)
     | emit_operate (I.MSKWH) = (0wx12, 0wx52)
     | emit_operate (I.MSKWL) = (0wx12, 0wx12)
     | emit_operate (I.SLL) = (0wx12, 0wx39)
     | emit_operate (I.SRA) = (0wx12, 0wx3C)
     | emit_operate (I.SRL) = (0wx12, 0wx34)
     | emit_operate (I.ZAP) = (0wx12, 0wx30)
     | emit_operate (I.ZAPNOT) = (0wx12, 0wx31)
     | emit_operate (I.MULL) = (0wx13, 0wx0)
     | emit_operate (I.MULQ) = (0wx13, 0wx20)
     | emit_operate (I.UMULH) = (0wx13, 0wx30)
   and emit_cmove (I.CMOVEQ) = (0wx24 : Word32.word)
     | emit_cmove (I.CMOVLBC) = (0wx16 : Word32.word)
     | emit_cmove (I.CMOVLBS) = (0wx14 : Word32.word)
     | emit_cmove (I.CMOVGE) = (0wx46 : Word32.word)
     | emit_cmove (I.CMOVGT) = (0wx66 : Word32.word)
     | emit_cmove (I.CMOVLE) = (0wx64 : Word32.word)
     | emit_cmove (I.CMOVLT) = (0wx44 : Word32.word)
     | emit_cmove (I.CMOVNE) = (0wx26 : Word32.word)
   and emit_operateV (I.ADDLV) = (0wx10, 0wx40)
     | emit_operateV (I.ADDQV) = (0wx10, 0wx60)
     | emit_operateV (I.SUBLV) = (0wx10, 0wx49)
     | emit_operateV (I.SUBQV) = (0wx10, 0wx69)
     | emit_operateV (I.MULLV) = (0wx13, 0wx40)
     | emit_operateV (I.MULQV) = (0wx13, 0wx60)
   and emit_funary (I.CVTLQ) = (0wx17, 0wx10)
     | emit_funary (I.CVTQL) = (0wx17, 0wx30)
     | emit_funary (I.CVTQLSV) = (0wx17, 0wx530)
     | emit_funary (I.CVTQLV) = (0wx17, 0wx130)
     | emit_funary (I.CVTQS) = (0wx16, 0wxBC)
     | emit_funary (I.CVTQSC) = (0wx16, 0wx3C)
     | emit_funary (I.CVTQT) = (0wx16, 0wxBE)
     | emit_funary (I.CVTQTC) = (0wx16, 0wx3E)
     | emit_funary (I.CVTTS) = (0wx16, 0wxAC)
     | emit_funary (I.CVTTSC) = (0wx16, 0wx2C)
     | emit_funary (I.CVTST) = (0wx16, 0wx2AC)
     | emit_funary (I.CVTSTS) = (0wx16, 0wx6AC)
     | emit_funary (I.CVTTQ) = (0wx16, 0wxAF)
     | emit_funary (I.CVTTQC) = (0wx16, 0wx2F)
   and emit_foperate (I.CPYS) = (0wx17, 0wx20)
     | emit_foperate (I.CPYSE) = (0wx17, 0wx22)
     | emit_foperate (I.CPYSN) = (0wx17, 0wx21)
     | emit_foperate (I.MF_FPCR) = (0wx17, 0wx25)
     | emit_foperate (I.MT_FPCR) = (0wx17, 0wx24)
     | emit_foperate (I.CMPTEQ) = (0wx16, 0wxA5)
     | emit_foperate (I.CMPTLT) = (0wx16, 0wxA6)
     | emit_foperate (I.CMPTLE) = (0wx16, 0wxA7)
     | emit_foperate (I.CMPTUN) = (0wx16, 0wxA4)
     | emit_foperate (I.CMPTEQSU) = (0wx16, 0wx5A5)
     | emit_foperate (I.CMPTLTSU) = (0wx16, 0wx5A6)
     | emit_foperate (I.CMPTLESU) = (0wx16, 0wx5A7)
     | emit_foperate (I.CMPTUNSU) = (0wx16, 0wx5A4)
     | emit_foperate (I.ADDS) = (0wx16, 0wx80)
     | emit_foperate (I.ADDT) = (0wx16, 0wxA0)
     | emit_foperate (I.DIVS) = (0wx16, 0wx83)
     | emit_foperate (I.DIVT) = (0wx16, 0wxA3)
     | emit_foperate (I.MULS) = (0wx16, 0wx82)
     | emit_foperate (I.MULT) = (0wx16, 0wxA2)
     | emit_foperate (I.SUBS) = (0wx16, 0wx81)
     | emit_foperate (I.SUBT) = (0wx16, 0wxA1)
   and emit_fcmove (I.FCMOVEQ) = (0wx2A : Word32.word)
     | emit_fcmove (I.FCMOVGE) = (0wx2D : Word32.word)
     | emit_fcmove (I.FCMOVGT) = (0wx2F : Word32.word)
     | emit_fcmove (I.FCMOVLE) = (0wx2E : Word32.word)
     | emit_fcmove (I.FCMOVLT) = (0wx2C : Word32.word)
     | emit_fcmove (I.FCMOVNE) = (0wx2B : Word32.word)
   and emit_foperateV (I.ADDSSUD) = (0wx5C0 : Word32.word)
     | emit_foperateV (I.ADDSSU) = (0wx580 : Word32.word)
     | emit_foperateV (I.ADDTSUD) = (0wx5E0 : Word32.word)
     | emit_foperateV (I.ADDTSU) = (0wx5A0 : Word32.word)
     | emit_foperateV (I.DIVSSUD) = (0wx5C3 : Word32.word)
     | emit_foperateV (I.DIVSSU) = (0wx583 : Word32.word)
     | emit_foperateV (I.DIVTSUD) = (0wx5E3 : Word32.word)
     | emit_foperateV (I.DIVTSU) = (0wx5A3 : Word32.word)
     | emit_foperateV (I.MULSSUD) = (0wx5C2 : Word32.word)
     | emit_foperateV (I.MULSSU) = (0wx582 : Word32.word)
     | emit_foperateV (I.MULTSUD) = (0wx5E2 : Word32.word)
     | emit_foperateV (I.MULTSU) = (0wx5A2 : Word32.word)
     | emit_foperateV (I.SUBSSUD) = (0wx5C1 : Word32.word)
     | emit_foperateV (I.SUBSSU) = (0wx581 : Word32.word)
     | emit_foperateV (I.SUBTSUD) = (0wx5E1 : Word32.word)
     | emit_foperateV (I.SUBTSU) = (0wx5A1 : Word32.word)
   and emit_osf_user_palcode (I.BPT) = (0wx80 : Word32.word)
     | emit_osf_user_palcode (I.BUGCHK) = (0wx81 : Word32.word)
     | emit_osf_user_palcode (I.CALLSYS) = (0wx83 : Word32.word)
     | emit_osf_user_palcode (I.GENTRAP) = (0wxAA : Word32.word)
     | emit_osf_user_palcode (I.IMB) = (0wx86 : Word32.word)
     | emit_osf_user_palcode (I.RDUNIQUE) = (0wx9E : Word32.word)
     | emit_osf_user_palcode (I.WRUNIQUE) = (0wx9F : Word32.word)
   fun Memory {opc, ra, rb, disp} =
       let val rb = emit_GP rb
       in eWord32 ((opc << 0wx1A) + ((ra << 0wx15) + ((rb << 0wx10) + (disp && 0wxFFFF))))
       end
   and Split {le} =
       let
(*#line 428.22 "alpha/alpha.mdl"*)
           val i = MLTreeEval.valueOf le

(*#line 429.22 "alpha/alpha.mdl"*)
           val w = itow i

(*#line 430.22 "alpha/alpha.mdl"*)
           val hi = w ~>> 0wx10

(*#line 431.22 "alpha/alpha.mdl"*)
           val lo = w && 0wxFFFF
       in (if (lo < 0wx8000)
             then (hi, lo)
             else (hi + 0wx1, lo - 0wx10000))
       end
   and High {le} =
       let
(*#line 434.21 "alpha/alpha.mdl"*)
           val (hi, _) = Split {le=le}
       in hi
       end
   and Low {le} =
       let
(*#line 435.21 "alpha/alpha.mdl"*)
           val (_, lo) = Split {le=le}
       in lo
       end
   and LoadStore {opc, ra, rb, disp} =
       let
(*#line 437.12 "alpha/alpha.mdl"*)
           val disp =
               (case disp of
                 I.REGop rb => emit_GP rb
               | I.IMMop i => itow i
               | I.HILABop le => High {le=le}
               | I.LOLABop le => Low {le=le}
               | I.LABop le => itow (MLTreeEval.valueOf le)
               )
       in Memory {opc=opc, ra=ra, rb=rb, disp=disp}
       end
   and ILoadStore {opc, r, b, d} =
       let val r = emit_GP r
       in LoadStore {opc=opc, ra=r, rb=b, disp=d}
       end
   and FLoadStore {opc, r, b, d} =
       let val r = emit_FP r
       in LoadStore {opc=opc, ra=r, rb=b, disp=d}
       end
   and Jump {ra, rb, h, disp} =
       let val ra = emit_GP ra
           val rb = emit_GP rb
           val disp = emit_int disp
       in eWord32 ((ra << 0wx15) + ((rb << 0wx10) + ((h << 0wxE) + ((disp && 0wx3FFF) + 0wx68000000))))
       end
   and Memory_fun {opc, ra, rb, func} =
       let val ra = emit_GP ra
           val rb = emit_GP rb
       in eWord32 ((opc << 0wx1A) + ((ra << 0wx15) + ((rb << 0wx10) + func)))
       end
   and Branch {opc, ra, disp} =
       let val opc = emit_branch opc
           val ra = emit_GP ra
       in eWord32 ((opc << 0wx1A) + ((ra << 0wx15) + (disp && 0wx1FFFFF)))
       end
   and Bsr {ra, disp} =
       let val ra = emit_GP ra
       in eWord32 ((ra << 0wx15) + ((disp && 0wx1FFFFF) + 0wxD0000000))
       end
   and Fbranch {opc, ra, disp} =
       let val opc = emit_fbranch opc
           val ra = emit_FP ra
       in eWord32 ((opc << 0wx1A) + ((ra << 0wx15) + (disp && 0wx1FFFFF)))
       end
   and Operate0 {opc, ra, rb, func, rc} =
       let val ra = emit_GP ra
           val rb = emit_GP rb
           val rc = emit_GP rc
       in eWord32 ((opc << 0wx1A) + ((ra << 0wx15) + ((rb << 0wx10) + ((func << 0wx5) + rc))))
       end
   and Operate1 {opc, ra, lit, func, rc} =
       let val ra = emit_GP ra
           val rc = emit_GP rc
       in eWord32 ((opc << 0wx1A) + ((ra << 0wx15) + (((lit && 0wxFF) << 0wxD) + ((func << 0wx5) + (rc + 0wx1000)))))
       end
   and Operate {opc, ra, rb, func, rc} =
       (case rb of
         I.REGop rb => Operate0 {opc=opc, ra=ra, rb=rb, func=func, rc=rc}
       | I.IMMop i => Operate1 {opc=opc, ra=ra, lit=itow i, func=func, rc=rc}
       | I.HILABop le => Operate1 {opc=opc, ra=ra, lit=High {le=le}, func=func,
            rc=rc}
       | I.LOLABop le => Operate1 {opc=opc, ra=ra, lit=Low {le=le}, func=func,
            rc=rc}
       | I.LABop le => Operate1 {opc=opc, ra=ra, lit=itow (MLTreeEval.valueOf le),
            func=func, rc=rc}
       )
   and Foperate {opc, fa, fb, func, fc} =
       let val fa = emit_FP fa
           val fb = emit_FP fb
           val fc = emit_FP fc
       in eWord32 ((opc << 0wx1A) + ((fa << 0wx15) + ((fb << 0wx10) + ((func << 0wx5) + fc))))
       end
   and Funary {opc, fb, func, fc} =
       let val fb = emit_FP fb
           val fc = emit_FP fc
       in eWord32 ((opc << 0wx1A) + ((fb << 0wx10) + ((func << 0wx5) + (fc + 0wx3E00000))))
       end
   and Pal {func} = eWord32 func

(*#line 473.7 "alpha/alpha.mdl"*)
   fun disp lab = (itow (((Label.addrOf lab) - ( ! loc)) - 4)) ~>> 0wx2

(*#line 474.7 "alpha/alpha.mdl"*)
   val zeroR = Option.valOf (C.zeroReg CellsBasis.GP)
       fun emitter instr =
       let
   fun emitInstr (I.LDA{r, b, d}) = ILoadStore {opc=0wx8, r=r, b=b, d=d}
     | emitInstr (I.LDAH{r, b, d}) = ILoadStore {opc=0wx9, r=r, b=b, d=d}
     | emitInstr (I.LOAD{ldOp, r, b, d, mem}) = ILoadStore {opc=emit_load ldOp,
          r=r, b=b, d=d}
     | emitInstr (I.STORE{stOp, r, b, d, mem}) = ILoadStore {opc=emit_store stOp,
          r=r, b=b, d=d}
     | emitInstr (I.FLOAD{ldOp, r, b, d, mem}) = FLoadStore {opc=emit_fload ldOp,
          r=r, b=b, d=d}
     | emitInstr (I.FSTORE{stOp, r, b, d, mem}) = FLoadStore {opc=emit_fstore stOp,
          r=r, b=b, d=d}
     | emitInstr (I.JMPL({r, b, d}, list)) = Jump {h=0wx0, ra=r, rb=b, disp=d}
     | emitInstr (I.JSR{r, b, d, defs, uses, cutsTo, mem}) = Jump {h=0wx1,
          ra=r, rb=b, disp=d}
     | emitInstr (I.BSR{r, lab, defs, uses, cutsTo, mem}) = Bsr {ra=r, disp=disp lab}
     | emitInstr (I.RET{r, b, d}) = Jump {h=0wx2, ra=r, rb=b, disp=d}
     | emitInstr (I.BRANCH{b, r, lab}) = Branch {opc=b, ra=r, disp=disp lab}
     | emitInstr (I.FBRANCH{b, f, lab}) = Fbranch {opc=b, ra=f, disp=disp lab}
     | emitInstr (I.OPERATE{oper, ra, rb, rc}) =
       let
(*#line 578.15 "alpha/alpha.mdl"*)
           val (opc, func) = emit_operate oper
       in Operate {opc=opc, func=func, ra=ra, rb=rb, rc=rc}
       end
     | emitInstr (I.OPERATEV{oper, ra, rb, rc}) =
       let
(*#line 585.15 "alpha/alpha.mdl"*)
           val (opc, func) = emit_operateV oper
       in Operate {opc=opc, func=func, ra=ra, rb=rb, rc=rc}
       end
     | emitInstr (I.CMOVE{oper, ra, rb, rc}) = Operate {opc=0wx11, func=emit_cmove oper,
          ra=ra, rb=rb, rc=rc}
     | emitInstr (I.PSEUDOARITH{oper, ra, rb, rc, tmps}) = error "PSEUDOARITH"
     | emitInstr (I.FUNARY{oper, fb, fc}) =
       let
(*#line 603.15 "alpha/alpha.mdl"*)
           val (opc, func) = emit_funary oper
       in Funary {opc=opc, func=func, fb=fb, fc=fc}
       end
     | emitInstr (I.FOPERATE{oper, fa, fb, fc}) =
       let
(*#line 611.15 "alpha/alpha.mdl"*)
           val (opc, func) = emit_foperate oper
       in Foperate {opc=opc, func=func, fa=fa, fb=fb, fc=fc}
       end
     | emitInstr (I.FOPERATEV{oper, fa, fb, fc}) = Foperate {opc=0wx16, func=emit_foperateV oper,
          fa=fa, fb=fb, fc=fc}
     | emitInstr (I.FCMOVE{oper, fa, fb, fc}) = Foperate {opc=0wx17, func=emit_fcmove oper,
          fa=fa, fb=fb, fc=fc}
     | emitInstr (I.TRAPB) = Memory_fun {opc=0wx18, ra=zeroR, rb=zeroR, func=0wx0}
     | emitInstr (I.CALL_PAL{code, def, use}) = Pal {func=emit_osf_user_palcode code}
     | emitInstr (I.SOURCE{}) = ()
     | emitInstr (I.SINK{}) = ()
     | emitInstr (I.PHI{}) = ()
       in
           emitInstr instr
       end

   fun emitInstruction(I.ANNOTATION{i, ...}) = emitInstruction(i)
     | emitInstruction(I.INSTR(i)) = emitter(i)
     | emitInstruction(I.LIVE _)  = ()
     | emitInstruction(I.KILL _)  = ()
   | emitInstruction _ = error "emitInstruction"

   in  S.STREAM{beginCluster=init,
                pseudoOp=pseudoOp,
                emit=emitInstruction,
                endCluster=fail,
                defineLabel=doNothing,
                entryLabel=doNothing,
                comment=doNothing,
                exitBlock=doNothing,
                annotation=doNothing,
                getAnnotations=getAnnotations
               }
   end
end

