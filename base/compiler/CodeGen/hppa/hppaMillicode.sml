functor HppaMillicode(Instr : HPPAINSTR) : HPPA_MILLICODE =
struct
  structure T = Instr.T
  structure C = Instr.C
  structure I = Instr
  structure Region = I.Region
  structure CB = CellsBasis

  val arg1 = C.GPReg 26
  val arg2 = C.GPReg 25
  val ra = C.GPReg 31				(* milli return address *)
  val rv = C.GPReg 29				(* milli return value *)
  val sp = C.stackptrR
  val stack = Region.stack

  val udivOffset = ~16
  val divOffset = ~20
  val mulOffset = ~24
  val muluOffset = ~112
  val cvti2dOffset = ~4

  fun copy {dst, src} = 
      I.COPY{k=CB.GP, sz=32, dst=dst, src=src, tmp=SOME(I.Direct(C.newReg()))}

  fun doMilliCall offset {rs, rt, rd} = let
    fun addList([], cs) = cs
      | addList(r::rs, cs) = addList(rs, C.addReg(r,cs))
    val tmpR = C.newReg()
    val defs = addList([rv,ra], C.empty)
    val uses = C.addReg(arg1, C.addReg(arg2, C.empty))
  in 
    [copy{dst=[arg1, arg2], src=[rs, rt]},
     I.loadi{li=I.LDW, r=C.stackptrR, i=I.IMMED offset, t=tmpR, mem=stack},
     I.ble{t=C.GPReg 31, cutsTo=[],
           b=tmpR, sr=5, d=I.IMMED 0, defs=defs, uses=uses, mem=stack},
     copy{dst=[rd], src=[rv]}]
  end

  val divu = doMilliCall udivOffset
  val divo = doMilliCall divOffset
  val mulo = doMilliCall mulOffset
  val mulu = doMilliCall muluOffset

  fun cvti2real fcnv {rs,fd} =
  let val tmpF = C.newFreg()
  in  [I.store{st=I.STW, b=C.stackptrR, d=I.IMMED cvti2dOffset,r=rs, mem=stack},
       I.fload{fl=I.FLDWS, b=C.stackptrR, d=cvti2dOffset, t=tmpF, mem=stack},
       I.fcnv{fcnv=fcnv, f=tmpF, t=fd}
      ]
  end

  val cvti2s = cvti2real I.FCNVXF_S
  val cvti2d = cvti2real I.FCNVXF_D
  val cvti2q = cvti2real I.FCNVXF_Q

end

