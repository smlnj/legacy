functor HppaLabelComp(Instr : HPPAINSTR) : LABEL_COMP = 
struct
  structure T = Instr.T
  structure I = Instr
  structure C = I.C

  type reduce = 
    { stm: T.stm -> unit, 
      rexp: T.rexp -> CellsBasis.cell, 
      emit: I.instruction -> unit }

  fun error msg = ErrorMsg.impossible("HppaLabelComp." ^ msg)

  fun ldLabelOpnd emit {label, pref} = I.LabExp(label,I.T)
  fun ldLabelEA emit lexp = (C.r0, I.LabExp(lexp, I.T))

  (* should change the return pointer to 2 to follow HPUX conventions *)
  fun doCall({stm,rexp,emit}:reduce, 
             T.CALL{funct, targets, defs, uses, region, ...}) = 
  let
        val addCCreg = CellsBasis.CellSet.add 
	fun live([], acc) = acc
	  | live(T.GPR(T.REG(_, r))::regs, acc) = live(regs, C.addReg(r, acc))
	  | live(T.FPR(T.FREG(_, f))::regs, acc) = live(regs, C.addFreg(f, acc))
	  | live(T.CCR(T.CC(_,c))::regs, acc) = live(regs, addCCreg(c, acc))
	  | live(T.CCR(T.FCC(_,c))::regs, acc) = live(regs, addCCreg(c, acc))
	  | live(_::regs, acc) = live(regs, acc)
	val returnPtr = C.GPReg 31
	val defs = C.addReg(returnPtr, live(defs, C.empty))
	val uses = live(uses, C.empty)
      in emit(I.ble{b=rexp funct, d=I.IMMED 0, sr=5, t=returnPtr, 
                    defs=defs, uses=uses, cutsTo=[], mem=region})
      end
    | doCall _ = error "doCall"

  fun doJmp({stm,rexp,emit}:reduce, T.JMP(exp, labs)) =
    (case exp
     of T.LABEL lab => emit(I.b{lab=lab,n=true})
      | _ => emit(I.bv{b=rexp(exp), x=C.GPReg 0, labs=labs, n=true})
    (*esac*))
    | doJmp _ = error "doJmp"

end
		      
