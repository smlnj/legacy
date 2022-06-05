(*
 * Hppa specific backend
 *)
structure HppaCG = 
  MachineGen
  ( structure MachSpec   = HppaSpec
    val abi_variant      = NONE
    structure T          = HppaMLTree
    structure CB         = CellsBasis
    structure ClientPseudoOps = HppaClientPseudoOps
    structure PseudoOps  = HppaPseudoOps
    structure Ext        = SMLNJMLTreeExt(* generic extension *)
    structure CpsRegs    = HppaCpsRegs
    structure InsnProps  = HppaProps
    structure Asm        = HppaAsmEmitter
    structure Shuffle    = HppaShuffle

    structure CCalls     = UnimplementedCCallsFn
			       (structure T = HppaMLTree
				val impossible = ErrorMsg.impossible)

    structure OmitFramePtr = struct
      exception NotImplemented
      structure CFG=HppaCFG
      structure I=HppaInstr
      val vfp = CpsRegs.vfp
      fun omitframeptr _ = raise NotImplemented
    end

    structure HppaMillicode = HppaMillicode(HppaInstr)

    structure HppaLabelComp = HppaLabelComp(HppaInstr)

    structure MLTreeComp=
       Hppa(structure HppaInstr = HppaInstr
            structure HppaMLTree = HppaMLTree
            structure MilliCode=HppaMillicode
            structure LabelComp=HppaLabelComp
            structure ExtensionComp = SMLNJMLTreeExtComp
               (structure I = HppaInstr
                structure T = HppaMLTree
		structure CFG = HppaCFG
		structure TS = HppaMLTreeStream
               )
            val costOfMultiply = ref 7
            val costOfDivision = ref 7
           )

    structure Jumps =
       HppaJumps(structure Instr=HppaInstr
		 structure MLTreeEval=HppaMLTreeEval
                 structure Shuffle=HppaShuffle)

    structure BackPatch =
       SpanDependencyResolution
         (structure CFG = HppaCFG
          structure Jumps     = Jumps
          structure Emitter   = HppaMCEmitter
          structure DelaySlot = HppaDelaySlots
             (structure I=HppaInstr
              structure P=InsnProps)
          structure Props = InsnProps
	  structure Asm = HppaAsmEmitter
         )

    structure RA = 
       RISC_RA
         (structure I         = HppaInstr
          structure CFG       = HppaCFG
          structure InsnProps = InsnProps 
          structure Rewrite   = HppaRewrite(HppaInstr) 
	  structure SpillInstr= HppaSpillInstr(HppaInstr)
          structure Asm       = HppaAsmEmitter
          structure SpillHeur = ChaitinSpillHeur
          structure Spill     = RASpill(structure InsnProps = InsnProps
                                        structure Asm = HppaAsmEmitter)

          (* NOTE: the spill offset grows backwards on the stack! 
           *)
          structure SpillTable = SpillTable(HppaSpec)

	  datatype spillOperandKind = SPILL_LOC | CONST_VAL
	  type spill_info = unit

          fun beforeRA _ = SpillTable.spillInit()

          val architecture = HppaSpec.architecture

          val sp        = I.C.stackptrR
          val spill     = CPSRegions.spill
          val tmpR      = I.C.asmTmpR
          val itow      = Word.fromInt
          val wtoi      = Word.toIntX
          fun low11(n)  = wtoi(Word.andb(itow n, 0wx7ff))
          fun high21(n) = wtoi(Word.~>>(itow n, 0w11))

          fun pure(I.INSTR(I.LOAD _)) = true
            | pure(I.INSTR(I.LOADI _)) = true
            | pure(I.INSTR(I.FLOAD _)) = true
            | pure(I.INSTR(I.FLOADX _)) = true
            | pure(I.INSTR(I.ARITH _)) = true
            | pure(I.INSTR(I.ARITHI _)) = true
            | pure(I.INSTR(I.FARITH _)) = true
            | pure(I.INSTR(I.FUNARY _)) = true
            | pure(I.INSTR(I.FCNV _)) = true
            | pure(I.ANNOTATION{i,...}) = pure i
            | pure _ = false
 
          (* make copy *) 
          structure Int =
          struct
             val avail = HppaCpsRegs.availR
             val dedicated = HppaCpsRegs.dedicatedR

	     fun mkDisp loc = T.LI(T.I.fromInt(32, ~(SpillTable.getRegLoc loc)))
             fun spillLoc{info, an, cell, id} = 
		  {opnd=I.Displace{base=sp, disp=mkDisp(RAGraph.FRAME id), mem=spill},
		   kind=SPILL_LOC}

             val mode = RACore.NO_OPTIMIZATION
          end

          structure Float = 
          struct
             val avail = HppaCpsRegs.availF
             val dedicated = HppaCpsRegs.dedicatedF
 
	     fun mkDisp loc = T.LI (T.I.fromInt(32,  ~(SpillTable.getFregLoc loc)))
 	     fun spillLoc(S, an, loc) = 
		I.Displace{base=sp, disp=mkDisp(RAGraph.FRAME loc), mem=spill}

             val mode = RACore.NO_OPTIMIZATION
          end
         )
  )
