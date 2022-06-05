(* sparcCG.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Sparc specific backend
 *)

structure SparcCG =
  MachineGen
  ( structure MachSpec   = SparcSpec
    val abi_variant      = NONE
    structure T          = SparcMLTree
    structure CB	 = CellsBasis
    structure ClientPseudoOps = SparcClientPseudoOps
    structure PseudoOps  = SparcPseudoOps
    structure Ext        = Sparc_SMLNJMLTreeExt(* sparc specific *)
    structure CpsRegs    = SparcCpsRegs
    structure InsnProps  = SparcProps
    structure Asm        = SparcAsmEmitter
    structure Shuffle    = SparcShuffle

    structure CCalls     =
      Sparc_CCalls (structure T = SparcMLTree  fun ix x = x)

    structure OmitFramePtr = struct
      structure CFG=SparcCFG
      structure I=SparcInstr
      val vfp = CpsRegs.vfp
      (* no rewriting necessary, backend uses %fp instead of %sp *)
      fun omitframeptr _ = ()
    end

    structure MLTreeUtils =
      MLTreeUtils(
	structure T = SparcMLTree
	fun hashSext  _ _ = 0w0
	fun hashRext  _ _ = 0w0
	fun hashFext  _ _ = 0w0
	fun hashCCext _ _ = 0w0
	(* Equality extensions *)
	fun eqSext  _ _ = false
	fun eqRext  _ _ = false
	fun eqFext  _ _ = false
	fun eqCCext _ _ = false
	(* Pretty printing extensions *)
	fun showSext  _ _ = ""
	fun showRext  _ _ = ""
	fun showFext  _ _ = ""
	fun showCCext _ _ = "")


    structure MLTreeComp =
       Sparc(structure SparcInstr = SparcInstr
             structure SparcMLTree = SparcMLTree
             structure PseudoInstrs = SparcPseudoInstrs
             structure ExtensionComp = SparcMLTreeExtComp
               (structure I = SparcInstr
                structure T = SparcMLTree
                structure Stream = SparcMLTreeStream
		structure CFG = SparcCFG
               )
             val V9 = false
             val muluCost = ref 5
             val multCost = ref 3
             val divuCost = ref 5
             val divtCost = ref 5
             val registerwindow = ref false
             val useBR = ref false
            )

    structure Jumps =
       SparcJumps(structure Instr=SparcInstr
		  structure MLTreeEval=SparcMLTreeEval
                  structure Shuffle=SparcShuffle)

    structure BackPatch =
       SpanDependencyResolution
         (structure CFG	      = SparcCFG
          structure Jumps     = Jumps
          structure Emitter   = SparcMCEmitter
          structure DelaySlot = SparcDelaySlots
             (structure I=SparcInstr
              structure P=InsnProps)
          structure Props = InsnProps
	  structure Asm = SparcAsmEmitter
         )

    structure RA =
       RISC_RA
         (structure I         = SparcInstr
          structure CFG       = SparcCFG
          structure InsnProps = InsnProps
          structure Rewrite   = SparcRewrite(SparcInstr)
	  structure SpillInstr= SparcSpillInstr(SparcInstr)
          structure Asm       = SparcAsmEmitter
          structure SpillHeur = ChaitinSpillHeur
          structure Spill     = RASpill(structure InsnProps = InsnProps
                                        structure Asm = SparcAsmEmitter)

          structure SpillTable = SpillTable(SparcSpec)
          val fp = I.C.frameptrR
          val spill = CPSRegions.spill
	  datatype spillOperandKind = SPILL_LOC | CONST_VAL
	  type spill_info = unit
          fun beforeRA _ = SpillTable.spillInit()

          val architecture = SparcSpec.architecture

          fun pure(I.ANNOTATION{i,...}) = pure i
            | pure(I.INSTR(I.LOAD _)) = true
            | pure(I.INSTR(I.FLOAD _)) = true
            | pure(I.INSTR(I.SETHI _)) = true
            | pure(I.INSTR(I.SHIFT _)) = true
            | pure(I.INSTR(I.FPop1 _)) = true
            | pure(I.INSTR(I.FPop2 _)) = true
            | pure _ = false

          (* make copy *)
          structure Int =
          struct
             val avail     = SparcCpsRegs.availR
             val dedicated = SparcCpsRegs.dedicatedR

	     fun mkDisp loc = T.LI(T.I.fromInt(32, SpillTable.getRegLoc loc))
             fun spillLoc{info, an, cell, id} =
		  {opnd=I.Displace{base=fp, disp=mkDisp(RAGraph.FRAME id), mem=spill},
		   kind=SPILL_LOC}

             val mode = RACore.NO_OPTIMIZATION
          end

          structure Float =
          struct
             val avail     = SparcCpsRegs.availF
             val dedicated = SparcCpsRegs.dedicatedF

	      fun mkDisp loc = T.LI(T.I.fromInt(32, SpillTable.getFregLoc loc))

             fun spillLoc(S, an, loc) =
		I.Displace{base=fp, disp=mkDisp(RAGraph.FRAME loc), mem=spill}

             val mode = RACore.NO_OPTIMIZATION
          end
         )
  )
