(* alpha32CG.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Alpha32CG =
  MachineGen
  ( structure I          = Alpha32Instr
    structure T          = Alpha32MLTree
    structure MachSpec   = Alpha32Spec
    val abi_variant      = NONE
    structure ClientPseudoOps = Alpha32ClientPseudoOps
    structure PseudoOps  = Alpha32PseudoOps
    structure Ext        = SMLNJMLTreeExt(* generic extension *)
    structure CpsRegs    = Alpha32CpsRegs
    structure InsnProps  = Alpha32Props
    structure Asm        = Alpha32AsmEmitter
    structure Shuffle    = Alpha32Shuffle

    structure CCalls     = UnimplementedCCallsFn
			       (structure T = Alpha32MLTree
				val impossible = ErrorMsg.impossible)

    structure OmitFramePtr = struct
      exception NotImplemented
      structure CFG=Alpha32CFG
      structure I=Alpha32Instr
      val vfp = CpsRegs.vfp
      fun omitframeptr _ = raise NotImplemented
    end
    structure CB = CellsBasis


    structure MLTreeComp=
       Alpha(structure AlphaInstr = Alpha32Instr
             structure AlphaMLTree = Alpha32MLTree
             structure PseudoInstrs = Alpha32PseudoInstrs
             structure ExtensionComp = SMLNJMLTreeExtComp
               (structure I = Alpha32Instr
                structure T = Alpha32MLTree
		structure CFG = Alpha32CFG
		structure TS = Alpha32MLTreeStream
               )
             val mode32bit = true (* simulate 32 bit mode *)
             val multCost = ref 8 (* just guessing *)
             val useMultByConst = ref false (* just guessing *)
             val byteWordLoadStores = ref false
             val SMLNJfloatingPoint = true (* must be true for SML/NJ *)
            )

    structure Jumps =
       AlphaJumps(structure Instr=Alpha32Instr
                  structure Shuffle=Alpha32Shuffle
		  structure MLTreeEval=Alpha32MLTreeEval)

    structure BackPatch =
       BBSched2(structure CFG=Alpha32CFG
                structure Jumps = Jumps
		structure Props = Alpha32Props
                structure Emitter = Alpha32MCEmitter)

    structure RA =
       RISC_RA
         (structure I         = Alpha32Instr
          structure CFG       = Alpha32CFG
          structure InsnProps = InsnProps
          structure Rewrite   = AlphaRewrite(Alpha32Instr)
	  structure SpillInstr= AlphaSpillInstr(Alpha32Instr)
          structure Asm       = Alpha32AsmEmitter
          structure SpillHeur = ChaitinSpillHeur
          structure Spill     = RASpill(structure InsnProps = InsnProps
                                        structure Asm = Alpha32AsmEmitter)

          val sp    = I.C.stackptrR
          val spill = CPSRegions.spill

          structure SpillTable = SpillTable(Alpha32Spec)

          val architecture = Alpha32Spec.architecture

	  datatype spillOperandKind = SPILL_LOC | CONST_VAL
	  type spill_info = unit

          fun beforeRA _ = SpillTable.spillInit()

          fun pure _ = false

          (* make copies *)
          structure Int =
          struct
              val avail     = Alpha32CpsRegs.availR
              val dedicated = Alpha32CpsRegs.dedicatedR

	      fun mkDisp loc = T.LI(T.I.fromInt (32, SpillTable.getRegLoc loc))

              fun spillLoc{info, an, cell, id} =
		  {opnd=I.Displace{base=sp, disp=mkDisp(RAGraph.FRAME id), mem=spill},
		   kind=SPILL_LOC}

              val mode = RACore.NO_OPTIMIZATION
          end

          structure Float =
          struct
              val avail     = Alpha32CpsRegs.availF
              val dedicated = Alpha32CpsRegs.dedicatedF

	      fun mkDisp loc = T.LI(T.I.fromInt (32, SpillTable.getFregLoc loc))
	      fun spillLoc(S, an, loc) =
		I.Displace{base=sp, disp=mkDisp(RAGraph.FRAME loc), mem=spill}

              val mode = RACore.NO_OPTIMIZATION
          end
         )
  )
