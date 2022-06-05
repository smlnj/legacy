(* amd64CG.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * AMD64 specific backend.
 *)

functor AMD64CG (

    structure CCallParams : sig
	val frameAlign : int
	val returnSmallStructsInRegs : bool
      end

    val abi_variant: string option

  ) = MachineGen (
    structure I          = AMD64Instr
    structure C          = I.C
    structure F          = AMD64CFG
    structure R          = AMD64CpsRegs
    structure CG         = Control.CG

    structure MachSpec   = AMD64Spec
    val abi_variant      = abi_variant
    structure ClientPseudoOps = AMD64ClientPseudoOps
    structure PseudoOps  = AMD64PseudoOps
    structure Ext        = SMLNJMLTreeExt (* generic extension *)
    structure CpsRegs    = AMD64CpsRegs
    structure InsnProps  = AMD64Props
    structure Asm        = AMD64AsmEmitter
    structure Shuffle    = AMD64Shuffle

    structure CCalls     = AMD64SVID_CCalls (structure T = AMD64MLTree)

    structure OmitFramePtr = AMD64OmitFramePointer(
	structure I = AMD64Instr
	structure CFG = AMD64CFG)

    val spill = CPSRegions.spill
    val stack = CPSRegions.stack

    fun error msg = MLRiscErrorMsg.error("AMD64CG",msg)

    val floats16ByteAligned = false

    structure MLTreeUtils = MLTreeUtils (
	structure T = AMD64MLTree
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

    structure MLTreeComp = AMD64Gen(
	structure I=AMD64Instr
	structure MLTreeUtils = MLTreeUtils
	structure ExtensionComp = SMLNJMLTreeExtComp (
	    structure I = AMD64Instr
	    structure T = AMD64MLTree
	    structure CFG = AMD64CFG
	    structure TS = AMD64MLTreeStream)
	structure MLTreeStream = AMD64MLTreeStream
	val floats16ByteAligned = floats16ByteAligned
      (* the signBit and negateSignBit constants are stored in the stack; see
       * base/runtime/mach-dep/AMD64.prim.asm.
       *)
	fun signBit ty = AMD64MLTree.ADD(64,
		AMD64MLTree.LI(IntInf.fromInt AMD64FrameLayout.signBitOffset),
		AMD64MLTree.REG(64, AMD64Cells.rsp))
	fun negateSignBit ty = AMD64MLTree.ADD(64,
		AMD64MLTree.LI(IntInf.fromInt AMD64FrameLayout.negSignBitOffset),
		AMD64MLTree.REG(64, AMD64Cells.rsp))
      )

    structure Jumps = AMD64Jumps(
	structure Instr=AMD64Instr
	structure AsmEmitter=AMD64AsmEmitter
	structure Eval=AMD64MLTreeEval
	structure Shuffle=AMD64Shuffle
	structure MCEmitter=AMD64MCEmitter)

    structure BackPatch = BackPatch(
	structure Jumps=Jumps
	structure Emitter=AMD64MCEmitter
	structure Props=InsnProps
	structure CFG = AMD64CFG
	structure Asm=AMD64AsmEmitter
	structure CodeString=CodeString)

    structure RA = RISC_RA (
        structure I         = AMD64Instr
	structure CFG       = AMD64CFG
	structure InsnProps = InsnProps
	structure Rewrite   = AMD64Rewrite(AMD64Instr)
	structure SpillInstr= AMD64SpillInstr(
	    structure I = AMD64Instr
	    structure Props = InsnProps
	    val floats16ByteAligned = floats16ByteAligned)
	structure Asm       = AMD64AsmEmitter
	structure SpillHeur = ChaitinSpillHeur	(* ChowHennessySpillHeur? *)
	structure Spill     = RASpill(
	    structure InsnProps = InsnProps
	    structure Asm = AMD64AsmEmitter)

	val sp    = I.C.stackptrR
	val spill = CPSRegions.spill

	structure SpillTable = SpillTable(AMD64Spec)

	val architecture = AMD64Spec.architecture

	datatype spillOperandKind = SPILL_LOC | CONST_VAL
	type spill_info = unit

	fun beforeRA _ = SpillTable.spillInit()

	fun pure _ = false

      (* make copies *)
	structure Int =
          struct
	    val avail     = AMD64CpsRegs.availR
	    val dedicated = AMD64CpsRegs.dedicatedR

	    fun mkDisp loc = I.Immed(Int32.fromInt(SpillTable.getRegLoc loc))

	    fun spillLoc{info, an, cell, id} = {
		    opnd=I.Displace{base=sp, disp=mkDisp(RAGraph.FRAME id), mem=spill},
		    kind=SPILL_LOC
		  }

	    val mode = RACore.NO_OPTIMIZATION
          end

	structure Float =
          struct
	    val avail     = AMD64CpsRegs.availF
	    val dedicated = AMD64CpsRegs.dedicatedF

	    fun mkDisp loc = I.Immed(Int32.fromInt(SpillTable.getFregLoc loc))

	    fun spillLoc(S, an, loc) =
		  I.Displace{base=sp, disp=mkDisp(RAGraph.FRAME loc), mem=spill}

	    val mode = RACore.NO_OPTIMIZATION
          end
      ) (* RA *)
  ) (* AMD64CG *)
