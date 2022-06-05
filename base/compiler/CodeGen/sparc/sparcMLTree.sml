(* sparcMLTree.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * customize MLRISC for the Sparc.
 *)

structure SparcMLTree =
  MLTreeF(structure Constant=SMLNJConstant
	  structure Region=CPSRegions
	  structure Extension=Sparc_SMLNJMLTreeExt
         )

structure SparcMLTreeEval =
    MLTreeEval
       (structure T = SparcMLTree
	fun eq _ _ =  false
        val eqRext = eq		val eqFext = eq
        val eqCCext = eq	val eqSext = eq)

structure SparcMLTreeHash =
    MLTreeHash
       (structure T = SparcMLTree
        fun h _ _ = 0w0
        val hashRext = h	val hashFext = h
        val hashCCext = h       val hashSext = h)

structure SparcGasPseudoOps =
   SparcGasPseudoOps(structure T=SparcMLTree
		   structure MLTreeEval=SparcMLTreeEval)

structure SparcClientPseudoOps =
   SMLNJPseudoOps(structure Asm=SparcGasPseudoOps)

structure SparcPseudoOps = PseudoOps(structure Client = SparcClientPseudoOps)

structure SparcStream = InstructionStream(SparcPseudoOps)

structure SparcMLTreeStream =
    MLTreeStream
      (structure T = SparcMLTree
       structure S = SparcStream)

(* specialised sparc instruction set *)
structure SparcInstr = SparcInstr(SparcMLTree)

structure SparcPseudoInstrs = SparcPseudoInstrs(SparcInstr)

structure SparcProps =
  SparcProps
    (structure SparcInstr = SparcInstr
     structure MLTreeEval = SparcMLTreeEval
     structure MLTreeHash = SparcMLTreeHash)


structure SparcShuffle = SparcShuffle(SparcInstr)

structure SparcAsmEmitter =
  SparcAsmEmitter(structure Instr=SparcInstr
		  structure Shuffle=SparcShuffle
                  structure S = SparcStream
		  structure MLTreeEval=SparcMLTreeEval
                  val V9 = false)

structure SparcMCEmitter =
  SparcMCEmitter(structure Instr=SparcInstr
		 structure Assembler=SparcAsmEmitter
                 structure Stream = SparcStream
		 structure MLTreeEval=SparcMLTreeEval
		 structure CodeString=CodeString)

(* flowgraph data structure specialized to Sparc instructions *)
structure SparcCFG =
  ControlFlowGraph
     (structure I = SparcInstr
      structure PseudoOps = SparcPseudoOps
      structure GraphImpl = DirectedGraph
      structure InsnProps = SparcProps
      structure Asm = SparcAsmEmitter)
