(* amd64MLTree.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Specialization of MLRisc for SMLNJ/AMD64
 *)

structure AMD64MLTree =
  MLTreeF(structure Constant = SMLNJConstant
          structure Region=CPSRegions
	  structure Extension=SMLNJMLTreeExt)

structure AMD64MLTreeEval =
    MLTreeEval
       (structure T = AMD64MLTree
	fun eq _ _ =  false
        val eqRext = eq		val eqFext = eq
        val eqCCext = eq	val eqSext = eq)

structure AMD64MLTreeHash =
    MLTreeHash
       (structure T = AMD64MLTree
        fun h _ _ = 0w0
        val hashRext = h	val hashFext = h
        val hashCCext = h       val hashSext = h)

structure AMD64GasPseudoOps =
   AMD64GasPseudoOps(structure T=AMD64MLTree
		   structure MLTreeEval=AMD64MLTreeEval)

structure AMD64ClientPseudoOps =
   SMLNJPseudoOps(structure Asm=AMD64GasPseudoOps)

structure AMD64PseudoOps = PseudoOps(structure Client = AMD64ClientPseudoOps)

structure AMD64Stream = InstructionStream(AMD64PseudoOps)

structure AMD64MLTreeStream =
    MLTreeStream
      (structure T = AMD64MLTree
       structure S = AMD64Stream)

(* specialised AMD64 instruction set *)
structure AMD64Instr = AMD64Instr(AMD64MLTree)

structure AMD64Props =
    AMD64Props
       (structure Instr=AMD64Instr
	structure MLTreeHash = AMD64MLTreeHash
        structure MLTreeEval = AMD64MLTreeEval)

structure AMD64Shuffle = AMD64Shuffle(AMD64Instr)

(* Assembly code emitter *)
structure AMD64AsmEmitter=
  AMD64AsmEmitter(structure Instr=AMD64Instr
		structure Shuffle=AMD64Shuffle
		structure MLTreeEval=AMD64MLTreeEval
		structure S = AMD64Stream
		val memRegBase=SOME(AMD64Instr.C.rsp))

(* Machine code emitter *)
structure AMD64MCEmitter =
  AMD64MCEmitter(structure Instr=AMD64Instr
	       structure Shuffle=AMD64Shuffle
	       structure AsmEmitter=AMD64AsmEmitter
	       structure MLTreeEval=AMD64MLTreeEval
	       val memRegBase=SOME(AMD64Instr.C.rsp))

(* Flowgraph data structure specialized to AMD64 instructions *)
structure AMD64CFG =
  ControlFlowGraph
     (structure I = AMD64Instr
      structure PseudoOps = AMD64PseudoOps
      structure GraphImpl = DirectedGraph
      structure InsnProps = AMD64Props
      structure Asm = AMD64AsmEmitter)
