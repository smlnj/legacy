(* x86MLTree.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* MLTree specialization *)
structure X86MLTree =
  MLTreeF(structure Constant = SMLNJConstant
          structure Region=CPSRegions
	  structure Extension=SMLNJMLTreeExt)

structure X86MLTreeEval =
    MLTreeEval
       (structure T = X86MLTree
	fun eq _ _ =  false
        val eqRext = eq		val eqFext = eq
        val eqCCext = eq	val eqSext = eq)

structure X86MLTreeHash =
    MLTreeHash
       (structure T = X86MLTree
        fun h _ _ = 0w0
        val hashRext = h	val hashFext = h
        val hashCCext = h       val hashSext = h)

structure X86GasPseudoOps =
   X86GasPseudoOps(structure T=X86MLTree
		   structure MLTreeEval=X86MLTreeEval)

structure X86ClientPseudoOps =
   SMLNJPseudoOps(structure Asm=X86GasPseudoOps)

structure X86PseudoOps = PseudoOps(structure Client = X86ClientPseudoOps)

structure X86Stream = InstructionStream(X86PseudoOps)

structure X86MLTreeStream =
    MLTreeStream
      (structure T = X86MLTree
       structure S = X86Stream)


(* specialised X86 instruction set *)
structure X86Instr = X86Instr(X86MLTree)

structure X86MemRegs = X86MemRegs(X86Instr)

structure X86Props =
    X86Props
       (structure Instr=X86Instr
	structure MLTreeHash = X86MLTreeHash
        structure MLTreeEval = X86MLTreeEval)

structure X86Rewrite = X86Rewrite(X86Instr)
structure X86Shuffle = X86Shuffle(X86Instr)

(* Assembly code emmitter *)
structure X86AsmEmitter=
  X86AsmEmitter(structure Instr=X86Instr
		structure Shuffle=X86Shuffle
		structure MemRegs=X86MemRegs
		structure MLTreeEval=X86MLTreeEval
		structure S = X86Stream
		val memRegBase=SOME(X86Instr.C.esp))


(* Machine code emitter *)
structure X86MCEmitter =
  X86MCEmitter(structure Instr=X86Instr
	       structure Shuffle=X86Shuffle
	       structure AsmEmitter=X86AsmEmitter
	       structure MemRegs=X86MemRegs
	       structure MLTreeEval=X86MLTreeEval
	       val memRegBase=SOME(X86Instr.C.esp))

(* Flowgraph data structure specialized to X86 instructions *)
structure X86CFG =
  ControlFlowGraph
     (structure I = X86Instr
      structure PseudoOps = X86PseudoOps
      structure GraphImpl = DirectedGraph
      structure InsnProps = X86Props
      structure Asm = X86AsmEmitter)

