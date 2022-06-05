(* alpha32MLTree.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Alpha32MLTree =
  MLTreeF(structure Constant=SMLNJConstant
	  structure Region=CPSRegions
	  structure Extension=SMLNJMLTreeExt
         )

structure Alpha32MLTreeEval =
   MLTreeEval
      (structure T = Alpha32MLTree
       fun eq _ _ =  false
       val eqRext = eq	 val eqFext = eq
       val eqCCext = eq	 val eqSext = eq)

structure Alpha32MLTreeHash =
  MLTreeHash
     (structure T = Alpha32MLTree
      fun h _ _ = 0w0
      val hashRext = h	 val hashFext = h
      val hashCCext = h  val hashSext = h)


structure Alpha32GasPseudoOps =
   AlphaGasPseudoOps(structure T=Alpha32MLTree
		     structure MLTreeEval = Alpha32MLTreeEval)

structure Alpha32ClientPseudoOps =
   SMLNJPseudoOps(structure Asm = Alpha32GasPseudoOps)

structure Alpha32PseudoOps = PseudoOps(structure Client=Alpha32ClientPseudoOps)

structure Alpha32Stream = InstructionStream(Alpha32PseudoOps)

structure Alpha32MLTreeStream =
  MLTreeStream
     (structure T = Alpha32MLTree
      structure S = Alpha32Stream)

(* specialised alpha32 instruction set *)
structure Alpha32Instr = AlphaInstr(Alpha32MLTree)

structure Alpha32Props =
   AlphaProps(structure Instr=Alpha32Instr
	      structure MLTreeHash=Alpha32MLTreeHash
	      structure MLTreeEval=Alpha32MLTreeEval)

structure Alpha32Shuffle = AlphaShuffle(Alpha32Instr)


structure Alpha32AsmEmitter=
  AlphaAsmEmitter(structure Instr=Alpha32Instr
	          structure PseudoOps=Alpha32PseudoOps
                  structure Stream=Alpha32Stream
		  structure Shuffle = Alpha32Shuffle
		  structure MLTreeEval=Alpha32MLTreeEval
		  structure S=Alpha32Stream)

structure Alpha32MCEmitter =
  AlphaMCEmitter(structure Instr=Alpha32Instr
		 structure PseudoOps=Alpha32PseudoOps
	         structure MLTreeEval=Alpha32MLTreeEval
                 structure Stream=Alpha32Stream
		 structure CodeString=CodeString)

structure Alpha32PseudoInstrs = Alpha32PseudoInstrs(Alpha32Instr)

(* Flowgraph data structure specialized to DEC alpha instructions *)
structure Alpha32CFG =
  ControlFlowGraph
     (structure I = Alpha32Instr
      structure PseudoOps = Alpha32PseudoOps
      structure GraphImpl = DirectedGraph
      structure InsnProps = Alpha32Props
      structure Asm = Alpha32AsmEmitter)
