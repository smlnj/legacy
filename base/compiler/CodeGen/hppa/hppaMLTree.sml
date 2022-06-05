(* hppaMLTree.sml --- customize MLRISC for the HPPA.
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

structure HppaMLTree = 
  MLTreeF(structure Constant=SMLNJConstant
	  structure Region=CPSRegions
	  structure Extension=SMLNJMLTreeExt
         )

structure HppaMLTreeEval =
    MLTreeEval
       (structure T = HppaMLTree
	fun eq _ _ =  false
        val eqRext = eq		val eqFext = eq
        val eqCCext = eq	val eqSext = eq)
					    
structure HppaMLTreeHash = 
    MLTreeHash
       (structure T = HppaMLTree
        fun h _ _ = 0w0
        val hashRext = h	val hashFext = h
        val hashCCext = h       val hashSext = h)

structure HppaGasPseudoOps = 
   HppaGasPseudoOps(structure T=HppaMLTree
		   structure MLTreeEval=HppaMLTreeEval)

structure HppaClientPseudoOps =
   SMLNJPseudoOps(structure Asm=HppaGasPseudoOps)

structure HppaPseudoOps = PseudoOps(structure Client = HppaClientPseudoOps)
	      
structure HppaStream = InstructionStream(HppaPseudoOps)

structure HppaMLTreeStream = 
    MLTreeStream
      (structure T = HppaMLTree
       structure S = HppaStream)

(* specialised hppa instruction set *)
structure HppaInstr = HppaInstr(HppaMLTree)

structure HppaShuffle = HppaShuffle(HppaInstr)

structure HppaProps = HppaProps(structure HppaInstr=HppaInstr
				structure MLTreeEval=HppaMLTreeEval
				structure MLTreeHash=HppaMLTreeHash)

structure HppaAsmEmitter = 
  HppaAsmEmitter(structure Instr=HppaInstr
		 structure Shuffle=HppaShuffle
                 structure S=HppaStream 
		 structure MLTreeEval=HppaMLTreeEval)

structure HppaMCEmitter = 
  HppaMCEmitter(structure Instr=HppaInstr
		structure Assembler=HppaAsmEmitter
                structure Stream=HppaStream 
		structure MLTreeEval=HppaMLTreeEval
		structure CodeString=CodeString)

(* flowgraph data structure specialized to Hppa instructions *)
structure HppaCFG = 
  ControlFlowGraph
     (structure I = HppaInstr
      structure PseudoOps = HppaPseudoOps
      structure GraphImpl = DirectedGraph
      structure InsnProps = HppaProps
      structure Asm = HppaAsmEmitter)


