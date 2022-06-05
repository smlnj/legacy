(* ppcMLTree.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PPCMLTree = MLTreeF(
    structure Constant = SMLNJConstant
    structure Region = CPSRegions
    structure Extension = SMLNJMLTreeExt)

structure PPCMLTreeEval = MLTreeEval(
    structure T = PPCMLTree
    fun eq _ _ =  false
    val eqRext = eq
    val eqFext = eq
    val eqCCext = eq
    val eqSext = eq)

structure PPCMLTreeHash = MLTreeHash(
    structure T = PPCMLTree
    fun h _ _ = 0w0
    val hashRext = h
    val hashFext = h
    val hashCCext = h
    val hashSext = h)

structure PPCGasPseudoOps = PPCGasPseudoOps(
    structure T = PPCMLTree
    structure MLTreeEval = PPCMLTreeEval)

structure PPCClientPseudoOps = SMLNJPseudoOps(structure Asm = PPCGasPseudoOps)

structure PPCPseudoOps = PseudoOps(structure Client = PPCClientPseudoOps)

structure PPCStream = InstructionStream(PPCPseudoOps)

structure PPCMLTreeStream = MLTreeStream(
    structure T = PPCMLTree
    structure S = PPCStream)

(* specialised powerpc instruction set *)
structure PPCInstr = PPCInstr(PPCMLTree)

structure PPCProps = PPCProps(
    structure PPCInstr = PPCInstr
    structure MLTreeEval = PPCMLTreeEval
    structure MLTreeHash = PPCMLTreeHash)

structure PPCShuffle = PPCShuffle(PPCInstr)

structure PPCAsmEmitter = PPCAsmEmitter(
    structure Instr = PPCInstr
    structure PseudoOps = PPCPseudoOps
    structure S = PPCStream
    structure MLTreeEval = PPCMLTreeEval
    structure Shuffle = PPCShuffle)

structure PPCMCEmitter = PPCMCEmitter(
    structure Instr = PPCInstr
    structure PseudoOps = PPCPseudoOps
    structure Stream = PPCStream
    structure MLTreeEval = PPCMLTreeEval
    structure CodeString = CodeString)

(* Flowgraph data structure specialized to DEC alpha instructions *)
structure PPCCFG = ControlFlowGraph(
    structure I = PPCInstr
    structure PseudoOps = PPCPseudoOps
    structure GraphImpl = DirectedGraph
    structure InsnProps = PPCProps
    structure Asm = PPCAsmEmitter)
