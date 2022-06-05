(* check-gc.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module checks that no other values aside from
 * the standard GC calling convention registers, can be live across
 * a call GC instruction.   Call GC blocks and instructions are assumed
 * to be marked with the special CALLGC annotation.
 *)

signature CHECK_GC =
sig
   structure CFG : CONTROL_FLOW_GRAPH
   val checkGC : CFG.cfg -> CFG.cfg
end

functor CheckGCFn(
      structure Asm : INSTRUCTION_EMITTER
      structure CFG : CONTROL_FLOW_GRAPH
          where I = Asm.I
	    and P = Asm.S.P
      structure InsnProps : INSN_PROPERTIES
          where I = CFG.I
      structure CpsRegs : CPSREGS
      val gcParamRegs : CpsRegs.T.rexp list
     ) : CHECK_GC =
struct

   structure CFG   = CFG
   structure L     = Liveness(CFG)
   structure I     = CFG.I
   structure C     = I.C
   structure G     = Graph
   structure H     = IntHashTable
   structure CB    = CellsBasis
   structure CS    = CB.CellSet
   structure T     = CpsRegs.T

   (* List of cells which are gc roots *and* dedicated registers *)
   val gc_roots =
           CB.SortedCells.uniq(
             List.foldr (fn (T.REG(_,r),S) => r::S |
                                 (_, S)    => S)
                  (CpsRegs.dedicatedR @ CpsRegs.dedicatedF) gcParamRegs)

   (* def/use for integer and floating point registers *)
   val defUseR = InsnProps.defUse CB.GP
   val defUseF = InsnProps.defUse CB.FP

   (* Flag for debugging this phase *)
   val debug_check_gc = MLRiscControl.mkFlag
         ("debug-check-gc", "Check GC debugging")
   val check_gc       = MLRiscControl.mkFlag
         ("check-gc", "Turn on GC checking")

   (* Dump a block of instructions *)
   fun showBlock (CFG.BLOCK{insns, ...}) =
   let val Asm.S.STREAM{emit, ...} =
           AsmStream.withStream TextIO.stdOut Asm.makeStream []
   in  app emit (rev (!insns))
   end

   (* Dump one instruction *)
   fun showInstr instr =
   let val Asm.S.STREAM{emit, ...} =
           AsmStream.withStream TextIO.stdOut Asm.makeStream []
   in  emit instr
   end

   (*
    * Checks gc
    *)
   fun checkIt(cfg as G.GRAPH graph) =
   let
       (* DefUse for one instruction *)
       fun defUse i =
       let val (d1,u1) = defUseR i
           val (d2,u2) = defUseF i
       in
           (d1@d2, u1@u2)
       end

       (* Cellset -> list *)
       val getCell = CS.toCellList

       (* Compute liveness for all register kinds *)
       val {liveIn, liveOut} =
           L.liveness { defUse=defUseR, getCell=getCell } cfg

       (* Pretty-print a list of cells *)
       fun cellsToString S = CS.toString(List.foldr CS.add CS.empty S)

       (* Check if an instruction is a call GC instruction *)
       fun isCallGC i =
           let val (_, a) = InsnProps.getAnnotations i
           in  #contains MLRiscAnnotations.CALLGC a
           end

       (* Check a call gc instruction *)
       fun checkCallGC (instr, liveOut, liveIn, block) =
       let val () = if !debug_check_gc then
                        (print ("live in="^cellsToString(liveIn)^"\n");
                         showInstr(instr);
                         print ("live out="^cellsToString(liveOut)^"\n"))
                    else ()
           val liveAcross = CB.SortedCells.difference(liveOut, gc_roots)
       in  if not(CB.SortedCells.isEmpty liveAcross) then
              (print("_______________________________________\n");
               print("WARNING: error in GC protocol:\n");
               print ("gc roots+dedicated="^cellsToString(gc_roots)^"\n");
               print ("live in="^cellsToString(liveIn)^"\n");
               showInstr(instr);
               print ("live out="^cellsToString(liveOut)^"\n");
               print ("In block:\n");
               showBlock(block);
               print("_______________________________________\n");
               ErrorMsg.impossible("CheckGC.gc protocol error")
              )
           else ()
       end

       (* Scan a GC block backwards and look for CALL GC instructions *)
       fun scanBlock (b,block as CFG.BLOCK{insns, ...}) =
       let val live = H.lookup liveOut b

           fun scan(live, []) = ()
             | scan(live, i::is) =
               let
                   val live' = L.liveStep defUse (i, live)
               in
                   if isCallGC i then checkCallGC(i, live, live',block) else ();
                   scan(live', is)
               end

           val () = if !debug_check_gc then
                      (print("Liveout="^cellsToString(live)^"\n");
                       showBlock(block))
                   else ()
       in
           scan(live, !insns)
       end

       (*
        * GC blocks are marked with the special annotation CALLGC.
        *)
       fun isGCBlock(b,CFG.BLOCK{annotations, ...}) =
           #contains MLRiscAnnotations.CALLGC (!annotations)

       (*
        * Check GC blocks
       *)
       fun checkBlock (b,b') =
           if isGCBlock(b,b') then scanBlock(b,b') else ()
   in
       (* Locate and check all blocks in the flowgraph *)
       #forall_nodes graph checkBlock
   end

   (* Main entry point *)
   fun checkGC cfg =
       (if !check_gc then checkIt cfg else (); cfg)

end
