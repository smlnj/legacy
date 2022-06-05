(* sparcMLTreeExtComp.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor SparcMLTreeExtComp
   (structure T : MLTREE
   		where Extension = Sparc_SMLNJMLTreeExt
    structure I : SPARCINSTR
    		where T = T
    structure Stream : MLTREE_STREAM
                where T = I.T
    structure CFG : CONTROL_FLOW_GRAPH
    		where I = I
                  and P = Stream.S.P
   ) : MLTREE_EXTENSION_COMP =
struct
   structure TS = Stream
   structure I = I
   structure T = I.T
   structure C = I.C
   structure Ext = Sparc_SMLNJMLTreeExt
   structure CFG = CFG
   structure SparcCompInstrExt =
     SparcCompInstrExt(structure I = I structure CFG = CFG structure TS=Stream)

   type reducer =
     (I.instruction,C.cellset,I.operand,I.addressing_mode, CFG.cfg) TS.reducer

   fun unimplemented _ = MLRiscErrorMsg.impossible "SparcMLTreeExtComp"

   val compileSext  = SparcCompInstrExt.compileSext
   val compileRext  = unimplemented
   val compileCCext = unimplemented
   val compileFext  = unimplemented
end
