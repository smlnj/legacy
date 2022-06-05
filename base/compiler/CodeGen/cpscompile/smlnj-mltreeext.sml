(* smlnj-mltreeext.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure SMLNJMLTreeExt : SMLNJ_MLTREE_EXT =
  struct
    type ('s,'r,'f,'c) sx = unit
    type ('s,'r,'f,'c) rx = unit
    type ('s,'r,'f,'c) ccx = unit
    datatype ('s,'r,'f,'c) fx
     = FSINE of 'f
     | FCOSINE of 'f
     | FTANGENT of 'f
  end

(* This is the default extension compilation module
 * used for all architectures except the x86.
 *)
functor SMLNJMLTreeExtComp
   (structure TS  : MLTREE_STREAM
    structure CFG : CONTROL_FLOW_GRAPH
		    where P = TS.S.P
   ) : MLTREE_EXTENSION_COMP =
struct
   structure TS = TS
   structure T  = TS.T
   structure I = CFG.I
   structure CFG = CFG

   type reducer =
     (I.instruction,I.C.cellset,I.operand,I.addressing_mode,CFG.cfg) TS.reducer

   fun unimplemented _ = MLRiscErrorMsg.impossible "SMLNJMLTreeExtComp"

   val compileSext  = unimplemented
   val compileRext  = unimplemented
   val compileFext  = unimplemented
   val compileCCext = unimplemented
end

