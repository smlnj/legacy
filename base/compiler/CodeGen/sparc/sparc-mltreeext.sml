(* sparc-mltreeext.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Sparc_SMLNJMLTreeExt : SMLNJ_MLTREE_EXT =
  struct
   type ('s,'r,'f,'c) sx = ('s,'r,'f,'c) SparcInstrExt.sext
   type ('s,'r,'f,'c) rx = unit
   type ('s,'r,'f,'c) ccx = unit
   datatype ('s,'r,'f,'c) fx
     = FSINE of 'f
     | FCOSINE of 'f
     | FTANGENT of 'f
  end
