(* COPYRIGHT (c) 1999 Lucent Technologies, Bell Labs *)

(* Signature to capture various aspects of the MLRISC back end *) 

signature MACHINE = sig
  structure P : INSN_PROPERTIES
  structure Asm : INSTRUCTION_EMITTER
		  where I = P.I
  structure CFG : CONTROL_FLOW_GRAPH 
		  where I = Asm.I
		    and P = Asm.S.P

  type mlriscPhase = string * (CFG.cfg -> CFG.cfg) 
  val makePhase     : string * (CFG.cfg -> CFG.cfg) -> mlriscPhase
  val raPhase       : mlriscPhase 
  val optimizerHook : mlriscPhase list ref
  val finish        : unit -> unit
end (* MACHINE *)


