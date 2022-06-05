(* Copyright 1999, Lucent Technologies, Bell Labs *)

structure MLRISCOptimizer = struct
  structure Glue = 
    MLRISCGlue(open Compiler.Machine
               structure FreqProps = FreqProps(P)
              )
  val _ = Compiler.Machine.optimizerHook := SOME(Glue.codegen)
end
