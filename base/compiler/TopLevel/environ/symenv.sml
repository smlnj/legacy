(* symenv.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure SymbolicEnv : SYMENV = PidEnvFn (type binding = FLINT.prog)
