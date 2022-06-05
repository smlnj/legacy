(* edcontrol.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

signature ELABDATA_CONTROL =
sig

  val saveLvarNames : bool ref

  val eedebugging : bool ref
      (* EntityEnv *)

  val mudebugging : bool ref
      (* ModuleUtil *)

  val tudebugging : bool ref
      (* TypesUtil *)

  val typesInternals : bool ref
      (* show types internal reps *)

  val modulesInternals : bool ref
      (* show modules internal reps *)

  val absynInternals : bool ref
      (* show absyn internal reps (MARKS) *)

  val varconInternals : bool ref
      (* show value/constructor internal reps *)

  val internals : bool ref
      (* (general) show internal reps *)

  val setInternals : unit -> unit
      (* set all internals controls to true *)
				 
  val resetInternals : unit -> unit
      (* set all internals controls to false *)

  val boxedconstconreps : bool ref
      (* constructor representation (used in ConRep) *)

end (* signature ELABDATA_CONTROL *)
