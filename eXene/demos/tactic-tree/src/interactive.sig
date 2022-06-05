(* interactive.sig  
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * signature for interactive tactic tree manager. 
 *)


signature INTERACTIVE_TT =
sig
	structure TTM : TTREE_MANAGER

	val create  : TTM.S.goal * string * (string * TTM.S.tactic) list -> TTM.ttree_state
	val view : TTM.ttree_state * string * (string * TTM.S.tactic) list  -> TTM.ttree_state
	val extract_event : TTM.ttree_state -> TTM.S.event
	val extract_tactic_text:  TTM.ttree_state -> string 
	val extract_text : TTM.ttree_state* string -> unit
end
