(* support.sig
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * signature for tactic tree support
 *)

signature TTREE_SUPPORT =
sig
type goal 
type event 
type validation 
type tactic 

val achieves : goal * event -> bool 

val apply_validation : (validation * event list) -> event
val apply_tactic : (tactic * goal) -> goal list * validation

val tactic_menu : (string * tactic) list 
val tactic_ref : tactic ref 

val goal_to_string : goal -> string 
val indentation : string
val unrefined : string 
val refined : string 
val elision : string 

val then_text : string 
val thenl_text : string 
val id_tac_text : string 

end 


