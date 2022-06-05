(* ttree-support.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * structure used to build tactic tree interface 
 *)

structure FOL_TTree_Support : TTREE_SUPPORT =
struct 

local open  FolProofs DispFol
in 

type goal = Tactics.sequent 
type event = Tactics.theorem 
type validation = (event list -> event)
type tactic = (goal -> goal list * validation) 

fun achieves (seq,prf) = (sequent_of_theorem prf) = seq

fun apply_validation (v,l) = (v l)
fun apply_tactic (t,g) = (t g)

val tactic_menu = [
("hypothesis",Tactics.hypothesis),
("intro",Tactics.intro),
("REPEAT intro",Tactics.REPEAT Tactics.intro),
("or_intro_left",Tactics.or_intro_left),
("or_intro_right", Tactics.or_intro_right), 
("axiom", Tactics.axiom), 
("symmetry",Tactics.symmetry),
("reflexivity",Tactics.reflexivity),
("induction",Tactics.induction)
]

val tactic_ref = ref ((fn g => ([g],fn [e] => e)) : tactic)

fun goal_to_string g =  
    let fun add_prefix (s,a::l) = (s^a)::l | add_prefix _ = ["impossible"]
	fun hyp_list_to_strings [] = [] 
	  | hyp_list_to_strings (FormulaBinding(s,A)::H) = 
             (add_prefix(s^": ",format_form (A,50))) @ (hyp_list_to_strings H)
	  | hyp_list_to_strings (TypeBinding(s,t)::H) = 
             (add_prefix(s^": ",format_otype (t,50))) @ (hyp_list_to_strings H)
    in 
       (fold (fn (s,r) => if r = "" then s else s ^ "\n" ^ r)
	        ((hyp_list_to_strings (hyp_of_sequent g)) 
                @ (add_prefix("|- ", format_form(formula_of_sequent g,50))))
               "")
    end 

val indentation  = " " 

val unrefined  = "by ?" 

val refined = "by " 

val elision  = "..."

val then_text = "THEN"

val thenl_text = "THENL"

val id_tac_text  = "id_tac"

end 
end 
