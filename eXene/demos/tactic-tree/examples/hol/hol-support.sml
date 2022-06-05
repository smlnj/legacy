structure HOL_TTree_Support : TTREE_SUPPORT =
struct 

type goal = (Theory.Thm.Term.term list * Theory.Thm.Term.term)
type event = Theory.Thm.thm
type validation = (event list -> event)
type tactic = (goal -> goal list * validation) 

fun achieves (g,e) = true  (* this should be changed *) 

fun make_validation v = v
fun apply_validation (v,l) = (v l)
fun make_tactic t = t
fun apply_tactic (t,g) = (t g)

val tactic_menu = [
	  ("CONJ_TAC",CONJ_TAC),
	  ("DISJ1_TAC",DISJ1_TAC),
	  ("DISJ2_TAC",DISJ2_TAC),
	  ("EQ_TAC",EQ_TAC),
	  ("GEN_TAC",GEN_TAC),
          ("STRIP_TAC",STRIP_TAC),
          ("REPEAT STRIP_TAC",REPEAT STRIP_TAC),
          ("DISCH_TAC",DISCH_TAC),
          ("COND_CASES_TAC",COND_CASES_TAC),
          ("REFL_TAC",REFL_TAC),
          ("AP_TERM_TAC",AP_TERM_TAC),
          ("AP_THM_TAC",AP_THM_TAC)
	  ] 

val tactic_ref = ref ((fn g => ([g],fn [e] => e)) : tactic)

fun goal_to_string (hl,c) = 
    let val pp  = PP.pp_to_string pp_term
    in fold (fn (s,r) => if r = "" then s else s ^ "\n" ^ r)
	    ((map pp hl) @ ["|- " ^ (pp c)] )
            ""
    end

val indentation  = " " 
val unrefined  = "by ?" 
val refined = "by " 
val elision = "..." 
val then_text = "THEN"
val thenl_text = "THENL"
val id_tac_text = "ALL_TAC"

end 



