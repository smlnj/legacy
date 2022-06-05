(* tactics.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * tactics for first-order arithmetic 
 *)
signature TACTICS =
sig

structure P : FOLPROOFS

type goal 
type event 
type validation 
type tactic 

val id_tac : tactic

val hypothesis : tactic
val and_intro: tactic
val and_elim: string -> tactic
val implies_intro: tactic
val implies_elim: string -> tactic
val not_intro: tactic
val not_elim: string -> tactic
val or_intro_left: tactic
val or_intro_right: tactic
val or_elim: string -> tactic
val all_intro: tactic
val all_elim: (string * string) -> tactic
val induction : tactic 
val some_intro: string -> tactic
val some_elim: string -> tactic

val cut: string -> tactic
val lemma: (string * P.theorem_t) -> tactic 

val symmetry : tactic
val reflexivity : tactic
val transitivity : string -> tactic
val axiom: tactic
val substitution: string * string * string * string -> tactic


val THEN : tactic * tactic -> tactic
val THENL : tactic * tactic list -> tactic
val ORELSE : tactic * tactic -> tactic
val PROGRESS : tactic -> tactic
val COMPLETE : tactic -> tactic
val REPEAT : tactic -> goal -> goal list * validation

val or_tactical : tactic -> tactic
val intro : tactic 
val intros : tactic 
val elim : string -> tactic 

end


structure Tactics =
struct 

structure P = FolProofs

open Fol FolProofs FolParseString
exception TacticFailure 
type goal = sequent_t
type event = theorem_t
type validation = (event list -> event)
type tactic = (goal -> goal list * validation)

fun first_n (0,_) = []
  | first_n (n,[]) = raise TacticFailure 
  | first_n (n,a::l) = a::(first_n(n-1,l))
			     
fun smear_function_list (l,[]) = []
  | smear_function_list (l,(gl,v)::tl) = 
        let val m = (length gl)
        in (v (first_n(m,l)))::(smear_function_list(nthtail(l,m),tl)) end

val id_tac :tactic = (fn g => ([g],(fn [t] => t | _ => raise TacticFailure)))
    
val THEN : tactic*tactic -> tactic = 
    fn (t1,t2) => (fn g => let val (gl1,v1) = (t1 g) 
                           in if gl1 = [] then (gl1,v1) 
			      else let val rl = (map t2 gl1)
			           in 
				       (fold (fn ((gls,_),r) => (gls @ r )) rl [],
					fn l => (v1 (smear_function_list(l,rl))))
			           end
			   end)

fun apply_fun_list ([],[]) = [] 
  | apply_fun_list (fl,[]) = raise TacticFailure
  | apply_fun_list ([],al) = raise TacticFailure
  | apply_fun_list (f::fl,a::al) = (f a) :: (apply_fun_list(fl,al))

val THENL : tactic*(tactic list) -> tactic = 
    fn (t1,tl2) => (fn g => let val (gl1,v1) = (t1 g) 
                           in if gl1 = [] then (gl1,v1) 
			      else let val rl = (apply_fun_list(tl2,gl1))
			           in 
				       (fold (fn ((gls,_),r) => (gls @ r )) rl [],
					fn l => (v1 (smear_function_list(l,rl))))
			           end
			   end)
		       
val ORELSE : tactic*tactic -> tactic = 
    fn (t1, t2) => (fn g => (t1 g) handle _ => (t2 g))

val PROGRESS :tactic -> tactic =
    fn t => (fn g => 
	     let val (gl,v) = (t g) 
	     in if [g] = gl then raise TacticFailure else (gl,v) 
	     end)
    
val COMPLETE :tactic -> tactic =
    fn t => (fn g => 
	     let val (gl,v) = (t g) 
	     in if [] = gl then ([],v) else raise TacticFailure
	     end)
    
fun REPEAT t:tactic = (ORELSE(THEN((PROGRESS t), (fn g => ((REPEAT t) g))),id_tac)) 

fun Try t:tactic = ORELSE(t,id_tac)  


(* FOL TACTICS *) 

val hypothesis : tactic =
  fn (Sequent(H,A)) => 
     let val s = string_of_formula_binding(A,H) 
     in ([],fn l => (hyp(Sequent(H,A)))) end 

val and_intro: tactic = 
  fn (Sequent(H,Conn("&",[A,B]))) => 
        ([Sequent(H,A),Sequent(H,B)],
	 (fn [t1,t2] => (andintro(t1,t2)) | _ => raise TacticFailure))
      | _ => raise TacticFailure  

val and_elim: string -> tactic = 
  fn s => (fn (Sequent(H,C)) => 
              case lookup_formula_binding(s,H) of 
	      (Conn("&",[A,B])) => 
	         ([Sequent(add_formula(B,add_formula(A,H)),C)],
		   fn [t] => (andelim(hyp(Sequent(H,And(A,B))),t))
                      | _ => raise TacticFailure)
	       |  _ => raise TacticFailure ) 
  
  
val or_intro_left: tactic =
  fn (Sequent(H,Conn("|",[A,B]))) => 
         ([Sequent(H,A)],fn [t] => (orintroleft (B,t)) | _ => raise TacticFailure)
     | _ => raise TacticFailure  
			    
val or_intro_right: tactic =
  fn (Sequent(H,Conn("|",[A,B]))) => 
         ([Sequent(H,B)],fn [t] => (orintroright (A,t)) | _ => raise TacticFailure)
     | _ => raise TacticFailure  
			    
val or_elim: string -> tactic = 
  fn s => (fn (Sequent(H,C)) => 
              case lookup_formula_binding(s,H) of 
	       (Conn("|",[A,B])) =>
	         ([Sequent(add_formula(A,H),C),Sequent(add_formula(B,H),C)],
		   fn [t1,t2] => (orelim(hyp(Sequent(H,Or(A,B))),t1,t2))
                      | _ => raise TacticFailure)
                | _ => raise TacticFailure) 
  
val implies_intro: tactic = 
  fn (Sequent(H,Conn("->",[A,B]))) => 
	      ([Sequent(add_formula(A,H),B)],
		   fn [t] => (impliesintro(string_of_formula_binding(A,
				     hyp_of_sequent(sequent_of_theorem t)),t))
                      | _ => raise TacticFailure)
      | _ => raise TacticFailure  

val implies_elim: string -> tactic = 
  fn s => (fn (Sequent(H,C)) => 
              case lookup_formula_binding(s,H) of 
	      (Conn("->",[A,B])) => 
                  ([Sequent(H,A),Sequent(add_formula(B,H),C)],
		   fn [t1,t2] => 
		      (implieselim(impliesintro(string_of_formula_binding(B,
				     hyp_of_sequent(sequent_of_theorem t2)),t2),
				   implieselim(hyp(Sequent(H,Implies(A,B))),t1)))
                      | _ => raise TacticFailure)
                | _ => raise TacticFailure) 

  
val cut: string -> tactic = 
  fn a => let val A = read_formula a
          in (fn (Sequent(H,C)) => 
                 ([Sequent(H,A),Sequent(add_formula(A,H),C)],
		   fn [t1,t2] => 
                      (implieselim(impliesintro(
                         string_of_formula_binding(A,hyp_of_sequent(sequent_of_theorem t2)),t2),t1))
                      | _ => raise TacticFailure))
           end  
  

val lemma: (string * theorem_t) -> tactic = 
  fn (a,e) => let val A = read_formula a
              in (fn (Sequent(H,C)) => 
                 ([Sequent(add_formula(A,H),C)],
		   fn [t] => 
                      (implieselim(impliesintro(
                         string_of_formula_binding(A,hyp_of_sequent(sequent_of_theorem t)),t),
				   weaken(H,e)))
                      | _ => raise TacticFailure)) 
	     end 

val not_intro: tactic = 
  fn (Sequent(H,Conn("~",[A]))) => 
	      ([Sequent(add_formula(A,H),Conn("false",[]))],
		   fn [t] => (notintro(string_of_formula_binding(A,
				     hyp_of_sequent(sequent_of_theorem t)),t))
                      | _ => raise TacticFailure)
      | _ => raise TacticFailure  

val not_elim: string -> tactic = 
  fn s => (fn (Sequent(H,B)) => 
              case lookup_formula_binding(s,H) of 
	      (Conn("~",[A])) => 
                  ([Sequent(H,A)],
		   fn [t] => (notelim(B,hyp(Sequent(H,Not(A))),t))
                      | _ => raise TacticFailure)
                | _ => raise TacticFailure) 

(* 
val dn_elim: tactic = 
  fn (Sequent(H,A)) => 
        ([Sequent(H,Not(Not(A)))],
         (fn [t] => (dnelim t) | _ => raise TacticFailure)) 
*) 

val all_intro: tactic = 
    fn (Sequent(H,Quant("forall",[(x,_)],A))) => 
	  ([Sequent(H,A)],fn [th] => (allintro(x,th)) | _ => raise TacticFailure)
     | _ => raise TacticFailure 

val all_elim: (string * string) -> tactic = 
  fn (s,ts) => 
    let val t = read_term ts 
     in (fn (Sequent(H,C)) => 
                    case lookup_formula_binding(s,H) of 
	             (Quant("forall",[(x,_)],A)) => 
                  let val f = subst_in_formula(A,[(x,t)])
		  in 
	            ([Sequent(add_formula(f,H),C)],
		     fn [th] => (implieselim(impliesintro(string_of_formula_binding(f,
				           hyp_of_sequent(sequent_of_theorem th)),th),
                                           allelim(t,hyp(Sequent(H,All([(x,IntType)],A))))))
                      | _ => raise TacticFailure)
		  end
	         | _ => raise TacticFailure)
          end  
		    



(* *)   
val some_intro: string -> tactic =
  fn s => let val t = read_term s 
          in (fn (Sequent(H,Quant("exists",[(x,_)],A))) => 
               ([Sequent(H,subst_in_formula(A,[(x,t)]))],
                fn [th] => (someintro (t,Some([(x,IntType)],A),th)) | _ => raise TacticFailure)
                 | _ => raise TacticFailure)
           end  
			    

val some_elim: string -> tactic = 
  fn s => (fn (Sequent(H,C)) => 
              case lookup_formula_binding(s,H) of 
	      (Quant("exists",[(x,ty)],A)) => 
	         ([Sequent(add_formula(A,H),C)],
		   fn [t] => (someelim(hyp(Sequent(H,Some([(x,ty)],A))),t))
                      | _ => raise TacticFailure)
	       |  _ => raise TacticFailure ) 
  
val reflexivity: tactic =
  fn (Sequent(H,Pred("=",[a,b]))) => 
         if a = b
         then ([],fn []  => (reflexivity(H,a)) | _ => raise TacticFailure)
         else raise TacticFailure 
   | _ => raise TacticFailure  
		     
val symmetry: tactic =
  fn (Sequent(H,Pred("=",[a,b]))) => 
         ([Sequent(H,Equal(b,a))],
           fn [th]  => (symmetry th) | _ => raise TacticFailure)
   | _ => raise TacticFailure  

val transitivity : string -> tactic =
  fn s => let val b = read_term s 
          in (fn (Sequent(H,Pred("=",[a,c]))) => 
                ([Sequent(H,Equal(a,b)),Sequent(H,Equal(b,c))],
                  fn [th1,th2]  => (transitivity(th1,th2)) | _ => raise TacticFailure)
                  | _ => raise TacticFailure)
           end  
		     

val axiom : tactic = 
    fn (Sequent(H, A as (Conn("~",[Pred("=",[IntTerm 0,Fun(Constant "+" ,[t,IntTerm 1])])]))))
          => ([],fn [] => (peanoone (Sequent(H,A))) | _ => raise TacticFailure)
     | (Sequent(H,A as (Pred("=",[Fun(Constant "+" ,[a,IntTerm 0]),b]))))
          => if a = b
             then ([],fn [] => (peanothree (Sequent(H,A))) | _ => raise TacticFailure)
             else raise TacticFailure 
     | (Sequent(H,A as (Pred("=",[Fun(Constant "+" ,[a,Fun(Constant "+" ,[b,IntTerm 1])]),
				  Fun(Constant "+" ,[Fun(Constant "+" ,[c,d]),IntTerm 1])]))))
          => if (a = c) andalso (b = d)
             then([],fn [] => (peanofour (Sequent(H,A))) | _ => raise TacticFailure)
             else raise TacticFailure
     | (Sequent(H,A as (Pred("=",[Fun(Constant "*",[a,IntTerm 0]),IntTerm 0]))))
          => ([],fn [] => (peanofive (Sequent(H,A))) | _ => raise TacticFailure)
     | (Sequent(H,A as (Pred("=",[Fun(Constant "*",[a,Fun(Constant "+" ,[b,IntTerm 1])]),
				  Fun(Constant "+" ,[Fun(Constant "*",[c,d]),e])]))))
          => if (a = c) andalso (b = d) andalso (a = e) 
             then ([],fn [] => (peanosix (Sequent(H,A))) | _ => raise TacticFailure)
             else raise TacticFailure
     | _ => raise TacticFailure 


val induction: tactic = 
    fn (Sequent(H,Quant("forall",[(x,_)],A))) => 
	  ([Sequent(H,subst_in_formula(A,[(x,IntTerm 0)])),
            Sequent(add_formula(A,H),
		    subst_in_formula(A,[(x,Fun(Constant "+" ,[Var x,IntTerm 1]))]))],
           fn [th1,th2] => (induction_pr(All([(x,IntType)],A),th1,th2)) 
            | _ => raise TacticFailure)
     | _ => raise TacticFailure 


val substitution: string*string*string*string -> tactic = 
    fn (f,x,t1,t2) => 
     let val A = read_formula f 
         val a = read_term t1
         val b = read_term t2
     in (fn (Sequent(H,B)) => 
          let val (C,D) = (subst_in_formula(A,[(x,a)]),subst_in_formula(A,[(x,b)]))
          in 
	    if B = D 
	    then ([Sequent(H,Pred("=",[a,b])),Sequent(H,C)],
                  fn [th1,th2] => substitution(A,x,th1,th2)
			   | _ => raise TacticFailure)
	    else raise TacticFailure
	   end) 
     end 
		            
(* derived tactics *) 

val intro = 
      ORELSE(and_intro,
      ORELSE(implies_intro,
             all_intro)) 

val intros =  REPEAT intro  

fun elim h = 
      ORELSE(and_elim h,
      ORELSE(implies_elim h,
             or_elim h))  

fun or_tactical tac  = 
     ORELSE(THEN(or_intro_right,tac),THEN(or_intro_left,tac))  

fun immediate g =  
        (REPEAT(ORELSE(hypothesis,
	       ORELSE(implies_intro,
	       ORELSE(and_intro,
               ORELSE(not_intro,
	       ORELSE(all_intro, or_tactical (COMPLETE immediate)))))))) g
                      
    

end 
