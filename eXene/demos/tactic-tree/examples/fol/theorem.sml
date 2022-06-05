(* theorem.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * defines theorem data type 
 *)
signature FOLPROOFS =
sig

structure Logic : FOL 

type theorem_t
type sequent_t
type hyp_list
type binding_t

    exception BindingNotFound
    exception Failure
    val add_formula : Logic.form * binding_t list -> binding_t list
    val allelim : Logic.term * theorem_t -> theorem_t
    val allintro : string * theorem_t -> theorem_t
    val andelim : theorem_t * theorem_t -> theorem_t
    val andintro : theorem_t * theorem_t -> theorem_t
    val delete_binding : ''a * ''a list -> ''a list
    val dnelim : theorem_t -> theorem_t
    val formula_in_hyp : Logic.form * binding_t list -> bool
    val formula_of_sequent : sequent_t -> Logic.form
    val hyp : sequent_t -> theorem_t
    val hyp_of_sequent : sequent_t -> hyp_list
    val implieselim : theorem_t * theorem_t -> theorem_t
    val impliesintro : string * theorem_t -> theorem_t
    val induction_pr : Logic.form * theorem_t * theorem_t -> theorem_t
    val lookup_formula_binding : string * binding_t list -> Logic.form
    val notelim : Logic.form * theorem_t * theorem_t -> theorem_t
    val notintro : string * theorem_t -> theorem_t
    val orelim : theorem_t * theorem_t * theorem_t -> theorem_t
    val orintroleft : Logic.form * theorem_t -> theorem_t
    val orintroright : Logic.form * theorem_t -> theorem_t
    val peanofive : sequent_t -> theorem_t
    val peanofour : sequent_t -> theorem_t
    val peanoone : sequent_t -> theorem_t
    val peanosix : sequent_t -> theorem_t
    val peanothree : sequent_t -> theorem_t
    val peanotwo : theorem_t -> theorem_t
    val reflexivity : hyp_list * Logic.term -> theorem_t
    val sequent_of_theorem_t : theorem_t -> sequent_t
    val someelim : theorem_t * theorem_t -> theorem_t
    val someintro : Logic.term * Logic.form * theorem_t -> theorem_t
    val string_of_formula_binding : Logic.form * binding_t list -> string
    val substitution : Logic.form * string * theorem_t * theorem_t -> theorem_t
    val symmetry : theorem_t -> theorem_t
    val transitivity : theorem_t * theorem_t -> theorem_t
    val weaken : hyp_list * theorem_t -> theorem_t

end 

structure FolProofs = 
struct 

exception BindingNotFound
exception Failure

structure Logic = Fol

open Logic
(* bindings and hyp lists *) 

(* bindings are of the form   a:A  or  x:otype *) 


datatype binding = FormulaBinding of string * form
                 | TypeBinding of string * otype 

type binding_t = binding 
type hyp_list = binding list

datatype sequent = Sequent of hyp_list * form

datatype theorem = 
        Hyp of sequent 
      | Weaken of sequent * theorem 
      | AndIntro of sequent * theorem * theorem
      | AndElim of sequent * theorem * theorem 
      | OrIntroLeft of sequent * theorem 
      | OrIntroRight of sequent * theorem 
      | OrElim of sequent * theorem * theorem * theorem 
      | ImpliesIntro of sequent * theorem 
      | ImpliesElim of sequent * theorem * theorem 
      | NotIntro of sequent * theorem 
      | NotElim of sequent * theorem * theorem 
      | DnElim of sequent * theorem
      | AllIntro of sequent * theorem 
      | AllElim of term * sequent * theorem 
      | SomeIntro of term * sequent * theorem 
      | SomeElim of sequent * theorem * theorem 
      | EqRefl of sequent
      | EqSym of sequent * theorem 
      | EqTrans of sequent * theorem * theorem 
      | PeanoOne of sequent 
      | PeanoTwo of sequent * theorem
      | PeanoThree of sequent 
      | PeanoFour of sequent 
      | PeanoFive of sequent 
      | PeanoSix of sequent 
      | Induction of sequent * theorem * theorem 
      | Substitution of  sequent * theorem * theorem 


type theorem_t = theorem
type sequent_t = sequent


fun formula_in_hyp (A,[] ) = false
  | formula_in_hyp (A,(FormulaBinding(_,B)::l)) = (A = B) orelse formula_in_hyp(A,l)
  | formula_in_hyp (A,(_::l)) = formula_in_hyp(A,l)
	       
fun lookup_formula_binding (s,[]) = raise BindingNotFound
  | lookup_formula_binding (s,FormulaBinding(x,A)::l) = 
                  if s = x then A else lookup_formula_binding(s,l)
  | lookup_formula_binding (s,_::l) = lookup_formula_binding(s,l)

fun string_of_formula_binding (A,[]) = raise BindingNotFound
  | string_of_formula_binding (A,FormulaBinding(x,B)::l) = 
            if A = B then x else string_of_formula_binding(A,l)
  | string_of_formula_binding (A,_::l) = string_of_formula_binding(A,l)
    
fun delete_binding(b,[]) = []
  | delete_binding (b,a::l) = if b = a then l else a::(delete_binding(b,l))
    
fun add_formula (A,l) = 
          rev (FormulaBinding("h" ^ (Integer.makestring(length l)),A) :: (rev l))

fun hyp_of_sequent(Sequent(H,_)) = H

fun formula_of_sequent(Sequent(_,A)) = A


fun hyp (Sequent(l,A)) = if formula_in_hyp(A,l) then Hyp(Sequent(l,A)) 
                                else raise Failure


fun sequent_of_theorem (Hyp s) = s 
  | sequent_of_theorem (Weaken(s,_)) = s
  | sequent_of_theorem (AndIntro(s,_,_)) = s
  | sequent_of_theorem (AndElim(s,_,_)) = s 
  | sequent_of_theorem (OrIntroLeft(s,_)) = s
  | sequent_of_theorem (OrIntroRight(s,_)) = s
  | sequent_of_theorem (OrElim(s,_,_,_)) = s
  | sequent_of_theorem (ImpliesIntro(s,_)) = s
  | sequent_of_theorem (ImpliesElim(s,_,_)) = s  
  | sequent_of_theorem (NotIntro(s,_)) = s
  | sequent_of_theorem (NotElim(s,_,_)) = s
  | sequent_of_theorem (DnElim(s,_)) = s
  | sequent_of_theorem (AllIntro(s,_)) = s
  | sequent_of_theorem (AllElim(_,s,_)) = s  
  | sequent_of_theorem (SomeIntro(_,s,_)) = s
  | sequent_of_theorem (SomeElim(s,_,_)) = s
  | sequent_of_theorem (EqRefl(s)) = s
  | sequent_of_theorem (EqSym(s,_)) = s
  | sequent_of_theorem (EqTrans(s,_,_)) = s
  | sequent_of_theorem (PeanoOne s) = s 
  | sequent_of_theorem (PeanoTwo(s,_)) = s
  | sequent_of_theorem (PeanoThree s) = s 
  | sequent_of_theorem (PeanoFour s) = s 
  | sequent_of_theorem (PeanoFive s) = s 
  | sequent_of_theorem (PeanoSix s) = s 
  | sequent_of_theorem (Induction(s,_,_)) = s
  | sequent_of_theorem (Substitution(s,_,_)) = s
 
fun weaken (H1,t) = 
    let val Sequent(H2,A) = sequent_of_theorem t
    in if H2 = [] 
       then Weaken(Sequent(H1,A),t)
       else raise Failure
    end

fun andintro (t1,t2) = 
    let val (Sequent(l1,A),Sequent(l2,B)) = 
	      (sequent_of_theorem t1,sequent_of_theorem t2)
    in if l1 = l2 then AndIntro(Sequent(l1,And(A,B)),t1,t2) 
                  else raise Failure end

fun andelim (t1,t2) = 
    case (sequent_of_theorem t1,sequent_of_theorem t2) 
    of  (Sequent(l1,Conn("&",[A,B])),Sequent(l2,C)) => 
	let val a = string_of_formula_binding (A,l2);
	    val b = string_of_formula_binding (B,l2)
        in if l1 = (delete_binding(FormulaBinding(a,A),
                    delete_binding(FormulaBinding(b,B),l2)))
           then AndElim(Sequent(l1,C),t1,t2) 
           else raise Failure 
        end
        |  _ => raise Failure
	    
fun orintroleft (B,t) = 
    let val (Sequent(H,A)) = sequent_of_theorem t
    in  OrIntroLeft(Sequent(H,Or(A,B)),t) end

fun orintroright (A,t) = 
    let val (Sequent(H,B)) = sequent_of_theorem t
    in  OrIntroRight(Sequent(H,Or(A,B)),t) end

fun orelim (t1,t2,t3) = 
     case (sequent_of_theorem t1,sequent_of_theorem t2,sequent_of_theorem t3)
     of   (Sequent(H1,Conn("|",[A,B])),Sequent(H2,C),Sequent(H3,D)) => 
             if C = D 
             then let val a = string_of_formula_binding(A,H2)
                      and b = string_of_formula_binding(B,H3)
		  in if (H1 = (delete_binding(FormulaBinding(a,A),H2))) andalso
                        (H1 = (delete_binding(FormulaBinding(b,B),H3)))
                     then OrElim(Sequent(H1,C),t1,t2,t3)
		     else raise Failure
                  end 
             else raise Failure 
	   | _ => raise Failure

fun impliesintro (st,t) =
    let val (Sequent(H,B)) = (sequent_of_theorem t);
        val A = lookup_formula_binding(st,H) 
    in ImpliesIntro(Sequent(delete_binding(FormulaBinding(st,A),H),Implies(A,B)),t) end
	

fun implieselim (t1,t2) = 
    case (sequent_of_theorem t1,sequent_of_theorem t2) 
    of  (Sequent(H1,Conn("->",[A,B])),Sequent(H2,C)) => 
           if (H1 = H2) andalso (A = C) 
           then ImpliesElim(Sequent(H1,B),t1,t2) 
           else raise Failure 
         | _ => raise Failure
	    
fun notintro (st,t) =
    case (sequent_of_theorem t) of 
          (Sequent(H,Conn("False",[]))) => 
              let val A = lookup_formula_binding(st,H) 
              in NotIntro(Sequent(delete_binding(FormulaBinding(st,A),H),Not(A)),t)
              end
          | _ => raise Failure
	

fun notelim (B,t1,t2) = 
    case (sequent_of_theorem t1,sequent_of_theorem t2) 
    of  (Sequent(H1,Conn("~",[A])),Sequent(H2,C)) => 
           if (H1 = H2) andalso (A = C) 
           then NotElim(Sequent(H1,B),t1,t2) 
           else raise Failure 
         | _ => raise Failure

fun dnelim t =
    case (sequent_of_theorem t) of 
          (Sequent(H,Conn("~",[Conn("~",[A])]))) => 
              DnElim(Sequent(H,A),t) 
          | _ => raise Failure
	
fun allintro (x,t) =          (**** NOTE: need to check vars  ***)
    let val (Sequent(H,A)) = (sequent_of_theorem t)
    in AllIntro(Sequent(delete_binding(TypeBinding(x,IntType),H),
                All([(x,IntType)],A)),t) end
	

fun allelim (t,th) =          (**** NOTE: need to check that t has type ty ***)
    case (sequent_of_theorem th)
    of  (Sequent(H,Quant("forall",[(x,ty)],A))) => 
           AllElim(t,Sequent(H,subst_in_formula(A,[(x,t)])),th)
         | _ => raise Failure

(**** NOTE: need to check that t has type ty ***)
fun someintro (t,Quant("exists",[(x,ty)],A),th) =     
    let val (Sequent(H,B)) = sequent_of_theorem th
    in if B = subst_in_formula(A,[(x,t)])
       then SomeIntro(t,Sequent(H,Some([(x,ty)],A)),th)
       else raise Failure 
    end
  | someintro (_,_,_) = raise Failure

fun someelim (t1,t2) = 
    case (sequent_of_theorem t1,sequent_of_theorem t2) 
    of  (Sequent(l1,Quant("exists",[(x,ty)],A)),Sequent(l2,C)) => 
	let val a = string_of_formula_binding (A,l2)
        in if l1 = delete_binding(TypeBinding(x,ty),
				  delete_binding(FormulaBinding(a,A),l2))
           then SomeElim(Sequent(l1,C),t1,t2) 
           else raise Failure 
        end
        |  _ => raise Failure
	        


fun reflexivity (H,t) = EqRefl(Sequent(H,Equal(t,t)))

fun symmetry th = 
        case (sequent_of_theorem th) of
             (Sequent(H,Pred("=",[a,b]))) => 
		 EqSym(Sequent(H,Equal(b,a)),th)
	   | _ => raise Failure
		 

fun transitivity (t1,t2) = 
    case (sequent_of_theorem t1,sequent_of_theorem t2) 
    of  (Sequent(H1,Pred("=",[a,b])),Sequent(H2,Pred("=",[d,c]))) => 
           if (H1 = H2) andalso (b = d) 
           then EqTrans(Sequent(H1,Equal(a,c)),t1,t2)
           else raise Failure 
        |  _ => raise Failure

fun substitution (A,x,th1,th2) = 
      case (sequent_of_theorem th1,sequent_of_theorem th2) of 
             (Sequent(H1,Pred("=",[a,b])),Sequent(H2,C)) => 
              if H1 = H2 
              then if C = (subst_in_formula(A,[(x,a)]))
                   then Substitution(Sequent(H1,subst_in_formula(A,[(x,b)])),th1,th2)
                   else raise Failure
              else raise Failure
	   |  _ => raise Failure
    
	        
fun peanoone s = 
    case s 
      of (Sequent(H,Conn("~",[Pred("=",[IntTerm 0,Fun(Constant "+",[t,IntTerm 1])])]))) => 
          PeanoOne s
         | _ => raise Failure

fun peanotwo th = 
        case (sequent_of_theorem th) of
          (Sequent(H,
		   Pred("=",[Fun(Constant "+",[x,IntTerm 1]),
				   Fun(Constant "+",[y,IntTerm 1])]))) => 
                   PeanoTwo(Sequent(H,Equal(x,y)),th)
           | _ => raise Failure

fun peanothree s = 
      case s 
        of (Sequent(H,Pred("=",[Fun(Constant "+",[a,IntTerm 0]),b]))) =>
             if a = b then PeanoThree(s) else raise Failure
           | _ => raise Failure

fun peanofour s = 
      case s 
        of (Sequent(H,Pred("=",[Fun(Constant "+",[a,Fun(Constant "+",[b,IntTerm 1])]),
			   Fun(Constant "+",[Fun(Constant "+",[c,d]),IntTerm 1])]))) =>  
             if (a = c) andalso (b = d) then PeanoFour(s) else raise Failure
         | _ => raise Failure


fun peanofive s = 
     case s 
       of (Sequent(H,Pred("=",[Fun(Constant "*",[a,IntTerm 0]),IntTerm 0]))) => 
                   PeanoFive(s) 
        | _ => raise Failure

fun peanosix s = 
     case s 
       of (Sequent(H,Pred("=",[Fun(Constant "*",[a,Fun(Constant "+",[b,IntTerm 1])]),
			  Fun(Constant "+",[Fun(Constant "*",[c,d]),e])]))) =>
                if (a = c) andalso (b = d) andalso (a = e) 
                then PeanoFour(s) else raise Failure
          | _ => raise Failure

fun induction_pr (Quant("forall",[(x,IntType)],A),t1,t2) = 
    let val (Sequent(H1,B),Sequent(H2,C))
          =  (sequent_of_theorem t1,sequent_of_theorem t2) 
    in let val f1 = subst_in_formula(A,[(x,IntTerm 0)])
       in if f1 = B 
              then let val s = string_of_formula_binding(A,H2) 
                   in let val H3 = delete_binding(TypeBinding(x,IntType),
						  delete_binding(FormulaBinding(s,A),H2))
                      in if H1 = H3 
                         then let val f2 = subst_in_formula(A,[(x,Fun(Var"+",[Var x,IntTerm 1]))])
                              in if f2 = C 
                                 then Induction(Sequent(H1,All([(x,IntType)],A)),t1,t2)
				 else raise Failure 
			      end 
                         else raise Failure 
		      end
		   end 
              else raise Failure 
        end
    end 
  | induction_pr (_,_,_) = raise Failure

end 
