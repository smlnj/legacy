(* syntax.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * 
 *)

signature FOL =
  sig

  datatype otype = BaseType of string  
                 | TypeCon of string * otype list 

  datatype term = Var   of string
                | IntTerm of int 
                | Constant of string 
		| Fun   of term * term list
  datatype form = Pred  of string * term list
		| Conn  of string * form list
		| Quant of string * (string * otype) list * form 
  datatype form_or_term = Form of form | Term of term 
  val prec_of: string -> int

  val IntType : otype
  val FunType : otype * otype -> otype
  val TupleType : otype list -> otype 

  val Equal : term * term -> form 
  val PropVar : (string * term list) -> form 
  val False : form 
  val And : form * form -> form 
  val Or : form * form -> form 
  val Implies : form * form -> form 
  val Not : form -> form 
  val All : (string * otype) list * form -> form 
  val Some : (string * otype) list * form -> form 

  val free_vars_of_formula : form -> string list
  val subst_in_formula : form * (string * term) list -> form
  end
  

functor FolFUN() : FOL =
  struct

  datatype otype = BaseType of string  
                 | TypeCon of string * otype list 

  datatype term = Var   of string
                | IntTerm of int 
                | Constant of string 
		| Fun   of term * term list
  datatype form = Pred  of string * term list
		| Conn  of string * form list
		| Quant of string * (string * otype) list * form
  datatype form_or_term = Form of form | Term of term 
  (*Precedence table: used by pretty printing *)
  fun prec_of "~"   = 4
    | prec_of "&"   = 3
    | prec_of "|"   = 2
    | prec_of "<->" = 1
    | prec_of "->" = 1
    | prec_of "*" = 2
    | prec_of "/" = 4
    | prec_of "--" = 3
    | prec_of "+" = 2
    | prec_of _   = ~1    (*means not an infix*)

infix mem

fun x mem []  =  false
  | x mem (y::l)  =  (x=y) orelse (x mem l)

val IntType = BaseType "N"

fun FunType (a,b) = TypeCon("-->",[a,b])

fun TupleType l = TypeCon("*",l)

fun PropVar (s,l) = Pred(s,l)

fun Equal (a,b) = Pred("=",[a,b])

val False = Conn("false",[])

fun And (A,B) = Conn("&",[A,B]) 

fun Or (A,B) = Conn("|",[A,B]) 

fun Implies (A,B) = Conn("->",[A,B]) 

fun Not A = Conn("~",[A])

fun All (bl,A) = Quant("forall",bl,A)

fun Some (bl,A) = Quant("exists",bl,A)

fun free_vars_of_term (Var s,l) = if s mem l then [] else [s]
  | free_vars_of_term (Fun(t,tl),l) = 
              (free_vars_of_term (t,l)) @
	       (fold (op @) (map (fn t => free_vars_of_term(t,l)) tl) [])
  | free_vars_of_term _ = [] 

fun free_vars_of_formula A = 
     let fun fvf (Pred(_,tl),l) = fold (op @) (map (fn t => free_vars_of_term(t,l)) tl) [] 
	   | fvf (Conn(_,fl),l) = fold (op @) (map (fn B => fvf(B,l)) fl) [] 
	   | fvf (Quant(_,bl,B),l) = fvf(B,(map (fn (x,_) => x) bl) @ l)
     in fvf(A,[]) end

fun thin_out_subst([],_) = [] 
  | thin_out_subst ((x,t)::s,vars) = 
       if (exists (fn y => (x = y)) vars) 
       then thin_out_subst(s,vars) 
       else (x,t) :: (thin_out_subst(s,vars))

fun new_var (y,l) = 
     let val z = y ^ "'" 
     in if (exists (fn s => (s = z)) l) 
        then new_var(z,l) 
        else z
     end

fun avoid_clash ([],bl,s,_,_) = (rev bl,s) 
  | avoid_clash ((x,ty)::l,bl,s,fvs,vars) = 
      if (exists (fn y => (x = y)) fvs) 
      then let val x' = new_var(x,fvs@vars) 
           in avoid_clash(l,(x',ty)::bl,(x,Var x')::s,fvs,x'::vars) end 
      else avoid_clash(l,(x,ty)::bl,s,fvs,vars)


fun apply_subst_to_var ([],y) = Var y
  | apply_subst_to_var ((x,t)::s,y) = 
            if x = y then t else apply_subst_to_var(s,y)

fun subst_in_term (Var v,s) = apply_subst_to_var (s,v)
  | subst_in_term (Fun(t,tl),s) = Fun(subst_in_term(t,s),map (fn t => subst_in_term (t,s)) tl)
  | subst_in_term (t,_) = t 

fun subst_in_formula (Pred(name,tl),s) = Pred (name,(map (fn t => subst_in_term (t,s)) tl))
  | subst_in_formula (Conn(name,fl),s) = Conn (name,(map (fn f => subst_in_formula (f,s)) fl))
  | subst_in_formula (Quant(name,bl,A),s) = 
        let val vs = (map (fn (x,_) => x) bl) 
        in case thin_out_subst(s,vs)
             of [] => Quant(name,bl,A)
              | s' => let val (bl',s'') = 
		       avoid_clash(bl,[],[],
				   (fold (fn ((_,t),r) => (free_vars_of_term(t,[]))@r) s []),
				   vs @ (map (fn (y,_) => y) s))
                      in case s'' 
                         of [] => Quant(name,bl,subst_in_formula(A,s'))
		          | _  => Quant(name,bl',
					subst_in_formula(subst_in_formula(A,s''),s'))
		      end 
        end 

end






