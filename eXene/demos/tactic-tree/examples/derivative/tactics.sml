signature DERIVATIVE_TACTICS =
sig

    exception TacticFailure

    type goal
    type derivative
    type tactic
    type validation

    val sinTac : tactic
    val cosTac : tactic
    val addTac : tactic
    val productTac : tactic
    val chainTac : tactic
    val negTac : tactic
    val eTac : tactic
    val expTac : tactic
    val singleVarTac : tactic
    val constantTac : tactic

end

structure DerivativeTactics = 
struct

    open Expression DerivativeRules

    exception TacticFailure

    type goal = expression
    type derivative = derivative
    type validation = (derivative list -> derivative)
    type tactic = (goal -> goal list * validation)

    val sinTac : tactic =
	fn (expr as (Sin (Term (Var x)))) => ([], fn l => (sinRule expr))
	 | _  => raise TacticFailure

    val cosTac : tactic =
	fn (expr as (Cos (Term (Var x)))) => ([], fn l => (cosRule expr))
	 | _ => raise TacticFailure

    val singleVarTac : tactic =
        fn (expr as (Term (Var x))) => ([], fn l => (singleVarRule expr))
	 | _  => raise TacticFailure

    val constantTac : tactic =
        fn (expr as (Term (Num n))) => ([], fn l => (constantRule expr))
	 | _ => raise TacticFailure

    val expTac : tactic =
	fn (expr as (Exp (Term (Var x), Term (Num n)))) => ([], fn l => (exponentRule expr))
	 | _  => raise TacticFailure

    val eTac : tactic =
	fn (e_of_f as (E f)) => ([f], fn [f'] => eRule (f',e_of_f) | _ => raise TacticFailure)
	 | _  => raise TacticFailure 

    val addTac : tactic =
	fn (Plus (f,g)) => ([f,g], fn [f',g'] => additionRule (f',g') | _ => raise TacticFailure)
	 | (Minus (f,g)) => ([f,g], fn [f',g'] => subtractionRule (f',g') | _ => raise TacticFailure)
	 | _ => raise TacticFailure
   
    val productTac : tactic =
	fn (Times (f,g)) => ([f,g], fn [f',g'] => productRule (f',g',f,g) | _ => raise TacticFailure)
	 | _ => raise TacticFailure

    val chainTac : tactic =
	fn (Sin g) => 
	   ([Sin (Term (Var "x")), g],
	    fn [f_of_x',g'] => chainRule(f_of_x',g',"x") | _ => raise TacticFailure)

	 | (Cos g) => 
	    ([Cos (Term (Var "x")), g],
	     fn [f_of_x',g'] => chainRule(f_of_x',g',"x") | _ => raise TacticFailure)

	 | (Exp(g,Term (Num n))) =>
	    ([Exp(Term (Var "x"),Term (Num n)),g], 
	     fn [f_of_x',g'] => chainRule(f_of_x',g',"x") | _ => raise TacticFailure)

	 | _ => raise TacticFailure

    val negTac : tactic =
	fn (Neg g) =>
	   ([g], fn [g'] => negRule(g') | _ => raise TacticFailure)

end







