signature DERIVATIVE_RULES =
sig

    structure F : EXPRESSION
    
    exception Failure

    type derivative

    val additionRule : derivative * derivative -> derivative
    val subtractionRule : derivative * derivative -> derivative
    val productRule : derivative * derivative * F.expression * F.expression -> derivative
    val chainRule : derivative * derivative * string -> derivative
    val eRule : derivative * F.expression -> derivative
    val negRule : derivative -> derivative
    val cosRule : F.expression -> derivative 
    val sinRule : F.expression -> derivative
    val singleVarRule : F.expression -> derivative 
    val constantRule : F.expression -> derivative
    val exponentRule : F.expression -> derivative


end


structure DerivativeRules =
struct 

    structure F = Expression
    
    exception Failure

    type derivative = F.expression

    fun textualSubstitution (F.Term (F.Var y),g,x) =
	    if y = x 
		then g
	    else F.Term (F.Var y)

      | textualSubstitution (F.Times(expr1,expr2),g,x) = 
	    F.Times(textualSubstitution (expr1,g,x),textualSubstitution (expr2,g,x))
      | textualSubstitution (F.Divide(expr1,expr2),g,x) = 
	    F.Divide (textualSubstitution (expr1,g,x),textualSubstitution (expr2,g,x))
      | textualSubstitution (F.Plus(expr1,expr2),g,x) =
	    F.Plus (textualSubstitution (expr1,g,x),textualSubstitution (expr2,g,x))
      | textualSubstitution (F.Minus(expr1,expr2),g,x) =
	    F.Minus(textualSubstitution (expr1,g,x),textualSubstitution (expr2,g,x))
      | textualSubstitution (F.Exp(expr1,expr2),g,x) =
	    F.Exp(textualSubstitution (expr1,g,x),textualSubstitution (expr2,g,x))
      | textualSubstitution (F.E expr,g,x) = F.E (textualSubstitution (expr,g,x))
      | textualSubstitution (F.Sin expr,g,x) = F.Sin (textualSubstitution (expr,g,x))
      | textualSubstitution (F.Cos expr,g,x) = F.Cos (textualSubstitution (expr,g,x))
      | textualSubstitution (F.Term (F.Num n),g,x) = F.Term (F.Num n)
		 	       
	    
    fun additionRule (f',g') = F.Plus (f',g')
    fun subtractionRule (f',g') = F.Minus(f',g')
    fun productRule (f',g',f,g) = F.Plus (F.Times (f',g),F.Times(g',f))
    fun chainRule (f_of_x',g',x) = F.Times (textualSubstitution (f_of_x',g',x),g')
    fun eRule (f',F.E f) = F.Times (f',F.E f)
    fun exponentRule (F.Exp(expr1,expr2)) = 
	(case (expr1,expr2) of
	     (F.Term (F.Var x), F.Term (F.Num n)) =>
		 if n = 1
		      then F.Term (F.Num 1)
		  else 
		      if n = 2
			  then F.Times(F.Term (F.Num 2), F.Term (F.Var x))
		      else F.Times(F.Term (F.Num n),F.Exp(F.Term (F.Var x),F.Term (F.Num (n-1)))))
      | exponentRule _ = raise Failure
    fun cosRule (F.Cos (F.Term (F.Var x))) = F.Sin (F.Term (F.Var x))
      | cosRule (F.Cos (F.Term (F.Num n))) = F.Term (F.Num 0)
      | cosRule _ = raise Failure
    fun sinRule (F.Sin (F.Term (F.Var x)))= F.Neg (F.Cos (F.Term (F.Var x)))
      | sinRule (F.Sin (F.Term (F.Num n))) = F.Term (F.Num 0)
      | sinRule _ = raise Failure
    fun singleVarRule (F.Term (F.Var x)) = F.Term (F.Num 1)
      | singleVarRule _ = raise Failure
    fun constantRule expr = F.Term (F.Num 0)
    fun negRule f' = F.Neg f'

end




