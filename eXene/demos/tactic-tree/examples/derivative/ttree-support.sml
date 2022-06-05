
structure Derivative_TTree_Support : TTREE_SUPPORT =
struct 


    type goal = Expression.expression
    type event = DerivativeRules.derivative
    type validation = (event list -> event)
    type tactic = (goal -> goal list * validation) 

local open Expression in	
    fun achieves (_,_) = true
	
    fun apply_validation (v,l) = (v l)
    fun apply_tactic (t,g) = (t g)
	
    val tactic_menu = [
		       ("singleVarTac",DerivativeTactics.singleVarTac), 
		       ("productTac", DerivativeTactics.productTac),
		       ("addTac", DerivativeTactics.addTac),
		       ("eTac", DerivativeTactics.eTac),
		       ("sinTac", DerivativeTactics.sinTac),
		       ("cosTac", DerivativeTactics.cosTac),
		       ("chainTac", DerivativeTactics.chainTac),
		       ("expTac", DerivativeTactics.expTac),
		       ("constantTac", DerivativeTactics.constantTac)
		       ]
	
    val tactic_ref = ref ((fn g => ([g],fn [e] => e)) : tactic)
	
    fun goal_to_string goal = let
	fun getString expr = let

	    fun unaryOpString(operand,operator) =
		(case operand of
		     (Term (Var x)) => operator ^ x
		   | (Term (Num n)) => operator ^ (Integer.makestring n)
		   | _ => operator ^ "(" ^ (getString operand) ^ ")")

	    fun binaryOpString (oper1,oper2,operator) =
		(case (oper1,oper2) of
		     (Term (Var x),Term (Num n)) => x ^ "^" ^ (Integer.makestring n)
		   | (Term (Num n),Term (Var x)) => (Integer.makestring n) ^ "^" ^ x
		   | _ => "(" ^ (getString oper1) ^ ")" ^ operator ^ "(" ^ (getString oper2) ^ ")" )
	
	in
 
	    case expr of
		(Term (Num n)) => Integer.makestring n
	      | (Term (Var x)) => x
	      | (Sin expr) => unaryOpString(expr,"sin")
	      | (Cos expr) => unaryOpString(expr,"cos")
	      | (E expr) => unaryOpString(expr,"e^")
	      | (Neg expr) => unaryOpString(expr,"~")
	      | (Exp(expr1,expr2)) => binaryOpString(expr1,expr2,"^")
	      | (Times(expr1,expr2)) => binaryOpString(expr1,expr2,"*")
	      | (Divide(expr1,expr2)) => binaryOpString(expr1,expr2,"/")
	      | (Plus(expr1,expr2)) => binaryOpString(expr1,expr2,"+")
	      | (Minus(expr1,expr2)) => binaryOpString(expr1,expr2,"-")
	end
	fun addNewlines formula =
	    if (length formula) > 40 
		then (rev (nthtail (rev formula,(length formula)-40))) @ ["\n"] @ (addNewlines (nthtail(formula,40)))
	    else formula
			    
    in 
	(implode (addNewlines (explode (getString goal))) ) ^ "\n"
    end 

    val indentation  = "|" 
	
    val unrefined  = "by ?" 
	
    val refined = "by " 
	
    val elision = "..."
	
    val then_text = "THEN"
	
    val thenl_text = "THENL"
	
    val id_tac_text = "id_tac"
    
end 
end



