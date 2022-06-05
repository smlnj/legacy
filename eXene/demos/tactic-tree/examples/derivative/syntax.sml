
signature EXPRESSION =
sig

    datatype term = 
	Var of string
      | Num of int

    datatype expression =
	Plus of expression * expression
      | Minus of expression * expression
      | Times of expression * expression
      | Divide of expression * expression
      | Exp of expression * expression
      | Neg of expression
      | E of expression
      | Sin of expression
      | Cos of expression
      | Term of term
      | Error
end

structure Expression : EXPRESSION = 
struct

    datatype term = 
	Var of string
      | Num of int

    datatype expression =
	Plus of expression * expression
      | Minus of expression * expression
      | Times of expression * expression
      | Divide of expression * expression
      | Exp of expression * expression
      | Neg of expression
      | E of expression
      | Sin of expression
      | Cos of expression
      | Term of term
      | Error
end
