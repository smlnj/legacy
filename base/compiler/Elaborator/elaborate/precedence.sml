(* Elaborator/elaborate/precedence.sml *)
(* Copyright 1996 by AT&T Bell Laboratories *)
(* Copyright 2025 by The Fellowship of SML/NJ (www.smlnj.org) *)
(* [DBM, 2025.03.25] Rewritten to clarify and simplify the precedence parsing algorithm. *)

signature PRECEDENCE =
sig
  val parse: {apply: 'a * 'a -> 'a, pair: 'a * 'a -> 'a} -> 
                'a Ast.fixitem list * StaticEnv.staticEnv * SourceMap.region -> 'a

end (* signature PRECEDENCE *)


structure Precedence : PRECEDENCE = 
struct    

local (* imports *)

  structure SM = SourceMap			
  structure EM = ErrorMsg 
  structure S = Symbol
  structure F = Fixity

in 

(* throughout, 'a instantiates uniformly to either Ast.exp or Ast.pat *)

type 'a token = 'a * F.fixity * S.symbol option
  (* a variant of 'a Ast.fixItem, making the F.fixity part explicit, the symbol option is
   * only used in error messages.
   * Note that the symbol, if present, is in the fixity name space, but this 
   * does not matter for printing its name. *)

(* 'a frame:  elements of the precedence parsing stack ('a = Ast.pat, Ast.exp) *)
datatype 'a frame  
  = INFIX of Symbol.symbol * int * 'a  (* int is an infix operator right binding power *)
  | NONFIX of 'a

(* the precedence parsing stack ('a = Ast.pat, Ast.exp) *)
type 'a stack = 'a frame list

(* parse: ['a] {apply: 'a * 'a -> 'a, pair: 'a * 'a -> 'a}
            -> 'a Ast.fixitem list * StaticEnv.staticEnv * SourceMap.region
            -> 'a
 * complete parsing of Ast exp or pattern (represented as partially parsed exp/pat Ast.fixitem list)
 * using precedence parsing
 * - this is not using the regions found in the fixitems. Do they have any other uses? *)
fun 'a parse {apply: 'a * 'a -> 'a, pair: 'a * 'a -> 'a}
	     (items: 'a Ast.fixitem list, env: StaticEnv.staticEnv, region: SM.region) : 'a =

    let fun err (msg: string) = EM.complainRegion (region, "Precedence.parse: " ^ msg)

        (* parseToken : 'a token * 'a stack -> 'a stack *)
	(* 1 step of parsing an expression - driven by loop function below *)
	fun parseToken ((e' , F.NONfix, _): 'a token, (NONFIX e :: rest): 'a stack) =
	      NONFIX (apply (e, e')) :: rest  (* make an application *)
	  | parseToken ((e, fixity, symbolOp): 'a token,
			stack as (INFIX _) :: _) =  (* top of stack is an infix item *)
	      (case fixity (* check fixity *)
		 of F.NONfix => NONFIX e :: stack  (* OK, stack looking for an infix arg *)
		  | F.INfix _ =>  (* error or bug *)
		    (case symbolOp
		      of SOME sym =>
			   (err (String.concat ["expression/pattern begins with infix identifier \"",
					     Symbol.name sym, "\""]); nil)
		       | NONE => EM.impossible "Precedence.parse: - bad token"))
	  | parseToken ( token as (e4, fixity as F.INfix(lbp,rbp), SOME sym): 'a token,
			 NONFIX e1 :: INFIX (_, bp, e2) :: NONFIX e3 :: stack' ) = 
	      (* bp is the rbp of e2, which should be a an infix variable/constructor *)
	      if lbp > bp
	      then INFIX (sym, rbp, e4) :: stack' (* e4 defeats e2, can grab e1 *)
	      else (if lbp = bp
		    then EM.warnRegion
			   (region,
			    "mixed left- and right-associative operators of same precedence")
		    else ();
		    (* "reduce" the three top-of-stack frames to one and start over *)
		    parseToken (token, NONFIX (apply (e2, pair (e3, e1))) :: stack'))

	  | parseToken ( (e', F.INfix(lbp,rbp), SOME sym),
		         stack as NONFIX _ :: stack')  = 
	      INFIX (sym, rbp, e') :: stack'  (* stack is seeking a right argument with power rbp *)

	  (* when stack is empty, i.e. this is the first token *)
          | parseToken ((e, fixity, symbolOp), nil) =
	      (case fixity
		 of F.NONfix => [NONFIX e]
		  | F.INfix _ =>  (* => symbolOp should be SOME sym *)
		     (case symbolOp
			of SOME sym =>
			     (err (String.concat ["expression/pattern begins with infix identifier \"",
						  Symbol.name sym, "\""]);
			      nil)
			  | NONE => EM.impossible "Precedence.parse: - bad token"))

	  | parseToken _ = EM.impossible "Precedence.parseToken"

        (* finish: 'a stack -> 'a *)
	(* clean up the stack, checking for errors *)
	fun finish (NONFIX e1 :: INFIX (_, _, e2) :: NONFIX e3 :: stack') = 
              (* infix application configuration of the stack;
	       * e3 is earlier than e1,
	       * e2 should be a variable exp or pat whose name symbol had an infix binding *)
	      finish (NONFIX (apply (e2, pair (e3,e1))) :: stack')
	  | finish [NONFIX e1] = e1   (* success! *)
	  | finish (INFIX (sym, _, e1) :: NONFIX e2 :: stack) = 
	      (err ("expression or pattern ends with infix identifier \"" 
		   ^ Symbol.name sym ^ "\"");
	       e2)  (* dummy error return value *)
	  | finish nil = EM.impossible "Precedence.parse:finish on nil stack"
	  | finish _ = EM.impossible "Precedence.parse:finish"

        (* itemToToken : 'a Ast.fixitem -> 'a token *)
	fun itemToToken ({item, fixity, ...}: 'a Ast.fixitem) : 'a token =
	      (item,
	       case fixity
		 of NONE => F.NONfix 
		  | SOME sym => Lookup.lookFix(env,sym),
	       fixity)  (* fixity : S.symbol option; SOME sym if sym is an infix variable name *)

        val tokens = map itemToToken items

     in finish (foldl parseToken nil tokens)
	(* or should it be foldr? I think foldl is right. The fixitems should be processed
         * left to right *)
    end (* end fun parse *)

end (* local *)
end (* structure Precedence *)
