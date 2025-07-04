(* Elaborator/elaborate/precedence.sml *)
(* Copyright 1996 by AT&T Bell Laboratories *)
(* Copyright 2025 by The Fellowship of SML/NJ (www.smlnj.org) *)
(* [DBM, 2025.03.25] Rewritten to clarify and simplify the precedence parsing algorithm. *)

signature PRECEDENCE =
sig
(*
  val parse: {apply: 'a * 'a -> 'a, pair: 'a * 'a -> 'a} -> 
                'a Ast.fixitem list * StaticEnv.staticEnv * SourceLoc.region -> 'a
*)
  val reparsePat : Ast.pat * SE.staticEnv * SL.region -> Ast.pat
  val reparseExp : Ast.exp * SE.staticEnv * SL.region -> Ast.exp

end (* signature PRECEDENCE *)


structure Precedence : PRECEDENCE = 
struct    

local (* imports *)

  structure SL = SourceLoc
  structure SM = SourceMap
  structure EM = ErrorMsg 

  structure S = Symbol
  structure F = Fixity

  structure ST = Ast

in 

(* throughout, 'a instantiates uniformly to either ST.exp or ST.pat *)

type 'a token = 'a * F.fixity * S.symbol option
  (* a variant of 'a ST.fixItem, making the F.fixity part explicit, the symbol option is
   * only used in error messages.
   * Note that the symbol, if present, is in the fixity name space, but this 
   * does not matter for printing its name. *)

(* 'a frame:  elements of the precedence parsing stack ('a = ST.pat, ST.exp) *)
datatype 'a frame  
  = INFIX of Symbol.symbol * int * 'a  (* int is an infix operator right binding power *)
  | NONFIX of 'a

(* the precedence parsing stack ('a = ST.pat, ST.exp) *)
type 'a stack = 'a frame list

(* parse: ['a] {apply: 'a * 'a -> 'a, pair: 'a * 'a -> 'a}
            -> 'a ST.fixitem list * StaticEnv.staticEnv * SourceMap.region
            -> 'a
 * complete parsing of Ast exp or pattern (represented as partially parsed exp/pat ST.fixitem list)
 * using precedence parsing
 * - this is not using the regions found in the fixitems. Do they have any other uses? *)
fun 'a precedenceParse
       {apply: 'a * 'a -> 'a, pair: 'a * 'a -> 'a}
       (items: 'a ST.fixitem list, env: StaticEnv.staticEnv, region: SL.region) : 'a =
    let fun err (msg: string) = EM.errorRegion (region, "Precedence.parse: " ^ msg)

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

        (* itemToToken : 'a ST.fixitem -> 'a token *)
	fun itemToToken ({item, fixity, ...}: 'a ST.fixitem) : 'a token =
	      (item,
	       case fixity
		 of NONE => F.NONfix 
		  | SOME sym => Lookup.lookFix(env,sym),
	       fixity)  (* fixity : S.symbol option; SOME sym if sym is an infix variable name *)

        val tokens = map itemToToken items

     in finish (foldl parseToken nil tokens)
	(* Or should it be foldr?
	 * I think foldl is right -- the fixitems should be processed in left to right order. *)
    end (* end fun precedenceParse *)


    (* PATTERN reparsing **********************************************************)

    (* span: SL.region * SL.region -> SL.region *)
    (* Returns a minimal length region the contains the two argument regions.
       This function should be moved to SourceLoc.
     *)
    fun span (SL.REGION (l1,r1), SL.REGION (l2,r2)) = 
	  SL.REGION(Int.min(l1,l2),Int.max(r1,r2))
      | span _ => SL.NULLregion

    (* apply_pat : ST.pat * ST.pat -> ST.pat
     * Returned pat is always an AppPat (marked if both arguments were marked).
     * ASSERT: region1 and region2 are not NULLregion.
     *)
    fun apply_pat (c as ST.MarkPat (_,region1), p as ST.MarkPat(_,region2))
	  ST.MarkPat (ST.AppPat {constr=c, argument=p}, span (region1, region2))
      | apply_pat (c, p) = ST.AppPat {constr=c, argument=p}

    (* pair_pat : ST.pat * ST.pat -> ST.pat
     * Returned pat is always a TuplePat of length 2, marked if both args were marked.
     * Actually a pattern pairing function, taking 2 pats and producing a 2 element TuplePat.
     * ASSERT: region1 and region2 are not NULLregion.
     *)
    fun pair_pat (ST.MarkPat(pat1, region1), ST.MarkPat(pat2,region2)) =
	  ST.MarkPat (ST.TuplePat[pat1, pat2], span (region1, region2))
      | pair_pat (a,b) = ST.TuplePat[a,b]

    (* parseFlatApp : ST.pat ST.fixitem list * SE.staticEnv * SL.region -> ST.pat *)
    (* the result of parseFlatApp (and Precedence.parse) can still contain FlatApp subterms,
     * so patCompleteParse still needs to be applied *)
    val parseFlatAppPat = Precedence.parse {apply = apply_pat, pair = pair_pat}

    (* reparsePat : ST.pat * SE.staticEnv * SL.region -> ST.pat
     * Recursively reparse a pattern, eliminating all FlatAppPat subpatterns.
     * recurse through the entire pat, eliminating any FlatAppPats.
     * We need to pass an env to pass to parseFlatApp in case there have been additional infix
     *   constructor symbols in FlatAppPat subpatterns that have to be parsed with parseFlatApp.
     * This function, and parseFlatApp, should be moved to the Precedence structure. The same
     * should be done for exp reparsing, I presume. Rename to "reparsePat".
     *)
    fun reparsePat (pat: ST.pat, env: SE.staticEnv, region: SL.region) : ST.pat =
	let fun recurse pat = 
		(case pat
		   of ST.MarkPat (pat', region') => MarkPat (reparsePat (pat', env, region'), region')
	              (* preserve the orignial Marked region *)
		    | ST.ListPat pats =>
	                ST.ListPat (map recurse pats)
		    | ST.TuplePat pats =>
		        ST.TuplePat (map recurse pats)
		    | ST.VectorPat pats =>
		        ST.VectorPat (map recurse pats)
		    | ST.OrPat pats =>
		        ST.OrPat (map recurse pats)
		    | ST.RecordPat {def, flexibility} =>
		        ST.RecordPat {def = map (fn (s, p) -> (s, recurse p)) def,
				      flexibility = flexibility}
		    | ST.AppPat {constr, argument} =>
	                ST.AppPat {constr = recurse constr, argument = recurse argument}
		    | ST.LayeredPat {varPat, expPat} =>
	                ST.LayeredPat {varPat = recurse varPat,
				       expPat = recurse expPat}
		    | ST.FlatAppPat patFixitems =>
	                parseFlatAppPat (patFixitems, env, region)
		    | _ => Pat)
	 in recurse pat
	end


    (* EXPRESSION reparsing **********************************************************)

    (* apply_exp : ST.exp * ST.exp -> ST.exp
     * Returned pat is always an AppPat (marked if both arguments were marked) *)
    fun apply_exp (ST.MarkExp (e1, region1), ST.MarkExp (e2, region2)) =
	  ST.MarkExp (ST.AppExp {function = e1, argument = e2}, span (region1, region2))
      | apply_exp (e1, e2) = ST.AppExp {function = e1, argument = e2}

    (* pair_exp : ST.exp * ST.exp -> ST.exp
     * Returned pat is always a TuplePat (marked if both args were marked).
     * Actually a pattern pairing function, taking 2 pats and producing a 2 element TuplePat. *)
    fun pair_exp (ST.MarkExp (e1, region1), ST.MarkExp(e2, region2)) =
	  ST.MarkExp (ST.TupleExp [e1, e2], span (region1, region2))
      | pair_exp (a,b) = ST.TupleExp [a,b]

    (* parseFlatAppExp : ST.exp ST.fixitem list * SE.staticEnv * SL.region -> ST.exp *)
    (* the result of parseFlatApp (and Precedence.parse) can still contain FlatApp subterms,
     * so patCompleteParse still needs to be applied *)
    val parseFlatAppExp = precedenceParse {apply = apply_exp, pair = pair_exp}

    (* reparseExp : ST.exp * SE.staticEnv * SL.region -> ST.exp
     * Recursively reparse a pattern, eliminating all FlatAppPat subpatterns.
     *) 
   fun reparseExp (exp: ST.exp, env: SE.staticEnv, region: SL.region) : ST.exp =
       let fun recurse exp = 
	       (case exp
		  of ST.VarExp => exp		(* variable *)
		   | ST.FnExp rules =>		(* abstraction *)
		       ST.FnExp (map (reparseRule (env, region)), rule)
		   | ST.AppExp {function, argument} =>  (* simple application *)
		       ST.AppExp {function = recurse function, argument = recurse argument}
		   | ST.MarkExp (exp, region') =>     (* mark an expression *)
		       ST.MarkExp (reparseEsp (exp, env, region'), region')
		   | ST.CaseExp {expr:exp, rules:rule list} => (* case expression *)
		       ST.CaseExp (exp = recurse exp, rules = map (reparseRule (exp, region)) rules}
		   | ST.LetExp {dec: dec, expr: exp} => (* let expression *)
		       ST.LetExp {dec = reparseDec (dec, env, region), expr = recurse expr}
		   | ST.SeqExp exps => (* sequence expression *)
		       ST.SeqExp (map recurse exps)
		   | ST.RecordExp (fields: (S.symbol * exp) list) => 
		       ST.RecordExp (map (fn (label, exp) => (label, recurse exp)) fields)
		   | ST.ListExp exps => (* list expression (of form [e1, ...]) *)
		       ST.ListExp (map recurse exps)
		   | ST.TupleExp exps => (* tuple expression *)
		       ST.TupleExp (map recurse exps)
		   | ST.VectorExp exps => (* vector expression *)
		       ST.VectorExp (map recurse exps)
		   | ST.ConstraintExp {expr:exp,constraint:ty} =>  (* type constraint *)
		       ST.ConstraintExp {expr = recurse expr, constraint = constraint}
		   | ST.HandleExp {expr:exp, rules:rule list} => (* exception handler *)
		       ST.HandleExp {expr = recurse expr,
				     rules = (map (reparseRule (env, region)) rules)}
		   | ST.RaiseExp exp =>  (* raise an exception *)
		       ST.RaiseExp (recurse exp)
		   | ST.IfExp {test: ST.exp, thenCase: ST.exp, elseCase: ST.exp} =>
		       ST.IfExp {test = recurse test,
				 thenCase = recurse thenCase,
				 elseCase = recurse elseCase}
		   | ST.AndalsoExp (exp1, exp2) =>
		       ST.AndalsoExp (recurse exp1, recurse exp2)
		   | ST.OrelseExp (exp1, exp2) =>
		       ST.OrelseExp (recurse exp1, recurse exp2)
		   | ST.WhileExp {test: ST.exp, expr: ST.exp} =>
		       ST.WhileExp {test = recurse test, expr = recurse expr}
		   | ST.FlatAppExp expFixitems =>	(* expressions before fixity parsing *)
		       parseFlatAppExp (expFixitems, env, region)
		   | _ => exp)
        in recurse exp
       end (* fun reparseExp *)

(*  Cases covered by _ => exp default rule; having no exp subterm *)
      | IntExp of literal		(* integer *)
      | WordExp of literal		(* word literal *)
      | RealExp of real_lit		(* floating point coded by its string *)
      | StringExp of string		(* string *)
      | CharExp of string		(* char *)
      | SelectorExp of S.symbol		(* selector of a record field *)
*)

(* reparseRule : SE.statenv * SL.region -> ST.rule -> ST.rule *)
and reparseRule : (env, region) (ST.Rule {pat, exp}) = 
    ST.Rule {pat = reparsePat (pat, env, region),
             exp = reparseExp (exp, env, region)}

(* reparseDec : ST.dec * SE.statenv * SL.region -> ST.dec *)
and reparseDec (dec, env, region) =

    (* DECLARATIONS (let and structure) *)
    and dec = ValDec of (vb list * tyvar list)		(* values *)
	    | ValrecDec of (rvb list * tyvar list)	(* recursive values *)
	    | DoDec of exp				(* 'do' exp *)
	    | FunDec of (fb list * tyvar list)		(* recurs functions *)
	    | TypeDec of tb list			(* type dec *)
	    | DatatypeDec of {datatycs: db list, withtycs: tb list} (* datatype dec *)
	    | DataReplDec of S.symbol * path              (* dt replication *)
	    | AbstypeDec of {abstycs: db list, withtycs: tb list, body: dec} (* abstract type *)
	    | ExceptionDec of eb list			(* exception *)
	    | StrDec of strb list			(* structure *)
	    | FctDec of fctb list			(* functor *)
	    | SigDec of sigb list			(* signature *)
	    | FsigDec of fsigb list			(* funsig *)
	    | LocalDec of dec * dec			(* local dec *)
	    | SeqDec of dec list			(* sequence of dec *)
	    | OpenDec of path list			(* open structures *)
	    | OvldDec of S.symbol * exp list            (* overloading (internal; restricted) *)
	    | FixDec of {fixity: F.fixity, ops: S.symbol list}  (* fixity *)
	    | MarkDec of dec * SL.region		        (* mark a dec *)

end (* local - imports *)
end (* structure Precedence *)
