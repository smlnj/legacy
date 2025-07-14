(* Elaborator/elaborate/reparse.sml *)
(* Copyright 1996 by AT&T Bell Laboratories *)
(* Copyright 2025 by The Fellowship of SML/NJ (www.smlnj.org) *)

(* [DBM, 2025.03.25] Rewritten to clarify and simplify the precedence parsing algorithm. 
 * [DBM, 2025.07.05] The parse function has been replaced by two specialized versions:
 *   reparseFlatAppPat for Ast patterns and reparseFlatAppExp for Ast expressiohns.
 * [DBM, 2025.07.12] Reparse is now a functor over an ITEM argument with REPARSE as its result signature.
 *)

(* Reparse: REPARSE is an auxiliary module used only in ElabCore. Its functionality
 * is used to "reparse" the contents of FlatAppPat and FlatAppExp (partially parsed applications)
 * using infix bindings provided by a static environment.
 *)

(* Reparse probably should be a functor over the <item; apply, pair, fixity> signature.
 * Then this functor would be applied twice, for item = Ast.pat and item = Ast.exp. *)


functor ReparseFct (I: ITEM): PRECEDENCE_PARSE where type item = I.item = 
struct    

local (* imports *) 

  structure SL = SourceLoc
  structure SM = SourceMap
  structure EM = ErrorMsg 

  structure S = Symbol
  structure F = Fixity
  structure ST = Ast  (* ST for "Syntax Tree" *)
  structure AU = AstUtil

  structure SE = StaticEnv		    

in 

  type item = I.item

  (* internal types *)
  type token
	 = Infix of item *    (* item is an infix symbol (in a pattern or expression) *)
		    string *   (* infix symbol's name -- used in an error message *)
		    int *      (* LBP -- left (precedence) binding power *)
		    int        (* RBP -- right (precedence) binding power *)
         | Nonfix of item     (* item is nonfix symbol or otherwise "non-binding" wrt precedence *)

  (* the precedence parsing stack ('a = ST.pat, ST.exp) *)
  type stack = token list

  exception PRECEDENCE

(* precedenceParse: I.item list * StaticEnv.staticEnv * SourceMap.region -> I.item
 * complete parsing of Ast exp or pattern (represented as partially parsed exp/pat ST.fixitem list)
 * using precedence parsing
 * - this is not using the regions found in the fixitems. Do they have any other uses? *)
fun precedenceParse (items: I.item list, env: StaticEnv.staticEnv, region: SL.region) : I.item option =
    let fun err (msg: string) = EM.errorRegion (region, "Reparse.precedenceParse: " ^ msg)

        (* parseItem : 'item token * 'item stack -> 'item stack *)
	(* 1 step of parsing an expression - folder function used in body of main let *)
	fun parseItem (item: I.item, stack: stack) : stack =
	    let val token =
		    (case I.fixity (item, env)
		       of NONE => Nonfix item
			| SOME args => Infix args)

	     in case (token, stack)
		 of (I.Nonfix item2, I.Nonfix item1 :: rest) =>
		       I.Nonfix (I.apply (item1, item2)) :: rest  (* construct an application *)

		   | (I.Nonfix item, I.Infix _ :: _) => token :: stack
		       (* OK, stack looking for an infix arg *)
		       (* top of stack was an infix frame, push nonfix token *)

		   | (I.Infix (_, name, _, _), I.Infix _ :: _) => 
		       (err (String.concat ["expression/pattern begins with infix identifier \"",
					  name, "\""]);
			raise PRECEDENCE) 

		   | (I.Infix (item4, name4, lbp4, rbp4),
		      I.Nonfix item1 :: I.Infix (item2, name2, _, rbp2) :: I.Nonfix item3 :: stack') =>
		        (* rbp is the RBP of e2, which should be a an infix VALspace symbol *)
		        if lbp4 > rbp2
		        then token :: stack' (* infix token defeats item2 token, will grab item1 later *)
		        else (if lbp2 = rbp4  (* check how precedences work *)
			      then EM.warnRegion
				      (region,
				       "mixed left- and right-associative operators of same precedence")
			      else ();
			      (* "reduce" the three top-of-stack frames to one and push token *)
			      token :: NONFIX (I.apply (item2, I.pair (item3, item1))) :: stack')

		   | (I.Infix _, I.Nonfix _ :: stack')  = 
		       token :: stack (* top of stack is seeking a right argument with power rbp *)

		   (* stack is empty => this is the first token *)
		   | (I.Nonfix item , nil) => [token]

		   | (I.Infix (_, name, _, _), nil) =>  
		       (err (String.concat ["expression/pattern begins with infix symbol \"",
					    Symbol.name sym, "\""]);
			raise PRECEDENCE)  (* error abort!? *)

		   |  _ => EM.impossible "Precedence.parseItem: bad token or stack"
	    end

        (* finish: I.item stack -> I.item option *)
	(* clean up the stack, checking for errors *)
	fun finish (I.Nonfix arg2 :: I.Infix (operator, _, _, _) :: I.Nonfix arg1 :: stack') = 
              (* infix application configuration of the stack;
	       * e3 is earlier than e1,
	       * e2 should be a variable exp or pat whose name symbol had an infix binding *)
	      finish (I.Nonfix (I.apply (operator, I.pair (arg1, arg2))) :: stack')
	  | finish [I.Nonfix e1] = SOME e1   (* success! *)
	  | finish (I.Infix (item1, _, _, _) :: I.Nonfix item2 :: stack) = 
	      (err ("expression or pattern ends with infix identifier \"" 
		   ^ Symbol.name sym ^ "\"");
	       NONE)
	  | finish nil = EM.impossible "Precedence.parse:finish on nil stack"
	  | finish _ = EM.impossible "Precedence.parse:finish"

     in finish (foldl parseItem items nil)
        handle PRECEDENCE => NONE

    end (* fun precedenceParse *)

end (* functor Reparse *)


structure PatternItem : ITEM =
struct 

    structure SL = SourceLocation
    structure ST = Ast
    structure F = Fixity
 
    type item = Ast.pat

    (* apply_pat : ST.pat * ST.pat -> ST.pat
     * Returned pat is always an AppPat (marked if both arguments were marked).
     * ASSERT: region1 and region2 are not NULLregion.
     *)
    fun apply (c as ST.MarkPat (_,region1), p as ST.MarkPat(_,region2))
	  ST.MarkPat (ST.AppPat {constr=c, argument=p}, SL.regionUnion (region1, region2))
      | apply (c, p) = ST.AppPat {constr=c, argument=p}

    (* pair_pat : ST.pat * ST.pat -> ST.pat
     * Returned pat is always a TuplePat of length 2, marked if both args were marked.
     * Actually a pattern pairing function, taking 2 pats and producing a 2 element TuplePat.
     * ASSERT: region1 and region2 are not NULLregion.
     *)
    fun pair (ST.MarkPat(pat1, region1), ST.MarkPat(pat2,region2)) =
	  ST.MarkPat (ST.TuplePat[pat1, pat2], span (region1, region2))
      | pair (a,b) = ST.TuplePat[a,b]

    (* fixity : item -> (item * string * int * int) option *)
    fun fixity pat =
	(case AU.patToFixSymbol pat
	   of NONE => NONE (* not an infix valSymbol pat *)
	    | SOME (fixsym, name) =>
	      (case LU.lookFix fixsym
	         of  F.NONfix => Nonfix pat
		  |  F.INFix (lbp, rbp) =>  SOME (pat, name, lbp, rbp)))

end (* structure PatternItem *)	    


structure ExpressionItem : ITEM =
struct 

    structure SL = SourceLocation
    structure ST = Ast
    structure F = Fixity		       
 
    type item = ST.exp
     
    (* apply_exp : item * item -> item
     * Returned pat is always an AppPat (marked if both arguments were marked) *)
    fun apply_exp (ST.MarkExp (e1, region1), ST.MarkExp (e2, region2)) =
	  ST.MarkExp (ST.AppExp {function = e1, argument = e2}, SL.regionUnion (region1, region2))
      | apply_exp (e1, e2) = ST.AppExp {function = e1, argument = e2}

    (* pair_exp : item * item -> item
     * Returned pat is always a TuplePat (marked if both args were marked).
     * Actually a pattern pairing function, taking 2 pats and producing a 2 element TuplePat. *)
    fun pair_exp (ST.MarkExp (e1, region1), ST.MarkExp(e2, region2)) =
	  ST.MarkExp (ST.TupleExp [e1, e2], span (region1, region2))
      | pair_exp (a,b) = ST.TupleExp [a,b]

    (* fixity_exp : SE.staticEnv * item -> (item * string * int * int) option *)
    fun fixity_exp (exp, env) =
	(case AU.expToFixSymbol exp
	   of NONE => NONE (* not an infix valSymbol exp *)
	    | SOME (fixsym, name) =>
	      (case LU.lookFix (env, fixsym)
	         of  F.NONfix => Nonfix exp
		  |  F.INFix (lbp, rbp) =>  SOME (exp, name, lbp, rbp)))

end (* structure ExpressionItem *)


structure Reparse : REPARSE =
struct
   
  structure  ReparsePat = ReparseFct (PatternItem) 
  structure  ReparseExp = ReparseFct (ExpressionItem)

  (* FlatAppPat reparsing *)
  val parseFlatAppPat = ReparsePat.precedenceParse

  (* FlatAppExp reparsing *)
  val parseFlatAppExp = ReparseExp.precedenceParse

end (* structure Reparse *)


(* An extra deep-reparsing function for patterns is possible, where a single global static
   environment will be available.  This can be added to Reparse if needed.

    (* reparsePat : ST.pat * SE.staticEnv * SL.region -> ST.pat
     * Recursively reparse a pattern, eliminating _all_ embedded FlatAppPat subpatterns,
     * recursing through the entire pat, eliminating any and all FlatAppPats nodes.
     * We need to pass an env to identify infix constructor symbols and their fixity properties.
     * Since there is no "let" form in patterns, a pattern and all of its subpatterns are
     * in the scope of the same static environment and hence the same infix bindings, so we
     * can safely recurse down through the whole pattern structure without changing the env.
     *)
     (* Do we really ever need to do a complete reparse, or is reparsing just the top node
      -- when it is a FlatAppPat -- all we need to do.  This depends perhaps on how we 
      do the initial "reparsing" pass on the lhs of a "fun" declaration (vb, rvb).
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
	                recurse (parseFlatAppPat (patFixitems, env, region))
		    | _ => Pat)
	 in recurse pat
	end

   But a deep expression reparsing function of type

     reparseExp : ST.exp * SE.staticEnv * SL.region -> ST.exp

   is not possible, because bodies of embedded let subexpressions would need to
   be called with a possibly augmented staticEnv argument for the body of let expressions.
   So reparsing for expressions cannot be fully separated from general elaboration becuase of
   the "let" case, which requires elaboration of the dec part of the let.

   Instead of using reparseExp, we use reparseFlatAppExp for the FlatAppExp case
   in the elabExp function and depend on the recursive definition of elabExp (in ElabCore)
   to generate the appropriate static environments (and hence infix bindings) for let bodies.


    (* Reparsing expressions is complicated by the fact that the declaration part of a
       let binding may introduce new infix declarations, so the fixity part of the
       reparsing environment may change, and the reparsing of the body will need to be
       done using that new fixity environment. So we just provide the reparseFlatAppExp
       function and let the recursion of elabExp deal with providing the proper env
       context (e.g. adjusting the env for the body of let expressions). *)

*)
