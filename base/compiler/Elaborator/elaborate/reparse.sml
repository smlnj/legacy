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

(* ReparseFct is a functor over the <item; apply, pair, fixity> signature.
 * This functor will be applied twice, for item = Ast.pat and item = Ast.exp. *)


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

  type item = I.item  (* = Ast.pat or Ast.exp *)

  (* internal types *)
  (* a token is essentially an item annotated with precedence information if the item is an
   * infix symbol *)
  datatype token
    = Infix of item *     (* item is a simple infix symbol (as a pattern or expression) *)
	       int *      (* LBP -- left precedence binding power of the infix symbol *)
	       int        (* RBP -- right precedence binding power of the infix symbol *)
    | Nonfix of item      (* item is nonfix symbol, path, or atomic wrt "non-binding" *)

  (* the precedence parsing stack ('a = ST.pat, ST.exp) *)
  type stack = token list

  exception PRECEDENCE
    (* raised on a precedence parsing error, but handled locally within
     * the precedenceParse function. *)

  (* precedenceParse: I.item list * StaticEnv.staticEnv * SourceMap.region -> I.item list
   * where I.item will be instantiated to Ast.pat or Ast.exp.
   * complete parsing of Ast exp or pattern (represented as partially parsed exp/pat item list)
   * using precedence parsing
   * - regions found in the items are not useful and are not used? *)
  fun precedenceParse (items: I.item list, env: StaticEnv.staticEnv, region: SL.region) : I.item option =
      let fun err (msg: string) =
	      (EM.errorRegion (region, "Reparse.precedenceParse: " ^ msg);
	       raise PRECEDENCE)

	  (* parseItem : 'item token * 'item stack -> 'item stack *)
	  (* 1 step of parsing an expression - folder function used in body of main let *)
	  fun parseItem (item: I.item, stack: stack) : stack =
	      let val token =
		      (case I.isInfix (item, env)
			 of NONE => Nonfix item       (* not an infix symbol *))
			  | SOME args => Infix args)  (* item is an infix symbol *)

	       in case (token, stack)
		   of (I.Nonfix item2, I.Nonfix item1 :: stack') =>
		      (* application of a Nonfix to a Nonfix on top of the stack.
		       * item2 always grabs item1 as an argument, but the application
		       * can fail in the case of patterns if item2 is . *)
		      (case I.apply (item1, item2)
			 of SOME item => Nonfix item :: stack'
			  | NONE => (* err "Nonfix application" *) token :: stack)
			            (* push the token onto the stack without "applying" it *)

		     | (I.Nonfix _, I.Infix _ :: _) => token :: stack
			 (* infix operator on top of stack wants to grab a right argument,
			  * the Nonfix token becomes its candidate right argument *)

		     | (I.Infix (item, _, _), I.Infix _ :: _) => 
		         (* two infix operators in a row is an error *)
			 err (String.concat ["term begins with infix symbol \"",
					      Option.valOf (I.name item), "\""])

		     | (I.Infix (item1, lbp1, rbp1),
			I.Nonfix item3 :: I.Infix (item2, name2, _, rbp2) :: I.Nonfix item4 :: stack') =>
		          (* the new Infix operator item1 on the right is fighting the left infix
			     operator item2 for control of the Nonfix item3 that is at the top of
			     the stack. *)
		          (case compare (rbp1, lbp2)
			     of GREATER => 
				  (* item3, item2, item4 tripple reduced to a Nonfix application *)
				  token :: NONFIX (I.apply (item2, I.pair (item4, item3))) :: stack')
		              | LESS => token :: stack'
				  (* item1 pushed on the stack, with item 3 becoming its left argument
				     for later "reduction" after item1 gets it right argument. *)
			      | EQUAL => (* tie goes to item2 -- left associativity in the mixed case *)
			         (EM.warnRegion
				    (region,
				     "mixed left- and right-associative operators of same precedence");
				  token :: NONFIX (I.apply (item2, I.pair (item4, item3))) :: stack')


		     | (I.Infix _, I.Nonfix _ :: stack')  = 
		         (* token if infix operator:
			  * The old nonfix token on top of the stack will be its left argument,
		          * The infix operator, pushed onto the top of stack, is seeking a nonfix
			  * right argument. *)
			 token :: stack

		     (* stack is empty => this is the first token; push it onto the empty stack *)
		     | (I.Nonfix _ , nil) => [token]

		     | (I.Infix (item, _, _), nil) =>  
			 err (String.concat ["term begins with infix symbol \"",
					      Option.valOf (I.name item), "\""])
		     |  _ => EM.impossible "Precedence.parseItem: bad token or stack"
	      end

	  (* finish: token stack -> token stack *)
	  (* finish parsing remaining infixes on the stack, checking for errors *)
	  fun finish (I.Nonfix arg2 :: I.Infix (operator, _, _, _) :: I.Nonfix arg1 :: stack') = 
		(* infix application configuration of the stack;
		 * arg1 is "earlier" than (thus to the left of) arg2, thus already on the stack,
		 * operator is an infix symbol pat or exp *)
		finish (I.Nonfix (I.apply (operator, I.pair (arg1, arg2))) :: stack')
	    | finish (I.Infix (item1, _, _) :: I.Nonfix _ :: stack) = 
		(err ("expression or pattern ends with infix identifier \"" 
		     ^ Symbol.name (Option.valOf (I.name item1)) ^ "\"");
		 NONE)
	    | finish stack = stack
(*	    | finish [I.Nonfix e1] = SOME e1   (* success! for pat *) *)

          (* in the result token stack returned by finish, all tokens should be Nonfix. *)

				 
          (* tokenToItem : token -> I.item *)
          fun tokenToItem (Nonfix item) = item
	    | filterStack (Infix _) = EM.impossible "finish"

          val reult =
	      map (filterStack (foldl parseItem items nil)
	      handle PRECEDENCE => nil

       in result
      end (* fun precedenceParse *)

(* A bit of postprocessing will be required in both the pat and exp cases after using precedenceParse. *)

end (* local -- imports *)
end (* functor ReparseFct *)


structure PatternItem : ITEM =
struct 

    structure SL = SourceLocation
    structure ST = Ast
    structure F = Fixity
 
    type item = Ast.pat

    (* apply : ST.pat * ST.pat -> ST.pat option *)
    (* Check that rator is a VarPat, yielding an operator path.
     * Thus this function can fail. *)
    fun apply (oper: ST.pat, rator: ST.pat) =
	  (case oper
	     of ST.Varpat path => SOME (ST.AppPath {constr = path, argument = rator})
	      | _ => NONE (* indicating that the oper was not a VarPat *)

    val nonfixApply = infixApply

    (* pair : ST.pat * ST.pat -> ST.pat
     * Returned pat is always a TuplePat of length 2, marked if both args were marked.
     * Actually a pattern pairing function, taking 2 pats and producing a 2 element TuplePat.
     * ASSERT: region1 and region2 are not NULLregion.
     *)
    fun pair (a,b) = ST.TuplePat[a,b]

    (* isInfix : item * SE.staticEnv -> (item * string * int * int) option *)
    fun isInfix (pat: item, env: SE.staticEnv) =
	(case pat
  	   of Ast.VarPat [name] =>
	        (case LU.lookFix (env, S.toFix name)
	           of  F.NONfix => NONE
		    |  F.INFix (lbp, rbp) =>  SOME (exp, name, lbp, rbp))
            | _ => NONE)

    (* name : item -> string option *)
    fun name (Ast.VarPat [n]) = SOME (Symbol.name n)
      | name _ = NONE

end (* structure PatternItem *)	    


structure ExpressionItem : ITEM =
struct 

    structure SL = SourceLocation
    structure ST = Ast
    structure F = Fixity		       
 
    type item = ST.exp
     
    (* apply : item * item -> item option
     * Always succeeds and returns SOME (AppExp (item1, itemd2)) because any item1 exp
     * is an acceptible operator at this point. *)
    fun apply (item1, item2) =
	  SOME (ST.AppExp {function = item1, argument = item2})  (* always succeeds *)

    (* pair : item * item -> item
     * Returned pat is always a TuplePat (marked if both args were marked).
     * Actually a pattern pairing function, taking 2 pats and producing a 2 element TuplePat.
     * We don't worry about location-marking the result. *)
    fun pair (a,b) = ST.TupleExp [a,b]

    (* isInfix : item * SE.staticEnv -> (item * string * int * int) option *)
    (* item (exp) is a variable that has an INFix binding in the given static environment *)
    fun isInfix (exp, env) =
	(case exp
  	   of Ast.VarExp [name] =>
	        (case LU.lookFix (env, S.toFix name)
	           of  F.NONfix => NONE
		    |  F.INFix (lbp, rbp) =>  SOME (exp, name, lbp, rbp))
            | _ => NONE)

    (* name : item -> string option *)
    fun name (Ast.VarExp [n]) = SOME (Symbol.name n)
      | name _ = NONE

end (* structure ExpressionItem *)

(* Example: exp items x y z w *)


(* structure Reparse *)
(* specializing the interpretation of the result of precedenceParse for patterns and
 * expressions. *)

structure Reparse : REPARSE =
struct
    
  structure  ReparsePat = ReparseFct (PatternItem) 
  structure  ReparseExp = ReparseFct (ExpressionItem)

  val reparsePats = ReparsePat.precedenceParse
  val reparseExps = ReparseExp.precedenceParse

  (* FlatAppPat reparsing *)
  (* checks that reparsing a list of pats produces a single result pat *)
  fun parseFlatAppPat pats =
      case reparsePats pats
        of [pat] => pat
	 | _ => (* multiple patterns is an error!? *)
	    EM.error "reparsePat: multiple patterns in result"

  (* fundec clause analyisys *)
  (* analyzes the result of reparseExps *)
  fun foo () = ()

  (* FlatAppExp reparsing *)
  fun parseFlatAppExp exps =


  (* what about processing the case where the precedenceParse produces multiple exps? *)
  val parseFlatAppExp = ReparseExp.precedenceParse

  (* reparsePat : ST.pat * SE.staticEnv * SL.region -> ST.pat
   * Recursively reparse a pattern, eliminating _all_ embedded FlatAppPat subpatterns,
   * recursing through the entire pat, eliminating any and all FlatAppPats nodes.
   * We need to pass an env to identify infix constructor symbols and their fixity properties.
   * Since there is no "let" form in patterns, a pattern and all of its subpatterns are
   * in the scope of the same static environment and hence the same infix bindings, so we
   * can safely recurse down through the whole pattern structure without changing the env. *)
  fun reparsePat (pat: ST.pat, env: SE.staticEnv, region: SL.region) : ST.pat =
        let fun recurse pat = 
		(case pat
		   of ST.MarkPat (pat', region') => MarkPat (reparsePat (pat', env, region'), region')
	              (* preserve the original Marked region *)
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

(*
   A similar full expression reparsing function of type

     reparseExp : ST.exp * SE.staticEnv * SL.region -> ST.exp

   is not possible, because bodies of embedded let subexpressions would need to
   be called with a possibly augmented staticEnv argument. Thus full reparsing for
   expressions cannot be separated from general elaboration becuase

   Instead, we have to use reparseFlatAppExp incrementally for the FlatAppExp case
   in the elabExp function and depend on the recursive definition of ElabCore..elabExp
   to generate the appropriate static environments (and hence infix bindings) for let bodies.
*)

end (* structure Reparse *)
