(* Elaborator/elaborate/elabcore.sml
 *
 * COPYRIGHT (c) 2017, 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* TODO:
 * 1. move full reparse fuction(s) for pat and exp into the Precedence structure. 
 *)

signature ELABCORE =
sig

  val elabABSTYPEdec :
        {abstycs: Ast.db list, withtycs: Ast.tb list, body: Ast.dec} *  (* arg type of Ast.AbstypeDec *)
        StaticEnv.staticEnv * (Types.tycon -> bool) * InvPath.path * SourceLoc.region
        -> Absyn.dec * StaticEnv.staticEnv (* * Modules.entityEnv ??? *)
    (* elabABSTYPEdec needs to be defined here because it needs to elaborate the body declaration
     * of the abstype declaration, and this body can include core declarations. *)

  val elabDec :
        Ast.dec * StaticEnv.staticEnv * (Types.tycon -> bool)
        * InvPath.path * SourceLoc.region
        -> Absyn.dec * StaticEnv.staticEnv

  val debugging : bool ref

end (* signature ELABCORE *)


structure ElabCore: ELABCORE =
struct

local (* imports *)

  structure Tbl = SymbolHashTable

  structure SL = SourceLoc  (* source regions *)
  structure SM = SourceMap  (* char pos to line * column coordinates *)
  structure EM = ErrorMsg

  structure S = Symbol
  structure F = Fixity  (* goes away? *)
  structure A = Access
  structure LV = LambdaVar
  structure SP = SymPath  (* direct symbolic paths *)
  structure IP = InvPath  (* reverse symbolic paths *)
  structure B  = Bindings
  structure SE = StaticEnv  (* multi-namespace static environments *)
  structure LU = Lookup   (* lookup for static environments *)

  structure V =  VarCon  (* => Variable, the Con part is covered by Types (datacon) *)
  structure ST = Ast    (* syntax trees, partially parsed (FlatAppPat, FlatAppExp, clause using fixitems) *)
  structure AS = Absyn
  structure AU = AbsynUtil

  structure T  = Types
  structure TS = TyvarSet
  structure TU = TypesUtil
  structure BT = BasicTypes

  structure M  = Modules
  structure MU = ModuleUtil
  structure EE = EntityEnv

  structure EU = ElabUtil
  structure ED = ElabDebug
  structure ET = ElabType

  (* open (Absyn => AS.; Ast => Ast.; Types => T.; Access => A.; ElabUtil => EU. *)

in

(* debugging print functions *)

val say = Control_Print.say

val debugging : bool ref = ElabControl.ecdebugging
(* a flag enabling printing of Elaboration debugging messages *)

(* debugmsg : string -> unit *)
fun debugmsg (msg: string) = if !debugging then (say msg; say "\n") else ()

(* bug : string -> 'a *)
fun bug (msg: string) = ErrorMsg.impossible ("ElabCore: " ^ msg)

(* debugPrint : ? *)
val debugPrint = (fn x => ED.debugPrint debugging x)


(* showDec : string * AS.dec * staticEnv.env -> unit (?)
 * general declaration printer using JHR PP library.
 * Needs to be revised to use DBM PrettyPrint library. *)
fun showDec (msg, dec, env) =
    debugPrint(msg,
	       (fn pps => fn dec => PPAbsyn.ppDec (env,NONE) pps (dec, 100)),
	       dec)

(* ========================================================================================== *)
(* some specialized error reporting functions *)

(* error0 : SL.region * EM.severity * string -> unit *)
(* error0 is EM.error "partially applied" to EM.nullErrorBody (=> empty format) *)
fun error0 (region: SL.region, severity: EM.severity, msg: string) : unit =
    EM.error region severity msg EM.nullErrorBody

(* error : string -> unit *)
(* error0 with region = nullRegion and severity = EM.COMPLAIN *)
fun error (msg: string) : unit =
    error0 (SL.NULLregion, EM.COMPLAIN, msg)

(* errorRegion : SL.region * string -> unit *)
fun errorRegion (region, msg) = error0 (region, EM.COMPLAIN, msg)

(* checkedUnion : SL.region * string -> (TS.tyvarset * TS.tyvarset, region) -> TS.tyvarset *)
(* check tyvarset unions for incompatible tyvars, e.g. 'a and ''a in "same scope" *)
fun checkedUnion (region: SL.region, Where: string)
      (tyvarset1: TS.tyvarset, tyvarset2 : TS.tyvarset) : TS.tyvarset =
    case TS.union (tyvarset1, tyvarset2)
      of SOME u => u
       | NONE => (errorRegion (region, "tyvarset union [" ^ Where ^"]"); TS.empty)

(* checkedDiff : SL.region * string -> (TS.tyvarset * TS.tyvarset, region) -> TS.tyvarset *)
(* check tyvarset differences for incompatible tyvars, e.g. 'a and ''a in "same scope" *)
fun checkedDiff (region: SL.region, Where: string)
      (tyvarset1: TS.tyvarset, tyvarset2 : TS.tyvarset) : TS.tyvarset =
    case TS.diff (tyvarset1, tyvarset2)
      of SOME u => u
       | NONE => (errorRegion (region, "tyvarset union [" ^ Where ^"]"); TS.empty)


(* ========================================================================================== *)
(* REAL32: still relevant? *)
(* bounds for Real64.real constant values; these should be moved to overload
 * resolution once we support more than one size of real. *)

val minSubnormalReal64 = RealLit.real{isNeg = false, whole="4", frac="9", exp = ~324}
val minNormalReal64 = RealLit.real{isNeg = false, whole="2", frac="2250738585072014", exp = ~308}
val maxReal64 = RealLit.real{isNeg = false, whole="1", frac="7976931348623157", exp = 308}


(* ========================================================================================== *)
(* tyvarset updater functions *)
(* what do these updater functions do? -- to be rediscovered and documented at some point! *)

(* tyvarsetUpdater -- the type of updater functions for tyvarsets.
 * These assign a tyvarset to a hidden ref that is embedded in an absyn structure for a declaration
 * (tyvars components of VB and RVB constructs in particular. *)
type tyvarsetUpdater = TS.tyvarset -> unit

(* nullUpdater : tyvarsetUpdater  -- was no_updater! *)
fun nullUpdater (_ : TS.tyvarset) = ()

(* AS.dec * SE.staticEnv -> AS.dec * SE.staticEnv * TS.tyvarset * tyvarsetUpdater *)
(* expands a dec * env pair to a (dec * env * tyvarset * tyvarsetUpdater) quadruple
 * by adding empty tyvarset and nullUpdater *)
fun noTyvars (dec, env) = (dec, env, TS.empty, nullUpdater)


(* ========================================================================================== *)
(* Types associated with parsing and elaboration function declarations that are used local
 * to elabFunDec, and are related to processing lazy function declarations (lazyKind). A
 * couple of these types need to be moved to ElabUtil because they are connected to the 
 * FUNdec function defined ther -- or alternatively FUNdec could be moved to this file. *)

(* lazyKind: used for communicating information about lazy FunDec decs
 * between preprocessing phase (fb_folder, formerly makevar) and the main part of elabFunDec. *)

datatype lazyKind = STRICT | DELAY | FORCE  (* attribute of (Ast) fundec (parsed fb) *)

(* fb_clause: the argument type, or contents, of the Ast.Clause data constructor.
   This represents a partially parsed clause of a function declaration (represented by an fb).
   This is defined here as documentation of the input for clause elaboration.
   An Ast.Fb (recursive function binding, a list of which is found in the FunDec declaration
   variant) contains a list of these clauses, plus a boolean flag specifying whether the function
   is lazy.

   The pats field (which ouught to be named "lhs") contains a list of "partially parsed"
   patterns of type Ast.pat Ast.fixitem, which constitute the LHS of a clause, while the
   exp field contains the RHS expression of the clause.

   The LHS of a clause ("pats") is actually a partially parsed list of (curried) parameters
   represented as a list of pat fixitem that were produced by parsing corresponding
   atomic pats (apats in ml.grm). The function name (the subject of the parent fb) and the
   actual argument pattern list are extracted by the parseLHS function
   (elabFunDec#fb_folder#parseLHS). *)

type fb_clause =  {pats: pat fixitem list, resultty: ty option, exp:exp}

(* ast_clause -- the basic, fully parsed, fb clause, the elements, containing just 
 * two things: the argpats (LHS) and exp (RHS), derived by parsing an fb_clause, and
 * not including the properties that are, in principle, common to all clauses
 * of an fb, like the function name, the curried arity, and the (optional) result type.
 * -- argpats:  the argument patterns (Ast.pat), fully parsed, after the analysis to
 *              extract the function name symbol. The number of argpats should be the
 *              same for all clauses of a function declaration (i.e., an Ast.fb).
 * -- exp:      the RHS expression of the clause. *)

type ast_clause = {argpats: Ast.pat list, exp: Ast.exp}  (* belongs in ElabUtil? *)

(* fundec0, fundec -- information about a function declaration (fb) accumulated in the
 * _parsing_ of its clauses in ast_clausesFolder, fundec0 corresponds to a "re-parsed" fb.
 * This re-parsing is mostly pre-elaboration, but optional result types are elaborated
 * so we can check consistency between clauses (while producing their respective tyvarsets,
 * which also should also be "consistent", whatever that means. For now, it means that the
 * tyvarsets of multiple result types can be union-ed, but this may be too weak. We need
 * to investigate how explicit type variables can be "shared" among clauses of an fb and
 * also across multiple fbs in a Ast.FunDec.
 *
 * Note that the funsym (function name), arity, and result type option are
 * considered attributes of the whole function declaration, and not of particular
 * clauses of the function declaration. Similarly, the lazyKind and region of a fundec
 * are also attributes of the declaration rather than particular clauses.
 *
 * Note also that it is unusual for the result type (if present) to occur in more than
 * one clause. It would normally only occur in the first clause. The fact that there might
 * be multiple result type specifications could be considered a flaw in the concrete syntax
 * of function declarations.
 *)

type fundec0  (* fold accumulator argument in ast_clausesFolder -- a "subset" of fundec *)
  = {funsym: S.symbol,   (* the symbol is the function name - must be consistent across clauses*)
     arity: int,         (* arity = length argpats - must be consistent across clauses *)
     resTyOp: (T.ty * TS.tyvarset) option  (* the elaborated optional result type - must be
					    * consistent across clauses *)
     clauses: ast_clause list}      (* the list of "parsed" function clauses *)

(* fundec adds lazyKind and region to fundec0 *)
type fundec
  = {funsym: S.symbol,   (* the symbol is the function name - must be consistent across clauses*)
     arity: int,         (* arity = length argpats - must be consistent across clauses *)
     resTyOp: (T.ty * TS.tyvarset) option, (* the elaborated optional result type - must be
					    * consistent across clauses *)
     clauses: ast_clause list,  (* the ast_clause list is the list of parsed function clauses *)
     lazy: lazyKind,
     region: SL.region}

(* as_clause: the Absyn record for a clause produced by elabClause
   When the pats list has length > 1, the function is curried. *)
type as_clause = (* belongs in ElabUtil? *)
     {pats : AS.pat list,
      exp : AS.exp} 

(* Absyn version of fundec, produced by the "mk_as_fundec" function by elaborating the Ast fundec. *)
type as_fundec =
     {funsym: S.symbol,
      clauses: as_clause list
      tyvarset: TS.tyvarset ref
      region: SL.region}

(* 
   Defn of type Ast.fixitem, repeated here for refernce.
   fixitems are produced by the parser (ml.grm.sml) as the result of parsing atomic patterns
   and atomic expressions.

   ('a |-> pat or 'a |-> exp)
   type 'a fixitem = {item: 'a,                (* a pattern, Ast.pat or expression, Ast.exp *)
                      fixity: S.symbol option, (* SOME f' <=> item = VarPat [f], where
                                                * f and f' are var- and fix- symbols with
						* the same name *)
		      region: SL.region}

   Where 'a will be instantiated to either Ast.pat (for pat reparsing) or Ast.exp
   (for exp reparsing).

*)

(* ========================================================================================== *)
(* some utility functions not defined in ElabUtil -- though they could be moved there! *)

(* stripExpAbs: AS.exp -> AS.exp
 * strip MARKexp and CONSTRAINTexp wrappers around an expression *)
fun stripExpAbs (MARKexp (e,_)) = stripExpAbs e
  | stripExpAbs (CONSTRAINTexp (e,_)) = stripExpAbs e
  | stripExpAbs e = e

(* stripExpAst : Ast.exp -> Ast.exp
 * Like stripExpAbs except operating on the Ast representtion. *)
fun stripExpAst(MarkExp(e,r'),r) = stripExpAst(e,r')
  | stripExpAst(ConstraintExp{expr=e,...},r) = stripExpAst(e,r)
  | stripExpAst(SeqExp[e],r) = stripExpAst(e,r)
  | stripExpAst(FlatAppExp[{item,region,...}],r) = stripExpAst(item,region)
  | stripExpAst x = x

(* dummyFNexp : AS.exp : As.exp
 * A dummy function expression representing "fn _ => raise <bogusExn>". *)
val dummyFNexp: AS.exp =
    FNexp([RULE(WILDpat,RAISEexp(CONexp(V.bogusEXN,[]),T.UNDEFty))],T.UNDEFty)

(* LAZY? *)

local
    (* mkCoreExp : [name:]string -> [env:]SE.staticEnv -> AS.exp *)
    fun mkCoreExp name env = AS.VARexp (ref (CoreAccess.getVar env [name]), [])
in
    val mkAssignExp : SE.staticEnv -> AS.exp = mkCoreExp "assign"  (* AS.exp for ":=" operator *)
    val mkBangExp : SE.staticEnv -> AS.exp   = mkCoreExp "deref"   (* AS.exp for "!" operator *)
end


(* ========================================================================================== *)
(* The elaboration functions *)

exception FreeOrVars
(* used when creating a hash table in the OrPat case of elabPat *)

(* ELABORATE GENERAL (core) DECLARATIONS -- the main elaboration function *)
and elabDec (dec, env, isFree, rpath, region) =

let
    val _ = debugmsg ">>ElabCore.elabDec"

    val completeMatch = EU.completeMatch (env,"Match")
    val _ = debugmsg "--ElabCore.elabDec << completeBind Match"
    val completeBind = EU.completeMatch (env,"Bind")
    val _ = debugmsg "--ElabCore.elabDec << completeBind Bind"

    (* newVALvar : S.symbol -> V.var *)
    fun newVALvar (sym: S.symbol) = V.mkVALvar (sym, A.namedAcc sym)

    (* PATTERN parsing and elaboration **********************************************************)

    (* apply_pat : Ast.pat * Ast.pat -> Ast.pat
     * Returned pat is always an AppPat (marked if both arguments were marked) *)
    fun apply_pat (c as MarkPat(_,SL.REGION(l1,r1)), p as MarkPat(_,SL.REGION(l2,r2))) =
	  MarkPat (AppPat {constr=c, argument=p}, SL.REGION(Int.min(l1,l2),Int.max(r1,r2)))
      | apply_pat (c, p) = AppPat {constr=c, argument=p}

    (* tuple_pat : Ast.pat * Ast.pat -> Ast.pat
     * Returned pat is always a TuplePat (marked if both args were marked).
     * Actually a pattern pairing function, taking 2 pats and producing a 2 element TuplePat. *)
    fun tuple_pat (a as Ast.MarkPat(_,SL.REGION(l,_)), b as Ast.MarkPat(_,SL.REGION(_,r))) =
	  Ast.MarkPat (Ast.TuplePat[a,b],SL.REGION(l,r))
      | tuple_pat (a,b) = Ast.TuplePat[a,b]

    (* parseFlatApp : Ast.pat Ast.fixitem list * SE.staticEnv * SL.region -> Ast.pat *)
    (* the result of parseFlatApp (and Precedence.parse) can still contain FlatApp subterms,
     * so patCompleteParse still needs to be applied *)
    val parseFlatApp = Precedence.parse {apply=apply_pat, pair=tuple_pat}

    (* patCompleteParse : Ast.pat * SE.staticEnv * SL.region -> Ast.pat
     * result pat contains no FlatAppPat subpatterns
     * recurse through the entire pat, eliminating any FlatAppPats.
     * We need to pass an env to pass to parseFlatApp in case there have been additional infix
     *   constructor symbols in FlatAppPat subpatterns that have to be parsed with parseFlatApp.
     * This function, and parseFlatApp, should be moved to the Precedence structure. The same
     * should be done for exp reparsing, I presume. Rename to "reparsePat".
     *)
    fun patCompleteParse (pat: Ast.pat, env: SE.staticEnv, region: SL.region) =
        (case pat
	   of Ast.MarkPat (pat', region') => MarkPat (patCompleteParse (pat', region'), region')
	        (* preserve the orignial Marked region *)
	    | Ast.ListPat pats => Ast.ListPat (map patCompleteParse pats)
	        let fun f p => patCompleteParse (p, region)
		 in Ast.ListPat (map f pats)
		end
	    | Ast.TuplePat pats =>
	        let fun f p => patCompleteParse (p, region)
		 in Ast.TuplePat (map f pats)
		end
	    | Ast.VectorPat pats =>
	        let fun f p => patCompleteParse (p, region)
		 in Ast.VectorPat (map f pats)
		end
	    | Ast.OrPat pats =>
	        let fun f p => patCompleteParse (p, region)
		 in Ast.OrPat (map f pats)
		end
	    | Ast.RecordPat {def, flexibility} =>
	        let fun f (s,p) => (s, patCompleteParse (p, region))
		 in Ast.RecordPat {def = map f def, flexibility = flexibility}
		end
	    | Ast.AppPat {constr, argument} =>
	        Ast.AppPat {constr = patCompleteParse (constr, region), patCompleteParse (argument, region)}
	    | Ast.LayeredPat {varPat, expPat} =>
	        Ast.LayeredPat {varPat = patCompleteParse (varPat, region),
				expPat = patCompleteParse (expPat, region)}
	    | Ast.FlatAppPat patFixitems =>
	        parseFlatApp (patFixitems, env, region)
	    | _ => pat

    (* LAZY: utilities for lazy sml translation **************************************************)

    (* will one forcingFun do, or should new ones be generated with
     * different bound variables for each use? (DBM) *)

    (* forceExp : AS.exp -> AS.exp *)
    fun forceExp (exp: AS.exp) =
	let val v = newVALvar (S.varSymbol "x")
	 in AS.APPexp (AS.FNexp(completeMatch
				  [AS.RULE (AS.APPpat (BT.dollarDcon, nil, AS.VARpat v), AS.VARexp (ref v, nil))],
				T.UNDEFty),
		       exp)
	     (* DBM: second arg of APPpat and VARexp = nil and
	      * of FNexp = T.UNDEFty ok? *)
	end

    (* delayExp : AS.exp -> AS.exp *)
    fun delayExp exp =
	AS.APPexp (AS.CONexp (BT.dollarDcon, nil), exp)

    (* lrvbMakeY : int -> V.var * AS.dec
     * lrvbMakeY n: build Absyn declaration of n-ary Y combinator for lazy val rec *)
    fun lrvbMakeY n =
	let fun upto 0 = nil
	      | upto n = n :: (upto (n-1))

	    val base: int list = rev(upto n)  (* n |-> [1,2,...,n] *)

	    fun repeat f = map f base

	    fun hold e = delayExp(forceExp e)

	    (* capture Match exn from coreEnv as a random exn for use internally
	       in the Y combinator definition. (maybe bogusEXN would work? *)
	    val exn = CoreAccess.getExn env ["Match"]

	    (* Y variable and local variables ri and fi and d *)
	    val yvar (* as VALvar{path,typ,access,prim} *) =
		newVALvar (S.varSymbol ("Y$"^(Int.toString n)))

	    fun mkVarSym s i = newVALvar (S.varSymbol(s^(Int.toString i)))
	    val rvars = repeat (mkVarSym "r$")
	    val fvars = repeat (mkVarSym "f$")
	    val dvar  = newVALvar (S.varSymbol "d$")

	    (* "ref($(raise Match))" *)
	    (* rdrExp : int -> AS.exp *)
	    fun rdrExp (i: int) =
		AS.APPexp (AS.CONexp (BT.refDcon, []),
			   delayExp (AS.RAISEexp (AS.CONexp (exn,[]), T.UNDEFty)))
	    val rpat  = AU.TUPLEpat (map AS.VARpat rvars)
	    val rexp  = AU.TUPLEexp (repeat rdrExp)
	    val rdec  = AS.VALdec ([AS.VB{pat=rpat, exp=rexp, boundtvs=[], tyvars=ref TS.empty}])

	    (* "$(force(!ri))" *)
	    fun dfbr rv = hold(APPexp(mkBangExp env,VARexp(ref rv,[])))
	    val ddec  = VALdec[VB{pat=VARpat dvar, exp=AU.TUPLEexp(map dfbr rvars),
				  boundtvs=[], tyvars=ref TS.empty}]

	    fun dexp () = VARexp(ref dvar,[])
	    fun setrExp (rv,fv) =
		APPexp(mkAssignExp env,
		       AU.TUPLEexp([AS.VARexp(ref rv,[]),
				 hold(AS.APPexp(AS.VARexp(ref fv,[]),dexp()))]))
	    val updates = ListPair.map setrExp (rvars,fvars)

	    val yexp = AS.FNexp(completeMatch
			       [AS.RULE(AU.TUPLEpat(map VARpat fvars),
				     AS.LETexp(SEQdec[rdec,ddec],
					    AS.SEQexp(updates@[dexp()])))],
			     T.UNDEFty)

	 in (yvar, AS.VALdec [AS.VB {pat = AS.VARpat yvar, exp = yexp, boundtvs = nil,
				     tyvars = ref TS.empty}])
	end (* fun lrvbMakeY *)


    (**** EXCEPTION DECLARATIONS ****)

    (* elabEb : Ast.eb * SE.staticEnv * SL.region -> AS.eb * S.symbol * SE.staticEnv * TS.tyvarset *)
    fun elabEb (eb: Ast.eb, env: SE.staticEnv, region: SL.region) =
	case eb
	  of Ast.EbGen{exn=ename,etype} =>
	       let val (ety, evt, etyOp, const) =
	       	       case etype
		         of NONE => (BT.exnTy, TS.empty, NONE, true) (* constant exn constructor *)
			  | SOME typ =>
			    let val (ty,vt) = ET.elabType(typ,env,region)
                             in (BT.--> (ty, BT.exnTy), vt, SOME ty, false)
			    end
	           val exn =
		       T.DATACON{name = ename, const = const, typ = ety, lazyp = false,
			       rep = A.EXN (A.LVAR (LV.namedLvar ename)), sign = A.CNIL}
		in (AS.EBgen {exn = exn, etype=etyOp, ident=STRINGexp(S.name ename)},
		    ename, SE.bind (ename, B.CONbind exn, SE.empty), evt)
	       end
	   | Ast.EbDef{exn = ename, edef = qid} =>
	       let val edef as T.DATACON {const, typ, sign, ...} =
		       (case LU.lookVal (env, SP.SPATH qid)
			  of SOME (V.CON (dcon as T.DATACON{rep=(A.EXN _), ...})) => dcon
			   | SOME (V.CON _) =>
			     (errorRegion
				(region,
				 "ElabCore.elabEb[EbDef]: found data constructor instead of exception");
			      V.bogusEXN)
			   | SOME (V.VAL _) =>
			     (errorRegion
			       (region,
				"ElabCore.elabEb[EbDef]: found variable instead of exception");
			      V.bogusEXN)
			   | NONE =>
			     (errorRegion
			        (region,
				 "ElabCore.elabEb[EbDef]: unbound exception name: "^(S.name ename));
			      V.bogusEXN)
		       (* end case *))
		     val rep = A.EXN (A.LVAR (LV.namedLvar ename))
		     val exn = T.DATACON {name=ename, const=const, typ=typ, lazyp=false,
					  sign=sign, rep=rep}
		 in (AS.EBdef {exn = exn, edef = edef},
		     ename,
		     SE.bind(ename, B.CONbind exn, SE.empty),
		     TS.empty)
		end
	   | Ast.MarkEb (eb, region) => elabEb (eb, env, region)

    fun elabEXCEPTIONdec (excbinds:Ast.eb list, env: SE.staticEnv, region) =
	let fun folder (exc,(ebs,enames,env_acc,tyvarset_acc)) =
		  let val (eb, ename, env_inc, tyvarset_inc) = elabEb (exc, env, region)
		   in if EU.checkForbiddenCons ename
		      then errorRegion
			    (region,
			     concat["exception name \"", S.name ename, "\" is forbidden"])
		      else ();
		      (eb::ebs, ename::enames, SE.atop(env_inc,env_acc),
		       checkedUnion (region, "elabEXCEPTIONdec") (tyvarset_inc,tyvarset_acc))
		  end
	    val (ebs,enames,env,tyvarset) = foldl folder ([], [], SE.empty, TS.empty) excbinds
	 in if EU.checkUniq enames
            then ()
	    else errorRegion (region, "duplicate exception declaration");
		 (* FIX: error message should include the duplicate exception constructor name(s) *)
	    (EXCEPTIONdec(rev ebs),env,tyvarset,nullUpdater)
	end


    (* elabPat : Ast.pat * SE.staticEnv * SL.region -> AS.pat * TS.tyvarset
     * The only case that can introduce new members of the tyvarset is Ast.ConstraintPat, where
     * they can come from the type constraint. *)
    fun elabPat (pat: Ast.pat, env: SE.staticEnv, region: SL.region) : AS.pat * TS.tyvarset =
    let val union = checkedUnion (region, "elabPat") in
    case pat
      of Ast.WildPat => (WILDpat, TS.empty)
       | Ast.VarPat path =>
	   (EU.clean_pat (EU.pat_id (SP.SPATH path, env)), TS.empty)
       | Ast.IntPat (src: string, s: IntInf.int) =>
	   (AS.NUMpat (src, {ty = T.VARty (T.mkTyvar (T.OVLDI [(s, region)])), ival = s}: AS.num_lit),
	    TS.empty)
       | Ast.WordPat (src: string, s: IntInf.int) =>
	   (AS.NUMpat (src, {ty = T.VARty (T.mkTyvar (T.OVLDW [(s, region)])), ival = s}: AS.num_lit),
	    TS.empty)
       | Ast.StringPat s => (STRINGpat s, TS.empty)
       | Ast.CharPat s => (CHARpat s, TS.empty)
       | Ast.RecordPat {def,flexibility} =>
	   let val (fields, tyvarset) = elabPatFields (def, env, region)
	    in (EU.makeRECORDpat (fields, flexibility), tyvarset)
	   end
       | Ast.ListPat nil =>
	   (EU.NILpat, TS.empty)
       | Ast.ListPat (a::rest) =>
	   let val (p, tyvarset) = elabPat (TuplePat [a, ListPat rest], env, region)
	    in (EU.CONSpat p, tyvarset)
	   end
       | Ast.TuplePat pats =>
	   let val (ps, tyvarset) = elabPatList(pats, env, region)
	    in (AU.TUPLEpat ps, tyvarset)
	   end
       | Ast.VectorPat pats =>
	   let val (ps, tyvarset) = elabPatList(pats, env, region)
	    in (AS.VECTORpat (ps, T.UNDEFty), tyvarset)
	   end
       | Ast.OrPat pats =>
         (* Check that the sub-patterns of an or-pattern have exactly the same
          * free variables, and rewrite the sub-pattersn so that all instances
          * of a given free variable have the same type ref and the same
          * access.
          * [DBM, 2025.03.11] This OrPat case has been rewritten for some reason in the smlnj repository.
          * The differences need to be understood. Was the change a bug fix?  Which version is newer?
          * Which version is "correct"?
          *)
	   let val (ps, tyvarset) = elabPatList(pats, env, region)
	       fun freeOrVars (pat::pats) =
		   let val tbl : (A.access * T.ty ref * int) Tbl.hash_table =
			   Tbl.mkTable (16, FreeOrVars)
		       fun insert kv = Tbl.insert tbl kv
		       fun look k = Tbl.lookup tbl k
		       fun errorMsg (x: S.symbol) =
			   errorRegion
			     (region,
			      ("variable " ^ S.name x ^ " does not occur in all branches of or-pattern"))
		       fun insFn (id: S.symbol, access, tyref) =
			   (insert (id, (access, tyref, 1)); (access, tyref))
		       fun bumpFn (id: S.symbol, access0: A.access, tyref0: T.ty ref) =
			   (let val (access, tyref, n) = look id
			     in insert (id, (access, tyref, n+1)); (access,tyref)
			    end
			    handle FreeOrVars =>
				   (errorMsg id; (access0, tyref0)))
		       fun checkFn (id: S.symbol, access0: A.access, tyref0: T.ty ref) =
                           (let val (access, tyref, _) = look id
                             in (access, tyref)
                            end
			    handle FreeOrVars =>
				   (errorMsg id; (access0, tyref0)))
		       fun doPat (insFn: (S.symbol * A.access * T.ty ref) -> A.access * T.ty ref) =
			   let fun doPat' (AS.VARpat (V.VALvar {access, prim, path, btvs, typ})) =
				     let val (access,typ) = insFn (SymPath.first path, access, typ)
				      in AS.VARpat (V.VALvar {access = access,
							      path = path,
							      prim = prim,
							      btvs = btvs,
							      typ = typ})
				     end
				 | doPat' (AS.RECORDpat{fields, flex, typ}) =
				     AS.RECORDpat
				       {fields = map (fn (l, p) => (l, doPat' p)) fields,
					flex = flex, typ = typ}
				 | doPat' (AS.APPpat(dc, ty, pat)) =
				     APPpat(dc, ty, doPat' pat)
				 | doPat' (AS.CONSTRAINTpat(pat, ty)) =
				     CONSTRAINTpat(doPat' pat, ty)
				 | doPat' (AS.LAYEREDpat(p1, p2)) =
				     LAYEREDpat(doPat' p1, doPat' p2)
				 | doPat' (AS.ORpat(p1, p2)) =
				     ORpat(doPat' p1, doPat checkFn p2)
				 | doPat' (AS.VECTORpat(pats, ty)) =
				     VECTORpat(map doPat' pats, ty)
				 | doPat' (AS.MARKpat(pat, region)) =
				     doPat' pat  (* could narrow the region for errorMsg here *)
				 | doPat' pat = pat
			      in doPat'
			     end
		       (* check that each variable occurs in each sub-pattern *)
		       fun checkComplete m (id, (_, _, n:int)) =
			   if (n = m) then () else (errorMsg id)

		       val pats = (doPat insFn pat) :: (map (doPat bumpFn) pats)

		    in Tbl.appi (checkComplete (length pats)) tbl;
		       pats

		   end (* freeOrVars *)

		 | freeOrVars _ = bug "freeOrVars"

	       val (pat, pats) =
		   (case freeOrVars ps
		      of (h::t) => (h, t)
		       | _ => bug "elabPat:no free or vars")

	       fun foldOr (p, []) = p
		 | foldOr (p, p'::r) = ORpat(p, foldOr(p', r))

	    in (foldOr (pat, pats), tyvarset)
	   end (* [ORpat] case *)

       (* DBM: questionable treatment of AppPat: constr field of Ast.AppPat should just be a path
        * which should be looked up in the env to get a datacon. EU.makeAPPpat should still work,
        * but without the unnecessary MARKpat (rator, region) case. *)
       | Ast.AppPat {constr, argument} =>
	   let fun getVar (Ast.MarkPat (p, region), _) = getVar (p, region)
		 | getVar (Ast.VarPat path, region) =
		      let val dcon_pat = EU.pat_id (SP.SPATH path, env)
			  val (arg_pat, tyvarset) = elabPat (argument, env, region)
		       in (EU.makeAPPpat (dcon_pat, arg_pat), tyvarset)
		      end
		 | getVar (_, region) =
		   (errorRegion (region, "non-constructor applied to argument in pattern");
		    (WILDpat, TS.empty))
	    in getVar (constr, region)
	   end

       | Ast.ConstraintPat {pattern=pat,constraint=ty} =>
	   let val (pat', tyvarset_pat) = elabPat (pat, env, region)
	       val (ty', tyvarset_ty) = ET.elabType (ty, env, region)
	    in (AS.CONSTRAINTpat (pat', ty'), union(tyvarset_pat, tyvarset_ty))
	   end

       | Ast.LayeredPat {varPat, expPat} =>
	   let val (pat1, tyvarset1) = elabPat (varPat, env, region)
	       val (pat2, tyvarset2) = elabPat (expPat, env, region)
	    in (EU.makeLAYEREDpat(pat1, pat2), union (tyvarset1, tyvarset2))
	   end

       | Ast.MarkPat (pat,region) =>
	   let val (pat', tyvarset) = elabPat(pat, env, region)
	    in (AS.MARKpat (pat', region), tyvarset)
	   end

       | Ast.FlatAppPat pats => elabPat (parseFlatApp (pats, env, region), env, region)

    end (* fun elabPat *)

    (* elabPatFields : [fields:](S.symbol * Ast.pat) list, [env:]SE.staticEnv, [region:]SL.region)
                    -> (S.symbol * Absyn.pat) list * TS.tyvarset *)
    and elabPatFields (fields: (S.symbol * Ast.pat) list, env: SE.staticEnv, region: SL.region) =
	let fun folder ((label,pat),(fields, tyvarset1)) =
	      let val (pat', tyvarset2) = elabPat (pat, env, region)
	       in ((label, pat') :: fields, checkedUnion (region, "elabPatFields") (tyvarset2, tyvarset1))
	      end
         in foldl folder ([],TS.empty) fields
	end

    (* elabPatList : Ast.pat list * SE.staticEnv * SL.region -> AS.pat list * TS.tyvarset *)
    and elabPatList(pats: Ast.pat list, env:SE.staticEnv, region: SL.region) =
	let fun folder (ast_pat, (pats_acc, tyvarset_acc)) =
		let val (pat', tyvarset_inc) = elabPat (ast_pat, env, region)
		 in (pat' :: pats_acc,
		     checkedUnion (region, "elabPatList") (tyvarset_inc, tyvarset_acc))
		end
	 in foldr folder ([],TS.empty) pats  (* foldr to maintain order *)
	end

    (**** EXPRESSIONS ****)

    (* expParse : Ast.exp Ast.fixitem list * SE.staticEnv * SL.region -> Ast.exp *)
    val expParse = Precedence.parse
		     {apply = (fn(f,a) => AppExp{function=f,argument=a}),
		      pair = (fn (a,b) => TupleExp[a,b])}

    (* elabExp : Ast.exp * SE.staticEnv * SL.region -> Absyn.exp * TS.tyvarset * tyvarsetUpdater *)
    fun elabExp (exp: Ast.exp, env: SE.staticEnv, region: SL.region)
		: (Absyn.exp * TS.tyvarset * tyvarsetUpdater) =
        let val union = checkedUnion (region, "elabExp") in
 	(case exp
	  of Ast.VarExp path =>  (* path : S.symbol list *)
	       let val sympath =  SP.SPATH path
		in ((case LU.lookVal (env, sympath)
		      of SOME value =>
		       (case value
			  of V.VAL v => AS.VARexp (ref v,[])
			   | V.CON (d as T.DATACON{lazyp,const,...}) =>
			       if lazyp
			       then  (* LAZY *)
				 if const
				 then delayExp (CONexp (d,[]))
				 else
				   let val var = newVALvar(S.varSymbol "x")
				    in AS.FNexp (completeMatch
					       [RULE (AS.VARpat var,
						      delayExp (AS.APPexp (AS.CONexp (d, []),
									   AS.VARexp (ref(var), []))))],
						 T.UNDEFty)  (* DBM: ??? *)
				    end
			       else AS.CONexp (d, []))  (* not LAZY *)
		     | NONE => (* path not found in env *)
			 (errorRegion (region, "elabExp: unbound path" ^ SymPath.toString sympath);
			  AS.SEQexp nil)), (* dummy return value of type AS.exp *)
		  TS.empty,
		  nullUpdater)
	       end (* [Ast.VarExp] *)
(* TODO: propagate the source string to Absyn for error reporting *)
	   | Ast.IntExp (src: string, s: IntInf.int) =>
	       (AS.NUMexp (src, {ty = T.VARty (T.mkTyvar (T.OVLDI [(s, region)])), ival = s}: AS.num_lit),
	        TS.empty, nullUpdater)
	   | Ast.WordExp (src: string, s: IntInf.int) =>
	       (AS.NUMexp (src, {ty = T.VARty (T.mkTyvar (T.OVLDW [(s, region)])), ival = s}: AS.num_lit),
	        TS.empty, nullUpdater)
	   | Ast.RealExp (src: string, r: RealLit.t) =>
	        let fun result r =
			(AS.REALexp (src, {rval = r, ty = BT.realTy}: AS.real_lit),
			 TS.empty,
			 nullUpdater)
		 in (* REAL32: this test gets moved to overload resolution *)
		     case Real64ToBits.classify r
		       of IEEEReal.INF =>
			    (* literal would cause overflow when converted to IEEE float format *)
			    (errorRegion (region, concat ["real literal '", src, "' is too large"]);
			     result r)
		       | IEEEReal.ZERO =>
			   if RealLit.isZero r
			   then result r
			   else (EM.error region EM.WARN
				  (String.concat
				     ["real literal '", src, "' is too small and will be rounded to ",
				      if (RealLit.isNeg r) then "~0.0" else "0.0"])
				  EM.nullErrorBody;
		               result (RealLit.zero(RealLit.isNeg r)))
		       | _ => result r
		end
	   | Ast.StringExp s => (AS.STRINGexp s, TS.empty, nullUpdater)
	   | Ast.CharExp s => (AS.CHARexp s, TS.empty, nullUpdater)
	   | Ast.RecordExp cells =>
	       let val (les, tyv, updater) = elabExpFields (cells, env, region)
		in (EU.makeRECORDexp les, tyv, updater)
	       end
	   | Ast.SeqExp exps =>
	       (case exps
		  of [exp] => elabExp (exp, env, region)
		   | nil => bug "elabExp(SeqExp[])"
		   | _ =>
		       let val (exps, tyvarset, updater) = elabExpList(exps,env,region)
		        in (AS.SEQexp exps, tyvarset ,updater)
		       end)
	   | Ast.ListExp nil => (EU.NILexp, TS.empty, nullUpdater)
	   | Ast.ListExp (a::rest) =>
	       let val (e,tyvarset,updater) =
                       elabExp (Ast.TupleExp [a, Ast.ListExp rest], env, region)
		in (AS.APPexp (EU.CONSexp, e), tyvarset, updater)
	       end
	   | Ast.TupleExp exps =>
	       let val (es,tyv,updt) = elabExpList(exps,env,region)
		in (EU.TUPLEexp es,tyv,updt)
	       end
	   | Ast.VectorExp exps =>
	       let val (exps', tyvarset, updater) = elabExpList (exps, env, region)
		in (AS.VECTORexp (exps', T.UNDEFty), tyvarset, updater)
	       end
	   | Ast.AppExp {function,argument} =>
	       let val (functionExp, tyvarset_fun, updater_fun) = elabExp (function, env, region)
		   and (argExp, tyvarset_arg, updater_arg) = elabExp(argument,env,region)
		   fun updater tv = (updater_fun tv; updater_arg tv)
		in (APPexp (functionExp, argExp), union (tyvarset_fun, tyvarset_arg), updater)
	       end
	   | Ast.ConstraintExp {expr, constraint} =>
	       let val (exp', tyvarset_exp, updater) = elabExp (exp, env, region)
		   val (constraint', tyvarset_constraint) = ET.elabType (constraint, env, region)
	        in (AS.CONSTRAINTexp (exp', constraint'),
		    union (tyvarset_exp, tyvarset_constraint),
		    updater)
	       end
	   | Ast.HandleExp {expr, rules} =>
	       let val (expr', tyvarset_expr, updater_expr) = elabExp (expr, env, region)
		   val (rules', tyvarset_rules, updater_rules) = elabMatch (rules, env, region)
		   fun updater tv = (updater_expr tv; updater_rules tv)
		in (EU.makeHANDLEexp (expr', rules'),
		    union (tyvarset_expr, tyvarset_rules),
		    updater)
	       end
	   | Ast.RaiseExp exp =>
	       let val (e,tyv,updater) = elabExp(exp,env,region)
		in (RAISEexp (e, T.UNDEFty), tyv, updater)
	       end
	   | Ast.LetExp {dec,expr} =>
	       let val (d1, e1, tyvarset1, updater1) =
			  elabDec' (dec, env, IP.IPATH[], region)
		   val (e2, tyvarset2, updater2) = elabExp (expr, SE.atop(e1,env), region)
		   fun updater tv = (updater1 tv; updater2 tv)
		in (LETexp (d1, e2), union (tyvarset1, tyvarset2), updater)
	       end
	   | Ast.CaseExp {expr,rules} =>
	       let val (e1, tyvarset1, updater1) = elabExp (expr,env,region)
		   val (rls2, tyvarset2, updater2) = elabMatch (rules,env,region)
		   fun updater tv = (updater1 tv;updater2 tv)
	        in (CASEexp (e1,completeMatch rls2, true), union (tyvarset1, tyvarset2), updater)
	       end
	   | Ast.IfExp {test,thenCase,elseCase} =>
	       let val (e1,tv1,updater1) = elabExp (test, env, region)
		   and (e2,tv2,updater2) = elabExp (thenCase, env, region)
		   and (e3,tv3,updater3) = elabExp (elseCase, env, region)
		   fun updater tv = (updater1 tv; updater2 tv; updater3 tv)
		in (AS.IFexp { test = e1, thenCase = e2, elseCase = e3 },
		    union (tv1, union(tv2,tv3)),
		    updater)
	       end
	   | Ast.AndalsoExp (exp1,exp2) =>
	       let val (e1, tyvarset1, updater1) = elabExp (exp1, env, region)
		   and (e2, tyvarset2, updater2) = elabExp (exp2, env, region)
		   fun updater tyvarset = (updater1 tyvarset; updater2 tyvarset)
		in (ANDALSOexp(e1, e2), union (tyvarset1,tyvarset2), updater)
	       end
	   | Ast.OrelseExp (exp1,exp2) =>
	       let val (e1, tv1, updater1) = elabExp (exp1, env, region)
		   and (e2, tv2, updater2) = elabExp (exp2, env, region)
		   fun updater tv = (updater1 tv; updater2 tv)
		in (ORELSEexp(e1, e2), union (tv1,tv2), updater)
	       end
	   | Ast.WhileExp {test,expr} =>
	       let val (test', tyvarset_test, updater_test) = elabExp (test, env, region)
		   and (expr', tyvarset_expr, updater_expr) = elabExp(expr,env,region)
		   fun updater tyvarset = (updater_test tyvarset; updater_expr tyvarset)
		in (AS.WHILEexp { test = test', expr = expr'},
		    union (tyvarset_test, tyvarset_expr),
		    updater)
	       end
	   | Ast.FnExp rules =>
	       let val (rls,tyv,updater) = elabMatch(rules,env,region)
		in (FNexp (completeMatch rls, T.UNDEFty), tyv, updater)
	       end
	   | Ast.MarkExp (exp,region) =>
	       let val (exp', tyvarset, updater) = elabExp(exp,env,region)
		in (AS.MARKexp (exp',region), tyvarset, updater)
	       end
	   | Ast.SelectorExp s =>
	       (let val v = newVALvar s
		 in AS.FNexp (completeMatch
			       [AS.RULE (AS.RECORDpat {fields=[(s, AS.VARpat v)], flex=true,
					               typ= ref T.UNDEFty},
					 AS.MARKexp (VARexp (ref v,[]),region))], T.UNDEFty)
		end,
		TS.empty, nullUpdater)
	   | Ast.FlatAppExp items => elabExp (expParse (items, env, region), env, region))
    end (* elabExp *)

    (* elabELabel : (S.symbol * Ast.exp) list * SE.staticEnv * SL.region
                    -> (S.symbol * AS.exp) list * TS.tyvarset * tyvarsetUpdater
    * should be called "elabFields" *)
    and elabExpFields (ast_fields, env, region) =
	  let fun folder ((lb2,e2),(les2,lvt2,updts2)) =
		  let val (e3, lvt3, updt3) = elabExp (e2, env, region)
		   in ((lb2,e3) :: les2, checkedUnion (region, "elabELabel") (lvt3,lvt2),
		       updt3 :: updts2)
		  end
	      val (les1,tyvarset,updaters) = foldr folder ([], TS.empty, []) ast_fields
	      fun updater tv : unit = app (fn f => f tv) updaters
	   in (les1, tyvarset, updater)
	  end

    (* elabExpList : Ast.exp list * SE.staticEnv * SL.region
                     -> AS.exp list * TS.tyvarset * tyvarsetUpdater *)
    and elabExpList (ast_exps, env, region) =
	let fun folder (exp, (exps, tyvarset, updates)) =
		  let val (exp', tyvarset', update) = elabExp (exp, env, region)
		   in (exp' :: exps,
		       checkedUnion (region, "elabExpList") (tyvarset', tyvarset),
		       update :: updates)
		  end
	    val (exps,tyvarset'',updates) = foldr folder ([],TS.empty,[]) ast_exps
	    fun update' tyvar : unit = app (fn f => f tyvar) updates
	 in (exps, tyvarset'', update')
	end

    (* elabMatch : Ast.rule list * SE.staticEnv * SL.region
                   -> AS.rule list * TS.tyvarset * tyvarsetUpdater *)
    and elabMatch (rules,env,region) =
	let fun folder (rule, (rules_acc, tyvarset_acc, updaters_acc)) =
		  let val (rule', tyvarset_inc, updater_inc) = elabRule (rule, env, region)
		   in (rule' :: rules_acc,
		       checkedUnion (region, "elabMatch") (tyvarset_inc, tyvarset_acc),
                       updater_inc :: updaters_acc)
                  end
	      val (rules', tyvarset, updaters) = foldr folder ([], TS.empty, []) rules
	      fun updater tv: unit = app (fn f => f tv) updaters
	 in (rules', tyvarset, updater)
	end

    and elabRule (Ast.Rule {pat,exp}, env, region)  =
	let val region' = case pat of MarkPat (p,region_new) => region_new | _ => region
	    val (pat', tyvarset_pat) = elabPat (pat, env, region)
	    val env' = SE.atop (EU.bindVARp [pat'], env)
	    val (exp', tyvarset_exp, updater_exp) = elabExp (exp, env', region)
	 in (AS.RULE (pat',exp'),
	     checkedUnion (region, "elabRule") (tyvarset_pat, tyvarset_exp),
	     updater_exp)
	end


    (**** SIMPLE DECLARATIONS ****)

    (* elabDec' : Ast.dec * SE.staticEnv * IP.path * SL.region
		  ->  AS.dec * SE.staticEnv * TS.tyvarset * tyvarsetUpdater *)
    and elabDec' (dec: Ast.dec, env: SE.staticEnv, rpath: IP.path, region: SL.region)
		: (AS.dec * SE.staticEnv * TS.tyvarset * tyvarsetUpdater) =
	(case dec
	  of Ast.TypeDec tbs =>
	      let val (dec', env') =
		  ET.elabTYPEdec(tbs,env,(* EU.TOP,??? *) rpath,region)
	       in noTyvars(dec', env')
	      end

	   | Ast.DatatypeDec(x) =>
	      let val (dtycs, wtycs, _, env') =
		      ET.elabDATATYPEdec(x,env,[],EE.empty,isFree,
                                         rpath,region)
	       in noTyvars(DATATYPEdec{datatycs=dtycs,withtycs=wtycs}, env')
	      end

	   | Ast.DataReplDec(name,path) =>
	      (* LAZY: not allowing "datatype lazy t = datatype t'" *)
	      (* BUG[DBM]: what to do if rhs is lazy "datatype"? -- error, not currently checked *)
	      (case LU.lookTyc (env, SP.SPATH path)
		 of (SOME (dtyc as T.GENtyc{kind=T.DATATYPE{stripped=false,...},...})) =>
		      let val dcons = TU.extractDcons dtyc
			  val envDcons =
			      foldl (fn (d as T.DATACON{name,...},e)=>
					SE.bind(name,B.CONbind d, e))
				    SE.empty
				    dcons
			  (* types of new datacon bindings same as the old *)
			  val env = SE.bind(name,B.TYCbind dtyc,envDcons)
		       in noTyvars (AS.DATATYPEdec {datatycs=[dtyc], withtycs=[]}, env)
		      end
		  | SOME _ => (* error if not a datatype (bug 1578.1) *)
		    (EM.error region EM.COMPLAIN
			    "rhs of datatype replication not a datatype"
			    EM.nullErrorBody;
		     noTyvars(AS.SEQdec[], SE.empty))
		  | NONE =>  (* error if FHS tycon name is not bound *)
		    (EM.error region EM.COMPLAIN
			    "rhs of datatype replication not found"
			    EM.nullErrorBody;
		     noTyvars(AS.SEQdec[], SE.empty)))

	   | Ast.AbstypeDec x =>
	      let val (dec', env') =
  		      elabABSTYPEdec (x, env, isFree, rpath, region)
	       in noTyvars (dec', env')
	      end

	   | Ast.ExceptionDec ebs => elabEXCEPTIONdec (ebs, env, region)

	   | Ast.ValDec(vbs,explicitTvs) =>
	       elabVALdec(vbs,explicitTvs,env,rpath,region)

	   | Ast.DoDec exp => elabDOdec(exp, env, region)

	   | Ast.FunDec (fbs, explicitTvs) =>
	       elabFUNdec(fbs, explicitTvs, env, rpath, region)

	   | Ast.ValrecDec(rvbs,explicitTvs) =>
	       elabVALRECdec(rvbs,explicitTvs,env,rpath,region)

	   | Ast.SeqDec ds => elabSEQdec(ds,env,rpath,region)

	   | Ast.LocalDec ld => elabLOCALdec(ld,env,rpath,region)

	   | Ast.OpenDec ds => elabOPENdec(ds,env,region)

	   | Ast.FixDec (ds as {fixity,ops}) =>
	       let val env =
		       foldr (fn (id,env) => SE.bind(id,B.FIXbind fixity,env))
			     SE.empty ops
		in (FIXdec ds, env, TS.empty, nullUpdater)
	       end

	   | Ast.OvldDec dec  => elabOVERLOADdec(dec,env,rpath,region)

	   | Ast.MarkDec(dec,region') =>
	       let val (dec', env, tyvarset, updater) = elabDec' (dec,env,rpath,region')
		in (AS.MARKdec (dec',region'), env, tyvarset, updater)
	       end

	   | Ast.StrDec _ => bug "strdec"
	   | Ast.FctDec _ => bug "fctdec"
	   | Ast.SigDec _ => bug "sigdec"
	   | Ast.FsigDec _ => bug "fsigdec")


    (**** OVERLOADING ****)

    (* elabOVERLOADdec : (S.symbol * Ast.exp list) * StaticEnv.env * IP.path * SL.region
                         -> AS.dec * StaticEnv.env * TS.tyvarset * updaterTy *)
    and elabOVERLOADdec ((id,exps), env, rpath, region) =
	(* exps are simple variable paths, with monomorphic types that
	 * are ground instances of the known typeScheme for id *)
	let fun getVar exp =
		(case exp
		   of AS.VARexp (ref(v), _) => v
		    | AS.MARKexp (e, _) => getVar e
		    | _ => bug "evalOVERLOADdec.getVar")
	    val val_vars = map (fn exp => getVar (#1 (elabExp (exp, env, region)))) exps
	    val ovldvar = V.OVLDvar{name = id, variants = val_vars}
	in
	    (AS.OVLDdec ovldvar, SE.bind(id, B.VALbind ovldvar, SE.empty),
             TS.empty, nullUpdater)
	end

    (**** LOCAL ****)

    and elabLOCALdec ((inner_decs, outer_decs), env, rpath:IP.path, region) =
	let val (decs_in, env_in, tyvarset_in, updater_in) =
		  elabDec' (inner_decs, env, IP.IPATH [], region)
	    val (decs_out, env_out, tyvarset_out, updater_out) =
		  elabDec' (outer_decs, SE.atop(env_in, env), rpath, region)
	    fun updater tv = (updater_in tv; updater_out tv)
	 in (AS.LOCALdec (decs_in,decs_out), env_out,
	     checkedUnion (region, "elabLOCALdec") (tyvarset_in, tyvarset_out),
	     updater)
	end

    (**** OPEN ****)

    and elabOPENdec (paths: S.symbol list list, env, region) =
        let fun processPath (path: S.symbol list) =
		  let val path' = SP.SPATH path
                   in case LU.lookStr(env, path')
		        of SOME str => (path', str)
			 | NONE => (errorRegion (region,
				    "elabOPENdec: undefined path: "^SP.toString path');
				    (path', M.ERRORstr))
                  end

	    val strs = map processPath paths

            fun loop (nil, env) = (AS.OPENdec strs, env, TS.empty, nullUpdater)
              | loop ((_, str)::rest, env) = loop(rest, MU.openStructure(env, str))

         in loop (strs, SE.empty)
        end

    (****  VALUE DECLARATIONS ****)
    (* elabVB : Ast.vb * tyvarset * SE.staticEnv * SL.region
                -> AS.dec * tyvarset * tyvarsetUpdater *)
    and elabVB (Ast.MarkVb (vb, region), etvs: TS.tyvarset, env: SE.staticEnv, _) =
          (* pass through MarkVb, replacing the region parameter *)
          let val (dec, tyvarset, updater) = elabVB (vb, etvs, env, region)
           in (AS.MARKdec (dec, region), tyvarset, updater)
          end
      | elabVB (Ast.Vb {pat, exp, lazyp}, etvs, env, region) =
	  let val union = checkedUnion (region, "elabVB")
              val diff = checkedDiff (region, "elabVB")
	      val (pat, tyvarset_pat) = elabPat (pat, env, region)
	      val (exp, tyvarset_exp, updater_exp) = elabExp (exp, env, region)
	      val exp = if lazyp  (* LAZY *)
		        then delayExp (forceExp exp)
			else exp

              (* tracking user or "explicit" (UBOUND) type variables *)
	      val tyvarsetRef = ref TS.empty
	          (* this should be a tyvarset ref, but it was a tyvar list ref *)
	      fun updater (tyvarset: TS.tyvarset): unit =
		  let val tyvarset' = TS.diffPure (tyvarset, etvs) (* DANGER! side effects? *)
		      val local_tyvarset =
			  diff (union (tyvarset_exp, union (tyvarset_pat, etvs)), tyvarset')
			  (* etvs should be the second argument to union
			   * to avoid having the explicit type variables
			   * instantiated by the union operation. Potential bug!
			   * WATCH OUT! union and diff can have side effects! *)
		      val down_tyvarset = union (local_tyvarset, tyvarset')
		   in tyvarsetRef := local_tyvarset;
		      updater_exp down_tyvarset
	          end

              (* The following code propagates a PRIMOP access
               * through a simple aliasing value binding.
               * WARNING [ZHONG] This is an old hack and should be
               * replaced.
	       * [DBM] This won't apply if lazyp=true.
               *)
              fun stripMarksVar (MARKpat(p as VARpat _, reg)) = p
                | stripMarksVar (MARKpat(p,reg)) = stripMarksVar p
                | stripMarksVar (CONSTRAINTpat (p, ty)) =
                    CONSTRAINTpat(stripMarksVar p, ty)
                | stripMarksVar p = p

	      val pat =
		  case stripExpAbs exp
		   of AS.VARexp (ref (V.VALvar{prim,...}),_) =>
			(case prim
			   of PrimopId.Prim _ =>
			      (case stripMarksVar pat
				of AS.CONSTRAINTpat(VARpat(V.VALvar{path,typ,btvs,
								    access,...}), ty) =>
				   AS.CONSTRAINTpat(VARpat(V.VALvar{path=path, typ=typ,
								    access=access,
								    btvs = btvs, prim=prim}),
						    ty)
				 | AS.VARpat(V.VALvar{path, typ, btvs, access, ...}) =>
				   AS.VARpat(V.VALvar{path=path, typ=typ,
						      btvs = btvs, access=access,
						      prim=prim})
				 | _ => pat)
			    | PrimopId.NonPrim => pat)
		    | _ => pat

	   in (VALdec [VB {exp = exp,
			   tyvars = tyvarsetRef,
			   pat = pat,
			   boundtvs = []}],
	       [pat],
	       updater)
	  end

    (* elabVALdec : Ast.vb list * T.tyvar list * SE.staticEnv * IP.path * SL.region
                    -> AS.dec * SE.staticEnv * TS.tyvarset * tyvarsetUpdater *)
    and elabVALdec (vbs: Ast.vb list, etvs: Ast.tyvar list, env, rpath, region) =
	  let val etvs: T.tyvar list = ET.elabTyvarList (etvs, region)
	      fun folder (vb_ast, (decs_acc, pats_acc, updaters_acc)) =
		      let val etvs = TS.mkTyvarset(map T.copyTyvar etvs)
			  val (dec_inc, pats_inc, updater_inc) = elabVB (vb_ast, etvs, env, region)
		       in (dec_inc::decs_acc, pats_inc @ pats_acc, updater_inc :: updaters_acc)
		      end
	      val (decs, pats, updaters) = foldr folder ([],[],[]) vbs
	      fun updater tv : unit = app (fn f => f tv) updaters
	   in (SEQdec decs, EU.bindVARp pats, TS.empty, updater)
	  end

    (* elabRVB : Ast.rvb * SE.staticEnv * SL.region
                 -> {match : ?, ty : T.ty?, name : S.symbol} * tyvarset * tyvarsetUpdater *)
    and elabRVB (MarkRvb(rvb,region), env, _ (* region *)) =
	  let val ({ match, ty, name }, tvs, u) = elabRVB (rvb, env, region)
	      val match' = AS.MARKexp (match, region)
	   in ({match = match', ty = ty, name = name}, tvs, u)
	  end
      | elabRVB (Ast.Rvb {var, fixity, exp, resultty, lazyp}, env, region) =
         (case stripExpAst (exp, region)
	    of (FnExp _, region') =>
	       (* DBM: where should region vs region' be used? Seems to depend on what
		  stripExpAst does. *)
	        let val (e, ev, updater) = elabExp (exp, env, region')
		    val (t,tv) =
			case resultty
			  of SOME ty_result =>
			       let val (ty_result', tyvarset2) = ET.elabType (ty_result, env, region)
				in (SOME ty_result', tyvarset2)
			       end
			   | NONE => (NONE,TS.empty)
		 in case fixity
		      of NONE => ()
		       | SOME(f,region) =>
			   (case LU.lookFix (env,f)
			      of F.NONfix => ()  (* NONfix OK *)
			       | _ =>
				 errorRegion (region,
				   ("elabRVB: infix symbol \""^ S.name f ^
				    "\" used where a nonfix identifier was expected")));
		    ({match = e, ty = t, name=var},
		     checkedUnion (region, "elabRVB") (ev, tv),
		     updater)
		end
	     | _ => (errorRegion (region, "fn expression required on rhs of val rec");
		     ({match = dummyFNexp, ty = NONE, name = var}, TS.empty, nullUpdater)))

    (* elabVALRECstrict : Ast.rvb list * TS.tyvarset * SE.staticEnv * SL.region
                          -> AS.dec * SE.staticEnv * TS.tyvarset (* empty *) * tyvarsetUpdater *)
    and elabVALRECstrict (rvbs, etvs, env, region) =
	let val union = checkedUnion (region, "elabVALRECstrict")
	    val diff = checkedDiff (region, "elabVALRECstrict")

	    val env' = ref(SE.empty: SE.staticEnv)

	    fun makevar region (p as Rvb {var,...}) =
		  let val v = newVALvar var
                      val nv = newVALvar var (* DBM: what is this for? *)
		   in env' := SE.bind(var,B.VALbind v,!env');
		      (v, p)
		  end
	      | makevar _ (p as MarkRvb (rvb,region)) =
		  let val (v,_) = makevar region rvb in (v,p) end

	    val rvbs' = map (makevar region) rvbs
	    fun folder ((v, rvb_ast), (rvbs_acc, tyvarset_acc, updaters_acc)) =
		let val (rvb_inc, tyvarset_inc, updater_inc) =
			elabRVB (rvb_ast, SE.atop (!env', env), region)
		 in ((v,rvb_inc)::rvbs_acc,
		     union (tyvarset_inc, tyvarset_acc),
		     updater_inc :: updaters_acc)
		end

	    val (rvbs, tyvarset, updaters) = foldl folder ([], TS.empty, []) rvbs'

            (* updater: tyvarsetUpdater -- updates tyvarsetRef *)
	    val tyvarsetRef: TS.tyvarset ref = ref TS.empty
	    fun updater (tyvarset: TS.tyvarset) : unit =
		let val tyvarset' = TS.diffPure (tyvarset, etvs)
                    val local_tyvarset = diff (union (tyvarset, etvs), tyvarset')
		    val down_tyvarset = union (local_tyvarset, tyvarset')
		 in tyvarsetRef := local_tyvarset;
		    app (fn f => f down_tyvarset) updaters
		end

            (*  check uniqueness of function names in the VALREC *)
	    val _ = if EU.checkUniq (map (fn (_, {name,...}) => name) rvbs)
		    then ()
		    else errorRegion (region, "duplicate function name in val rec dec")

            val (ndec, nenv) =
  	        EU.wrapRECdec (map (fn (v, {ty, match, name}) =>
				       RVB {var = v, resultty = ty,
					    tyvars = tyvarsetRef,
					    exp = match,
					    boundtvs = []})
			       rvbs)
         in (ndec, nenv, TS.empty, updater)
	end (* fun elabVALRECstrict *)

    (* LAZY: "val rec lazy ..." *)
    and elabVALREClazy (rvbs, etvs, env, region) =
	let val union = checkedUnion (region, "elabVALREClazy")
            val diff = checkedDiff (region, "elabVALREClazy")

	    fun split [] = ([],[])
	      | split ((Rvb {var,exp,resultty,lazyp,...})::xs) =
		 let val (a,b) = split xs in ((var,resultty)::a,(exp,lazyp)::b) end
	      | split ((MarkRvb (x,_))::xs) = split (x::xs) (* loosing regions *)

	    val (yvar,declY) = lrvbMakeY (length rvbs)

	    val (lhss,exps) = split rvbs
	    val argpat = TuplePat (map (fn (sym, NONE) => VarPat[sym]
					 | (sym, SOME ty) =>
					     ConstraintPat {pattern = VarPat[sym],
							    constraint = ty})
				       lhss)

	    fun folder ((exp, lazyp), (fexps_acc, tyvarset_acc, updaters_acc)) =
		let val (pat', tyvarset_pat) = elabPat (argpat, env, region)
		    val env' = SE.atop (EU.bindVARp [pat'], env)
		    val (exp', tyvarset_exp, updater) = elabExp (exp, env', region)
		 in (FNexp (completeMatch [RULE (pat', if lazyp then exp' else delayExp exp')],
			    T.UNDEFty) :: fexps_acc,
		     union (union (tyvarset_pat, tyvarset_exp), tyvarset_acc),
		     updater::updaters_acc)
		end

	    val (fns, tyvars, updaters) = foldr folder ([], TS.empty, []) exps

	    val lhsSyms = map #1 lhss  (* left hand side symbols *)
	    val lhsVars = map newVALvar lhsSyms

	    (* copied from original elabVALRECdec -- modified by [DBM, 2025.03.15] *)
	    val tyvarsetRef : TS.tyvarset ref = ref TS.empty
	    fun updt (tyvarset: TS.tyvarset) : unit =
		let val tyvarset' = TS.diffPure (tyvarset, etvs) (* DANGER! side effects? *)
 		    val local_tyvarset = diff (union (tyvarset, etvs), tyvarset')
		    val down_tyvarset = union (local_tyvarset, tyvarset')
		 in tyvarsetRef := local_tyvarset;  (* result returned via ref assignment *)
		    app (fn f => f down_tyvarset) updaters  (* side effects *)
		end

	    val declAppY =
		VALdec [VB {pat = AU.TUPLEpat(map VARpat lhsVars),
			    exp = AS.APPexp (AS.VARexp (ref yvar,[]), AU.TUPLEexp fns),
			    tyvars = tyvarsetRef,
			    boundtvs = nil}]

	    fun forceStrict ((sym, var1, lazyp), (vbs, vars)) =
		  let val var2 = newVALvar sym
		      val vb = if lazyp
			       then VB {pat = VARpat var2,
				        exp = VARexp (ref var1,[]),boundtvs=[],
				        tyvars = ref TS.empty}
			       else VB {pat = APPpat(BT.dollarDcon,[],(VARpat var2)),
				        exp = VARexp (ref var1,[]),boundtvs=[],
				        tyvars = ref TS.empty}
		   in (vb::vbs, var2::vars)
		  end

	    fun zip3 (x::xs,y::ys,z::zs) = (x,y,z)::zip3(xs,ys,zs)
	      | zip3 (nil,_,_) = nil
	      | zip3 _ = bug "zip3"

	    val (vbs,vars) =
		foldr forceStrict ([],[]) (zip3(lhsSyms,lhsVars,map #2 exps))

	    val env' = ListPair.foldl
		  (fn (s, v, env) => SE.bind(s, B.VALbind v, env))
		    SE.empty
		      (lhsSyms, vars)

	    val absyn = LOCALdec(SEQdec[declY,declAppY],VALdec vbs)
	 in showDec ("elabVALREClazy: ", absyn, env');
	    (absyn, env', TS.empty, updt)  (* TS.empty OK? *)
	end (* fun elabVALREClazy *)

    (* elabVALRECdec : Ast.rvb list * <etvs> * SE.staticEnv * IP.path * SL.region
                       -> <return type of elabVALRECstrict> *)
    and elabVALRECdec (rvbs: rvb list, etvs, env, rpath:IP.path, region) =
	let val etvs = TS.mkTyvarset (ET.elabTyvarList (etvs, region))
	    fun isLazy(Rvb{lazyp,...}) = lazyp
	      | isLazy(MarkRvb(rvb,_)) = isLazy rvb
         in if List.exists isLazy rvbs
	    then elabVALREClazy (rvbs,etvs,env,region)
	    else elabVALRECstrict (rvbs,etvs,env,region)
	end

    (* elabDOdec : [exp:]Ast.exp * [env:]SE.staticEnv * [region:]SL.region
                   -> AS.dec * SE.staticEnv * TS.tyvarset * tyvarsetUpdater *)
    and elabDOdec (exp, env, region) =
	let val union = checkedUnion (region, "elabDOdec")  (* checked union of tyvarsets *)
            val diff = checkedDiff (region, "elabDOdec")  (* checked diff of tyvarsets *)
	    val (absyn_exp, tyvarset_exp, updater_exp) = elabExp (exp, env, region)
	    fun updater tyvarset =
		let val local_tyvarset = diff (tyvarset_exp, tyvarset)
		    val down_tyvarset = union (local_tyvarset, tyvarset)
		 in updater_exp down_tyvarset
		end
	 in (DOdec absyn_exp, SE.empty, TS.empty, updater)
	end

    (* elabFunDec : Ast.fb list * tyvar list * SE.staticEnv * IP.path * SL.region
                    -> AS.dec * SE.staticEnv * TS.tyvarset * tyvarsetUpdater *)
    (* The fbs and etvs are the arguments of an Ast.FunDec constructor (function dec),
     *   while env_eFd, rpath, region represent the context. *)
    and elabFunDec (fbs: Ast.fb list, tyvars: Ast.tyvar list, env_eFd: SE.staticEnv, rpath: IP.path,
		    region: SL.region) =
	let val eFd_tyvarset: TS.tyvarset = TS.mkTyvarset (ET.elabTyvarList (tyvars, region))

           (* specialized checking (for incompatible tyvars) versions of TS.union and TS.diff *)
            val union = checkedUnion (region, "elabFunDec")  (* checked union of tyvarsets *)
            val diff = checkedDiff (region, "elabFunDec")  (* checked diff of tyvarsets *)

            (* fb_folder: SL.region
	                -> (Ast.fb * (fundec list * SE.staticEnv) -> (fundec list * SE.staticEnv)
	       Parse the fb clauses to determine the function name, etc. to translate an fb
	       into a fundec.
	       fb_folder is a folder function to be folded over the fb list of an Ast.FunDec.
	       (Formerly named "makevar".)
	       NOTE: fb's may be marked, but their clauses are not marked (no MarkClause form). *)
	    fun fb_folder _ (MarkFb (fb, fbregion)) = fb_folder fbregion fb
	      | fb_folder fbregion (Ast.Fb (clauses_fb: fb_clause list, lazyp: bool),
				    (clauses_c: (symbol * ast_clause list * SL.region) list,
				   env_fbf: SE.staticEnv)) =
		 let
		     (* getfix : S.symbol option -> F.fixity *)
		     (* returns NONfix for NONE, other wise, the sym, which is produced by the
			Symbol.var'n'fix function, should be in the fixity name space *)
		     fun getfix (SOME sym) = LU.lookFix (env, sym)
		       | getfix NONE = F.NONfix  (* associated item is not a variable pattern *)

                     (* ensureNonfix : Ast.pat Ast.fixitem -> Ast.pat *)
		     (* For non-variable patterns, fixity is NONE and getfix will return NONfix. For
                      * variable patterns, an error occurs if the variable is an infix *)
		     fun ensureNonfix ({item: Ast.pat,
					fixity: S.symbol option,
					region: SL.region}: Ast.pat Ast.fixitem) : Ast.pat =
			 (case fixity
			    of NONE => () (* OK *)
			     | SOME symbol =>
			         (case LU.lookFix (env, symbol)
				    of (F.NONfix, _) => ()  (* OK *)
				     | _ => EM.errorRegion (region,
							    String.concat ["infix operator \"",  S.name sym,
									   "\" used without \"op\" in fun dec"]));
			  patCompleteParse (item, env, region))

		     (* getname : Ast.pat * SL.region -> S.symbol
	              * Get the "name" of a simple VARpat (optionally MARKed);
		      * fails and reports error if applied to other, compound, pats *)
		     fun getname (MarkPat (pat, region), env, _) =
			   getname (pat, env, region) (* strip MarkPat, using the MarkPat region *)
		       | getname (VarPat [v], _) =
			   case LU.lookVar v  (* check that v is not bound to a datacon *)
			     of NONE => (*ok *) v
			      | SOME (CONbind _) =>
				  errorRegion (region, "function name is bound to a datacon" ^ S.name v)
		       | getname (_, region) =  (* should not happen!!! apat id case in ml.grm *)
                           (bug "function symbol in fb clause is not a simple identifier");
			    EU.bogusID)  (* for potential error recovery *)

                     (* stripMarkPat : Ast.pat -> Ast.pat *)
		     (* strips MarkPats even if nested, ignoring region *)
                     fun stripMarkPat (Ast.MarkPat (pat, _)) = stripMarkPat pat
		       | stripMarkPat pat = pat

		     (* stripMarkFixitem: Ast.pat Ast.fixitem -> Ast.pat Ast.fixitem *)
		     (* stripping all MarkPats from the item pat while preserving the fixitem region *)
		     fun stripMarkFixitem ({item, fixity, region}: patfixitem) : patfixitem
 			   {item = stripMarkPat pat, fixity=fixity, region=region}

		     (* NOTE: patfixitems are produced (only) by the apat productions in the ml.grm grammar. *)

		     (* Parsing the LHS of a Fb clause is rather complicated!

			Parsing of LHS atomic patterns (a pat fixitem list) of an fb clause
			is done by the parseLHS0 function, which checks for various cases: where the defined
			function symbol is the first atomic pattern, or the cases where the where the defined
			function symbol is infix and is used as infix (rather than "op f") in the clause LHS.

			with an atomic pattern that is an infix identifier application. If so, it parses that
                        first pattern.
			If the result is a variable pattern (with a singleton path), it takes that
			variable to be the function name.
			it checks that there are more than one fixitems
			(need both a function variable.  The problem being solved is that the LHS
			of a Fb clause is a list of pattern fixitems that needs to be analyzed to
			find the name (funSym) of the function being declared and bound. *)

		        (* "Parse" a clause LHS, i.e., a list of pattern fixitems (derived from
			   parsing the apats (atomic patterns) representing the LHS of a clause
			   in the grammar.
			   We first check whether a 3 element
			 * patfixitem list should be parsed as an infix application
			 * (of a datacon or the defined function variable)
			 * Note. The resulting Ast.pat list may contain pats that need further parsing,
			   i.e. FlatAppPats
			 * We choose not to support mixing infix function notation and currying, e.g.
			    fun x f y z w = ... where f is infix, and not even fun (x f y) z = ....
			    If f is infix, the only options are fun p1 f p2 where p1 and p2 are atomic
			    patterns (without "applied variables"). Thus we don't support case 31 below.
			    Case 4c is not supported by the Defn (Revised) grammar.
			Cases:
			   1. fun f apat_1 ... apat_n =     -- f nonfix id (a variable symbol w NONfix fixity)
			                                       1st apat is VarPat [f]
			   2. fun op f apat_1 ... apat_n =  -- f possibly infix id (doen't matter)
			                                       1st apat is VarPat [f] even if f INfix
			   3. fun (apat_1 f apat_2) =       -- f infix id (no currying! so exactly 3 patFixitems)
			                                       1st apat is FlatAppPat [<apat_1>, <f>, <apat_2>]
			   3c. fun (apat_1 f apat_2) apat_3 ... =
			                                    -- f infix id, additional curried patterns !!
			   4. fun apat_1 f apat_2 =         -- f infix id (2nd apat), no currying!)
			                                       where apat_1 does not have var head operator
			   4c. fun apat_1 f apat_2 apat_3 ... =
			                                    -- f infix id, additional curried patterns !!
			   5. fun apat_1 f apat_2 : ty =

			   NOTES: (1) only 1. and 2. support currying (3c not supported).
			          (2) Case 3 is the argpat1 = [item=FlatAppPat items, ...} case.
				      where f is NONfix, non-dcon "head" of the (completely) parsed FlatAppPat.
			          (3) Case 4 is the argpats = [a, <f>, b] case with f infix. a and b can contain variables
			              and constructor ids. f is the funsym

			   Defn of "head identifier" of a pattern
			      path if the pattern is VarPat path
			      path if the pattern is AppPat{constr=VarPat path, ...}
			      The head identifier of a "proper" argument pattern should name a dcon.
			      The head identifier of an "improper" pattern is the function name.
			     -- the top identifier (VarPat[f] or AppPat{constr=VarPat[f},...}.
			     f could be a variable or a dcon name, and we can use env to check whether it is a dcon nmae.

			   The function name is either the head identifier of the first argpat, or that of the 2nd argpat,
			   (assuming it is a VarPat) if it is infix.  The head identifier of a proper pattern should not be
			   a variable; it instead should be bound to a dcon.
			   ASSERT: the type checker should verify that the rator identifier of an AppPat names a dcon.
			 *)

			 (* In example "fun x f y = x; -- where f infix" what prevented parseLHS0 from choosing x
			    as the function name in parseLHS0?
			    Here argpats = [{item=VerPat [x], fixity=NONE, ...},
					    {item=VarPat [f], fixity=SOME f, ...},
					    {item=VarPat [y], fixity=NONE, ...}]
			    Do we need some look-ahead in parseLHS0 to prevent x from being chosen?
			    Or should parseLHS1 come first -- have priority over parseLHS0.  That is, should we check
			    first for the "fun p1 f p2" pattern with infix f?  Otherwise we REQUIRE that p1 provides
			    the function symbol, either as p1 = VarPat [f] or p1 = AppPat {constr=VarPat[f],...}
			    where f is a NONfix, non-constructor identifier.

			    Another Error example: "fun (x y) f z = ..." where f infix, non-dcon, if x is non-dcon.
			    i.e. if infix f is the function id, then the other pattern expressions should not contain
			    variables as constr (rators of applications). This is generally true of any argument
			    patterns other than the particular pattern term containing the function id.
			    All rators in such true argument patterns should, at some point, be checked to
			    name dcons.  Does the type checker do this? I would assume so.
			    Something to be verified.  (Note that all rators in proper pattern terms should
			    be VarPat (path), i.e. identifiers or qids.)
			  *)

			 (* parseLHS0 is used to check for the case where argpat has exactlhy three fixitems, the
			  * second of which is an id f (VarPat [f]) where f is infix (non-dcon variable) (Case 3).
			  * and the first and third fixitems are nonfix (if they are VarPat [x]).
			  * Example: "fun x f y = ..." where x or y is an infix symbol is an error.
			  *
			  * parseLHS1 (= old parseLHS) is used to check for the case where argpat1 is a FlatAppPat.
			  * it is invoked after parseLHS0 has "failed".
			  * We don't need to check for the FlattAppPat case. Instead parseLHS1 is passed
			  * a complete parse of argpat1, so we just need to examine that pattern to see if
			  * it is either a VarPat [f] or an AppPat{constr=VarPat [f], in which case we take
			  * f to be the function symbol (ater looking it up to make sure it is not a datacon
			  * name).
			  *
			  * parseLHS2 parses the 3rd alternative afer parseLHS0 and parseLHS1 have "failed"
			  *  (i) check that there are at least 2 argpats (LHS Case 2.).
			  *  (ii) Then the first (fully parsed) argpat should be a either a nonfix (& non-dcon)
			  * identifier pattern where the identifier is the function symbol, or
			  * a compound (AppPat) where the constr field is an identifier pattern with
			  * a nonfix symbol that is taken to be the function symbol.
			  *
                          * We make use of the fact that the operator in a compound (AppPat) pattern is always
			  * VarPat path. If path is not a singleton path we assume it denotes a data
			  * constructor (datacon). Otherwise it could be either a datacon or a variable, and
			  * if it is a variable that the variable symbol will be the function symbol.
			  *
			  * We do not check to distinguish datacon identifiers from variable identifiers.
			  * This distinction will be checked later in type checking, where look up the path
			  * in the environment to see if it is bound to a datacon.
			  * But what if the function symbol is the name of a datacon in the environment?
			  * This should be an error (we can't rebind that symbol to a function).  So if we
			  * have a candidate function symbol, we need to look it up to make sure that it is
			  * not already bound to a datacon.

			  * Error case for no argpats in the LHS. (1 argpat case may be ok) *)

		     (* parseLHS0 : patfixitem list -> S.symbol * Ast.pat list
		      * Case 4: infix funcition symbol as second argpat out of 3.
		      *   1. argument is a list of 3 patfixitems ("argpats").
		      *   2. argument 2 is an infix variable symbol (VarPat [f]).
		      *   3. arguments 1 and 3 are "nonfix" (either atomic compound, or nonfix variable
		      *      when completely parsed.
		      *   4. all 3 arguments "completely parsed" to patterns, eliminating FlatAppPats.
		      *      But actually, we might get away with only completely parsing patfixitem 2.
		      *
		      * The first patfixitem is a FlatAppPat of a triple of patfixitems,
		      * where the middle one (#item b) could be an infix function variable.
		      * From the grammar (ml.grm), we know that the pats are all patfixitems
		      * because the LHS of a Clause is a list of apats which parse to patfixitems.
		      * Apply parseFlatApp to the first patfixitem to determine its structure:
		      *  1: it parses to a VarPat [v]. Then v is the candidate funsym.
			    Could further check that env(v) is a variable, not a constructor.
			 2: it parses to a VarPat (path) where path has length > 1. This is
			    an error.
			 3: It parses to an AppPat (p, argp) in which case we look at p as above
			    as a candidate funsym.  If it qualifies, add argp to the remaining
			    arguments patterns (items).
			 4: If it parses to any other pattern (maybe except ConstraintPat, which
			    we could strip down to its base pat) we keep the resulting pat and
			    check whether the next (2nd) item (VarPat) is an infix symbol, in which
			    case that symbol is the funsym.
			 5. If we can't verify a funsym from this analysis, we look at the next
			    pat to see if it is a variable, and thus a funsym candidate, if it turns
			    out to be an infix variable.
		      *)

		     (* parseLHS1 : patfixitem list -> (S.symbol * Ast.pat list) option
			result type options are fully parsed
			parseLHS1 continues the parsing of the original patfixitem list when parseLHS gives up.
			We try to extract the function symbol from the first patfixitem.
			Thus we (completely) parse the first patfixitem and analyze the result to obtain the
			function symbol (as top operator) and 1st argument pattern. *)
  	             fun parseLHS1 (argpats as patfixitem1::rest: patfixitem list) =
			 (case stripMarkPat (patCompleteParse patfixitem1)
			    of Ast.VarPat _ =>  (* in this case, rest should not be null *)
			         (case getName (p, env, region)
				    of NONE => (EM.errorRegion (region, "can't determine function symbol"); NONE)
				     | SOME f => SOME (f, map ensureNonfix rest)) (* done! *)
			     | Ast.AppPat {constr = Ast.VarPat [f], argument} => (* in this case, rest could be null *)
			         (* first parses to AppPat, constr must be a single id pat,
				  * the id of which might denote a datacon or might be the function symbol.*)
			         (case getName (constr, env, region)
				    of NONE => (EM.errorRegion (region, "can't determine function symbol"); NONE)
				     | SOME f => SOME (f, argument :: map ensureNonfix rest)) (* done! *)
			     | pat => (EM.errorRegion (region,
						       "can't extract function symbol from first patfixitem");
				       NONE))
		       | parseLHS1 nil => (EM.errorRegion (region, "empty LHS in a clause"); NONE)

		     (* parseLHS : patfixitem list -> (S.symbol * Ast.pat list) option
                        parseLHS deals with the case (Case 3) where the function symbol is infix and is the 2nd
			of exactly three patfixitems: "fun p f q = ..." where
			-- f is an infix identifier (VARpat[f]), which must not be bound to a datacon in the env.
			-- p and q are atomic (i.e. pat fixitems) and are NONfix (not infix identifiers).
			Note that we are not allowing (unparenthesized) infix function declarations with currying.
			If these conditions are not met, we call parseLHS1 as a backup with the
			  original list of patfixitem arguments.
			The resulting pat list (argument pattern(s) is a singleton list containing
			the tuple pat Ast.TuplePat[p, q]. *)
		     fun parseLHS (argpats : patfixitem list) : (S.symbol * Ast.pat list) option =
			 (case argpats
			    of [a , b as {item, fixity = SOME f,...}, c] =>
			         (case LU.lookFix f
			            of F.INfix _ =>
					 (* ensureNonfix ensures that a and b are "nonfix and completely parses them
					  * getName ensures that v (var of f) is not a datacon name *)
					 (case getName (item, env, region)
					    of NONE => bug "parseLHS"  (* item should be a VarPat in this case *)
					     | SOME f => SOME (f, [Ast.TuplePat [ensureNonfix a, ensureNonfix c]]))
				     | F.NONfix => parseLHS1 argpats)
					     (* try Case 3 (argpat1 parses to an infix application) *)
			     | _ => parseLHS1 argpats)

                    (* When parsing the list of clauses (Ast.Clause), we start with the first clause,
		       get its funSym, arity, resultTy option,  then for subsequent clauses, check
		       that they have the same funSym, arity, and (if SOME) the same resultTy.
		       This requires elaborating the resultTy, if present, so that they can be compared
		       as T.types.
		       This means that funsym and resultTy do not need to be stored in the ast_clause
		       record, but can be managed separately during clause parsing and accumulation of
		       the parsed clauses. *)

		     (* parseAClause : Ast.clause -> fundec0 *)
		     (* used to parse a single Ast.clause from the Ast.fb clause list of an fb.
		      * It yields a fundec value that contains this clause's idea of the function name,
		      * the curry arity, and the (optional) result type.
		      * Used to produce a base case and then used in parseClauseFolder to fold over the rest
		      * of an fb's clauses. *)
 		     fun parseAClause (Ast.Clause {pats, resultty, exp}) : fundec0 option = 
			 (case parseLHS pats
			    of NONE => (EM.errorRegion (fbregion, "LHS of a clause failed to parse");
					NONE)
			     | SOME (funsym, argpats) =>
			          let val resTyOp =
					  (case resultty
					     of NONE => NONE
					      | SOME ast_ty => SOME (ET.elabType (ast_ty, env, region)))
				   in SOME (funsym, length argpats, resTyOp, [{argpats=argpats, exp=exp}])
				  end) 

                     (* ast_clausesFolder : Ast.clause * fundec0 -> fundec0 *)
		     (* used to fold over the clauses of a single fb *)
		     fun ast_clausesFolder
			     ((clause as Ast.Clause {pats, resultty, exp}),
			      (fd as (funsym_c: S.symbol, arity_c: int, resTyOp_c: T.ty option,
				      clauses_c: ast_clause list)))
			   : fundec0 =
			   (case parseAClause clause
			      of NONE => NONE
			       | SOME (funsym_i, arity_i, resTyOp_i, clauses_i) =>
			         let

			       (* check that clauses have consistent arities *)
			       val arity_r = (if arity_i = arity_c
					      then arity_c
					      else (EM.errorRegion
						     (region, "inconsistent curried arities in function clauses");
						    arity_c)

                               (* check that clauses have consistent resulttys *)
			       val resTyOp_r : (T.ty * TS.tyvarset) option =
				   (case resultty
				      of NONE => resTyOp_c
				       | SOME ast_ty => 
					   let val (ty_i, tyvarset_i) = ET.elabType (ast_ty, env, fbregion)
					    in (case (ty_i, resTyOp_c)
						  of (T.ERRORty, tyop) => tyop
						   | (tyop, NONE) => SOME (ty_i, tyvarset_i)
						   | (_, SOME (ty_c, tyvarset_c)) =>
						       (* or _unify_ ty_i, ty_c? -- to deal with ERRORty *)
						       (if TU.equalType (ty_i, ty_c)
							then (case union (tyvarset_i, tyvarset_c)
							        of SOME tyvarset_r => SOME (tyc_c, tyvarset_r)
								 | NONE =
								     (EM.errorRegion (fbregion,
						                         "type variable clash in result types of clauses");
								      NONE))
							else (EM.errorRegion (region,
							         "inconsistent result types in function clauses");
							      resTyOp_c)))
					   end)

			       (* check that clauses have consistent function symbols *)
			       val funsym_r =
				     if S.equal (funsym_i, funsym_c)
   				     then funsym_c (* OK *)
				     else (EM.errorRegion (region, "fun dec clauses have different function names");
					   funsym_c)

			       val clauses_r = clauses_i @ clauses_c

			    in (funsym_r, arity_r, resTyOp_r, clauses_r, STRICT, fb_region)
			   end

 		     (* parseFb: Ast.clause list -> fundec0 option *)
		     fun parseFb nil = (EM.errorRegion (region, "no clauses in function declaration"); NONE)
		       | parseFb [clause] = parseAClause clause
		       | parseFb (clause :: rest) =
			   (case parseAClause clause
			      of NONE => NONE
			       | SOME fundec0 => SOME (foldl ast_clausesFolder fundec0 rest)

		     (* mkLazy : fundec0 * SE.staticEnv * SL.region -> fundec list * SE.staticEnv *)
		     fun mkLazy ((funsym, arity, resTyOp, clauses): fundec0, env: SE.staticEnv, region: SL.region) = 
			  let (* newArgPaths: (S.symbol list * int) -> S.symbol list; ASSERT: int >= 0 *)
			      (* newArgPaths (nil, 3) ==> [[$1], [$2], [$3]] *)
			      fun newArgPaths (args, 0) = args
				| newArgPaths (args, n) = newArgPaths ([S.varSymbol("$"^Int.toString n)] :: args, n-1)

			      (* curryApp : Ast.exp * Ast.exp list -> Ast.exp *)
			      fun curryApp (f, nil) = f
				| curryApp (f, x::xs) =
				    curryApp (Ast.AppExp {function=f, argument=x}, xs)

			      val lazySym: S.symbol = S.varSymbol(S.name funSym ^ "_")

                              val outerArgPaths : Ast.path list = newArgPaths (nil, arity)

			      val outerClause =
				   {argpats = map Ast.VarPat outerArgPaths,
				    exp = curryApp (Ast.VarExp [lazySym], map Ast.VarExp outerArgPaths)}

			   in ([(lazySym, arity, resTyOp, clauses, FORCE, region),
			        (funSym, arity, resTyOp, [outerClause], DELAY, region)],
			       SE.bind (funSym, B.VALbind (newVALvar funsym),
					SE.bind (lazySym, B.VALbind (newVALvar lazySym), env)))
			  end

		     (* mkStrict : fundec0 * SE.staticEnv * SL.region -> fundec * SE.staticEnv *)
		     fun mkStrict ((funsym, arity, resTyOp, clauses), env, region) =
			        ((funsym, arity, resTyOp, clauses, STRICT, region),
				   SE.bind (funSym, B.VALbind (newVALvar funsym), env))

		  in (case parseFb clauses_fb
		        of NONE => bogusFundec  (* error recovery return value, probably useless *)
			 | SOME fundec0 =>
				  if lazyp
				  then ( lazy function declaration *)
				    let val (lazyfundecs, env_inc) = mkLazy (fundec0, env_fbf, fbregion)
				     in (lazy_fundecs @ fundecs_c, env_inc)
				    end
				  else (* normal, STRICT case *)
				    let val (strictfundec, env_inc) = mkStrict (fundec0, env_fbf, fbregion)
				     in (strict_fundec :: fundecs_c, env_inc)
				    end)

		  end (* fb_folder *)


	    (* "parsing" of Ast.FunDec fb list to obtain list of fundecs and an
	       incremental static environment binding their function symbols. This uses
	       the region originally passed to elabFunDec and an initial empty static
	       environment, so the incremental environment returned (env_fbs) contains
	       only bindings for the function name symbols. The list of fundecs returned
	       is in reverse order of the original fb values in the Ast.FunDec. *)
	    val (fundecs, env_fbs) = foldl (fb_folder region) ([]: fundec list , SE.empty) fbs


	    (* check that the function names in the FunDec are unique. *)
	    val _ = if EU.checkUniq (map (#1) fundecs)
		    then ()
		    else errorRegion (region, "duplicate function names in function declaration")

	    (* env_post_fbs: cummulative environment, the original elabFunDec env (eFd_env)
	       augmented with function name bindings (env_fbs). *)
	    val env_post_fbs = SE.atop (env_fbs, env)

            (* elabClause : ast_clause * lazyKind * (T.ty * TS.tyvarset) option * SL.region
	       		    -> as_clause * TS.tyvarset * tyvarsetUpdater *)
	    fun elabClause ({argpats, exp}: ast_clause, resTyOp, lazyKind, region: SL.region) =
		let val (pats, tyvarset_pats) = elabPatList (argpats, env, region)
                    val nenv = SE.atop (EU.bindVARp pats, env_post_fbs)
		    val (as_exp, tyvarset_exp, updater_exp) = elabExp (exp, nenv, region)
		    (* LAZY fb: wrap delay or force around elaborated rhs as appropriate. *)
		    val exp' =
			case lazyKind
			  of STRICT => exp_as
			   | DELAY => delayExp exp_as
			   | FORCE => forceExp exp_as
		    val (tyOp, tyvarset_resultty) =
		        case resTyOp
		          of NONE => (NONE, TS.empty)
			   | SOME (ty, tyvarset) => (SOME ty, tyvarset)
		 in ({pats = pats, exp = as_exp},
		     union (tyvarset_pats, union (tyvarset_exp, tyvarset_resultty)),
		     updater_exp)
		end

	    (* fundecFolder : fundec * (as_fundecs * TS.tyvarset * TS.tyvarsetUpdater)
	                      -> as_fundecs * TS.tyvarset * TS.tyvarsetUpdater *)
	    fun fundecFolder ((funsym, _, _, clauses, _, region): fundec, (as_fundecs_c, tyvarset_c, updaters_c)) =
		let fun folder (ast_clause, (clauses_c, tyvarset_c, updaters_c)) =
			  let val (as_clause_i, tyvarset_i, updater_i) = elabClause (ast_clause, region)
			   in (as_clause_i :: clauses_c, union (tyvarset_i, tyvarset_c),
			       updater_i :: updaters_c)
			  end
		    val (clauses, tyvarset', updaters') =
			  foldl folder ([], TS.empty, []) clauses
 		 in ((funsym, rev clauses, region) :: as_fundecs_c,
		     union (tyvarset', tyvarset_c),
		     updaters' @ updaters_c)
		end

	    val (as_fundecs: as_fundec list, fbsTyvarset: TS.tyvarset, updaters: TS.tyvarUpdater list) =
		foldl fundecFolder ([], TS.empty, []) fundecs

	    (* a single common ref cell for the tyvarset for all the fb bindings? *)
	    val tyvarsetRef : TS.tyvarset ref = ref TS.empty

	    fun fbsUpdater (tyvarset: TS.tyvarset) : unit =
		let val tyvarset' = TS.diffPure (tyvarset, eFd_tyvarset)  (* subtract "external"? tyvars *)
		    val local_tyvarset = diff (union (fbsTyvarset, eFb_tyvarset), tyvarset')
		    val down_tyvarset = union (local_tyvarset, tyvarset')
		 in tyvarsetRef := local_tyvarset;
		    app (fn f => f down_tyvarset) updaters
		end

	    (* mk_as_fundec : S.symbol * as_clause list * SL.region -> as_fundec *)
            (* defines tyvars field to be tyvarsetRef *)
	    fun mk_as_fundec (funsym : S.symbol, as_clauses: as_clause list, region: SL.region) =
		  {funsym = funsym, clauses = clauses, tyvars = tyvarsetRef, region = region}
	      | mk_as_fundec _ = bug "elabFunDec#mk_as_fundec"  (* if var is not VALvar *)

(*
  Absyn.rule = RULE of AS.pat * AS.exp

  ElabUtil:
  ElabUtil.completeMatch : (StaticEnv.staticEnv * string) -> Absyn.rule list -> Absyn.rule list
  ElabUtil.FUNdec :
       (Absyn.rule list -> Absyn.rule list)  (* rule "completion" function *) 
       * {funsym : S.symbol, (* var : VarCon.var CHANGED! *)
          clauses: as_clause list   (* as_clause type needs to be defined in ElabUtil *)
          tyvarset: TyvarSet.tyvarset ref,   (* <== CHANGED *)
	  region: SourceLoc.region } list  (* i.e. as_fundec list, as_fundec should be defined in ElabUtil *)
       -> (Absyn.dec * StaticEnv.staticEnv)  (* what does this staticEnv contain? *)

  locally (inside elabDec) EU.completeMatch is partially applied inside elabDec, yielding: 
  val completeMatch : Absyn.rule list -> Absyn.rule list
*)

            val (dec', env') = EU.FUNdec (completeMatch (map mk_as_fundec as_fundecs))

	 in showDec ("elabFunDec: ", dec', env');
	    (dec', env', TS.empty, fbsUpdater)

	end (* function elabFunDec *)

    (* elabSEQdec:  [ast_decs:]Ast.decl list * [env_base:]SE.staticEnv * [rpath:]IP.path
                    * [region:]SL.region)
                    -> AS.decl * SE.staticEnv * TS.tyvarset * updaterTy (?) *)
    and elabSEQdec (ast_decs: Ast.dec list, env_base: SE.staticEnv, rpath: IP.path, region: SL.region) =
	let fun folder (ast_dec, (decs_acc, env_acc, tyvarset_acc, updaters_acc)) =
		  let val (dec, env_inc, tyvarset_inc, updater_inc) =
			   elabDec' (ast_dec, SE.atop(env_acc,env_base), rpath, region)
		   in (dec::decs_acc, SE.atop (env_acc, env_base),
                       checkedUnion (region, "elabSEQdec") (tyvarset_inc,tyvarset_acc),
		       updater_inc :: updaters_acc)
		  end
            (* need to use foldl to nest the local, incremental environments properly *)
	    val (decs, env_loc, tyvarset_full, updaters_full) =
		foldl folder ([], SE.empty, TS.empty, []) ast_decs
	    fun updater_fin tv : unit = app (fn f => f tv) updaters_full
	 in (SEQdec (rev decs), env_loc, tyvarset_full, updater_fin)
	end

    val _ = debugmsg ("EC.elabDec calling elabDec' - foo")

    val (dec', env', tyvars, updater) = elabDec' (dec, env, rpath, region)

 in updater tyvars;
    (dec',env')

end (* function elabDec *)


(* ABSTRACT TYPE DECLARATIONS  ************************************)

(* Found here at the end of ElabCore because it calls elabDec. *)
fun elabABSTYPEdec({abstycs: Ast.db list, withtycs: Ast.tb list, body: Ast.dec},
		    env: SE.staticEnv, isFree: (T.tycon -> bool),
		    rpath: IP.path, region: SL.region) =
  let val (datatycs, withtycs, _, env1) =
          ET.elabDATATYPEdec({datatycs=abstycs,withtycs=withtycs}, env,
                             [], EE.empty, isFree, rpath, region)

      val (body, env2) =
          elabDec(body,SE.atop(env1,env),isFree,rpath,region)

      (* datatycs will be changed to abstycs during type checking
	 by changing the eqprop field *)
      fun bind (x, e) = SE.bind(TU.tycName x, B.TYCbind x, e)

      val envt = foldl bind (foldl bind SE.empty datatycs) withtycs

   in (ABSTYPEdec {abstycs = datatycs, withtycs = withtycs, body = body},
       SE.atop (env2, envt))
  end (* function elabABSTYPEdec *)


end (* top-level local; imports *)
end (* structure ElabCore *)
