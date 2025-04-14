(* Elaborator/elaborate/elabcore.sml
 *
 * COPYRIGHT (c) 2017, 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
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

  structure SL = SourceLoc
  structure SM = SourceMap
  structure EM = ErrorMsg

  structure S = Symbol
  structure F = Fixity
  structure A = Access
  structure LV = LambdaVar
  structure SP = SymPath
  structure IP = InvPath
  structure SE = StaticEnv
  structure B  = Bindings
  structure LU = Lookup

  structure V = VarCon  (* -> Variable *)
  structure AS = Absyn
  structure AU = AbsynUtil

  structure T  = Types
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure TS = TyvarSet

  structure M  = Modules
  structure MU = ModuleUtil
  structure EE = EntityEnv

  structure EU = ElabUtil
  structure ED = ElabDebug
  structure ET = ElabType

  open Absyn Ast (* Types => t; Access => A.; ElabUtil => EU. *)

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
(* conditional abstract syntax marking functions for pats, exps and decs *)

(* cMARKpat : AS.pat * SL.region -> AS.pat *)
fun cMARKpat (pat: AS.pat, region: SL.region) =
    if !ElabControl.markabsyn then AS.MARKpat (pat, region) else pat

(* cMARKexp : AS.exp * SL.region -> AS.exp *)
fun cMARKexp (exp: AS.exp, region: SL.region) =
    if !ElabControl.markabsyn then MARKexp (exp, region) else exp

(* cMARKdec : AS.dec * SL.region -> AS.dec *)
fun cMARKdec (dec: AS.dec, region: SL.region) =
    if !ElabControl.markabsyn then MARKdec (dec, region) else dec


(* ========================================================================================== *)
(* REAL32: *)
(* bounds for Real64.real constant values; these should be moved to overload
 * resolution once we support more than one size of real. *)

val minSubnormalReal64 = RealLit.real{isNeg = false, whole="4", frac="9", exp = ~324}
val minNormalReal64 = RealLit.real{isNeg = false, whole="2", frac="2250738585072014", exp = ~308}
val maxReal64 = RealLit.real{isNeg = false, whole="1", frac="7976931348623157", exp = 308}

(* bodies substituted inline, so no longer needed

fun mkIntLiteralTy (v : IntInf.int, r : SL.region) : ty =
      T.VARty (T.mkTyvar (T.OVLDI [(v, r)]))

fun mkWordLiteralTy (v : IntInf.int, r : SL.region) : ty =
      T.VARt (T.mkTyvar (OVLDW [(v, r)]))

(* REAL32: eventually this will be an overload instance *)
fun mkRealLiteralTy (v : RealLit.t, r : SL.region) : ty = BT.realTy

*)


(* ========================================================================================== *)
(* tyvarset updater functions *)
(* what do these updater functions do? -- to be rediscovered! *)

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
(* [LAZY] types associated with function clauses that are used local to elabFUNdec,
 * and are related to processing lazy function declarations. *)

(* clauseKind: used for communicating information about lazy FunDec decs
 *  between preprocessing phase (fb_folder, formerly makevar) and the main part of elabFUNdec. *)
datatype clauseKind = STRICT | LZouter | LZinner

(* clauseTy: the argument type, or contents, of the Ast.Clause data constructor.
   This is defined here as documentation of the input for clause elaboration. The type does not
   appear in the code at the moment.
   An Ast.Fb (recursive function binding, a list of which is found in the FunDec declaration
   variant) contains a list of these clauses, plus a boolean flag specifying whether the function
   being declared is lazy.
   The pats field contains a list of "partially parsed" patterns of type Ast.pat Ast.fixitem,
   which constitute the LHS of a clause, while the exp field contains the RHS expression of the clause.
   The LHS of a clause is a partially parsed list of (curried) parameters is represented as
   a pat fixitem list (pats) that were produced by parsing corresponding atomic pats (apats in ml.grm).
   The function name (the subject of the parent fb) is extracted by the parsePatFixitems function
   defined around line 1391 (elabFUNdec#makevar#parsePatFixitems. *)

type clauseTy =  {pats: pat fixitem list, resultty: ty option, exp:exp}

(* clauseTy_ast: the Ast clause record produced by parseClause,
 * not yet elaborated, but with LHS fully parsed, i.e., no remaining FlatAppPats and funsym determined.
 * Note that funsyn and resultTy (if SOME) and number of argpats should be consistent across
 * the clauses of a given function dec. Perhaps the common elements, funsym, resultTy (if SOME) 
 * should be factored out of the clauses. kind is different; it will be the same (STRICT) for all clauses
 * of a strict function, but kinds of the clauses of a lazy function will vary between LZouter and LZinner. *)
type clauseTy_ast = {kind: clauseKind,
		       (* for potentially lazy function declarations, presumably the same
			* value for all clauses of a given function. *)
		     funsym: S.symbol,
		       (* name of the function being defined. Presumabley the same for all clauses
			* of a given function. This is checked. *)
		     argpats: Ast.pat list,
		       (* LHS function argument patterns, fully parsed; function is curried if more than 1.
			* The number of argpats should be the same for all clauses of a function dec. *)
	             resultty: Ast.ty option, (* optional result type specification *)
		     exp: Ast.exp}            (* the clause RHS expression *)

(* clauseTy_absyn: the Absyn record for a clause produced by elabClause 
   When the pats list has length > 1, the function is curried. *)
type clauseTy_absyn = {pats : AS.pat list, resultty: T.ty option, exp : AS.exp}

(* Defn of fixitem tycon from structure Ast, repeated here for refernce.
   fixitems are produced by the parser (ml.grm.sml) for atomic patterns and atomic expressions.
 
   ('a -> pat or 'a -> exp)
   type 'a fixitem = {item: 'a,
                      fixity: S.symbol option, (* FIX: Bad field label. SOME iff item is a variable  *)
		      region: SL.region} 

   Where 'a will be instantiated to either Ast.pat (for pat reparsing) or Ast.exp (for exp reparsing).
*)
  
(* ========================================================================================== *)
(* some utility functions not defined in ElabUtil -- though they could be moved there! *)

(* stripExpAbs: AS.exp -> AS.exp
 * strip MARKexp and CONSTRAINTexp wrappers around an expression *)
fun stripExpAbs (MARKexp(e,_)) = stripExpAbs e
  | stripExpAbs (CONSTRAINTexp(e,_)) = stripExpAbs e
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

(* LAZY *)

local
    (* mkCoreExp : [name:]string -> [env:]SE.staticEnv -> AS.exp *)
    fun mkCoreExp name env = AS.VARexp (ref (CoreAccess.getVar env [name]), [])
in
    val mkAssignExp : SE.staticEnv -> AS.exp = mkCoreExp "assign"  (* AS.exp for ":=" operator *)
    val mkBangExp : SE.staticEnv -> AS.exp   = mkCoreExp "deref"   (* AS.exp for "!" operator *) 
end


(* ========================================================================================== *)
(* finally, the elaboration functions *)

(**** ABSTRACT TYPE DECLARATIONS (should go away) ****)
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


exception FreeOrVars
(* used when creating a hash table in the OrPat case *)

(* ELABORATE GENERAL (core) DECLARATIONS -- the main elaboration function *)
and elabDec (dec, env, isFree, rpath, region) =

let
    val _ = debugmsg ">>ElabCore.elabDec"

    val completeMatch = EU.completeMatch(env,"Match")
    val _ = debugmsg "--ElabCore.elabDec << completeBind Match"
    val completeBind = EU.completeMatch(env,"Bind")
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
    val parseFlatApp = Precedence.parse {apply=apply_pat, pair=tuple_pat}

    (* patCompleteParse : pat * region -> pat *)
    (* do we need to add an env argument? *)					
    fun patCompleteParse (pat: Ast.pat, region: SL.region) =
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
	    in (cMARKpat (pat', region), tyvarset)
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
		in (cMARKexp (exp',region), tyvarset, updater)
	       end
	   | Ast.SelectorExp s =>
	       (let val v = newVALvar s
		 in AS.FNexp (completeMatch
			       [AS.RULE (AS.RECORDpat {fields=[(s, AS.VARpat v)], flex=true,
					               typ= ref T.UNDEFty},
					 cMARKexp (VARexp (ref v,[]),region))], T.UNDEFty)
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
	      (* BUG[DBM]: what to do if rhs is lazy "datatype"? *)
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
		  | NONE => 
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
		in (FIXdec ds,env,TS.empty,nullUpdater)
	       end

	   | Ast.OvldDec dec  => elabOVERLOADdec(dec,env,rpath,region)

	   | Ast.MarkDec(dec,region') =>
	       let val (dec', env, tyvarset, updater) = elabDec' (dec,env,rpath,region')
		in (cMARKdec (dec',region'), env, tyvarset, updater)
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
           in (cMARKdec (dec, region), tyvarset, updater)
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
	      val match' = cMARKexp (match, region)
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

    (* elabFUNdec : Ast.fb list * tyvar list * SE.staticEnv * IP.path * SL.region
                    -> AS.dec * SE.staticEnv * TS.tyvarset * tyvarsetUpdater *)
    (* The fbs and etvs are the arguments of an Ast.FunDec constructor (function dec),
     *   while env, rpath, region provide the context. *)
    and elabFUNdec (fbs: Ast.fb list, etvs: Ast.tyvar list, env: SE.staticEnv, rpath: IP.path,
		    region: SL.region) =
	let val etvs: TS.tyvarset = TS.mkTyvarset (ET.elabTyvarList (etvs, region))

           (* specialized checking (for incompatible tyvars) versions of TS.union and TS.diff *)
            val union = checkedUnion (region, "elabFUNdec")  (* checked union of tyvarsets *)
            val diff = checkedDiff (region, "elabFUNdec")  (* checked diff of tyvarsets *)

            (* fb_folder: SL.region
	                -> (Ast.fb * ((symbol * clauseTy_ast * region) list * SE.staticEnv)
	                -> ((symbol * clauseTy_ast list * region) list * SE.staticEnv)
	       parse the function header (clause LHS) to determine the function name and to separate
	       and parse the argument patterns. 
	       fb_folder is a folder function to be folded over the fbs list. *)
	    fun fb_folder _ (MarkFb (fb, fbregion)) = fb_folder fbregion fb
	      | fb_folder fbregion (Fb (clauses_fbl: clauseTy list, lazyp: bool),
				  (clauses_cum: (symbol * clauseTy_ast list * SL.region) list,  env': SE.staticEnv)) =
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
				     | _ => errorRegion (region, String.concat ["infix operator \"",  S.name sym,
										"\" used without \"op\" in fun dec"]));
			  item)  (* without further parsing, just outer fixitem stripped off *)

		     (* getname : Ast.pat * SL.region -> S.symbol
	              * Get the "name" of a simple VARpat (optionally MARKed);
		      * fails and reports error if applied to other, compound, pats *)
		     fun getname (MarkPat (pat, region),_) = getname (pat,region) (* ignore MarkPat *)
		       | getname (VarPat [v], _) = v  (* path of a VarPat must be singleton (simple variable),
						       * then return the variable symbol *)
		       | getname (_, region) =
                           (errorRegion (region, "illegal function symbol in clause");
			    EU.bogusID)

                     (* stripMarkPat : Ast.pat -> Ast.pat *)
		     (* strips MarkPats even if nested, ignoring region *)
                     fun stripMarkPat (Ast.MarkPat (pat, _)) = stripMarkPat pat
		       | stripMarkPat pat = pat


		     (* stripMarkFixitem: Ast.pat Ast.fixitem -> Ast.pat Ast.fixitem *)
		     (* stripping a MarkPat from the fixitem pat while preserving the fixitem region *)
		     fun stripMarkFixitem ({item, fixity, region}: patfixitem) : patfixitem
 			   {item = stripMarkPat pat, fixity=fixity, region=region}

	    (* Multi-pass "parsing" of Fb-clause LHS patterns (presented as pat fixitem list),
	       where the first pass, parsePatFixitems, checks for the case where
	       the lhs is an infix function application. The second pass is
	       parsePatFixitems0 and it checks that there are more than one fixitems
	       (need both a function variable.  The problem being solved is that the LHS
	       of a Fb clause is a list of pattern fixitems that needs to be analyzed to 
	       find the name (funSym) of the function being declared and bound. *)
	    (* NOTE: patfixitems are produced by the apat productions in the ml.grm grammar. *)

		     (* parsePatFixitems : patfixitem list ->  S.symbol * Ast.pat list *)
		        (* "Parse" a clause LHS, i.e., a list of pattern fixitems (derived from
			   parsing the apats (atomic patterns) representing the LHS of a clause
			   in the grammar.
			   We first check whether a 3 element
			 * patfixitem list should be parsed as an infix application
			 * (of a datacon or the defined function variable)
			 * Note. The resulting Ast.pat list may contain pats that need further parsing,
			   i.e. FlatAppPats *)

  	             fun parsePatFixitems (first as {item = Ast.FlatAppPat fixitems, fixity = NONE, region} :: rest) =
			   (* First "arg" item is FlatAppPat that can represent an arbitrarily compicated
			      pattern application term. But it's top function symbol, say f,  will be the funsym for
			      the clause. Thus we parse (parseFlatApp) first and deconstruct it to get the 
			      function symbol f and we add f's argument to the checked rest.  The parsed FlatAppPat
			      can turn out to be a variable pat (fixitems is a singleton, which should be a variable
			      item.  Otherwise complain.
			      Note that the FlatAppPat first was parsed as a paranthesized sequence of apats *)
			      (* Is the parsed FlatAppPat an AppPat or a VarPat or something else? *)
			      (* missing the possibility that the next clause (clause 2) may apply after parsing
				 firt. Need to call parsePatFixitems recursively on the new list, knowing
				 that the new parsed pat is not a FlatAppPat. *)
			      (case stripMarkPat (parseFlatApp (fixitems, env, region))
			       (* need to strip Mark, if any, to be safe. Can this happen? *)
			         of Ast.AppPat {constr = Ast.VarPat [path], argument} =>  (* first parses to AppPat *)
				       (case path
				          of [f] => (f, {item = argument, fixity = NONE, region = region}
							:: map ensureNonfix rest)
					   | _ => EM.errorRegion (region, "function symbol is not a variable"))
				       (v, argument :: (map ensureNonfix rest))
				  | Ast.VarPat [path] =>
				       (case path
				          of [f] => (f, {item = argument, fixity = NONE, region = region}
							:: map ensureNonfix rest)
					   | _ => EM.errorRegion (region, "function symbol is not a variable"))
				  | pat =>  (* not a variable, nor an AppPat *)
				       parsePatFixitems ({item = pat, fixity = NONE, region = region} :: rest))

 			   (* The first patfixitem is a FlatAppPat of a triple of patfixitems,
			    * so the middle one (#item b) could be an infix function variable. 
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

			 | parsePatFixitems [{a :: b as {fixity = SOME f,...} :: c :: rest]] =
		           (* The "args" apat listfixitems of length >= 3 and b (the second fixitem)
			      is an variable item (#item b is a VarPat, possibly Marked).
			    * Check if f (b's symbol) is infix. If so, its arguments will be
			      Ast.Tuple [#item a, #item c], checking that they are NONfix
			      (#item a and #item x better not be infix variable items!).
                            * Add argument tuple pat [a, c] to the checked rest to construct the true clause
			    * arguments.
			    * If f is NONfix? Then "a" should be NONfix variable fixitem from which we
			    * obtain the function symbol. *)
			   (case LU.lookFix f (* we already have the (fixity) symbol f for fixitem b *)
			     of INfix _ => (f,
					    Ast.TuplePat [ensureNonfix a, ensureNonfix b] :: map ensureNonfix rest)
			      | NONfix =>
				   (getName (a, region),
				    ensureNonfix b :: ensureNonfix c :: (map ensureNonfix rest)))

			 | parsePatFixitems [{item,region,...}] =  (* only one apat in clause LHS -> Error! *)
			   (* only one pseudo-arg apat, and it is not compound because it is not a FlatAppPat!
			    * (because the only compound (AppPat) patterns the parser produces are FlatAppPats)
                            * Thus the item must be a variable pattern representing the function name,
			    * which implies there are no argument patterns! *)
			   (errorRegion (region, "missing function arguments in LHS of a clause");
			    (bogusFunSym, nil))  (* dummy value returned for potential error recovery *)

			 | parsePatFixitems ((a as {region,...}) :: rest) =  (* rest is not null, at least 2 apats *)
			   (* first item is not a FlatAppPat triple, so it must be the NONfix function name *)
			   (getname (ensureNonfix a, region),
			    map ensureNonfix rest)


			 | parsePatFixitems nil = bug "parsePatFixitems0: no fixitems - empty LHS in clause?"

		     (* parseClause : Ast.clause -> clauseTy_ast *)
		     (* kind is set to STRICT by default *)
		     fun parseClause (Ast.Clause {pats, resultty, exp}: Ast.clause) : clauseTy_ast =
		         let val (funsym, argpats: Ast.pat list) = parsePatFixitems (map stripMarkItem pats)
			  in {kind = STRICT, funsym = funsym, argpats = argpats,
			      resultty = resultty, exp = exp}  (* resultty and exp come directly from the 
								  Ast.clause argument *)
			 end

		     (* clauses, funSym : symbol*)
		     (* funSym is defined as the funsym of the first clause *)
		     (* note difference between "funsym" and "funSym" as defined above.  Each clause has
		      * its "funsym", while funSym should be the name of the function being declared in the
		      * current fb. *)
		     val (ast_clauses: clauseTy_ast list, funSym: S.symbol) =
                         case map parseClause clauses_fb
			   of nil => bug "elabDec'#elabFUNdec#fb_folder: no fb clauses"
			    | (clauses as ({funsym,...}::_)) => (clauses, funsym)

		     val _ = if List.exists (fn {funsym,...}: clauseTy_ast => not (S.eq (funsym, funSym)))
					    clauses
			     then  errorRegion (fbregion, "clauses do not all have same function name")
			     else ()

		     (* create funVar: V.var from the funSym symbol *)
		     val funVar: V.var = newVALvar funSym    (* was "val v = newVALvar var" *)

		     (* checking that the number of curried argument patterns is the same for all clauses *)
		     val argcount : int =
			 case clauses
			   of ({argpats,...}) :: rest =>
				let val len = length argpats
				 in if List.exists
					(fn {argpats,...} => len <> length argpats)
					rest
				    then errorRegion (fbregion,
						      "clauses do not all have same number of patterns")
				    else ();
				    len
				end
			    | [] => bug "elabFUNdec: no clauses"

		  in if lazyp (* LAZY *)
		     then let (* newArgs: (S.symbol list * int) -> S.symbol list *)
			      (* newArgs (nil, 3) ==> [$1, $2, $3] *)
			      fun newArgs (args,0) = args
				| newArgs (args,n) =
				  newArgs ([S.varSymbol("$"^Int.toString n)]::args,
					   n-1)

			      (* curryApp : Ast.exp * Ast.exp list -> Ast.exp *)
			      fun curryApp (f, nil) = f
				| curryApp (f, x::xs) =
				    curryApp (AppExp {function=f, argument=x}, xs)

			      val lazySym: S.symbol = S.varSymbol(S.name funSym ^ "_")

			      val localVar: V.var = newVALvar lazySym

			      fun mkLazy (new, resty, nil) = (rev new,resty)
			        | mkLazy (new, resty,
					  {kind,funsym,argpats,resultty,exp}::rest) =
				  mkLazy ({kind = LZinner, funsym = lazySym, argpats = argpats,
					   resultty = NONE, (* moved to outer clause *)
					   exp = exp}
				          ::new,
				          case resty of NONE => resultty | _ => resty,
					  rest)

                              (* BUG: this captures the first resultty encountered,
			         if any, and discards the rest, not checking
				 consistency of any redundant resultty constraints *)
			      val (innerclauses, resultty) =
				  mkLazy ([], NONE, clauses)

                              val outerargs = newArgs([],argcount)

			      val outerclause =
				  {kind = LZouter, funsym = funSym, resultty = resultty,
				   argpats = map VarPat outerargs,
				   exp = curryApp (VarExp[lazySym], map VarExp outerargs)}

			   in ((localVar, innerclauses, fbregion) :: (funVar, [outerclause], fbregion) :: clauses_cum,
			       SE.bind(funSym, B.VALbind funVar, SE.bind (lazySym, B.VALbind localVar, env')))
			  end
		     else ((funVar, clauses, fbregion) :: clauses_cum, SE.bind (funSym, B.VALbind funVar, env'))
		 end (* fun fb_folder *)

	    val (fundecs, env') = foldl (fb_folder region) ([]: (S.symbol * clauseTy_ast * SL.region) list , SE.empty) fbs

	    val env'' = SE.atop (env', env)

            (* elabClause : clauseTy_ast * SL.region
	       		    -> clauseTy_absyn * TS.tyvarset * tyvarsetUpdater *)
	    fun elabClause ({kind, argpats, resultty, exp, funsym}: clauseTy_ast, region: SL.region) =
		let val (pats, tyvarset_pats) = elabPatList (argpats, env, region)
                    val nenv = SE.atop (EU.bindVARp pats, env'')
		    val (exp_as, tyvarset_exp, updater_exp) = elabExp (exp, nenv, region)
		    (* LAZY: wrap delay or force around rhs as appropriate*)
		    val exp' =
			case kind
			  of STRICT => exp_as
			   | LZouter => delayExp exp_as
			   | LZinner => forceExp exp_as
		    val (tyOp, tyvarset_resultty) =
		      case resultty
		       of NONE => (NONE,TS.empty)
			| SOME ty =>
			    let val (ty', tyvarset) = ET.elabType (ty, env, region)
			     in (SOME ty', tyvarset)
			    end
		 in ({pats = pats, resultty = tyOp, exp = exp'},
		     union (tyvarset_pats, union (tyvarset_exp, tyvarset_resultty)),
		     updater_exp)
		end

	    fun fundecFolder ((var, clauses, region), (fs, tyvarset, updaters)) =
		let fun folder (clause_ast, (clauses_acc, tyvarset_acc, updaters_acc)) =
			  let val (clause, tyvarset_inc, updater_inc) = elabClause (clause_ast, region)
			   in (clause::clauses_acc, union (tyvarset_inc, tyvarset_acc),
			       updater_inc::updaters_acc)
			  end
		    val (clauses_cum, tyvarset', updaters') =
			  foldl folder ([], TS.empty, []) clauses
 		 in ((var, rev clauses_cum, region) :: fs,
		     union (tyvarset, tyvarset),
		     updaters' @ updaters)
		end

	    val (fbs', fbsTyvarset, updaters) = foldl fundecFolder ([], TS.empty, []) fundecs

	    val tyvarsetRef : TS.tyvarset ref = ref TS.empty
	      (* single common ref cell for the tyvarset for all bindings? *)

	    fun fbsUpdater (tyvarset: TS.tyvarset) : unit =
		let val tyvarset' = TS.diffPure (tyvarset, etvs)  (* subtract "external"? tyvars *)
		    val local_tyvarset = diff (union (fbsTyvarset, etvs), tyvarset')
		    val down_tyvarset = union (local_tyvarset, tyvarset')
		 in tyvarsetRef := local_tyvarset;
		    app (fn f => f down_tyvarset) updaters
		end

	    (* makefb : V.var * clauseTy_absyn list * SL.region -> fbTy1 *)
            (* checks that the var: V.var is a VALvar; defines tyvars field to be tyvarsetRef *)
	    fun makefb (var as V.VALvar{ ... }: V.var, clauses: clauseTy_absyn list, region: SL.region) =
		  {var = var, clauses = clauses, tyvars = tyvarsetRef, region = region}
	      | makefb _ = bug "makeFUNdec.makefb"  (* if var is not VALvar *)

	 in if EU.checkUniq (map (fn (V.VALvar{path=SymPath.SPATH[x],...},_,_) => x
			           | _ => bug "makeFUNdec:checkuniq: var path")
			         fbs')
	    then ()
	    else errorRegion (region, "duplicate function names in fun dec");

	    let val (dec', env') = EU.FUNdec (completeMatch (map makefb fbs'))
             in showDec ("elabFUNdec: ", dec', env');
		(dec', env', TS.empty, fbsUpdater)
            end

	end (* function elabFUNdec *)

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

end (* top-level local; imports *)
end (* structure ElabCore *)
