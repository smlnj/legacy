(* elabutil.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ElabUtil : ELABUTIL =
struct

local structure SP = SymPath
      structure LU = Lookup
      structure A = Access
      structure B  = Bindings
      structure SE = StaticEnv
      structure EE = EntityEnv
      structure TS = TyvarSet
      structure S = Symbol
      structure V = VarCon
      structure BT = BasicTypes

      open Symbol Absyn Ast ErrorMsg PrintUtil AstUtil Types BasicTypes
           EqTypes ModuleUtil TypesUtil VarCon

in

(* debugging *)
val say = Control_Print.say
val debugging = ref false
fun debugmsg (msg: string) =
    if !debugging then (say msg; say "\n") else ()

fun bug msg = ErrorMsg.impossible("ElabUtil: "^msg)

fun for l f = app f l
fun discard _ = ()
fun single x = [x]

val internalSym = SpecialSymbols.internalVarId

(* elaboration context *)

datatype context
  = TOP      (* at top level -- not inside any module, rigid *)
  | INSTR    (* inside a rigid structure, i.e. not inside any functor body *)

  | INFCT of {flex: Stamps.stamp -> bool,  depth: DebIndex.depth}
             (* within functor body *)
  | INSIG    (* within a signature body *)

type compInfo = Absyn.dec CompInfo.compInfo

fun newVALvar(s, mkv) = V.mkVALvar(s, A.namedAcc(s, mkv))

fun smash f l =
    let fun h(a,(pl,oldl,newl)) =
	  let val (p,old,new) = f a
	   in (p::pl,old@oldl,new@newl)
	  end
     in foldr h (nil,nil,nil) l
    end

local
  fun uniq ((a0 as (a,_,_))::(r as (b,_,_)::_)) =
	if S.eq(a,b) then uniq r else a0::uniq r
    | uniq l = l
  fun gtr ((a,_,_), (b,_,_)) =  let
	val a' = S.name a and b' = S.name b
	val a0 = String.sub(a',0) and b0 = String.sub(b',0)
	in
	  if Char.isDigit a0
	      then if Char.isDigit b0
		then size a' > size b' orelse size a' = size b' andalso a' > b'
		else false
	      else if Char.isDigit b0
		then true
		else (a' > b')
	end
 in fun sort3 x = uniq (ListMergeSort.sort gtr x)
end

val EQUALsym = S.varSymbol "="

val anonParamName = S.strSymbol "<AnonParam>"

(* following could go in Absyn *)
val bogusID = S.varSymbol "*bogus*"
val bogusExnID = S.varSymbol "*Bogus*"

val TRUEpat = CONpat(trueDcon,[])
val TRUEexp = CONexp(trueDcon,[])
val FALSEpat = CONpat(falseDcon,[])
val FALSEexp = CONexp(falseDcon,[])

val NILpat = CONpat(nilDcon,[])
val NILexp = CONexp(nilDcon,[])
val CONSpat = fn pat => APPpat(consDcon,[],pat)
val CONSexp = CONexp(consDcon,[])

val unitExp = AbsynUtil.unitExp
val unitPat = RECORDpat{fields = nil, flex = false, typ = ref UNDEFty}
val bogusExp = VARexp(ref(V.mkVALvar(bogusID, A.nullAcc)), [])

(* Verifies that all the elements of a list are unique *)
fun checkUniq (err,message,names) =
    let val names' = ListMergeSort.sort S.symbolGt names
	fun check (x::y::rest) =
	     (if S.eq(x,y)
	      then err COMPLAIN (message ^ ": " ^ S.name x) nullErrorBody
	      else ();
	      check (y::rest))
	  | check _ = ()
     in check names'
    end

(* symbols that are forbidden for use as data or exn constructor names *)
val forbiddenConstructors =
    [EQUALsym, S.varSymbol "it", S.varSymbol "true", S.varSymbol "false",
     S.varSymbol "nil", S.varSymbol "::", S.varSymbol "ref"]

(* checks whether names contains a forbidden constructor name *)
fun checkForbiddenCons symbol =
    List.exists (fn x => S.eq(symbol,x)) forbiddenConstructors

(*
 * Extract all the variables from a pattern
 * NOTE: the "freeOrVars" function in elabcore.sml should probably
 * be merged with this.
 *)
fun bindVARp (patlist,err) =
    let val vl = ref (nil: symbol list)
	val env = ref(SE.empty: SE.staticEnv)
	fun f (VARpat(v as VALvar{path=SP.SPATH[name],...})) =
	       (if S.eq(name, EQUALsym)
		then err WARN "rebinding \"=\" is not allowed" nullErrorBody
		else ();
		env := SE.bind(name,B.VALbind v,!env);
		vl := name :: !vl)
	  | f (RECORDpat{fields,...}) = app(fn(_,pat)=>f pat) fields
	  | f (VECTORpat(pats,_)) = app f pats
	  | f (APPpat(_,_,pat)) = f pat
	  | f (CONSTRAINTpat(pat,_)) = f pat
	  | f (LAYEREDpat(p1,p2)) = (f p1; f p2)
	  | f (ORpat(p1, p2)) = (f p1; bindVARp([p2], err); ())
	  | f (MARKpat(p,_)) = f p
	  | f _ = ()
     in app f patlist;
	checkUniq (err,"duplicate variable in pattern(s)",!vl);
	!env
    end

(*
fun isPrimPat (VARpat{info, ...}) = II.isPrimInfo(info)
  | isPrimPat (COSTRAINTpat(VARpat{info, ...}, _)) = II.isPrimInfo(info)
  | isPrimPat _ = false
*)


(* sort the labels in a record the order is redefined to take the usual
   ordering on numbers expressed by strings (tuples) *)

local
  fun sort x =
    ListMergeSort.sort (fn ((a,_),(b,_)) => TypesUtil.gtLabel (a,b)) x
in fun sortRecord(l,err) =
     (checkUniq(err, "duplicate label in record",map #1 l);
      sort l)
end

fun makeRECORDexp(fields,err) =
    let val fields' = map (fn(id,exp)=> (id,(exp,ref 0))) fields
	fun assign(i,(_,(_,r))::tl) = (r := i; assign(i+1,tl))
	  | assign(_,nil) = ()
	fun f(i,(id,(exp,ref n))::r) = (LABEL{name=id,number=n},exp)::f(i+1,r)
	  | f(_,nil) = nil
     in assign(0, sortRecord(fields',err)); RECORDexp(f(0,fields'))
    end

val TUPLEexp = AbsynUtil.TUPLEexp

fun TPSELexp(e, i) =
    let val lab = LABEL{number=i-1, name=(Tuples.numlabel i)}
     in SELECTexp(lab, e)
    end

(* Adds a default case to a list of rules.
   If given list is marked, all ordinarily-marked expressions
     in default case are also marked, using end of given list
     as location.
   KLUDGE! The debugger distinguishes marks in the default case by
     the fact that start and end locations for these marks
     are the same! DBM: Is that you, Andrew Tolmach?  Is this
     kludge still relevant?  Probably not! *)
fun completeMatch'' rule [r as RULE(pat,MARKexp(_,(_,right)))] =
      [r, rule (fn exp => MARKexp(exp,(right,right)))]
  | completeMatch'' rule
                    [r as RULE(pat,CONSTRAINTexp(MARKexp(_,(_,right)),_))] =
      [r, rule (fn exp => MARKexp(exp,(right,right)))]
  | completeMatch'' rule [r] = [r,rule (fn exp => exp)]
  | completeMatch'' rule (a::r) = a :: completeMatch'' rule r
  | completeMatch'' _ _ = bug "completeMatch''"

(* used in handleExp *)
fun completeMatch' (RULE(p,e)) =
    completeMatch'' (fn marker => RULE(p,marker e))

fun completeMatch(env,exnName: string) =
    completeMatch''
      (fn marker =>
          RULE(WILDpat,
	       marker(RAISEexp(CONexp(CoreAccess.getExn env [exnName],[]),
			       UNDEFty))))
(** Updated to the ty option type - GK *)

val trivialCompleteMatch = completeMatch(SE.empty,"Match")

val TUPLEpat = AbsynUtil.TUPLEpat

fun wrapRECdecGen (rvbs, compInfo as {mkLvar=mkv, ...} : compInfo) =
  let fun g (RVB{var=v as VALvar{path=SP.SPATH [sym], ...}, ...}, nvars) =
          let val nv = newVALvar(sym, mkv)
          in ((v, nv, sym)::nvars)
          end
	| g _ = bug "wrapRECdecGen:RVB"
      val vars = foldr g [] rvbs
      val odec = VALRECdec rvbs

      val tyvars =
        case rvbs
         of (RVB{tyvars,...})::_ => tyvars
          | _ => bug "unexpected empty rvbs list in wrapRECdecGen"

   in (vars,
       case vars
        of [(v, nv, sym)] =>
            (VALdec [VB{pat=VARpat nv, boundtvs=[], tyvars=tyvars,
                        exp=LETexp(odec, VARexp(ref v, []))}])
         | _ =>
          (let val vs = map (fn (v, _, _) => VARexp(ref v, [])) vars
               val rootv = newVALvar(internalSym, mkv)
               val rvexp = VARexp(ref rootv, [])
               val nvdec =
                 VALdec([VB{pat=VARpat rootv, boundtvs=[], tyvars=tyvars,
                            exp=LETexp(odec, TUPLEexp vs)}])

               fun h([], _, d) =
                     LOCALdec(nvdec, SEQdec(rev d))
                 | h((_,nv,_)::r, i, d) =
                     let val nvb = VB{pat=VARpat nv, boundtvs=[],
                                      exp=TPSELexp(rvexp,i),tyvars=ref []}
                      in h(r, i+1, VALdec([nvb])::d)
                     end
            in h(vars, 1, [])
           end))
  end

fun wrapRECdec0 (rvbs, compInfo) =
  let val (vars, ndec) = wrapRECdecGen(rvbs, compInfo)
   in case vars
       of [(_, nv, _)] => (nv, ndec)
        | _ => bug "unexpected case in wrapRECdec0"
  end

fun wrapRECdec (rvbs, compInfo) =
  let val (vars, ndec) = wrapRECdecGen(rvbs, compInfo)
      fun h((v, nv, sym), env) = SE.bind(sym, B.VALbind nv, env)
      val nenv = foldl h SE.empty vars
   in (ndec, nenv)
  end

val argVarSym = S.varSymbol "arg"

fun cMARKexp (e, r) = if !ElabControl.markabsyn then MARKexp (e, r) else e

fun FUNdec (completeMatch, fbl,
	    compInfo as {mkLvar=mkv,errorMatch,...}: compInfo) =
    let fun fb2rvb ({var, clauses as ({pats,resultty,exp}::_),tyvars,region}) =
	    let fun getvar _ =  newVALvar(argVarSym, mkv)
		val vars = map getvar pats
		fun not1(f,[a]) = a
		  | not1(f,l) = f l
		fun dovar valvar = VARexp(ref(valvar),[])
		fun doclause ({pats,exp,resultty=NONE}) =
			      RULE(not1(TUPLEpat,pats), exp)
		  | doclause ({pats,exp,resultty=SOME ty}) =
			      RULE(not1(TUPLEpat,pats),CONSTRAINTexp(exp,ty))

(*  -- Matthias says: this seems to generate slightly bogus marks:
 *
		val mark =  case (hd clauses, List.last clauses)
	                     of ({exp=MARKexp(_,(a,_)),...},
				 {exp=MARKexp(_,(_,b)),...}) =>
			         (fn e => MARKexp(e,(a,b)))
			      | _ => fn e => e
*)
		fun makeexp [var] =
                      FNexp(completeMatch(map doclause clauses),UNDEFty)
		  | makeexp vars =
                      foldr (fn (w,e) =>
                             FNexp(completeMatch [RULE(VARpat w,(*mark*) e)],
                                   UNDEFty))
				(CASEexp(TUPLEexp(map dovar vars),
					 completeMatch (map doclause clauses),
                                         true))
				vars
	     in RVB {var=var,
		     exp=cMARKexp (makeexp vars, region),
                     boundtvs=[],
		     resultty=NONE,
		     tyvars=tyvars}
	    end
          | fb2rvb _ = bug "FUNdec"
     in wrapRECdec (map fb2rvb fbl, compInfo)
    end

fun makeHANDLEexp(exp, rules, compInfo as {mkLvar=mkv, ...}: compInfo) =
    let val v = newVALvar(exnID, mkv)
        val r = RULE(VARpat v, RAISEexp(VARexp(ref(v),[]),UNDEFty)) (** Updated to the ty option type - GK*)
	val rules = completeMatch' r rules
     in HANDLEexp(exp, (rules,UNDEFty))
    end


(* transform a VarPat into either a variable or a constructor. If we are given
   a long path (>1) then it has to be a constructor. *)

fun pat_id (spath, env, err, compInfo as {mkLvar=mkv, ...}: compInfo) =
    case spath
      of SymPath.SPATH[id] =>
	   ((case LU.lookValSym (env,id,fn _ => raise SE.Unbound)
	       of V.CON c => CONpat(c,[])
	        | _ => VARpat(newVALvar(id,mkv)))
	    handle SE.Unbound => VARpat(newVALvar(id,mkv)))
       | _ =>
	   CONpat((case LU.lookVal (env,spath,err)
		     of V.VAL c =>
			(err COMPLAIN
			  ("variable found where constructor is required: "^
			   SymPath.toString spath)
			  nullErrorBody;
			 (bogusCON,[]))
		      | V.CON c => (c,[]))
		   handle SE.Unbound => bug "unbound untrapped")

fun makeRECORDpat(l,flex,err) =
    RECORDpat{fields=sortRecord(l,err), flex=flex, typ=ref UNDEFty}

fun clean_pat err (CONpat(DATACON{const=false,name,...},_)) =
      (err COMPLAIN ("data constructor "^S.name name^
		     " used without argument in pattern")
         nullErrorBody;
       WILDpat)
  | clean_pat err (p as CONpat(DATACON{lazyp=true,...},_)) =
      APPpat(BT.dollarDcon,[],p) (* LAZY *) (* second argument = nil OK? *)
  | clean_pat err (MARKpat(p,region)) = MARKpat(clean_pat err p, region)
  | clean_pat err p = p

fun pat_to_string WILDpat = "_"
  | pat_to_string (VARpat(VALvar{path,...})) = SP.toString path
  | pat_to_string (CONpat(DATACON{name,...},_)) = S.name name
  | pat_to_string (NUMpat(src, _)) = src
  | pat_to_string (STRINGpat s) = s
  | pat_to_string (CHARpat s) = "#"^s
  | pat_to_string (RECORDpat _) = "<record>"
  | pat_to_string (APPpat _) = "<application>"
  | pat_to_string (CONSTRAINTpat _) = "<constraint pattern>"
  | pat_to_string (LAYEREDpat _) = "<layered pattern>"
  | pat_to_string (VECTORpat _) = "<vector pattern>"
  | pat_to_string (ORpat _) = "<or pattern>"
  | pat_to_string (MARKpat _) = "<marked pattern>"
  | pat_to_string _ = "<illegal pattern>"

fun makeAPPpat err (CONpat(d as DATACON{const=false,lazyp,...},tvs),p) =
      let val p1 = APPpat(d, tvs, p)
       in if lazyp (* LAZY *)
	  then APPpat(BT.dollarDcon, [], p1)
          else p1
      end
  | makeAPPpat err (CONpat(d as DATACON{name,...},_),_) =
      (err COMPLAIN
        ("constant constructor applied to argument in pattern:"
	 ^ S.name name)
         nullErrorBody;
       WILDpat)
  | makeAPPpat err (MARKpat(rator,region),p) =
      MARKpat(makeAPPpat err (rator,p), region)
  | makeAPPpat err (rator,_) =
      (err COMPLAIN (concat["non-constructor applied to argument in pattern: ",
			     pat_to_string rator])
         nullErrorBody;
       WILDpat)

fun makeLAYEREDpat ((x as VARpat _), y, _) = LAYEREDpat(x,y)
  | makeLAYEREDpat ((x as MARKpat(VARpat _, reg)), y, _) = LAYEREDpat(x,y)
  | makeLAYEREDpat (CONSTRAINTpat(x,t), y, err) =
      makeLAYEREDpat(x, CONSTRAINTpat(y,t), err)
  | makeLAYEREDpat (MARKpat(CONSTRAINTpat(x,t),reg), y, err) =
      makeLAYEREDpat(MARKpat(x,reg), CONSTRAINTpat(y,t), err)
  | makeLAYEREDpat (x,y,err) =
      (err COMPLAIN "pattern to left of \"as\" must be variable" nullErrorBody;
       y)

(* checkBoundTyvars: check whether the tyvars appearing in a type (used) are
   bound (as parameters in a type declaration) *)
fun checkBoundTyvars(used,bound,err) =
    let val boundset =
              foldr (fn (v,s) => TS.union(TS.singleton v,s,err))
	        TS.empty bound
	fun nasty(ref(INSTANTIATED(VARty v))) = nasty v
	  | nasty(ubound as ref(UBOUND _)) =
	     err COMPLAIN ("unbound type variable in type declaration: " ^
			   (PPType.tyvarPrintname ubound))
		 nullErrorBody
	  | nasty _ = bug "checkBoundTyvars"
     in app nasty (TS.elements(TS.diff(used, boundset, err)))
    end

(* labsym : Absyn.numberedLabel -> Symbol.symbol *)
fun labsym (LABEL{name, ...}) = name

exception IsRec

(** FLINT in front end **)
(** formerly defined in translate/nonrec.sml; now done during type checking *)
fun recDecs (rvbs as [RVB {var as V.VALvar{access=A.LVAR v, ...},
                           exp, resultty, tyvars, boundtvs}]) =
     let fun findexp e =
            (case e
              of VARexp (ref(V.VALvar{access=A.LVAR x, ...}), _) =>
                   if v=x then raise IsRec else ()
	       | VARexp _ => ()
               | RECORDexp l => app (fn (lab, x)=>findexp x) l
               | SEQexp l => app findexp l
               | APPexp (a,b) => (findexp a; findexp b)
               | CONSTRAINTexp (x,_) => findexp x
               | HANDLEexp (x, (l, _)) =>
		   (findexp x; app (fn RULE (_, x) => findexp x) l)
               | RAISEexp (x, _) => findexp x
               | LETexp (d, x) => (finddec d; findexp x)
               | CASEexp (x, l, _) =>
                   (findexp x; app (fn RULE (_, x) => findexp x) l)
	       | IFexp { test, thenCase, elseCase } =>
		   (findexp test; findexp thenCase; findexp elseCase)
	       | (ANDALSOexp (e1, e2) | ORELSEexp (e1, e2) |
		  WHILEexp { test = e1, expr = e2 }) =>
		   (findexp e1; findexp e2)
               | FNexp (l, _) =>  app (fn RULE (_, x) => findexp x) l
               | MARKexp (x, _) => findexp x
	       | SELECTexp (_, e) => findexp e
	       | VECTORexp (el, _) => app findexp el
	       | (CONexp _ | NUMexp _ | REALexp _ | STRINGexp _ | CHARexp _) => ())

          and finddec d =
            (case d
              of VALdec vbl => app (fn (VB{exp,...}) => findexp exp) vbl
               | VALRECdec rvbl => app (fn(RVB{exp,...})=>findexp exp) rvbl
               | LOCALdec (a,b) => (finddec a; finddec b)
               | SEQdec l => app finddec l
               | ABSTYPEdec {body, ...} => finddec body
               | MARKdec (dec,_) => finddec dec
               | _ => ())

       in (findexp exp;
           VALdec [VB{pat=VARpat var, tyvars=tyvars, boundtvs=boundtvs,
                      exp = case resultty
                             of SOME ty => CONSTRAINTexp(exp,ty)
                              | NONE => exp}])
          handle IsRec => VALRECdec rvbs
      end

  | recDecs rvbs = VALRECdec rvbs


(* hasModules tests whether there are explicit module declarations in a decl.
 * This is used in elabMod when elaborating LOCALdec as a cheap
 * approximate check of whether a declaration contains any functor
 * declarations. *)
fun hasModules(StrDec _) = true
  | hasModules(FctDec _) = true
  | hasModules(LocalDec(dec_in,dec_out)) =
      hasModules dec_in orelse hasModules dec_out
  | hasModules(SeqDec decs) =
      List.exists hasModules decs
  | hasModules(MarkDec(dec,_)) = hasModules dec
  | hasModules _ = false


end (* top-level local *)
end (* structure ElabUtil *)
