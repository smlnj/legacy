(* matchcomp.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature MATCH_COMP =
sig

  type toTcLt = (Types.ty -> PLambdaType.tyc) * (Types.ty -> PLambdaType.lty)

  type genintinfswitch =
       PLambda.lexp * (IntInf.int * PLambda.lexp) list * PLambda.lexp
       -> PLambda.lexp

  val bindCompile :
        StaticEnv.staticEnv * (Absyn.pat * PLambda.lexp) list
          * (PLambda.lexp -> PLambda.lexp) * LambdaVar.lvar * toTcLt
          * ErrorMsg.complainer
	  * genintinfswitch
	-> PLambda.lexp

  val matchCompile :
        StaticEnv.staticEnv * (Absyn.pat * PLambda.lexp) list
          * (PLambda.lexp -> PLambda.lexp) * LambdaVar.lvar * toTcLt
          * ErrorMsg.complainer
	  * genintinfswitch
	-> PLambda.lexp

  val handCompile :
        StaticEnv.staticEnv * (Absyn.pat * PLambda.lexp) list
          * (PLambda.lexp -> PLambda.lexp) * LambdaVar.lvar * toTcLt
          * ErrorMsg.complainer * genintinfswitch
	-> PLambda.lexp

end (* signature MATCH_COMP *)


structure MatchComp : MATCH_COMP =
struct

local structure DA = Access
      structure BT = BasicTypes
      structure LT = PLambdaType
      structure TU = TypesUtil
      structure PO = Primop
      structure MP = PPLexp
      structure EM = ErrorMsg
      structure TP = Types
      structure LN = LiteralToNum
      structure PP = PrettyPrint

      open VarCon Types
      open Absyn PLambda
      open PrettyPrint
      open MCCommon

in

(* utility functions for managing rule lists (type rules) *)
val intersect=SortedList.intersect
val union = SortedList.merge
val setDifference = SortedList.difference
fun member(i,set) = SortedList.member set i

val debugging = Control.MC.debugging
fun bug s = EM.impossible ("MatchComp: " ^ s)
fun say msg = (Control.Print.say msg; Control.Print.flush ())
fun debugsay msg =
    if !debugging then say msg else ()
val pd = Control.Print.printDepth
fun ppLexp le =
    PP.with_default_pp(fn ppstrm => MP.ppLexp (!pd) ppstrm le)
fun ppDectree dt =
    PP.with_default_pp(fn ppstrm => PPMatchComp.ppDectree (!pd) ppstrm dt)

type toTcLt = (ty -> LT.tyc) * (ty -> LT.lty)

type genintinfswitch =
     PLambda.lexp * (IntInf.int * PLambda.lexp) list * PLambda.lexp
     -> PLambda.lexp

type matchRepTy = ((path * pat) list * path list * lvar) list
(* type of matchRep defined in doMatchCompile, previously also referred to as ruleDesc *)
type matchRepsTy = (matchRepTy * (lvar * PLambda.lexp)) list

(*
 * MAJOR CLEANUP REQUIRED ! The function mkv is currently directly taken
 * from the LambdaVar module; I think it should be taken from the
 * "compInfo". Similarly, should we replace all mkLvar in the backend
 * with the mkv in "compInfo" ? (ZHONG)
 *)
val mkv = LambdaVar.mkLvar

(** translating the typ field in DATACON into lty; constant datacons
    will take ltc_unit as the argument *)
fun toDconLty toLty ty =
  (case ty
    of TP.POLYty{sign, tyfun=TYFUN{arity, body}} =>
         if BT.isArrowType body then toLty ty
         else toLty (TP.POLYty{sign=sign,
                               tyfun=TYFUN{arity=arity,
                                           body=BT.-->(BT.unitTy, body)}})
     | _ => if BT.isArrowType ty then toLty ty
            else toLty (BT.-->(BT.unitTy, ty)))

fun numToCon (v, ty) =
    let fun mkWORD sz = WORDpcon{ival = v, ty = sz}
	fun mkINT sz = INTpcon{ival = v, ty = sz}
     in if TU.equalType(ty, BT.intTy)
	  then mkINT Target.defaultIntSz
	else if TU.equalType(ty, BT.int32Ty)
	  then mkINT 32
	else if TU.equalType(ty, BT.int64Ty)
	  then mkINT 64
	else if TU.equalType(ty, BT.intinfTy)
	  then mkINT 0
	else if TU.equalType(ty, BT.wordTy)
	  then mkWORD Target.defaultIntSz
	else if TU.equalType(ty, BT.word8Ty)
(* QUESTION: perhaps we should preserve the size (e.g., in the case of
 * word8) for better jump tables?
 *)
	  then mkWORD Target.defaultIntSz
	else if TU.equalType(ty, BT.word32Ty)
	  then mkWORD 32
        else if TU.equalType(ty, BT.word64Ty)
          then mkWORD 64
	  else bug "numToCon: unrecognized numeric type"
      end

(* default integer pattern constant *)
fun intCon n = INTpcon{ival = IntInf.fromInt n, ty = Target.defaultIntSz}

(* pattern constant for character literal *)
(* QUESTION: perhaps this should be a Word8.word literal? *)
fun charCon s = intCon (Char.ord (String.sub (s, 0)))

(**************************************************************************)

fun allConses (hds, tls) =
      List.concat (map (fn hd => (map (fn tl => hd::tl) tls)) hds)

fun orExpand (ORpat(pat1,pat2)) =
      (orExpand pat1)@(orExpand pat2)
  | orExpand (pat as RECORDpat{fields,...}) =
     map (mkRECORDpat pat) (foldr allConses [nil] (map (orExpand o #2) fields))
  | orExpand (VECTORpat(pats,t)) =
      map (fn p => VECTORpat(p,t)) (foldr allConses [nil] (map orExpand pats))
  | orExpand (APPpat(k,t,pat)) =
      map (fn pat => APPpat(k,t,pat)) (orExpand pat)
  | orExpand (CONSTRAINTpat(pat,_)) =
      orExpand pat
  | orExpand (LAYEREDpat(CONSTRAINTpat(lpat, _), bpat)) =
      orExpand (LAYEREDpat(lpat, bpat))
  | orExpand (LAYEREDpat(lpat, bpat)) =
      map (fn pat => LAYEREDpat(lpat,pat)) (orExpand bpat)
  | orExpand pat = [pat]

(* lookupVar : (valvar * 'value) list -> valvar -> 'value *)
fun lookupVar ((VALvar{path=p2,...}, value)::rest) (v as VALvar{path=p1,...}) =
       if SymPath.equal(p1,p2) then value else lookupVar rest v
  | lookupVar [] _ = bug "lookupVar unbound"
  | lookupVar _ _ = bug "lookupVar unexpected arg"

(* boundVariables : pat -> VarCon.var list *)
fun boundVariables (VARpat v) = [v]
  | boundVariables (CONSTRAINTpat(pat,_)) = boundVariables pat
  | boundVariables (LAYEREDpat(pat1, pat2)) =
      (boundVariables(pat1))@(boundVariables(pat2))
  | boundVariables (APPpat(k,t,pat)) = boundVariables pat
  | boundVariables (RECORDpat{fields,...}) =
      List.concat (map (boundVariables o #2) fields)
  | boundVariables (VECTORpat(pats,_)) = List.concat (map boundVariables pats)
  | boundVariables (ORpat (pat1,_)) = boundVariables pat1
  | boundVariables (MARKpat _) = bug "MARKpat"
  | boundVariables _ = nil

(* patternBindings : pat * path -> (VarCon.var * path) list *)
fun patternBindings (VARpat v, path) = [(v, path)]
  | patternBindings (CONSTRAINTpat(pat,_), path) = patternBindings(pat, path)
  | patternBindings (LAYEREDpat(pat1, pat2), path) =
      patternBindings(pat1, path) @ patternBindings(pat2, path)
  | patternBindings (APPpat(k,t,pat), path) =
      patternBindings(pat, DELTAPATH(DATApcon(k, t), path))
  | patternBindings (RECORDpat{fields,...}, path) =
      let fun doGen(n, nil) = nil
            | doGen(n, (lab,pat)::rest) =
                (patternBindings(pat,PIPATH(n,path))) @ (doGen(n+1,rest))
       in doGen(0, fields)
      end
  | patternBindings (VECTORpat(pats,t), path) =
      let fun doGen(n, nil) = nil
            | doGen(n, pat::rest) =
                (patternBindings(pat,VPIPATH(n,t,path))) @ (doGen(n+1,rest))
       in doGen(0, pats)
      end
  | patternBindings (ORpat _, _) = bug "patternBindings - unexpected OR pattern"
  | patternBindings (MARKpat _, path) = bug "patternBindings - MARKpat"
  | patternBindings _ = nil

fun vartolvar (VALvar{access=DA.LVAR v, typ,...}, toLty) = (v, toLty (!typ))
  | vartolvar _ = bug "vartolvar - bad variable"

(* preProcessPat : toLtyTy -> pat * lexp    (* a rule, with translated rhs *)
 *                         -> matchRepTy * (lvar * lexp) *)
fun preProcessPat toLty (pat, rhs) =
  let val pat = AbsynUtil.stripPatMarks pat
      val bindings = boundVariables pat
      val fname = mkv()

      fun genRHSFun ([], rhs) = FN(mkv(), LT.ltc_unit, rhs)
        | genRHSFun ([v], rhs) =
            let val (argvar,argt) = vartolvar(v, toLty)
             in FN(argvar,argt,rhs)
            end
        | genRHSFun (vl, rhs) =
            let val argvar = mkv()
                fun foo (nil, n) = (rhs,nil)
                  | foo (v::vl, n) =
                      let val (lv,lt) = vartolvar(v, toLty)
                          val (le,tt) = foo(vl,n+1)
                       in (LET(lv, SELECT(n,VAR argvar), le), lt :: tt)
                      end
                val (body,tt) = foo(vl,0)
             in FN(argvar, LT.ltc_tuple tt, body)
            end

      val rhsFun = genRHSFun (bindings, rhs)
      val pats = orExpand pat
      fun expand nil = nil
        | expand (pat::rest) =
            let val newlist = [(ROOTPATH, pat)]
		val pathenv = patternBindings(pat, ROOTPATH)
                val bindingPaths =
		    map (lookupVar pathenv) bindings
             in (newlist, bindingPaths, fname)::(expand rest)
            end
   in (expand pats, (fname, rhsFun))
  end

fun makeAndor (matchRep,err) = let
    fun addBinding (v, rule, AND{bindings, subtrees}) =
	  AND{bindings=(rule,v)::bindings, subtrees=subtrees}
      | addBinding (v, rule, CASE{bindings, sign, cases}) =
	  CASE{bindings=(rule,v)::bindings, cases=cases, sign = sign}
      | addBinding (v, rule, LEAF{bindings}) =
	  LEAF{bindings=(rule,v)::bindings}

    and genAndor (VARpat v, rule) = LEAF {bindings = [(rule, v)]}
      | genAndor (WILDpat, _) = LEAF {bindings = nil}
      | genAndor (CONSTRAINTpat(pat, _), rule) = genAndor(pat, rule)
      | genAndor (LAYEREDpat(CONSTRAINTpat(lpat,_), bpat), rule) =
	  genAndor (LAYEREDpat(lpat, bpat), rule)
      | genAndor (LAYEREDpat(VARpat v, bpat), rule) =
	  addBinding (v, rule, genAndor (bpat, rule))
      | genAndor (NUMpat(_, {ival, ty}), rule) =
	  let val con = numToCon(ival, ty)
	   in CASE {bindings = nil, sign = DA.CNIL, cases = [(con, [rule], nil)]}
	  end
      | genAndor (STRINGpat s, rule) =
	  CASE {bindings = nil, sign = DA.CNIL,	cases = [(STRINGpcon s, [rule], nil)]}

	(*
	 * NOTE: the following won't work for cross compiling
	 *       to multi-byte characters.
	 *)
      | genAndor (CHARpat s, rule) =
	  CASE{bindings = nil, sign = DA.CNIL, cases = [(charCon s, [rule], nil)]}
      | genAndor (RECORDpat{fields,...}, rule) =
	  AND{bindings = nil, subtrees=multiGen(map #2 fields, rule)}
      | genAndor (VECTORpat(pats,t), rule) =
	  CASE {bindings = nil, sign = DA.CNIL,
		cases = [(VLENpcon (length pats, t), [rule],
			  multiGen(pats, rule))]}
      | genAndor (CONpat(k,t), rule) =
	  CASE {bindings = nil, sign = signOfCon k,
		cases = [(DATApcon(k, t), [rule], nil)]}
      | genAndor (APPpat(k,t,pat), rule) =
	  CASE {bindings = nil, sign = signOfCon k,
		cases = [(DATApcon(k,t), [rule], [genAndor(pat, rule)])]}
      | genAndor _ =
	  bug "genandor - unexpected pat arg"

    and multiGen (pats, rule) = map (fn pat => genAndor(pat,rule)) pats

    and mergeAndor (VARpat v, andor, rule) = addBinding (v, rule, andor)
      | mergeAndor (WILDpat, andor, rule) = andor
      | mergeAndor (CONSTRAINTpat(pat, _), andor, rule) =
	  mergeAndor(pat, andor, rule)
      | mergeAndor (LAYEREDpat(CONSTRAINTpat(lpat,_), bpat), andor, rule) =
	  mergeAndor (LAYEREDpat(lpat, bpat), andor, rule)
      | mergeAndor (LAYEREDpat(VARpat v, bpat), andor, rule) =
	  addBinding (v, rule, mergeAndor (bpat, andor, rule))
      | mergeAndor (CONpat(k,t), LEAF{bindings}, rule) =
	  CASE {bindings = nil, sign = signOfCon k,
		cases = [(DATApcon(k,t), [rule], nil)]}
      | mergeAndor (APPpat(k,t,pat), LEAF{bindings}, rule) =
	  CASE {bindings = bindings, sign = signOfCon k,
		cases = [(DATApcon(k,t), [rule], [genAndor(pat, rule)])]}
      | mergeAndor (pat, LEAF{bindings}, rule) =
	  (case genAndor(pat, rule)
	     of CASE{bindings=nil, sign, cases} =>
		  CASE{bindings=bindings, sign=sign, cases=cases}
	      | AND{bindings=nil, subtrees} =>
		  AND{bindings=bindings, subtrees=subtrees}
	      | _ => bug "mergeAndor - genAndor returned bogusly")
      | mergeAndor (NUMpat(_, {ival, ty}), c as CASE{bindings, cases, sign}, rule) =
	  let val pcon = numToCon(ival, ty)
	   in CASE{bindings = bindings, sign = sign,
		  cases = addACase(pcon, [], rule, cases)}
	  end
      | mergeAndor (NUMpat(_, {ival, ty}), c as AND _, rule) =
	  bug "mergeAndor - bad pattern merge: NUMpat AND"
      | mergeAndor (STRINGpat s, CASE{bindings, cases, sign}, rule) =
	  CASE {bindings = bindings, sign=sign,
		cases = addACase(STRINGpcon s, nil, rule, cases)}

      (*
       * NOTE: the following won't work for cross compiling
       * to multi-byte characters
       *)
      | mergeAndor (CHARpat s, CASE{bindings, cases, sign}, rule) =
	  CASE{bindings = bindings, sign=sign,
	       cases = addACase(charCon s, nil, rule, cases)}
      | mergeAndor (RECORDpat{fields,...},
		    AND{bindings, subtrees}, rule) =
	  AND{bindings = bindings,
	      subtrees=multiMerge(map #2 fields, subtrees, rule)}
      | mergeAndor (VECTORpat(pats,t), CASE{bindings, cases, sign}, rule) =
	  CASE {bindings = bindings, sign = sign,
		cases = addACase(VLENpcon(length pats, t),pats,rule,cases)}
      | mergeAndor (CONpat(k,t), CASE{bindings, cases, sign}, rule) =
	  CASE {bindings=bindings, sign=sign,
		cases=addACase(DATApcon(k,t), nil, rule, cases)}
      | mergeAndor (APPpat(k,t,pat), CASE{bindings, cases, sign}, rule) =
	  CASE {bindings=bindings, sign=sign,
		cases=addACase(DATApcon(k,t), [pat], rule, cases)}
      | mergeAndor (CONpat _, AND _, _) =
	  bug "mergeAndor - concrete constructor can't match record"
      | mergeAndor (APPpat _, AND _, _) =
	  bug "mergeAndor - concrete constructor application can't match record"
      | mergeAndor _ =
	  bug "mergeAndor - unexpected arg"

    (* simulate 64-bit words and ints as pairs of 32-bit words *)
    and mergeAndor64 ((hi, lo), c, rule) = let
	  fun p32 w = NUMpat("<lit>", {ival = w, ty = BT.word32Ty})
	  in
	    mergeAndor (AbsynUtil.TUPLEpat [p32 hi, p32 lo], c, rule)
	  end

    and addACase (pcon, pats, rule, nil) =
	  [(pcon, [rule], multiGen(pats, rule))]
      | addACase (pcon, pats, rule,
		 (aCase as (pcon', rules, subtrees))::rest) =
	  if constantEq(pcon, pcon') then
	    (pcon, rule::rules, multiMerge(pats, subtrees, rule))::rest
	  else
	    aCase::(addACase(pcon, pats, rule, rest))

    and multiMerge (nil, nil, rule) = nil
      | multiMerge (pat::pats, subtree::subtrees, rule) =
	 (mergeAndor(pat, subtree, rule))::(multiMerge(pats, subtrees, rule))
      | multiMerge _ = bug "multiMerge - list length mismatch"


    fun mergePatWithAndorList (path, pat, nil, n) =
	  [(path, genAndor(pat, n))]
      | mergePatWithAndorList(path, pat, (path',andor)::rest, n) =
	  if pathEq(path, path') then (path, mergeAndor(pat, andor, n))::rest
	  else (path',andor)::(mergePatWithAndorList(path, pat, rest, n))

    fun genAndorList (nil, n) = bug "genAndorList - no patterns"
      | genAndorList ([(path, pat)], n) = [(path, genAndor(pat, n))]
      | genAndorList ((path, pat)::rest, n) =
	  mergePatWithAndorList(path, pat, genAndorList(rest, n), n)

    fun mergeAndorList (nil, aol, n) = bug "mergeAndorList - no patterns"
      | mergeAndorList ([(path, pat)], aol, n) =
	  mergePatWithAndorList(path, pat, aol, n)
      | mergeAndorList ((path, pat)::rest, aol, n) =
	  mergePatWithAndorList(path, pat, mergeAndorList(rest, aol, n), n)

    fun makeAndor' (nil, n) = bug "makeAndor' - no rules"
      | makeAndor' ([(pats, _, _)], n) =
	  genAndorList (pats, n)
      | makeAndor' ((pats, env, bindings)::rest, n) =
	  mergeAndorList(pats, makeAndor'(rest, n+1), n)

in makeAndor' (matchRep,0) (* handle Foo => raise (Internal 99) *)
end (* fun makeAndor *)

(* addABinding : path * ruleno * decision list -> decision list *)
fun addABinding (path, rule, nil) = [BINDDEC(path, [rule])]
  | addABinding (path, rule, (bind as BINDDEC(path', rules))::rest) =
      if pathEq(path, path') then BINDDEC(path, rule::rules)::rest
      else bind::(addABinding(path, rule, rest))
  | addABinding _ = bug "addABinding - non BINDDEC in binding list"

(* flattenBindings : (ruleno * var) list * path * ruleno list -> decision list *)
fun flattenBindings (nil, path, active) = nil
  | flattenBindings (((rule, v)::rest), path, active) =
      if member(rule, active) then
	addABinding(path, rule, flattenBindings(rest, path,active))
      else
	flattenBindings(rest, path, active)

(* flattenAndor : andor * path * ruleset -> decision list *)
and flattenAndor (AND {bindings, subtrees}, path, active) =
    let val _ = debugsay "flattenAndor:AND\n"
	val btests = flattenBindings(bindings, path, active)
	  fun dotree (n, nil) = nil
	    | dotree (n, subtree::rest) =
		let val othertests = dotree(n + 1, rest)
		 in (flattenAndor(subtree,PIPATH(n,path),active))@othertests
		end
       in btests@(dotree(0, subtrees))
      end
  | flattenAndor (CASE {bindings, cases, sign}, path, active) =
      let val btests = flattenBindings(bindings, path, active)
       in btests@((flattenCases(cases, path, active,sign))::nil)
      end
  | flattenAndor (LEAF {bindings}, path, active) =
      let val btests = flattenBindings(bindings, path, active)
       in btests @ nil
      end

(* flattenACase : andorCase * path * ruleset * ruleset
		  -> pcon * rules * decision list *)
and flattenACase ((VLENpcon(n, t), rules, subtrees), path, active, defaults) =
      let val stillActive = intersect(union(rules, defaults), active)
	  val ruleActive = intersect(rules, active)
	  fun flattenVSubs (n, nil) = nil
	    | flattenVSubs (n, subtree::rest) =
	      (flattenAndor(subtree, VPIPATH(n,t,path), stillActive))
	      @ (flattenVSubs(n + 1, rest))
       in (intCon n, ruleActive, flattenVSubs(0, subtrees))
      end
  | flattenACase ((k as DATApcon (_,t), rules,[subtree]),path,active,defaults) =
      let val stillActive = intersect(union(rules, defaults), active)
	  val ruleActive = intersect(rules, active)
	  val newPath = DELTAPATH(k,path)
       in (k,ruleActive,flattenAndor(subtree,newPath,stillActive))
      end
  | flattenACase ((constant,rules,nil),path,active,defaults) =
      (constant, intersect(rules, active), nil)
  | flattenACase _ =
      bug "flattenACase - unexpected arg"

(* flattenCases : andorCase list * path * ruleset * A.consig -> decision   *)
and flattenCases (cases, path, active, sign) =
  let fun calcDefaults (nil, active) = active
	| calcDefaults ((_,rules,_)::rest, active)  =
	    calcDefaults(rest, setDifference(active, rules))
      val defaults = calcDefaults(cases, active)
      fun doit nil = nil
	| doit (aCase::rest) =
	    ((flattenACase(aCase, path, active, defaults))
	     :: (doit(rest)))
   in case cases
       of (VLENpcon (n,t), _, _)::_ =>
	     CASEDEC(VLENPATH(t, path), sign, doit cases, defaults)
	| cases => CASEDEC(path, sign, doit cases, defaults)
  end

fun bindings (n, l) = case (List.nth(l, n)) of (_,_,x) => x

(* flattenAndors : (path * andor) list * rules -> (path list * decision list) list *)
fun flattenAndors (nil, allrules) = nil
  | flattenAndors ((path, andor)::rest, allrules) =
      ([path], flattenAndor(andor, path, allrules))
        :: (flattenAndors(rest, allrules))

(* removePath : path * path list -> path list *)
fun removePath (path, path1::rest) =
      if pathEq(path, path1) then rest
      else path1::(removePath(path, rest))
  | removePath (path, nil) = nil

(* fireConstraint : path
 *                  * (path list * decision list) list     -- [(needPaths, decisions), ...]
 *                  * decision list                        -- ready list
 *                  * (path list * decision list) list     -- delayed list
 *                  -> decision list * (path list * decision list) list *)
fun fireConstraint (path, (needPaths, decisions)::rest, ready, delayed) =
      (case removePath(path, needPaths)
         of nil => fireConstraint(path, rest, decisions@ready, delayed)
          | x => fireConstraint(path, rest, ready, (x,decisions)::delayed))
  | fireConstraint (path, nil, ready, delayed) =
      (ready, delayed)

exception PickBest

fun relevant (CASEDEC(_,_,_,defaults), rulenum) =
      not (member(rulenum, defaults))
  | relevant (BINDDEC _, _) =
      bug "relevant - unexpected BINDDEC arg"

fun metric (CASEDEC(_,_,cases, defaults)) = (length defaults, length cases)
  | metric (BINDDEC _) = bug "metric - unexpected BINDDEC arg"

fun metricBetter((a:int,b:int),(c,d)) = a < c orelse (a = c andalso b < d)

fun doPickBest(nil, _, _, _, NONE) = raise PickBest
  | doPickBest(nil, _, _, _, SOME n) = n
  | doPickBest((BINDDEC _)::rest, _, n, _, _) = n
  | doPickBest((CASEDEC(_, DA.CSIG(1,0), _, _))::rest, _, n, _, _) = n
  | doPickBest((CASEDEC(_, DA.CSIG(0,1), _, _))::rest, _, n, _, _) = n
  | doPickBest(aCase::rest, active as act1::_, n, NONE, NONE) =
	  if relevant (aCase, act1) then
	    doPickBest(rest, active, n + 1, SOME(metric aCase), SOME n)
	  else
	    doPickBest(rest, active, n + 1, NONE, NONE)
  | doPickBest(aCase::rest, active as act1::_, n, SOME m, SOME i) =
	  if relevant (aCase, act1) then
	    let val myMetric = metric aCase
	    in
	      if metricBetter(myMetric, m) then
            doPickBest(rest, active, n + 1, SOME(myMetric), SOME n)
	      else
            doPickBest(rest, active, n + 1, SOME m, SOME i)
        end
	  else
	    doPickBest(rest, active, n + 1, SOME m, SOME i)
  | doPickBest _ = bug "doPickBest - unexpected arg"

(* pickBest : decision list * rules -> ruleno *)
fun pickBest (l, active) = doPickBest(l, active, 0, NONE, NONE)

(* extractNth : int * 'a list -> 'a * 'a list *)
fun extractNth(0, a::b) = (a, b)
  | extractNth(n, a::b) =
      let val (c,d) = extractNth(n - 1, b) in (c, a::d) end
  | extractNth _ = bug "extractNth - n too big"

(* genDecisionTree : (decision list * (path list * decision list) list) * rules
 *                   -> dectree *)
fun genDecisionTree((decisions, delayed), active as active1::_) =
      ((case extractNth(pickBest(decisions, active), decisions)
         of (BINDDEC(path, _), rest) =>
	          genDecisionTree(fireConstraint(path,delayed,rest,nil),active)
          | (CASEDEC(path, sign, cases, defaults), rest) =>
             let fun isActive(_,rules,_) = intersect(rules, active) <> []
                 val activeCases = List.filter isActive cases
                 val caseTrees =
                   gencases(activeCases, rest, delayed, defaults, active)
                 val defActive = intersect(active, defaults)
                 fun len (DA.CSIG(i,j)) = i+j
                   | len (DA.CNIL) = 0
		 val defTree =
                   if length activeCases = len sign then NONE
                   else SOME (genDecisionTree((rest, delayed), defActive))
              in CASETEST(path, sign, caseTrees, defTree)
             end)
       handle PickBest => (RHS active1))
  | genDecisionTree (_,nil) = bug "genDecisionTree - nil active"

and gencases (nil, decs, delayed, defaults, active) = nil
  | gencases ((pcon,rules,guarded)::rest,decs,delayed,defaults,active)=
      let val rActive = intersect(union(defaults, rules), active)
       in (pcon, genDecisionTree((decs@guarded, delayed),rActive))
          :: (gencases(rest,decs,delayed,defaults,active))
      end

local open PrintUtil
      val printDepth = Control.Print.printDepth
in

fun matchPrint (env,rules,unused) ppstrm =
  let fun matchPrint' ([],_,_) = ()
        | matchPrint' ([(pat,_)],_,_) = () (* never print last rule *)
        | matchPrint' ((pat,_)::more,[],_) =
           (PP.string ppstrm "        ";
            PPAbsyn.ppPat env ppstrm (pat,!printDepth);
            PP.string ppstrm " => ...";
            PP.newline ppstrm;
            matchPrint' (more,[],0))
        | matchPrint' ((pat,_)::more,(taglist as (tag::tags)),i) =
           if i = tag then
            (PP.string ppstrm "  -->   ";
             PPAbsyn.ppPat env ppstrm (pat,!printDepth);
             PP.string ppstrm " => ...";
             PP.newline ppstrm;
             matchPrint'(more,tags,i+1))
           else
            (PP.string ppstrm "        ";
             PPAbsyn.ppPat env ppstrm (pat,!printDepth);
             PP.string ppstrm " => ...";
             PP.newline ppstrm;
             matchPrint'(more,taglist,i+1))
   in PP.newline ppstrm;
      PP.openHVBox ppstrm (PP.Rel 0);
      matchPrint'(rules,unused,0);
      PP.closeBox ppstrm
  end

fun bindPrint (env,(pat,_)::_) ppstrm =
      (PP.newline ppstrm; PP.string ppstrm "        ";
       PPAbsyn.ppPat env ppstrm (pat,!printDepth);
       PP.string ppstrm " = ...")
  | bindPrint _ _ = bug "bindPrint -- unexpected args"

end (* local printutil *)

(* rulesUsed : dectree -> rules
 *  returns all rules used in the dectree, maintaining ordering
 *  (because union operation does) *)
fun rulesUsed (RHS n) = [n]
  | rulesUsed (BIND(_, dt)) = rulesUsed dt
  | rulesUsed (CASETEST(_, _, cases, NONE)) =
      foldr (fn((_,a), b) => union(rulesUsed a, b)) nil cases
  | rulesUsed (CASETEST(_, _, cases, SOME dt)) =
      foldr (fn((_,a), b) => union(rulesUsed a, b)) (rulesUsed dt) cases

(* fixupUnused : rules * matchRepsTy -> rules *)
(* this code is buggy - the elements of mr aren't what it thinks they are *)
fun fixupUnused (unused: ruleset, mr: matchRepsTy) =
    let fun fixup (nil, _, _, _, out) = out
	  | fixup (unused, (nil, _)::rest, n, m, out) =
	    fixup (unused, rest, n, m + 1, out)
	  | fixup (unused::urest, (rule::rules, x)::mrest, n, m, nil) =
	    if unused = n then
		fixup(urest, (rules, x)::mrest, n + 1, m, [m])
	    else
		fixup(unused::urest, (rules, x)::mrest, n + 1, m, nil)
	  | fixup (unused::urest, (rule::rules, z)::mrest, n, m, x::y) =
	    if unused = n then
		(if m <> x then
		     fixup(urest, (rules, z)::mrest, n + 1, m, m::x::y)
		 else fixup(urest, (rules, z)::mrest, n + 1, m, x::y))
	    else fixup(unused::urest, (rules, z)::mrest, n + 1, m, x::y)
	  | fixup _ = bug "fixup - unexpected arg"
    in rev(fixup(unused, mr, 0, 0, nil))
    end

(* redundant : ruleset * ruleno -> bool
 *  true if rules contains a member not equal to ruleno
 *  i.e. false only if rules = [ruleno]  ??? looks bogus *)
fun redundant (nil, n: int) = false
  | redundant (a::b, n) = a <> n orelse redundant (b, n)

fun complement(n, m, a::b) =
      if n < a then n::(complement(n + 1, m, a::b))
      else complement(n + 1, m, b)
  | complement(n, m, nil) =
      if n < m then n::(complement(n + 1, m, nil)) else nil

type pathList = path list
(* conjectured invariant: a pathList has no duplicate members *)
(* question: does the order of paths in a path list matter? Apparently not. *)

type pathSet = (int * pathList) list
(* terminology: the int component of a pathSet member is called its index.
 * invariant?: index represents common "depth" of paths in pathList, or
 *    in other words, all paths in the pathList have metric = index
 * invariant: indexes occur in (strictly?) ascending order
 * invariant?: no duplicate indexes
 *)

(* dividePathList: (path -> bool) * pathList * pathList * pathList
 *                  -> pathList * pathList
 *   divide path list into a pair of a list of paths satisfying pred and
 *   a list of paths not satisfying pred. *)
fun dividePathList (pred, paths) =
    let fun divide(pred, nil, accyes, accno) = (accyes, accno)
	  | divide(pred, path::rest, accyes, accno) =
	    if pred path then divide(pred, rest, path::accyes, accno)
	    else divide(pred, rest, accyes, path::accno)
    in  divide(pred, paths, nil, nil)
    end

(* addPathToPathList : path * pathList -> pathList
 *  add path to the end of pathList if is not already in pathList *)
fun addPathToPathList (path, paths as path1::rest) =
      if pathEq(path, path1) then paths
      else path1::(addPathToPathList(path, rest))
  | addPathToPathList (path, nil) = [path]

(* unitePathLists : pathList * pathList -> pathList
 *  merge two pathLists, suppressing duplicate paths; the new members of
 *  the first pathlist are added to the end of the 2nd pathList in reverse
 *  order *)
fun unitePathLists(paths1, nil) = paths1
  | unitePathLists(nil, paths2) = paths2
  | unitePathLists(path1::rest1, paths2) =
      addPathToPathList(path1, unitePathLists(rest1, paths2))

(* inPathList : path * pathList -> bool
 *  path is equal (pathEq) to a member of pathList *)
fun inPathList (path1, nil) = false
  | inPathList (path1, path2::rest) =
      pathEq(path1, path2) orelse inPathList(path1, rest)

(* intersectPathLists : pathList * pathList -> pathList
 *  produces pathList containing members of first pathList that
 *  also occur in the second pathList *)
fun intersectPathLists(paths1, nil) = nil
  | intersectPathLists(nil, paths2) = nil
  | intersectPathLists(path1::rest1, paths2) =
      if inPathList(path1,paths2) then
        path1::(intersectPathLists(rest1, paths2))
      else
        intersectPathLists(rest1, paths2)

(* differencPathLists: pathList * pathList -> pathList
 *  sublist of first pathList containing paths that do not occur
 *  in second pathList *)
fun differencePathLists(paths1, nil) = paths1
  | differencePathLists(nil, paths2) = nil
  | differencePathLists(path1::rest1, paths2) =
      if inPathList(path1,paths2) then
        differencePathLists(rest1, paths2)
      else
        path1::(differencePathLists(rest1, paths2))

(* intersectPathsets : pathSet * pathSet -> pathSet
 *   intersection pathSet contains only elements with a common
 *   index, with the corresponding pathList being the intersection
 *   of the respective pathLists
 *)
fun intersectPathsets(pathset1, nil) = nil
  | intersectPathsets(nil, pathset2) = nil
  | intersectPathsets(pathset1 as (n1:int, paths1)::rest1,
                      pathset2 as (n2, paths2)::rest2) =
      if n1 = n2 then
        case intersectPathLists(paths1, paths2)
          of nil => intersectPathsets(rest1, rest2)
           | pl => (n1, pl)::(intersectPathsets(rest1, rest2))
      else if n1 < n2 then
        intersectPathsets(rest1, pathset2)
      else
        intersectPathsets(pathset1, rest2)

(* unitePathsets : pathSet * pathSet -> pathSet
 *  merge two pathSets, consolidating elements with same index
 *  by taking union of corresponding pathLists *)
fun unitePathsets(pathset1, nil) = pathset1
  | unitePathsets(nil, pathset2) = pathset2
  | unitePathsets(pathset1 as (n1:int, paths1)::rest1,
                  pathset2 as (n2, paths2)::rest2) =
      if n1 = n2 then
        (n1, unitePathLists(paths1, paths2))
          :: (unitePathsets(rest1, rest2))
      else if n1 < n2 then
        (n1, paths1)::(unitePathsets(rest1, pathset2))
      else
        (n2, paths2)::(unitePathsets(pathset1, rest2))

(* differencePathsets : pathSet * pathSet -> pathSet
 *  to form the result pathSet:
 *  for each element (n, pl) of pathSet 1, if there is a corresponding
 *  element (n, pl') of pathSet 2 and pl_new = pl - pl' is not nil,
 *  retain the modified element (n, pl_new), otherwise drop the element.
*)
fun differencePathsets(pathset1: pathSet, nil: pathSet) = pathset1
  | differencePathsets(nil, pathset2) = nil
  | differencePathsets(pathset1 as (n1, paths1)::rest1,
                       pathset2 as (n2, paths2)::rest2) =
      if n1 = n2 then
        case differencePathLists(paths1, paths2)
          of nil => differencePathsets(rest1, rest2)
           | pl => (n1, pl)::(differencePathsets(rest1, rest2))
      else if n1 < n2 then
        (n1, paths1)::(differencePathsets(rest1, pathset2))
      else
        differencePathsets(pathset1, rest2)

(* dividePathset : (path -> bool) * pathset -> pathset * pathset
 *  form two pathSets by splitting pathSet elements into two elements
 *  having pathLists that satisfy or don't satisfy the predicate,
 *  dropping new elements if their pathList in nil.  So either or
 *  both of the result pathSets may have fewer elements than the original *)
fun dividePathset(pred, nil) = (nil, nil)
  | dividePathset(pred, (n, paths)::rest) =
      let val (yesSet, noSet) = dividePathset(pred, rest)
      in case dividePathList(pred, paths)
           of (nil, nil) => bug "diviePathset - both empty"
            | (nil, no) => (yesSet, (n,no)::noSet)
            | (yes, nil) => ((n, yes)::yesSet, noSet)
            | (yes, no) => ((n, yes)::yesSet, (n,no)::noSet)
      end

(* pathDepends : path -> path -> bool
 *  is path 1 a "suffix" of path 2  -- need to define "suffix" of a path,
 *  which seems clear when path is "linear" *)
fun pathDepends path1 ROOTPATH = pathEq(path1, ROOTPATH)
  | pathDepends path1 (path2 as PIPATH(_, subpath)) =
      pathEq(path1, path2) orelse pathDepends path1 subpath
  | pathDepends path1 (path2 as VPIPATH(_,_,subpath)) =
      pathEq(path1, path2) orelse pathDepends path1 subpath
  | pathDepends path1 (path2 as DELTAPATH(_,subpath)) =
      pathEq(path1, path2) orelse pathDepends path1 subpath
  | pathDepends path1 (path2 as (VLENPATH (_, subpath))) =
      pathEq(path1, path2) orelse pathDepends path1 subpath

(* pathLength : path -> int
 *  the length of the path - 1  (ROOTPATH ~ nil) *)
fun pathLength ROOTPATH = 0
  | pathLength (PIPATH(_, subpath)) =
      1 + pathLength subpath
  | pathLength (VPIPATH(_,_,subpath)) =
      1 + pathLength subpath
  | pathLength (DELTAPATH(_,subpath)) =
      1 + pathLength subpath
  | pathLength (VLENPATH (_, subpath)) =
      1 + pathLength subpath

(* addPathToPathset : path * pathSet -> pathSet
 *  add, nonredundantly, path to the pathSet element whose index
 *  matches the path metric of path, adding a new element if no index
 *  matches the path metric
 *)
fun addPathToPathset (path, pathset) =
    let fun add(path, len, nil) = [(len, [path])]
	  | add(path, len, (n:int, paths)::rest) =
	    if n = len then (n, addPathToPathList(path, paths))::rest
	    else if n < len then
		(n,paths) :: add(path, len, rest)
	    else (len, [path])::(n, paths)::rest
    in add (path, pathLength path, pathset)
    end

(* wrapBindings : pathSet * dectree -> dectree
 *   wrap the dectree in BINDs, for all the paths in pathSet, first outermost *)
fun wrapBindings (nil, rhs) = rhs
  | wrapBindings ((_,paths)::rest, rhs) =
    let fun bind(nil, rhs) = rhs
          | bind(path::rest, rhs) = BIND(path, bind(rest, rhs))
     in bind(paths, wrapBindings(rest, rhs))
    end

(* subPaths : path -> pathSet
 *  deconstruct a path into a pathSet *)
fun subPaths ROOTPATH = [(0, [ROOTPATH])]
  | subPaths (path as (VLENPATH (_, subpath))) =
      (subPaths subpath)@[(pathLength path, [path])]
  | subPaths (path as VPIPATH (_,_,subpath)) =
      (subPaths subpath)@[(pathLength path, [path])]
  | subPaths (path as PIPATH (_,subpath)) =
      (subPaths subpath)@[(pathLength path, [path])]
  | subPaths (path as DELTAPATH (_,subpath)) =
      (subPaths subpath)@[(pathLength path, [path])]

(* rhsbindings : int * matchRepTy -> pathSet
 *  select the nth rule description, then select its pathList (2nd) component
 *  translate each path into a pathSet, and merge the pathSets
 *)
fun rhsbindings (n, matchRep) =
     let val (_, paths, _) = List.nth(matchRep, n)
      in foldr unitePathsets [] (map subPaths paths)
     end

(* pass1cases : (pcon * dectree) list * pathSet * pathSet option * matchRepTy * path
 *              -> (pcon * dectree) list * pathSet *)
fun pass1cases ((pcon,subtree)::rest, envin, SOME envout, rhs, path) =
        let val (subtree', myEnvout) = pass1(subtree, envin, rhs)
            val (mustBindHere, otherBindings) =
                  dividePathset(pathDepends(DELTAPATH(pcon,path)),myEnvout)
            val envoutSoFar = intersectPathsets(envout, otherBindings)
            val (rest', envout') =
                  pass1cases(rest, envin, SOME envoutSoFar, rhs, path)
            val iBind2 = differencePathsets(otherBindings, envout')
            val subtree'' =
                  wrapBindings(unitePathsets(mustBindHere, iBind2), subtree')
         in ((pcon,subtree'')::rest', envout')
        end
  | pass1cases ((pcon,subtree)::rest, envin, NONE, rhs, path) =
        let val (subtree', myEnvout) = pass1(subtree, envin, rhs)
            val (mustBindHere, otherBindings) =
                  dividePathset(pathDepends(DELTAPATH(pcon,path)),myEnvout)
            val (rest', envout') =
                  pass1cases(rest, envin, SOME otherBindings, rhs, path)
            val iBind2 = differencePathsets(otherBindings, envout')
            val subtree'' =
                  wrapBindings(unitePathsets(mustBindHere, iBind2), subtree')
         in ((pcon,subtree'')::rest', envout')
        end
  | pass1cases (nil, envin, SOME envout, rhs, path) =
        (nil, unitePathsets(envin, envout))
  | pass1cases (nil, envin, NONE, rhs, path) = bug "pass1cases - unexpected arg"

(* pass1 : dectree * pathSet * matchRepTy -> dectree * pathSet *)
and pass1(RHS n, envin, rhs) = (RHS n, rhsbindings(n, rhs))
  | pass1(CASETEST(path, sign, cases, NONE), envin, rhs) =
        let val (cases', envout') =
              pass1cases(cases, unitePathsets(envin, subPaths path),
                         NONE, rhs, path)
         in (CASETEST(path, sign, cases', NONE), envout')
        end
  | pass1(CASETEST(path, sign, cases, SOME subtree), envin, rhs) =
        let val newenv = unitePathsets(envin, subPaths path)
            val (subtree', subEnvout) = pass1(subtree, newenv, rhs)
            val (cases', envout') =
              pass1cases(cases, newenv, SOME subEnvout, rhs, path)
            val subbindings = differencePathsets(subEnvout, envout')
            val subtree'' = wrapBindings(subbindings, subtree')
         in (CASETEST(path, sign, cases', SOME subtree''), envout')
        end
  | pass1 _ = bug "pass1 - unexpected arg"


(* generate : dectree * matchRepTy * lvar * (toTycTy * toLtyTy) * giisTy
 *            -> codeTy
 * Given a decision tree for a match, a matchRep list and the lvar
 * bound to the value to be matched, produce code for the match.
 *)
fun generate (dt, matchRep, rootVar, (toTyc, toLty), giis) =
  let val (subtree, envout) = pass1(dt, [(0, [ROOTPATH])], matchRep)
      fun mkDcon (DATACON {name, rep, typ, ...}) =
            (name, rep, toDconLty toLty typ)
      fun genpath (PIPATH(n, path), env) =
            SELECT(n, VAR(lookupPath(path, env)))
        | genpath (p as DELTAPATH(pcon, path), env) =
            VAR(lookupPath(p, env))
        | genpath (VPIPATH(n, t, path), env) =
	    let val tc = toTyc t
		val lt_sub =
                    let val x = LT.ltc_vector (LT.ltc_tv 0)
                    in LT.ltc_poly([LT.tkc_mono],
				   [LT.ltc_parrow(LT.ltc_tuple [x, LT.ltc_int], LT.ltc_tv 0)])
                    end
	    in APP(PRIM(PO.SUBSCRIPTV, lt_sub, [tc]),
		   RECORD[ VAR(lookupPath(path, env)),
		           INT{ival = IntInf.fromInt n, ty = Target.defaultIntSz} ])
            end
        | genpath (VLENPATH (t, path), env) =
            let val tc = toTyc t
                val lt_len = LT.ltc_poly([LT.tkc_mono],
                                 [LT.ltc_parrow(LT.ltc_tv 0, LT.ltc_int)])
                val argtc = LT.tcc_vector tc
             in APP(PRIM(PO.LENGTH, lt_len, [argtc]),
                    VAR(lookupPath(path, env)))
            end
        | genpath (ROOTPATH, env) = VAR(lookupPath(ROOTPATH, env))

      (* moved to trans/translate.sml for new match compiler *)
      fun genswitch (sv, sign, [(DATAcon((_, DA.REF, lt), ts, x), e)], NONE) =
            LET(x, APP (PRIM (Primop.DEREF, LT.lt_swap lt, ts), sv), e)
        | genswitch (sv, sign, [(DATAcon((_, DA.SUSP(SOME(_, DA.LVAR f)), lt),
                                        ts, x), e)], NONE) =
            let val v = mkv()
             in LET(x, LET(v, TAPP(VAR f, ts), APP(VAR v, sv)), e)
            end
	| genswitch (sv, sign, cases as ((INTcon{ty=0, ...}, _) :: _), default) =
	    let fun strip (INTcon{ty=0, ival}, e) = (ival, e)
		  | strip _ = bug "genswitch - INTINFcon"
	    in
		case default of
		    NONE => bug "getswitch - no default in switch on IntInf"
		  | SOME d => giis (sv, map strip cases, d)
	    end
        | genswitch x = SWITCH x

      fun pass2rhs (n, env, matchRep) =
        (case List.nth(matchRep, n)
          of (_, [path], fname) => APP(VAR fname, VAR(lookupPath(path, env)))
           | (_, paths, fname) =>
               APP(VAR fname,
                 RECORD (map (fn path => VAR(lookupPath(path, env))) paths)))

      fun pass2 (BIND(DELTAPATH _, subtree), env, rhs) =
            pass2(subtree, env, rhs)
            (** we no longer generate explicit DECON anymore, instead,
                we add a binding at each switch case. *)
        | pass2 (BIND(path, subtree), env, rhs) =
            let val newvar = mkv()
                val subcode = pass2(subtree, (path, newvar)::env, rhs)
             in LET(newvar, genpath(path, env), subcode)
            end
        | pass2 (CASETEST(path, sign, [], NONE), _, _) =
            bug "pass2 - empty cases"
        | pass2 (CASETEST(path, sign, [], SOME subtree), env, rhs) =
            pass2(subtree,env,rhs)
        | pass2 (CASETEST(path, sign, cases, dft), env, rhs) =
            let val switchVar = VAR(lookupPath(path, env))
		val switchCases = pass2cases(path,cases,env,rhs)
		val switchDefault = Option.map (fn subtree => pass2(subtree,env,rhs)) dft
             in genswitch(switchVar, sign, switchCases, switchDefault)
            end
        | pass2 (RHS n, env, rhs) = pass2rhs(n, env, rhs)

      and pass2cases (path, nil, env, rhs) = nil
        | pass2cases(path, (pcon,subtree)::rest, env, rhs) =
            let (** always implicitly bind a new variable at each branch. *)
                val (ncon, nenv) = pconToCon(pcon, path, env)
                val res = (ncon, pass2(subtree, nenv, rhs))
             in res::(pass2cases(path, rest, env, rhs))
            end

      (* pconToCon : pcon * path * (path * lvar) list -> lexp * (path * lvar) list *)
      and pconToCon (pcon, path, env) =
	  (case pcon
	     of DATApcon (dc, ts) =>
		  let val newvar = mkv()
		      val nts = map (toTyc o TP.VARty) ts
		      val nenv = (DELTAPATH(pcon, path), newvar)::env
		   in (DATAcon (mkDcon dc, nts, newvar), nenv)
		  end
	      | VLENpcon(i, t) => (VLENcon i, env)
	      | INTpcon i => (INTcon i, env)
	      | WORDpcon w => (WORDcon w, env)
	      | STRINGpcon s => (STRINGcon s, env)
	    (* end case *))

   in case wrapBindings(envout, subtree)
       of BIND(ROOTPATH, subtree') =>
            pass2(subtree', [(ROOTPATH, rootVar)], matchRep)
        | _ => pass2(subtree, [], matchRep)
  end

(* doMatchCompile : (pat * exp) list * (lexp -> lexp?) * lvar * toLcLtTy * errTy * giisTy
                    -> lexp * ruleset * bool * bool ? *)
fun doMatchCompile(absyn_rules, finish, rootvar, toTcLt as (_, toLty), err, giis) =
  let val lastRule = length absyn_rules - 1
      val matchReps = map (preProcessPat toLty) absyn_rules
      val (matchRep,rhsRep) =
          foldr (fn ((a,b),(c,d)) => (a@c,b::d)) ([], []) matchReps
      val allRules = List.tabulate(length matchRep, fn x => x);
          (* length matchRep can be > length absyn_rules if OR pats are expanded
           * (by orExpand in preProcessPat) *)
      val flattened = flattenAndors(makeAndor(matchRep,err),allRules)
      val ready = fireConstraint(ROOTPATH,flattened,nil,nil)
      val dt = genDecisionTree(ready,allRules)
      val _ = PPMatchComp.debugPrint debugging
	       ("#dectree#", fn ppstrm => fn dt => PPMatchComp.ppDectree (!pd) ppstrm dt, dt)
      val numRules = length matchRep
      val rawUnusedRules = complement(0,numRules,rulesUsed dt)
      val unusedRules = fixupUnused(rawUnusedRules,matchReps)
      val exhaustive = member(lastRule,unusedRules)
      val redundantF = redundant(unusedRules, lastRule)

      fun g((fname, fbody), body) = LET(fname, fbody, body)
      val code = foldr g (generate(dt, matchRep, rootvar, toTcLt,giis)) rhsRep

   in (finish(code), unusedRules, redundantF, exhaustive)
  end

(* type as_match = (Absyn.pat * Absyn.exp) list *)

(* noVarsIn : as_match -> bool
 * Test pat, the guard pattern of the first match rule of a match,
 * for the occurence of variables (including layering variables)
 * or wildcards.  Return true if any are present, false otherwise.
 *)
fun noVarsIn ((pat,_)::_) =
      let fun var WILDpat = true (* might want to flag this *)
            | var (VARpat _) = true
            | var (LAYEREDpat _) = true
            | var (CONSTRAINTpat(p,_)) = var p
            | var (APPpat(_,_,p)) = var p
            | var (RECORDpat{fields,...}) = List.exists (var o #2) fields
            | var (VECTORpat(pats,_)) = List.exists var pats
            | var (ORpat (pat1,pat2)) = var pat1 orelse var pat2
            | var _ = false
       in not(var pat)
      end
  | noVarsIn _ = bug "noVarsIn - unexpected arg"


(*
 * The three entry points for the match compiler.
 *
 * They take as arguments an environment (env); a match represented
 * as a list of pattern--lambda expression pairs (match); and a
 * function to use in printing warning messages (warn).
 *
 * env and warn are only used in the printing of diagnostic information.
 *
 * If the control flag Control.MC.printArgs is set, they print match.
 *
 * They call doMatchCompile to actually compile match.
 * This returns a 4-tuple (code, unused, redundant, exhaustive).
 * code is lambda code that implements match.  unused
 * is a list of the indices of the unused rules.  redundant
 * and exhaustive are boolean flags which are set if
 * match is redundant or exhaustive respectively.
 *
 * They print warning messages as appropriate, as described below.
 * If the control flag Control.MC.printRet is set, they print code.
 *
 * They return code.
 *
 * They assume that match has one element for each rule of the match
 * to be compiled, in order, plus a single, additional, final element.
 * This element must have a pattern that is always matched
 * (in practice, it is either a variable or wildcard), and a
 * lambda expression that implements the appropriate behavior
 * for argument values that satisfy none of the guard patterns.
 * A pattern is exhaustive if this dummy rule is never used,
 * and is irredundant if all of the other rules are used.
 *)

local open Control.MC (* make various control flags visible *)
in

(*
 * bindCompile: Entry point for compiling matches induced by val declarations
 * (e.g., val listHead::listTail = list).
 * The match (rules) is a two  element list. The first rule corresponds
 * to the let binding itself, while the second is a default rule
 * (usually "_ => raise Bind") added, e.g. in the function mkVBs in
 * translate.sml, or by applying ElabUtil.completeMatch.
 * Thus the match itself will always be exhaustive, but the case where the
 * let binding per se is nonexhaustive will still be detected by doMatchCompile
 * (see the comment above), and if the control flag Control.MC.bindNonExhaustiveWarn
 * is set then a nonexhaustive binding warning is printed. If the control
 * flag Control.MC.bindNoVariableWarn is set, and the first pattern
 * (i.e., the only non-dummy pattern) of match contains no variables or
 * wildcards, a warning is printed. Arguably, a pattern containing no
 * variables, but one or more wildcards, should also trigger a warning,
 * but this would cause warnings on constructions like
 * val _ = <exp>  and  val _:<ty> = <exp>.
 *)
fun bindCompile (env, rules, finish, rootv, toTcLt, err, giis) =
  let val _ =
        if !printArgs then (say "BC called with:"; MP.ppMatch env rules)
        else ()
      val (code, _, _, exhaustive) =
        doMatchCompile(rules, finish, rootv, toTcLt, err, giis)

      val nonexhaustiveF =
	  not exhaustive andalso
	  (!bindNonExhaustiveWarn orelse !bindNonExhaustiveError)
      val noVarsF = !bindNoVariableWarn andalso noVarsIn rules

   in if nonexhaustiveF
      then err (if !bindNonExhaustiveError then EM.COMPLAIN else EM.WARN)
	       ("binding not exhaustive" ^
	                (if noVarsF then " and contains no variables" else ""))
		       (bindPrint(env,rules))
      else if noVarsF
           then err EM.WARN "binding contains no variables"
                    (bindPrint(env,rules))
           else ();

      if !printRet then
        (say "MC:  returns with\n"; ppLexp code)
      else ();
      code
  end

(*
 * Entry point for compiling matches induced by exception handlers.
 * (e.g., handle Bind => Foo).  If the control flag
 *  Control.MC.matchRedundantWarn is set, and match is redundant,
 *  a warning is printed.  If Control.MC.matchRedundantError is also
 *  set, the warning is promoted to an error message.
 *)
fun handCompile (env, rules, finish, rootv, toTcLt, err, giis) =
  let val _ =
        if !printArgs then (say "HC called with: "; MP.ppMatch env rules)
        else ()
      val (code, unused, redundant, _) =
        doMatchCompile(rules, finish, rootv, toTcLt, err, giis)
      val  redundantF= !matchRedundantWarn andalso redundant

   in if redundantF
      then err
	     (if !matchRedundantError then EM.COMPLAIN else EM.WARN)
	     "redundant patterns in match"
             (matchPrint(env,rules,unused))
      else ();

      if !printRet
      then (say "MC:  returns with\n"; ppLexp code)
      else ();
      code
  end

(*
 * Entry point for compiling matches induced by function expressions
 * (and thus case expression, if-then-else expressions, while expressions
 * and fun declarations) (e.g., fn (x::y) => ([x],y)). If the control flag
 * Control.MC.matchRedundantWarn is set, and match is redundant, a warning
 * is printed; if Control.MC.matchRedundantError is also set, the warning
 * is promoted to an error. If the control flag Control.MC.matchExhaustive
 * is set, and match is nonexhaustive, a warning is printed.
 *)
fun matchCompile (env, rules, finish, rootv, toTcLt, err, giis) =
  let val _ =
        if !printArgs then (say "MC called with: "; MP.ppMatch env rules)
        else ()
      val (code, unused, redundant, exhaustive) =
        doMatchCompile(rules, finish, rootv, toTcLt, err, giis)

      val nonexhaustiveF =
	  not exhaustive andalso
	  (!matchNonExhaustiveError orelse !matchNonExhaustiveWarn)
      val redundantF =
	  redundant andalso (!matchRedundantError orelse !matchRedundantWarn)
   in case (nonexhaustiveF,redundantF)
       of (true, true) =>
            err (if !matchRedundantError orelse !matchNonExhaustiveError
		     then EM.COMPLAIN else EM.WARN)
	        "match redundant and nonexhaustive"
	        (matchPrint(env, rules, unused))

        | (true, false) =>
            err (if !matchNonExhaustiveError then EM.COMPLAIN else EM.WARN)
                "match nonexhaustive"
		(matchPrint(env, rules, unused))

        | (false, true) =>
            err (if !matchRedundantError then EM.COMPLAIN else EM.WARN)
	      "match redundant" (matchPrint(env, rules, unused))

        | _ => ();

      if (!printRet)
      then (say "MatchComp:  returns with\n"; ppLexp code) else ();
      code
  end


val matchCompile =
  Stats.doPhase(Stats.makePhase "Compiler 045 matchcomp") matchCompile

end (* local Control.MC *)

end (* topleve local *)
end (* structure MatchComp *)
