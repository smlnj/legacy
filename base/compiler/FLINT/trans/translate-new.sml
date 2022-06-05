(* translate.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TRANSLATE =
sig

  (* Invariant: transDec always applies to a top-level absyn declaration *)
  val transDec : { rootdec: Absyn.dec,
		   exportLvars: Access.lvar list,
                   oldenv: StaticEnv.staticEnv,
                   env: StaticEnv.staticEnv,
		   cproto_conv: string,
		   compInfo: Absyn.dec CompInfo.compInfo }
                 -> {flint: FLINT.prog,
                     imports: (PersStamps.persstamp
                               * ImportTree.importTree) list}

end (* signature TRANSLATE *)

structure Translate : TRANSLATE =
struct

local structure B  = Bindings
      structure BT = BasicTypes
      structure DA = Access
      structure DI = DebIndex
      structure EM = ErrorMsg
      structure LT = PLambdaType   (* = LtyExtern *)
      structure M  = Modules
      structure MC = MatchComp
      structure PO = Primop
      structure PP = PrettyPrint
      structure PU = PPUtil
      structure S  = Symbol
      structure SP = SymPath
      structure LN = LiteralToNum
      structure TT = TransTypes
      structure TP = Types
      structure TU = TypesUtil
      structure V  = VarCon
      structure EU = ElabUtil
      structure Tgt = Target

      structure IIMap = RedBlackMapFn (type ord_key = IntInf.int
					val compare = IntInf.compare)

      open Absyn PLambda TransUtil
in

(****************************************************************************
 *                   DEBUGGING AND PRETTYPRINTING                           *
 ****************************************************************************)

val debugging = FLINT_Control.trdebugging
fun bug msg = EM.impossible("Translate: " ^ msg)
val say = Control.Print.say
fun warn s = say ("*** WARNING: " ^ s ^ "\n")

fun debugmsg (msg : string) =
    if !debugging then (say msg; say "\n") else ()

val ppDepth = Control.Print.printDepth

val with_pp = PP.with_default_pp

fun ppType ty =
    ElabDebug.withInternals
     (fn () => ElabDebug.debugPrint debugging
		("type: ",PPType.ppType StaticEnv.empty, ty))

fun ppLexp lexp =
    PP.with_default_pp(fn s => PPLexp.ppLexp 20 s lexp)


(****************************************************************************
 *                   TRANSLATING NUMBER LITERALS                            *
 ****************************************************************************)

(* transNum : Types.ty IntConst.t * TP.ty -> int IntConst.t *)
(* Translates a front-end numeric literal (Types.ty IntConst.t) into a FLINT-style
 * numeric literal representation (int IntCons.t).
 * QUESTION: perhaps we should preserve the size, in the case of
 * word8, for better jump tables? *)
fun transNum ({ival, ty}: TP.ty IntConst.t) : con =
    let fun mkWORD sz = WORDcon{ival = ival, ty = sz}  (* FLINT-style literal *)
	fun mkINT sz  = INTcon{ival = ival, ty = sz}   (* FLINT-style literal *)
	val defaultIntSz = 63 (* = Target.defaultIntSz *)
     in if TU.equalType(ty, BT.intTy)
	  then mkINT defaultIntSz
	else if TU.equalType(ty, BT.int32Ty)
	  then mkINT 32
	else if TU.equalType(ty, BT.int64Ty)
	  then mkINT 64
	else if TU.equalType(ty, BT.intinfTy)
	  then mkINT 0
	else if TU.equalType(ty, BT.wordTy)
	  then mkWORD defaultIntSz
	else if TU.equalType(ty, BT.word8Ty)
	  then mkWORD defaultIntSz  (* or:  mkWORD 8 (if we want accurate char size) *)
	else if TU.equalType(ty, BT.word32Ty)
	  then mkWORD 32
        else if TU.equalType(ty, BT.word64Ty)
          then mkWORD 64
	else bug "transNum"
    end

(****************************************************************************
 *                          MAIN FUNCTION                                   *
 *                                                                          *
 *  val transDec : Absyn.dec * Access.lvar list                             *
 *                 * StaticEnv.staticEnv * CompBasic.compInfo               *
 *                 -> {flint: FLINT.prog,                                   *
 *                     imports: (PersStamps.persstamp                       *
 *                               * ImportTree.importTree) list}             *
 ****************************************************************************)

fun transDec
	{ rootdec, exportLvars, oldenv, env, cproto_conv,
	 compInfo as {errorMatch,error,...}: Absyn.dec CompInfo.compInfo } =
let

(* We take mkLvar from compInfo.  This should answer Zhong's question... *)
(*
(*
 * MAJOR CLEANUP REQUIRED ! The function mkv is currently directly taken
 * from the LambdaVar module; I think it should be taken from the
 * "compInfo". Similarly, should we replace all mkLvar in the backend
 * with the mkv in "compInfo" ? (ZHONG)
 *)
val mkv = LambdaVar.mkLvar
fun mkvN NONE = mkv()
  | mkvN (SOME s) = LambdaVar.namedLvar s
*)

val mkvN = #mkLvar compInfo
fun mkv () = mkvN NONE


(** generate the set of ML-to-FLINT type translation functions *)
val {tpsKnd, tpsTyc, toTyc, toLty, strLty, fctLty} =
    TT.genTT()
fun toTcLt d = (toTyc d, toLty d)

(* toDconLty : DebIndex.depth -> Types.ty -> lty *)
(** translating the typ field in DATACON into lty; constant datacons
    will take ltc_unit as the argument *)
fun toDconLty d ty =
    (case ty
       of TP.POLYty{sign, tyfun=TP.TYFUN{arity, body}} =>
            if BT.isArrowType body then toLty d ty
            else toLty d (TP.POLYty{sign=sign,
				    tyfun=TP.TYFUN{arity=arity,
						   body=BT.-->(BT.unitTy, body)}})
	| _ => if BT.isArrowType ty then toLty d ty
               else toLty d (BT.-->(BT.unitTy, ty)))

fun CON' ((_, DA.REF, lt), ts, e) = APP (PRIM (PO.MAKEREF, lt, ts), e)
  | CON' ((_, DA.SUSP (SOME(DA.LVAR d, _)), lt), ts, e) =
      let val v   = mkv ()
          val fe = FN (v, LT.ltc_tuple [], e)
       in APP(TAPP (VAR d, ts), fe)
      end
  | CON' x = CON x


(* mkDcon : Types.datacon -> Plambda.dataconstr *)
fun mkDcon (TP.DATACON {name, rep, typ, ...}) =
      (name, rep, toDconLty DI.top typ)   (* QUESTION: d = DI.top OK? *)

fun patToConsig (APPpat(dcon,_,_)) = TU.dataconSign dcon
  | patToConsig (CONpat(dcon,_)) = TU.dataconSign dcon
  | patToConsig _ = DA.CNIL

(* patToCon : AS.pat * -> Plambda.con
 * maps a "shallow" pattern to a Plambda.con*)
(* ### REWRITE AND MERGE THESE TWO COMMENTS ### *)
(* How does the fresh lvar introduced in the CONpat and APPpat cases get
 * connected to the rhs expression of rules? Is a lambda-abstraction over
 * the new lvar wrapped around the corresponding rhs?  Or is a Let-binding
 * of the lvar to the argument of the constuctor (unit for constant constructors)
 * introduced somewhere (or implicit in the semantics of SWITCH)?  We have already
 * introduced svars that represent to argument to constructors. *)
    (* The pattern argument is a "shallow" pattern, i.e. a constant or a
     * datacon applied to a variable.
     * How does the fresh lvar introduced in the CONpat case get
     * It doesn't matter since there is nothing for it to bind to in
     * a destructed CONpat.
     * In the old match compiler, fresh lvars created for CONpat and APPpat
     * are mapped by an "environment" from a path identifying the pattern point.
     * Here we get the lvar from the var (VALvar) in the shallow pattern for
     * APPpat and VECLENpat. This variable is already refered to in the corresponding
     * rule rhs. *)
(* patToCon has been moved inside function mkExp to give it access to the tTyc type
 * translation function. *)

(*
 * The following code implements the exception tracking and
 * errormsg reporting.
 *)

local
  val region = ref(0,0)
  val markexn = PRIM(PO.MARKEXN,
		  LT.ltc_parrow(LT.ltc_tuple [LT.ltc_exn, LT.ltc_string],
				LT.ltc_exn), [])
in

fun withRegion loc f x =
  let val r = !region
   in (region := loc; f x before region:=r)
      handle e => (region := r; raise e)
  end

fun mkRaise(x, lt) =
  let val e = if !Control.trackExn
              then APP(markexn, RECORD[x, STRING(errorMatch(!region))])
              else x
   in RAISE(e, lt)
  end

fun complain msg = error (!region) msg
fun repErr msg = complain EM.COMPLAIN msg EM.nullErrorBody
fun repWarn msg = complain EM.WARN msg EM.nullErrorBody
fun repPolyEq () =
    if !Control.polyEqWarn then complain EM.WARN "calling polyEqual" EM.nullErrorBody
    else ()


(** This may shadow previous definition of mkv ... this version reports the
    site of introduction of the lvar *)
fun mkv () = mkvN NONE

end (* markexn-local *)

(***************************************************************************
 *          SHARING AND LIFTING OF STRUCTURE IMPORTS AND ACCESSES          *
 ***************************************************************************)

exception HASHTABLE
type key = int

(** hashtable: hashkey(lvar) ->  accesspath hash * accesspath * resvar *)
type info = (key * int list * lvar)
val hashtable : info list LambdaVar.Tbl.hash_table =
    LambdaVar.Tbl.mkTable(32,HASHTABLE)

(* hashkey : int list -> int *)
fun hashkey l = foldr (fn (x,y) => ((x * 10 + y) mod 1019)) 0 l

(* buildHeader : (lvar = int) -> (lexp -> lexp) *)
(* creates a wrapper function that wraps a nested sequence of let declarations
 * around its argument, where the definiens of each let is nested SELECTS for
 * the accesspath in each info item starting from VAR v for the argument v. *)
fun buildHeader v =
  let val info = LambdaVar.Tbl.lookup hashtable v
      fun h((_, l, w), hdr) =
             let val le = foldl (fn (k,e) => SELECT(k,e)) (VAR v) l
	      in fn e => hdr(LET(w, le, e))
	     end
   in foldr h ident info
  end handle _ => ident

(* bindvar : (lvar = int) * int list * symbol option -> lvar *)
(* the int argument will actually be an lvar (breaking abstraction for lvar) *)
fun bindvar (v, [], _) = v
  | bindvar (v, accesspath, nameOp) =
      let val infolist = (LambdaVar.Tbl.lookup hashtable v) handle _ => []
          val key = hashkey accesspath  (* hash of accesspath *)
          fun look [] =
                let val lvar = mkvN nameOp
                in LambdaVar.Tbl.insert hashtable (v, (key,accesspath,lvar)::infolist);
		   lvar
                end
            | look ((key',accesspath',lvar)::rest) =
                if (key' = key) andalso (accesspath'=accesspath) then lvar else look rest
       in look infolist
      end

datatype pidInfo = ANON of (int * pidInfo) list
                 | NAMED of lvar * lty * (int * pidInfo) list

(* mkPidInfo : lty * int list * symbol option -> pidInfo * lvar *)
fun mkPidInfo (t, l, nameOp) =
  let val v = mkvN nameOp
      fun h [] = NAMED(v, t, [])
        | h (a::r) = ANON [(a, h r)]
   in (h l, v)
  end

fun mergePidInfo (pi, t, l, nameOp) =
  let fun h (z as NAMED(v,_,_), []) = (z, v)
        | h (ANON xl, [])  =
              let val v = mkvN nameOp
               in (NAMED(v, t, xl), v)
              end
        | h (z, a::r) =
              let val (xl, mknode) =
                    case z of ANON c => (c, ANON)
                            | NAMED (v,tt,c) => (c, fn x => NAMED(v,tt,x))

                  fun dump ((np, v), z, y) =
                        let val nz = (a, np)::z
                         in (mknode((rev y) @ nz), v)
                        end

                  fun look ([], y) = dump(mkPidInfo(t, r, nameOp), [], y)
                    | look (u as ((x as (i,pi))::z), y) =
                        if i < a then look(z, x::y)
                        else if i = a then dump(h(pi, r), z, y)
                             else dump(mkPidInfo(t, r, nameOp), u, y)

               in look(xl, [])
              end
   in h(pi, l)
  end (* end of mergePidInfo *)

(** a map that stores information about external references *)
val persmap = ref (PersMap.empty : pidInfo PersMap.map)

(* mkPid : pid * lty * int list * symbol option -> lvar *)
fun mkPid (pid, t, l, nameOp) =
    case PersMap.find (!persmap, pid)
      of NONE =>
	  let val (pinfo, var) = mkPidInfo (t, l, nameOp)
	   in persmap := PersMap.insert(!persmap, pid, pinfo);
	      var
	  end
       | SOME pinfo =>
	  let val (npinfo, var) = mergePidInfo (pinfo, t, l, nameOp)
	      fun rmv (key, map) =
		  let val (newMap, _) = PersMap.remove(map, key)
		  in newMap
		  end handle e => map
	   in persmap := PersMap.insert(rmv(pid, !persmap), pid, npinfo);
	      var
	  end

val iimap = ref (IIMap.empty : lvar IIMap.map)

fun getII n =
    case IIMap.find (!iimap, n) of
	SOME v => v
      | NONE => let val v = mkv ()
		in
		    iimap := IIMap.insert (!iimap, n, v);
		    v
		end

(** converting an access w. type into a lambda expression *)
fun mkAccT (p, t, nameOp) =
  let fun h(DA.LVAR v, l) = bindvar(v, l, nameOp)
        | h(DA.EXTERN pid, l) = mkPid(pid, t, l, nameOp)
        | h(DA.PATH(a,i), l) = h(a, i::l)
        | h _ = bug "unexpected access in mkAccT"
   in VAR (h(p, []))
  end (* new def for mkAccT *)

(** converting an access into a lambda expression *)
fun mkAcc (p, nameOp) =
  let fun h(DA.LVAR v, l) = bindvar(v, l, nameOp)
        | h(DA.PATH(a,i), l) = h(a, i::l)
        | h _ = bug "unexpected access in mkAcc"
   in VAR (h(p, []))
  end (* new def for mkAcc *)

(*
 * These two functions are major gross hacks. The NoCore exceptions would
 * be raised when compiling boot/dummy.sml, boot/assembly.sig, and
 * boot/core.sml; the assumption is that the result of coreExn and coreAcc
 * would never be used when compiling these three files. A good way to
 * clean up this is to put all the core constructors and primitives into
 * the primitive environment. (ZHONG)
 *
 * NOTE: the CoreAccess structure (ElabData/stateenv/coreacc.sml) also
 * defines a NoCore exception, but does not export it.  Does it make
 * sense to combine these things?
 *)
exception NoCore

fun coreExn ids =
    (case CoreAccess.getCon' (fn () => raise NoCore) oldenv ids
      of TP.DATACON { name, rep as DA.EXN _, typ, ... } =>
	   let val nt = toDconLty DI.top typ
	       val nrep = mkRep(rep, nt, name)
	       val _ = debugmsg ">>coreExn in translate.sml: "
              (* val _ = PPLexp.printLexp (CON'((name, nrep, nt), [], unitLexp))
	         val _ = print "\n" *)
            in SOME (CON'((name, nrep, nt), [], unitLexp))
           end
       | _ => bug "coreExn in translate"
      (* end case *))
    handle NoCore => NONE

(* coreAcc : symbol -> lexp *)
and coreAcc id =
    (case CoreAccess.getVar' (fn () => raise NoCore) oldenv [id]
       of V.VALvar { access, typ, path, ... } =>
	    mkAccT(access, toLty DI.top (!typ), getNameOp path)
        | _ => bug "coreAcc in translate"
    (* end case *))
    handle NoCore =>
	(warn(concat["no Core access for '", id, "'\n"]);
	 INT{ival = 0, ty = Tgt.defaultIntSz})

(* mkRep : Access.conrep * lty * S.symbol -> Access.conrep *)
(* expands the flex record pattern and convert the EXN access pat *)
(* internalize the conrep's access, always exceptions *)
and mkRep (rep, lt, name) =
  let fun accessToLvar (DA.LVAR v, l, t)  = bindvar(v, l, SOME name)
        | accessToLvar (DA.PATH(a, i), l, t) = accessToLvar(a, i::l, t)
        | accessToLvar (DA.EXTERN p, l, t) = mkPid(p, t, l, SOME name)
        | accessToLvar _ = bug "unexpected access in mkRep"

   in case rep
       of (DA.EXN access) =>
             let val (argt, _) = LT.ltd_parrow lt
              in DA.EXN (DA.LVAR (accessToLvar(access, [], LT.ltc_etag argt)))
             end
        | (DA.SUSP NONE) =>  (* a hack to support "delay-force" primitives *)
             (case (coreAcc "delay", coreAcc "force")
               of (VAR x, VAR y) => DA.SUSP(SOME (DA.LVAR x, DA.LVAR y))
                | _ => bug "unexpected case on conrep SUSP 1")
        | (DA.SUSP (SOME _)) => bug "unexpected case on conrep SUSP 2"
        | _ => rep
  end

(** converting a value of access into a lambda expression
 ** [KM???} But it is ignoring the prim argument!!!
 ** [DBM: 5/1/07]: I've eliminated the unused prim argument.
 **)
fun mkAccInfo (acc, getLty, nameOp) =
  if extern acc then mkAccT(acc, getLty(), nameOp) else mkAcc (acc, nameOp)

(* fillPat : AS.pat * int -> AS.pat *)
(* (1) fills out flex record patterns according to the known record type, turning them
 *     into nonflex record patterns.  Using WILDpat when restoring the elided fields.
 * (2) uses mkRep to adjust representations for exception constructors and
 *     the SUSP pseudo-constructor *)
fun fillPat(pat, d) =
  let fun fill (CONSTRAINTpat (p,_)) = fill p
	| fill (MARKpat (p,_)) = fill p
        | fill (LAYEREDpat (p,q)) = LAYEREDpat(fill p, fill q)
        | fill (RECORDpat {fields, flex=false, typ}) =
            RECORDpat{fields = map (fn (lab, p) => (lab, fill p)) fields,
                      typ = typ, flex = false}
        | fill (pat as RECORDpat {fields, flex=true, typ}) =
            let val fields' = map (fn (l,p) => (l, fill p)) fields

                fun find (t as TP.CONty(TP.RECORDtyc labels, _)) =
                             (typ := t; labels)
                  | find _ = bug "fillPat found unresolved flex record type"

                fun merge (a as ((id,p)::r), lab::s) =
                      if S.eq(id,lab) then (id,p) :: merge(r,s)
                                      else (lab,WILDpat) :: merge(a,s)
                  | merge ([], lab::s) = (lab,WILDpat) :: merge([], s)
                  | merge ([], []) = []
                  | merge _ = bug "merge in translate"

             in RECORDpat{fields = merge(fields',
                                         find(TU.headReduceType (!typ))),
                          flex = false, typ = typ}
            end
        | fill (VECTORpat(pats,ty)) = VECTORpat(map fill pats, ty)
        | fill (ORpat(p1, p2)) = ORpat(fill p1, fill p2)
        | fill (CONpat(TP.DATACON{name,const,typ,rep,sign,lazyp}, ts)) =
            CONpat(TP.DATACON{name=name, const=const, typ=typ, lazyp=lazyp,
                              sign=sign,rep=mkRep(rep,toDconLty d typ,name)},
                   ts)
        | fill (APPpat(TP.DATACON{name,const,typ,rep,sign,lazyp}, ts, pat)) =
            APPpat(TP.DATACON{name=name, const=const, typ=typ,
                              sign=sign, lazyp=lazyp,
                              rep=mkRep(rep, toDconLty d typ, name)},
                   ts, fill pat)
        | fill xp = xp

   in fill pat
  end (* function fillPat *)

(** The runtime polymorphic equality and string equality dictionary. *)
val eqDict =
  let val strEqRef : lexp option ref = ref NONE
      val polyEqRef : lexp option ref = ref NONE
      val intInfEqRef : lexp option ref = ref NONE

      fun getStrEq () =
        (case (!strEqRef)
          of SOME e => e
           | NONE => (let val e = coreAcc "stringequal"
                       in strEqRef := (SOME e); e
                      end))

      fun getIntInfEq () =		(* same as polyeq, but silent *)
	  case !intInfEqRef of
	      SOME e => e
	    | NONE => let val e =
			      TAPP (coreAcc "polyequal",
				    [toTyc DI.top BT.intinfTy])
		      in
			  intInfEqRef := SOME e; e
		      end

      fun getPolyEq () =
        (repPolyEq();
	 case (!polyEqRef)
          of SOME e => e
           | NONE => (let val e = coreAcc "polyequal"
                       in polyEqRef := (SOME e); e
                      end))
   in {getStrEq=getStrEq, getIntInfEq=getIntInfEq, getPolyEq=getPolyEq}
  end

val eqGen = PEqual.equal (eqDict, env)

val boolsign = BT.boolsign
val (trueDcon', falseDcon') =
  let val lt = LT.ltc_parrow(LT.ltc_unit, LT.ltc_bool)
      fun h (TP.DATACON{name,rep,typ,...}) = (name, rep, lt)
   in (h BT.trueDcon, h BT.falseDcon)
  end

val trueLexp = CON(trueDcon', [], unitLexp)
val falseLexp = CON(falseDcon', [], unitLexp)

fun COND(a,b,c) =
  SWITCH(a,boolsign, [(DATAcon(trueDcon', [], mkv()),b),
                      (DATAcon(falseDcon', [], mkv()),c)], NONE)

fun composeNOT (eq, t) =
  let val v = mkv()
      val argt = LT.ltc_tuple [t, t]
   in FN(v, argt, COND(APP(eq, VAR v), falseLexp, trueLexp))
  end

val lt_unit = LT.ltc_unit
val lt_u_u = LT.ltc_parrow (lt_unit, lt_unit)

(* translation of prim ops *)
val transPrim =
    TransPrim.trans {coreAcc = coreAcc, coreExn = coreExn, mkv = mkv,
		     mkRaise = mkRaise}

(* genintinfswitch : var * (con * exp) list * lexp -> lexp *)
(* generates PLambda.lexp code for a case over an IntInf.int value. *)
(* where does this belong?  At what level should it be coded?  To Absyn? *)
(* This belongs in trans/translate.sml. It's input should be an Absyn CASEexp
 * matching against IntInf.int constant keys.
 * E.g. key0 = {ival, ty=IntInf.int}: Types.ty IntConst.t
 * translate.sml has to recognize this special form of a shallow Case. *)
fun genintinfswitch (sv: LambdaVar.lvar, cases, default) =
    let (* build a chain of equality tests for checking large pattern values *)
	fun build [] = default
	  | build ((n, e) :: r) =
	      COND (APP (#getIntInfEq eqDict (), RECORD [VAR sv, VAR (getII n)]),
		    e, build r)
	(* make a small int constant pattern *)
	fun mkSmall n = INTcon{ival = IntInf.fromInt n, ty = Tgt.defaultIntSz}
	(* split pattern values into small values and large values;
	 * small values can be handled directly using SWITCH *)
	fun split ([], s, l) = (rev s, rev l)
	  | split ((n, e) :: r, sm, lg) =
	      (case LN.lowVal n
		 of SOME l => split (r, (mkSmall l, e) :: sm, lg)
		  | NONE => split (r, sm, (n, e) :: lg)
	      (* end case *))
	fun gen () =
	      (case split (cases, [], [])
		 of ([], largeints) => build largeints
		  | (smallints, largeints) =>
		      let val iv = mkv ()
		       in LET (iv, APP (coreAcc "infLowValue", VAR sv),
			       SWITCH (VAR iv, DA.CNIL, smallints, SOME (build largeints)))
		      end
	      (* end case *))
       in gen ()
      end
(* similar special cases for REF and SUSP constructors in pattern.  See genswitch in
 * FLINT/trans/matchcomp.sml. *)


(***************************************************************************
 *                                                                         *
 * Translating various bindings into lambda expressions:                   *
 *                                                                         *
 *   val mkVar : V.var * DI.depth -> L.lexp                                *
 *   val mkVE : V.var * TP.ty list -> L.lexp                                *
 *   val mkCE : T.datacon * TP.ty list * L.lexp option * DI.depth -> L.lexp *
 *   val mkStr : M.Structure * DI.depth -> L.lexp                          *
 *   val mkFct : M.Functor * DI.depth -> L.lexp                            *
 *   val mkBnd : DI.depth -> B.binding -> L.lexp                           *
 *                                                                         *
 ***************************************************************************)
(* [KM???] mkVar is calling mkAccInfo, which just drops the prim!!! *)
fun mkVar (v as V.VALvar{access, prim, btvs, typ, path}, d) =
      mkAccInfo(access, fn () => toLty d (!typ), getNameOp path)
  | mkVar _ = bug "unexpected vars in mkVar"

(* mkVE : V.var * type list * depth -> lexp
 * This translates a variable, which might be bound to a primop.
 * In the case of a primop variable, this function reconstructs the
 * type parameters of instantiation of the intrinsic primop type relative
 * to the variable occurrence type *)
fun mkVE (e as V.VALvar { typ, prim = PrimopId.Prim p, ... }, ts, d) =
      let val occurenceTy = instPoly(!typ, ts)
              (* compute the occurrence type of the variable *)
          val primop = PrimopBind.defnOf p
          val intrinsicType = PrimopBind.typeOf p
	  val _ = debugmsg ">>mkVE: before matchInstTypes"
	  val intrinsicParams =
              (* compute intrinsic instantiation params of intrinsicType *)
              case (TU.matchInstTypes(true, d, occurenceTy, intrinsicType)
                      : (TP.tyvar list * TP.tyvar list) option )
                of SOME(_, tvs) =>
		   (if !debugging then
                      complain EM.WARN
                        "mkVE ->matchInstTypes -> pruneTyvar"
                        (fn ppstrm =>
                          (PP.string ppstrm
                            ("tvs length: " ^ Int.toString (length tvs));
                           PP.newline ppstrm;
                           PPVal.ppDebugVar
                            (fn x => "") ppstrm env e;
                           if (length tvs) = 1
                           then PPType.ppType env ppstrm (TP.VARty (hd tvs))
                           else ()))
                    else ();
                    map TU.pruneTyvar tvs)
                 | NONE =>
                   (ElabDebug.withInternals (fn () => (complain EM.COMPLAIN
                      "mkVE:primop intrinsic type doesn't match occurrence type"
                      (fn ppstrm =>
                          (PP.string ppstrm "VALvar: ";
                           PPVal.ppVar ppstrm e;
                           PP.newline ppstrm;
                           PP.string ppstrm "occtypes: ";
                           PPType.ppType env ppstrm occurenceTy;
                           PP.newline ppstrm;
                           PP.string ppstrm "intrinsicType: ";
                           PPType.ppType env ppstrm intrinsicType;
                           PP.newline ppstrm;
                           PP.string ppstrm "instpoly occ: ";
                           PPType.ppType env ppstrm
                             (#1 (TU.instantiatePoly occurenceTy));
                           PP.newline ppstrm;
                           PP.string ppstrm "instpoly intrinsicType: ";
                           PPType.ppType env ppstrm
                             (#1 (TU.instantiatePoly intrinsicType))));
                    bug "mkVE -- NONE")))
	  val _ = debugmsg "<<mkVE: after matchInstTypes"
       in case (primop, intrinsicParams)
            of (PO.POLYEQL, [t]) => eqGen(intrinsicType, t, toTcLt d)
             | (PO.POLYNEQ, [t]) =>
               composeNOT(eqGen(intrinsicType, t, toTcLt d), toLty d t)
             | (PO.RAW_CCALL NONE, [a, b, c]) =>
               let val i = SOME (CProto.decode cproto_conv
                                   { fun_ty = a, encoding = b })
                           handle CProto.BadEncoding => NONE
               in PRIM (PO.RAW_CCALL i, toLty d intrinsicType,
                        map (toTyc d) intrinsicParams)
               end
             | _ => (** where do these intrinsicType originate?
			A: PrimopBindings *)
		    transPrim(primop, (toLty d intrinsicType),
                              map (toTyc d) intrinsicParams)
      end
  | mkVE (v as V.VALvar{typ, prim = PrimopId.NonPrim, path, ...}, ts, d) =
    (* non primop variable *)
      (if !debugging
       then (print "### mkVE nonprimop\n";
             print (SymPath.toString path); print "\n";
             ppType (!typ); print "\n";
             print "|ts| = "; print (Int.toString(length ts)); print "\n";
             app ppType ts; print "\n")
       else ();
       case ts
         of [] => mkVar (v, d)
          | _ => TAPP(mkVar(v, d), map (toTyc d) ts))
                 (* dbm: when does this second case occur? *)
  | mkVE _ = bug "non VALvar passed to mkVE"


fun mkCE (TP.DATACON{const, rep, name, typ, ...}, ts, apOp, d) =
  let val lt = toDconLty d typ
      val rep' = mkRep(rep, lt, name)
      val dc = (name, rep', lt)
      val ts' = map (toTyc d o TP.VARty) ts
   in if const then CON'(dc, ts', unitLexp)
      else (case apOp
             of SOME le => CON'(dc, ts', le)
              | NONE =>
                 let val (argT, _) = LT.ltd_parrow(LT.lt_pinst(lt, ts'))
                     val v = mkv()
                  in FN(v, argT, CON'(dc, ts', VAR v))
                 end)
  end

fun mkStr (s as M.STR { access, prim, ... }, d) =
    mkAccInfo(access, fn () => strLty(s, d, compInfo), NONE)
  | mkStr _ = bug "unexpected structures in mkStr"

fun mkFct (f as M.FCT { access, prim, ... }, d) =
    mkAccInfo(access, fn () => fctLty(f, d, compInfo), NONE)
  | mkFct _ = bug "unexpected functors in mkFct"

fun mkBnd d =
  let fun g (B.VALbind v) = mkVar(v, d)
        | g (B.STRbind s) = mkStr(s, d)
        | g (B.FCTbind f) = mkFct(f, d)
        | g (B.CONbind (TP.DATACON{rep=(DA.EXN acc), name, typ, ...})) =
          let val nt = toDconLty d typ
              val (argt,_) = LT.ltd_parrow nt
          in mkAccT (acc, LT.ltc_etag argt, SOME name)
          end
        | g _ = bug "unexpected bindings in mkBnd"
   in g
  end


(***************************************************************************
 *                                                                         *
 * Translating core absyn declarations into lambda expressions:            *
 *                                                                         *
 *    val mkVBs  : Absyn.vb list * depth -> PLambda.lexp -> PLambda.lexp   *
 *    val mkRVBs : Absyn.rvb list * depth -> PLambda.lexp -> PLambda.lexp  *
 *    val mkEBs  : Absyn.eb list * depth -> PLambda.lexp -> PLambda.lexp   *
 *                                                                         *
 * mkVBs(vbs,d) produces a function taking a "body" or "scope" lexp.       *
 * Top-level variable binding have special handling specified at the end   *
 * of the main translate function, transDec.                               *
 ***************************************************************************)

(* mkPE : Absyn.exp * depth * Types.tyvar list -> PLambda.lexp
 * translate an expression with potential type parameters *)
(* The boundtvs (and there instantiation as LBOUNDs is done in the
 * type checker (generalizePat). We will check here (checkBoundtvs) to make
 * sure that was done as expected. This check can be removed after testing. *)
fun mkPE (exp, d, []) = mkExp(exp, d)
    (* exp is not polymorphic, hence no polymorphic abstraction around exp *)
  | mkPE (exp, d, boundtvs) =
    (* ASSERT: d = depth field of all elements of boundtvs. The index fields
     *   of the boundtvs should run from 0 to length(boundtvs)-1. *)
      let

	  (* val savedtvs = map ! boundtvs
             * save original contents of boundtvs for later restoration
             * by the restore function below  -- omit this *)

          (* LBOUND equality property probably does not matter at this point
           * because typechecking and signature matching already completed.
           * [GK 2/24/08] *)
          fun checkBoundtvs ([], _) = ()
            | checkBoundtvs (ref (TP.OPEN{eq,...})::rest, i) =
	        (* this should not happen since the type variables in
                 * boundtvs should have all been set to LBOUND by the type checker.
	        (tv := TP.LBOUND {depth=d,eq=eq,index=i};
		 checkBoundtvs (i+1, rest)) *)
	        bug "mkPE:checkBoundtvs:OPEN"
            | checkBoundtvs (ref (TP.LBOUND{depth=d',index=i',...})::rest, i) =
              (if d <> d'
	       then (say ("### mkPE.checkBoundtvs: d = "^(Int.toString d)^", d' = "^(Int.toString d')^"\n");
		     bug "mkPE:checkBoundtvs:LBOUND:depth")
               else ();
               if i <> i'
	       then (say ("### mkPE.checkBoundtvs: i = "^(Int.toString i)^
                          ", i' = "^(Int.toString i')^"\n");
		     bug "mkPE:checkBoundtvs:LBOUND:index")
               else ();
	       checkBoundtvs (rest, i+1))
            | checkBoundtvs _ = bug "mkPE:checkBoundtvs:tvkind"

          val _ = checkBoundtvs(0, boundtvs)
              (* check that the elements of boundtvs are of kind LBOUND with the
	       * expeced depth and index. This indicates that the type checker has
               * marked them properly as parameter type variables for the polymorphic exp *)

          val exp' = mkExp(exp, DI.next d)
             (* translate exp with an increase in depth to indicate that the expression is
              * going to be wrapped by a type abstraction (TFN); see body *)

          (* restore tyvar states to that before the translation
          fun restore ([], []) = ()
            | restore (a::r, b::z) = (b := a; restore(r, z))
            | restore _ = bug "unexpected cases in mkPE"

          (* [dbm, 6/22/06] We should not need to restore the original
             contents of the uninstantiated meta type variables?
             Only seems to be necessary if a given tyvar gets generalized
             in two different valbinds. We claim that this does not
             happen (Single Generalization Conjecture) *)

          val _ = restore(savedtvs, boundtvs) *)

          val len = length(boundtvs)

       in TFN(LT.tkc_arg(len), exp')
      end

(* mkVBs  : Absyn.vb list * depth -> PLambda.lexp -> PLambda.lexp *)
(* implicit _body_: lexp parameter, representing the scope of the vb declarations *)
(* FIX: why is the alpha-conversion of the pattern needed? (call of aconvertPat) *)
and mkVBs (vbs, d) =
  let fun mkVB (VB{pat,exp,boundtvs,...}, body) =  (* tyvars field of VB not needed *)
	  case AU.stripPatMarks pat  (* strip out MARKpat's *)
            of (VARpat(V.VALvar{access=DA.LVAR v, ...}) |
                CONSTRAINTpat(VARpat(V.VALvar{access=DA.LVAR v, ...}),_)) =>
                  (* simple variable pattern: No special case needed for primops [dbm: 5/1/07] *)
		  (* ASSERT: boundtvs = !btvs of the unique pattern variable *)
                  LET(v, mkPE(exp, d, boundtvs), body)

              | pat =>
		(* boundtvs is cumulative bound metatyvars for the whole pattern *)
		let val (newpat,oldvars,newvars) = aconvertPat(pat, compInfo)
		      (* this is the only call of aconvertPat; it replaces pattern variables with
		       * new versions with fresh lvar access values. Why is this needed? *)
		    val newVarExps = map (fn v => VARexp(ref v,[])) newvars
		    val rhsTy = BT.tupleTy(map (fn (V.VALvar{typ,...}) => !typ) newvars)
		    val bindRule = RULE(newpat, EU.TUPLEexp(newVarExps))
		    val newexp = CASEexp(exp, [bindRule], false)
				 (* evaluates to tuple of (new) pattern var values *)
 		    (* DBM: FIX. Don't need defaultRule, will be handled by matchComp(...,bindExn). *)

		 in case oldvars
		     of [] => (* variable-free pattern, implies boundtvs = [], hence no type abstraction *)
			  LET(mkv(), mkExp(newexp, d), body)
		          (* fresh let-bound lvar doesn't occur in body *)
		      | _ =>
			let val newVar = mkv() (* new local variable to be let-bound to newexp *)
			    fun lookup (tv: Types.tyvar, nil) = NONE
			      | lookup (tv, (tv',k)::r) =
				  if tv = tv' then SOME k else lookup tv r
			    fun buildDec([], i, body) = body
			      | buildDec(bvar::rest, i, body) =  (* bvar = the next pat-bound var *)
				let val V.VALvar{access=DA.LVAR(bval_lv),btvs,...} = bvar
				    val btvs = !btvs
				    (* bound metatyvars for bvar;
				       btvs should be a subset of boundtvs *)
				    val btvsArity = length(btvs) (* type abstraction arity of bvar *)
					(* length of boundtvs gives type abstraction arity of newVar *)
				    val defn =
					case (boundtvs, btvs)
					  of ([],[]) => SELECT(i,VAR(newVar))
					   | (_, []) => SELECT(i,TAPP(VAR(newVar),
								      map (fn _ => LT.tcc_void) boundtvs))
					   | _ => (* coordinating type args of newVar and bvar *)
					      let val indices = List.tabulate(btvsArity, (fn x => x))
						    (* 0-based indexes into btvs, the bound type variable
						     * sequence for _this_ bvar *)
						  val tvToIndex = ListPair.zip(btvs,indices)
						  val targs = map (fn tv =>
								      case lookup tv tvToIndex
								        of NONE => LT.tcc_void
									 | SOME k => LT.tcc_var(1,k))
								  boundtvs
					      in TFN(LT.tkc_arg(btvsArity),  (* tuple of "mono" kinds *)
						     SELECT(i,TAPP(VAR(newVar),targs)))
						 (* the polymorphic value bound to the ith pattern var *)
					      end
				 in buildDec(rest,i+1,LET(bvar_lv, defn, body))
				end

			   in LET(newVar,mkPE(newexp,d,boundtvs),
				  buildDec(oldvars, 0, body))
			  end
		end

   in fold mkVB vbs
	(* missing fold(r) argument is the _init_ arg representing the _body_. type is lexp -> lexp *)
  end (* mkVBs *)

and mkRVBs (rvbs, d) =
  let fun mkRVB (RVB{var=V.VALvar{access=DA.LVAR v, typ=ref ty, ...},
                     exp, boundtvs=btvs, ...}, (vlist, tlist, elist)) =
            let val ee = mkExp(exp, d) (* was mkPE(exp, d, btvs) *)
                (* [ZHONG?] we no longer track type bindings at RVB anymore ! *)
                val vt = toLty d ty
            in (v::vlist, vt::tlist, ee::elist)
            end
        | mkRVB _ = bug "unexpected valrec bindings in mkRVBs"

      val (vlist, tlist, elist) = foldr mkRVB ([], [], []) rvbs

   in fn b => FIX(vlist, tlist, elist, b)
  end

and mkEBs (ebs, d) =
  let fun g (EBgen {exn=TP.DATACON{rep=DA.EXN(DA.LVAR v), typ, ...},
                    ident, ...}, b) =
              let val nt = toDconLty d typ
                  val (argt, _) = LT.ltd_parrow nt
               in LET(v, ETAG(mkExp(ident, d), argt), b)
              end
        | g (EBdef {exn=TP.DATACON{rep=DA.EXN(DA.LVAR v), typ, name, ...},
                    edef=TP.DATACON{rep=DA.EXN(acc), ...}}, b) =
              let val nt = toDconLty d typ
                  val (argt, _) = LT.ltd_parrow nt
               in LET(v, mkAccT(acc, LT.ltc_etag argt, SOME name), b)
              end
        | g _ = bug "unexpected exn bindings in mkEBs"

   in fold g ebs
  end


(***************************************************************************
 *                                                                         *
 * Translating module exprs and decls into lambda expressions:             *
 *                                                                         *
 *    val mkStrexp : Absyn.strexp * depth -> PLambda.lexp                   *
 *    val mkFctexp : Absyn.fctexp * depth -> PLambda.lexp                   *
 *    val mkStrbs  : Absyn.strb list * depth -> PLambda.lexp -> PLambda.lexp *
 *    val mkFctbs  : Absyn.fctb list * depth -> PLambda.lexp -> PLambda.lexp *
 *                                                                         *
 ***************************************************************************)
and mkStrexp (se, d) =
  let fun g (VARstr s) = mkStr(s, d)
        | g (STRstr bs) = SRECORD (map (mkBnd d) bs)
        | g (APPstr {oper, arg, argtycs}) =
              let val e1 = mkFct(oper, d)
                  val tycs = map (tpsTyc d) argtycs
                  val e2 = mkStr(arg, d)
               in APP(TAPP(e1, tycs), e2)
              end
        | g (LETstr (dec, b)) = mkDec (dec, d) (g b)
        | g (MARKstr (b, reg)) = withRegion reg g b

   in g se
  end

and mkFctexp (fe, d) =
  let fun g (VARfct f) = mkFct(f, d)
        | g (FCTfct {param as M.STR { access, ... }, argtycs, def }) =
	  (case access of
	       DA.LVAR v =>
               let val knds = map tpsKnd argtycs
                   val nd = DI.next d  (* reflecting type abstraction *)
                   val body = mkStrexp (def, nd)
                   val hdr = buildHeader v
               (* binding of all v's components *)
               in
		   TFN(knds, FN(v, strLty(param, nd, compInfo), hdr body))
               end
	     | _ => bug "mkFctexp: unexpected access")
        | g (LETfct (dec, b)) = mkDec (dec, d) (g b)
        | g (MARKfct (b, reg)) = withRegion reg g b
        | g _ = bug "unexpected functor expressions in mkFctexp"

   in g fe
  end

and mkStrbs (sbs, d) =
  let fun g (STRB{str=M.STR { access, ... }, def, ... }, b) =
	  (case access of
	       DA.LVAR v =>
               let val hdr = buildHeader v
               (* binding of all v's components *)
               in
		   LET(v, mkStrexp(def, d), hdr b)
               end
	     | _ => bug "mkStrbs: unexpected access")
        | g _ = bug "unexpected structure bindings in mkStrbs"
  in fold g sbs
  end

and mkFctbs (fbs, d) =
  let fun g (FCTB{fct=M.FCT { access, ... }, def, ... }, b) =
	  (case access of
	       DA.LVAR v =>
               let val hdr = buildHeader v
               in
		   LET(v, mkFctexp(def, d), hdr b)
               end
	     | _ => bug "mkFctbs: unexpected access")
        | g _ = bug "unexpected functor bindings in mkStrbs"
  in fold g fbs
  end


(***************************************************************************
 * Translating absyn decls and exprs into lambda expression:               *
 *                                                                         *
 *    val mkExp : A.exp * DI.depth -> PLambda.lexp                         *
 *    val mkDec : A.dec * DI.depth -> PLambda.lexp -> PLambda.lexp         *
 *                                                                         *
 ***************************************************************************)
and mkDec (dec, d) =
  let (* mkDec0 : A.dec -> lexp -> lexp *)
      fun mkDec0 (VALdec vbs) = mkVBs(vbs, d)
        | mkDec0 (VALRECdec rvbs) = mkRVBs(rvbs, d)
	| mkDec0 (DOdec exp) = (fn body => LET(mkv(), mkExp(exp, d), body))
        | mkDec0 (ABSTYPEdec{body,...}) = mkDec0 body
        | mkDec0 (EXCEPTIONdec ebs) = mkEBs(ebs, d)
        | mkDec0 (STRdec sbs) = mkStrbs(sbs, d)
        | mkDec0 (FCTdec fbs) = mkFctbs(fbs, d)
        | mkDec0 (LOCALdec(ld, vd)) = (mkDec0 ld) o (mkDec0 vd)
        | mkDec0 (SEQdec ds) =  foldr (op o) ident (map mkDec0 ds)
        | mkDec0 (MARKdec(x, reg)) =
              let val f = withRegion reg mkDec0 x
               in fn y => withRegion reg f y
              end
        | mkDec0 (OPENdec xs) =
              let (* special hack to make the import tree simpler *)
                  fun mkos (_, s as M.STR { access = acc, ... }) =
                      if extern acc then
                          let val _ = mkAccT(acc, strLty(s, d, compInfo), NONE)
                          in ()
                          end
                      else ()
                    | mkos _ = ()
               in app mkos xs; ident
              end
        | mkDec0 _ = ident
   in mkDec0 dec
  end

and mkExp (exp, d) =
  let val tTyc = toTyc d
      val tLty = toLty d

      (* patToCon : AS.pat * -> Plambda.con *)
      (* this function was brought inside mkExp to give it access to tTyc above *)
      fun patToCon pat =
	  (case pat
	     of CONpat (datacon, tvs) =>
		  let val dummyvar = mkv()
			(* fresh lvar to be bound to nonexistent datacon "argument". This
			 * var bindings should be a separate var option component to SWITCH
			 * cases. *)
		      val nts = map (toTyc o TP.VARty) tvs
		   in DATAcon (mkDcon datacon, nts, dummyvar)
		  end
	      | APPpat (datacon, tvs, VARpat(V.VALvar{access=A.LVAR lvar,...})) =>
		  let val nts = map (tTyc o TP.VARty) tvs
		   in DATAcon (mkDcon datacon, nts, lvar)
		  end
	      | VECTORpat(pats, ty) => VLENcon (length pats)
	      | NUMpat (src, lit as {ival,ty}) =>
		  if List.exists (fn ty' => TU.equalType(ty,ty'))
			 [BT.intTy, BT.int32Ty, BT.int64Ty, BT.intinfTy]
		  then INTcon (transNum lit)
		  else WORDcon (transNum lit)
	      | STRINGpat s => STRINGcon s
	    (* end case *))

      fun mkRules rules = map (fn (RULE(p, e)) => (fillPat(p, d), mkExp0 e)) rules

      and mkExp0 (VARexp (ref v, ts)) =
            (debugmsg ">>mkExp VARexp";
	     mkVE(v, map TP.VARty ts, d))
        | mkExp0 (CONexp (dc, ts)) =
	  (let val _ = debugmsg ">>mkExp CONexp: "
	       val c = mkCE(dc, ts, NONE, d)
	       val _ = if !debugging then ppLexp c else ()
	   in c end)
        | mkExp0 (APPexp (CONexp(dc, ts), e2)) =
	  (let val _ = debugmsg ">>mkExp APPexp: "
	       val c = mkCE(dc, ts, SOME(mkExp0 e2), d)
	       val _ = if !debugging then ppLexp c else ()
	   in c end)
        | mkExp0 (NUMexp(src, {ival, ty})) = (
	    debugmsg ">>mkExp NUMexp";
	    if TU.equalType (ty, BT.intTy) then INT{ival = ival, ty = Tgt.defaultIntSz}
	    else if TU.equalType (ty, BT.int32Ty) then INT{ival = ival, ty = 32}
	    else if TU.equalType (ty, BT.int64Ty) then INT{ival = ival, ty = 64}
	    else if TU.equalType (ty, BT.intinfTy) then VAR (getII ival)
	    else if TU.equalType (ty, BT.wordTy) then WORD{ival = ival, ty = Tgt.defaultIntSz}
	  (* NOTE: 8-bit word is promoted to default tagged word representation *)
	    else if TU.equalType (ty, BT.word8Ty) then WORD{ival = ival, ty = Tgt.defaultIntSz}
	    else if TU.equalType (ty, BT.word32Ty) then WORD{ival = ival, ty = 32}
	    else if TU.equalType (ty, BT.word64Ty) then WORD{ival = ival, ty = 64}
	      else (ppType ty; bug "translate NUMexp"))
(* REAL32: handle 32-bit reals *)
        | mkExp0 (REALexp(_, {rval, ty})) = REAL{rval = rval, ty = Tgt.defaultRealSz}
        | mkExp0 (STRINGexp s) = STRING s
(* QUESTION: do we want to map characters to word8? *)
(** NOTE: the following won't work for cross compiling to multi-byte characters **)
        | mkExp0 (CHARexp s) =
	    INT{ival = IntInf.fromInt(Char.ord(String.sub(s, 0))),
		ty = Tgt.defaultIntSz}

        | mkExp0 (RECORDexp []) = unitLexp
        | mkExp0 (RECORDexp xs) =
            if sorted xs then RECORD (map (fn (_,e) => mkExp0 e) xs)
            else let val vars = map (fn (l,e) => (l,(mkExp0 e, mkv()))) xs
                     fun bind ((_,(e,v)),x) = LET(v,e,x)
                     val bexp = map (fn (_,(_,v)) => VAR v) (sortrec vars)
                  in foldr bind (RECORD bexp) vars
                 end

        | mkExp0 (SELECTexp(var,i,true)) =  (* record selection *)
	    (case V.varAccess var
	      of Access.LVAR lvar => SELECT(i, VAR lvar)
	       | _ => bug "mkExp0:SELECTexp:RECORD")

        | mkExp0 (VECTORexp ([], ty)) =
             TAPP(coreAcc "vector0", [tTyc ty])
        | mkExp0 (VECTORexp (xs, ty)) =
             let val tc = tTyc ty
                 val vars = map (fn e => (mkExp0 e, mkv())) xs
                 fun bind ((e,v),x) = LET(v, e, x)
                 val bexp = map (fn (_,v) => VAR v) vars
              in foldr bind (VECTOR (bexp, tc)) vars
             end

        | mkExp0 (SELECTexp(var,index,false)) =  (* vector selection *)
	    let val Access.LVAR lvar = V.varAccess var
	        val elemTy = TU.vectorElemTy (V.varType var)
	        val tc = tTyc elemTy
		val lt_sub =
                    let val vecTyc = LT.ltc_vector (LT.ltc_tv 0)
                    in LT.ltc_poly([LT.tkc_mono],
				   [LT.ltc_parrow(LT.ltc_tuple [vecTyc, LT.ltc_int],
						  LT.ltc_tv 0)])
                    end
		val indexLexp = INT{ival = IntInf.fromInt index, ty = Target.defaultIntSz}
	     in APP(PRIM(PO.SUBSCRIPTV, lt_sub, [tc]),
		    RECORD[ VAR lvar, indexLexp ])
            end


        | mkExp0 (SEQexp [e]) = mkExp0 e
        | mkExp0 (SEQexp (e::r)) = LET(mkv(), mkExp0 e, mkExp0 (SEQexp r))

        | mkExp0 (APPexp (e1, e2)) = APP(mkExp0 e1, mkExp0 e2)
        | mkExp0 (MARKexp (e, reg)) = withRegion reg mkExp0 e
        | mkExp0 (CONSTRAINTexp (e,_)) = mkExp0 e

        | mkExp0 (RAISEexp (e, ty)) = mkRaise(mkExp0 e, tLty ty)

        | mkExp0 (HANDLEexp (baseExp, (rules, ty))) =
	    (* ty will always be UNDEFty, because (rules, ty) produced by
             * ElabUtil.makeHANDLEexp. *)
	    (case rules
	       of [RULE(VARpat exnvar, handlerExp)] =>
		    (case V.varAccess exnvar
		       of DA.LVAR exnlvar =>
			    HANDLE (mkExp0 baseExp, FN(exnlvar, tLty ty, mkExp0 handlerExp))
			| _ => bug "mkExp0:HANDLEexp:exnvar:access")
		| _ => bug "mkExp0:HANDLEexp")

        | mkExp0 (FNexp (rules, argty)) =
	     (* ty is the type of the patterns (lhs) of the rules, produced
	      * by the type checker for FNexp *)
             (case rules
	        of [RULE(VARpat paramvar, body)] =>
		     (case V.varAccess paramvar
		        of DA.LVAR paramlvar =>
			     FN (paramlvar, tLty argty, mkExp0 body)
			 | _ => bug "mkExp0:FNexp:paramvar:access")
		    | _ => bug "mkExp0:HANDLEexp")

        (* For SWITCHexp, we translate to SWITCH lexp except for REF and SUSP
         * scrutinees.  NOTE: translate should never see an Absyn.CASEexp.
	 * All CASEexp's are produced by match compilation, never directly
	 * by the elaborator-typechecker (source Absyn CASEexp have been
         * converted to LETexp in typecheck).
         * The pat of each rule will be "shallow", meaning it is a constant
         * (INTpat, ..., CONpat) or an APPpat of the form
         * APPpat(dcon, tvs, VARpat v), where v is an "internal" (svar)
         * variable. The rhs corresponding to an APPpat will use the unique
         * variable in its pattern (if any) to refer to the constructor argument
	 * The special single (pseudo-) constructor pattern cases involving
         * the "ref" and "susp" constructors, and the special case of switching on
         * intinf constants are treated as speical cases. For all other cases,
         * immediately builds a SWITCH.
	 * The defaultOp arg will need to be SOME when the rule pats are not
         * complete (exhaustive) -- we don't add a default rule. *)
	(* ASSERT: length rules > 0, and length rules = 1 in the case of SINGLE
         * dcons, in which case defaultOp = NONE. *)
        | mkExp0 (SWITCHexp (scrutinee: V.var, rules, defaultOp)) =
	     (* non-degenerate case, multiple rules *)
             let val scrutineeLvar =
		     case V.varAccess scrutinee
		       of DA.LVAR lvar => lvar
		        | _ => bug "mkExp0:SWITCHexp:scrutinee"
		 val RULE(pat1, rhs1) = hd rules  (* destruct the first pattern *)
		 val con1 = patToCon pat1
		 val consig = patToConsig pat1
		 val defaultOp' = Option.map mkExp0 defaultOp
		 fun trRule (RULE(pat, rhs)) = (patToCon pat, mkExp0 rhs)
             in case con1
		 of DATAcon((_, DA.REF, lt), ts, lvar) =>
		      (* ref pseudo-constructor, single, hence 1 rule *)
		      LET(lvar,
			  APP (PRIM (Primop.DEREF, LT.lt_swap lt, ts), VAR scrutineeLvar),
			  mkExp0 rhs1)
		  | DATAcon((_, DA.SUSP(SOME(_, DA.LVAR f)), lt), ts, lvar) =>
		      (* susp pseudo-constructor, single, hence 1 rule *)
		      let val v = mkv()
		      in LET(lvar,
			     LET(v, TAPP(VAR f, ts), APP(VAR v, VAR scrutineeLvar)),
			     mkExp0 rhs1)
		      end
		  | INTcon{ty=0, ...} => (* IntInf.int constant *)
		      let fun strip (INTcon{ty=0, ival}, e) = (ival, e)
			    | strip _ = bug "genswitch - INTINFcon"
		      in genintinfswitch (scrutineeLvar, map strip (map trRule rules),
					  Option.valOf defaultOp')
		      end
		  | VLENcon i => (* lvar will be bound to the vector *)
		    let val lengthLvar = mkv()
			val elemtc = tTyc(TU.vectorElemTy(V.varType scrutinee))
			val lt_len = LT.ltc_poly([LT.tkc_mono],
						 [LT.ltc_parrow(LT.ltc_tv 0, LT.ltc_int)])
			val vectortc = LT.tcc_vector elemtc
		     in LET(lengthLvar,
			    APP(PRIM(PO.LENGTH, lt_len, [vectortc]),
				VAR scrutineeLvar),
			    SWITCH(VAR lengthLvar, DA.CNIL, map trRule rules, defaultOp'))
		    end
		  | _ => SWITCH(VAR scrutineeLvar, consig, map trRule rules, defaultOp')
             end

	| mkExp0 (IFexp { test, thenCase, elseCase }) =
	    COND (mkExp0 test, mkExp0 thenCase, mkExp0 elseCase)

	| mkExp0 (ANDALSOexp (e1, e2)) =
	    COND (mkExp0 e1, mkExp0 e2, falseLexp)

	| mkExp0 (ORELSEexp (e1, e2)) =
	    COND (mkExp0 e1, trueLexp, mkExp0 e2)

	| mkExp0 (WHILEexp { test, expr }) =
	    let val fv = mkv ()
		val body =
		    FN (mkv (), lt_unit,
			COND (mkExp0 test,
			      LET (mkv (), mkExp0 expr, APP (VAR fv, unitLexp)),
			      unitLexp))
	    in
		FIX ([fv], [lt_u_u], [body], APP (VAR fv, unitLexp))
	    end

        | mkExp0 (LETexp (dc, e)) = mkDec (dc, d) (mkExp0 e)

        | mkExp0 e =
             EM.impossibleWithBody "untranslateable expression"
              (fn ppstrm => (PP.string ppstrm " expression: ";
                            PPAbsyn.ppExp (env,NONE) ppstrm (e, !ppDepth)))

   in mkExp0 exp
  end

and transIntInf d s =
    (* This is a temporary solution.  Since IntInf literals
     * are created using a core function call, there is
     * no indication within the program that we are really
     * dealing with a constant value that -- in principle --
     * could be subject to such things as constant folding. *)
    let val consexp = CONexp (BT.consDcon, [ref (TP.INSTANTIATED BT.wordTy)])
	fun build [] = CONexp (BT.nilDcon, [ref (TP.INSTANTIATED BT.wordTy)])
	  | build (d :: ds) = let
	      val i = Word.toIntX d
	      in
		APPexp (consexp, EU.TUPLEexp [
		    NUMexp("<lit>", {ival = IntInf.fromInt i, ty = BT.wordTy}),
		    build ds
		  ])
	      end
	fun mkSmallFn s = coreAcc(if LN.isNegative s then "makeSmallNegInf" else "makeSmallPosInf")
	fun mkFn s = coreAcc(if LN.isNegative s then "makeNegInf" else "makePosInf")
	fun small w =
	      APP (mkSmallFn s,
		mkExp (
		  NUMexp("<lit>", {ival = IntInf.fromInt (Word.toIntX w), ty = BT.wordTy}),
		  d))
        in
	  case LN.repDigits s
           of [] => small 0w0
	    | [w] => small w
	    | ws => APP (mkFn s, mkExp (build ws, d))
        end

(* Wrap bindings for IntInf.int literals around body. *)
fun wrapII body = let
    fun one (n, v, b) = LET (v, transIntInf DI.top n, b)
in
    IIMap.foldli one body (!iimap)
end

(* wrapPidInfo: lexp * (pid * pidInfo) list -> lexp * importTree *)
fun wrapPidInfo (body, pidinfos) =
  let val imports =
        let fun p2itree (ANON xl) =
                  ImportTree.ITNODE (map (fn (i,z) => (i, p2itree z)) xl)
              | p2itree (NAMED _) = ImportTree.ITNODE []
         in map (fn (p, pi) => (p, p2itree pi)) pidinfos
        end
(*
      val _ = let val _ = say "\n ****************** \n"
                  val _ = say "\n the current import tree is :\n"
                  fun tree (ImportTree.ITNODE []) = ["\n"]
                    | tree (ImportTree.ITNODE xl) =
                        foldr (fn ((i, x), z) =>
                          let val ts = tree x
                              val u = (Int.toString i)  ^ "   "
                           in (map (fn y => (u ^ y)) ts) @ z
                          end) [] xl
                  fun pp (p, n) =
                    (say ("Pid " ^ (PersStamps.toHex p) ^ "\n");
                     app say (tree n))
               in app pp imports; say "\n ****************** \n"
              end
*)
      val plexp =
        let fun get ((_, ANON xl), z) = foldl get z xl
              | get ((_, u as NAMED (_,t,_)), (n,cs,ts)) =
                  (n+1, (n,u)::cs, t::ts)

            (* get the fringe information *)
            val getp = fn ((_, pi), z) => get((0, pi), z)
            val (finfos, lts) =
              let val (_, fx, lx) = foldl getp (0,[],[]) pidinfos
               in (rev fx, rev lx)
              end

            (* do the selection of all import variables *)
            fun mksel (u, xl, be) =
              let fun g ((i, pi), be) =
                    let val (v, xs) = case pi of ANON z => (mkv(), z)
                                               | NAMED(v,_,z) => (v, z)
                     in LET(v, SELECT(i, u), mksel(VAR v, xs, be))
                    end
               in foldr g be xl
              end
            val impvar = mkv()
            val implty = LT.ltc_str lts
            val nbody = mksel (VAR impvar, finfos, body)
         in FN(impvar, implty, nbody)
        end
   in (plexp, imports)
  end (* function wrapPidInfo *)

(** the list of things being exported from the current compilation unit *)
val exportLexp = SRECORD (map VAR exportLvars)

val _ = debugmsg ">>mkDec"
(** translating the ML absyn into the PLambda expression *)
val body = mkDec (rootdec, DI.top) exportLexp
val _ = debugmsg "<<mkDec"
val _ = if CompInfo.anyErrors compInfo
	then raise EM.Error
	else ()
(** add bindings for intinf constants *)
val body = wrapII body

(** wrapping up the body with the imported variables *)
val (plexp, imports) = wrapPidInfo (body, PersMap.listItemsi (!persmap))

(** type check body (including kind check) **)
val ltyerrors = if !FLINT_Control.checkPLambda
		then ChkPlexp.checkLtyTop(plexp,0)
		else false
val _ = if ltyerrors
        then (print "**** Translate: checkLty failed ****\n";
              with_pp(fn str =>
                (PU.pps str "absyn:"; PP.newline str;
                 ElabDebug.withInternals
                  (fn () => PPAbsyn.ppDec (env,NONE) str (rootdec,1000));
		 PP.newline str;
                 PU.pps str "lexp:"; PP.newline str;
                 PPLexp.ppLexp 25 str plexp));
              complain EM.WARN "checkLty" EM.nullErrorBody;
	     bug "PLambda type check error!")
        else ()


val _ = if !Control.FLINT.print
	  then (say ("\n\n[After Translate" ^ " ...]\n\n"); ppLexp plexp)
	  else ()

(** normalizing the plambda expression into FLINT *)
val flint = let val _ = debugmsg ">>norm"
		val _ = if !debugging
			then complain EM.WARN ">>flintnm" EM.nullErrorBody
			else ()
		val n = FlintNM.norm plexp
		val _ = debugmsg "<<postnorm"
	    in n end

in {flint = flint, imports = imports}
end (* function transDec *)

end (* top-level local *)
end (* structure Translate *)
