(* typecheck.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TYPECHECK =
sig

  val decType : StaticEnv.staticEnv * Absyn.dec * int * bool
		* ErrorMsg.errorFn * (unit -> bool) * SourceMap.region
                -> Absyn.dec
    (* decType(senv,dec,tdepth,toplev,err,region):
         senv: the context static environment
         dec: the declaration to be type checked
         tdepth: abstraction depth of lambda-bound type variables
         err: error function
         region: source region of dec
     *)
  val debugging : bool ref

end (* signature TYPECHECK *)


(* No longer functorized to factor out dependencies on FLINT (ii2ty, ii_ispure)
 * Instead, TypesUtil depends directly on InlInfo -- it calls InlInfo.isPrimCast
 * to test for the CAST primop in function isValue. *)

structure Typecheck : TYPECHECK =
struct

local open Types VarCon BasicTypes TypesUtil Unify Absyn
	   ErrorMsg PPUtil PPType PPAbsyn

  structure SE = StaticEnv
  structure DI = DebIndex
  structure DA = Access
  structure EU = ElabUtil
  structure ED = ElabDebug
  structure PP = PrettyPrint

in

(* debugging *)
val say = Control_Print.say
val debugging = ElabControl.tcdebugging
fun debugmsg (msg: string) = if !debugging then (say msg; say "\n") else ()
val debugPrint = (fn x => ED.debugPrint debugging x)

fun bug msg = ErrorMsg.impossible("TypeCheck: "^msg)

infix 9 sub
infix -->

val printDepth = Control_Print.printDepth
val showCulprits = ElabControl.showTypeErrorCulprits

fun refNewDcon(DATACON{name,const,rep,typ,sign,lazyp}) =
  DATACON{name=name,const=const,rep=rep,typ=refPatType,sign=sign,lazyp=lazyp}

exception NotThere

fun message(msg,mode: Unify.unifyFail) =
    String.concat[msg," [",Unify.failMessage mode,"]"]

fun mkDummy0 () = BasicTypes.unitTy

(*
 * decType : SE.staticEnv * A.dec * bool * EM.errorFn * region -> A.dec
 *)
fun decType(env,dec,tdepth,toplev,err,anyErrors,region) =
let

(* setup for recording and resolving overloaded variables and literals *)
val { pushv = olv_push, pushl = oll_push, resolve = ol_resolve } = Overload.new ()

val ppType = PPType.ppType env
val ppTycon = PPType.ppTycon env
val ppPat = PPAbsyn.ppPat env
val ppExp = PPAbsyn.ppExp(env,NONE)
val ppRule = PPAbsyn.ppRule(env,NONE)
val ppVB = PPAbsyn.ppVB(env,NONE)
val ppRVB = PPAbsyn.ppRVB(env,NONE)
val ppDec = PPAbsyn.ppDec(env, NONE)

val ppDec' =
  (fn ppstrm => fn d => PPAbsyn.ppDec (env,NONE) ppstrm (d,!printDepth))

fun ppDecDebug (msg,dec) =
  ED.withInternals(fn () => ED.debugPrint debugging (msg, ppDec', dec))

fun ppTypeDebug (msg,ty) =
  ED.withInternals(fn () => ED.debugPrint debugging (msg, ppType, ty))

fun ppTyvarDebug tv =
  ED.withInternals(fn () => debugmsg (PPType.tyvarPrintname tv))

fun ppRegion ppstrm ((l,u): SourceMap.region) =
    (PP.string ppstrm (Int.toString l);
     PP.string ppstrm "-";
     PP.string ppstrm (Int.toString u))

fun ppModeErrorMsg ppstrm (mode: Unify.unifyFail) =
    if !showCulprits then
      (case mode
	of TYC(tyc1,tyc2,reg1,reg2) =>
	   (PP.newline ppstrm;
	    PP.string ppstrm "Mode: tycon mismatch"; PP.newline ppstrm;
	    PP.string ppstrm "tycon1: ";
	    ppTycon ppstrm tyc1; PP.newline ppstrm;
	    PP.string ppstrm "from: "; ppRegion ppstrm reg1; PP.newline ppstrm;
	    PP.string ppstrm "tycon2: ";
	    ppTycon ppstrm tyc2; PP.newline ppstrm;
	    PP.string ppstrm "from: "; ppRegion ppstrm reg2)
	 | TYP(ty1,ty2,reg1,reg2) =>
	   (PP.newline ppstrm;
	    PP.string ppstrm "Mode: type mismatch"; PP.newline ppstrm;
	    PP.string ppstrm "type1: ";
	    ppType ppstrm ty1; PP.newline ppstrm;
	    PP.string ppstrm "from: "; ppRegion ppstrm reg1; PP.newline ppstrm;
	    PP.string ppstrm "type2: ";
	    ppType ppstrm ty2; PP.newline ppstrm;
	    PP.string ppstrm "from: "; ppRegion ppstrm reg2)
	  | _ => ())
    else ()

(* setup for recording FLEX tyvars and checking that they are eventually
 * resolved to exact record types. This is to prevent the leakage of
 * unresolved flex record types into the middle end. *)
val flexTyVars : (tyvar * region) list ref = ref nil

fun registerFlex (x as (tv : tyvar, region: region)) =
    flexTyVars := x :: !flexTyVars

fun checkFlex (): unit =
    let fun check1 (tv,r) =
            (case !tv
               of OPEN{kind=FLEX _,...} =>
                  (err region COMPLAIN
			  "unresolved flex record (hidden)"
		       (fn ppstrm =>
			     (PPType.resetPPType();
			      PP.newline ppstrm;
			      PP.string ppstrm "type: ";
			      ppType ppstrm (VARty(tv)))))
                | INSTANTIATED _ => ()
                | _ => bug "checkFlex")
    in if anyErrors () then ()
       else app check1 (!flexTyVars)
    end

(* managing source locations (srcloc = SourceMap.region) *)

val nullRegion = SourceMap.nullRegion

(* translating a marked type to its origin srcloc *)
(* We need to worry about immediately nested MARKty's, where a wider
 * region is wrapped immediately around a narrower one. Hence the
 * first rule. *)
fun tyToLoc (MARKty(t as MARKty _,region)) = tyToLoc t
  | tyToLoc (MARKty(ty,region)) = region
  | tyToLoc _ = SourceMap.nullRegion

fun unifyErr{ty1,name1,ty2,name2,message=m,region,kind,kindname,phrase} =
    (unifyTy(ty1, ty2, tyToLoc ty1, tyToLoc ty2); true) handle Unify(mode) =>
      (err region COMPLAIN (message(m,mode))
       (fn ppstrm =>
	 (PPType.resetPPType();
	  let val len1 = size name1
	      val len2 = size name2
	      val spaces = "                                   "
	      val pad1 = substring(spaces,0,Int.max(0,len2-len1))
	      val pad2 = substring(spaces,0,Int.max(0,len2-len1))
	      val m = if m = ""
		      then concat[name1, " and ", name2, " do not agree"]
		      else m   (* but name1 and name2 may be "" ! *)
	  in if name1="" then ()
             else (PP.newline ppstrm;
                   PP.string ppstrm (concat[name1, ": ", pad1]);
	           ppType ppstrm ty1);
	     if name2="" then ()
	      else (PP.newline ppstrm;
                    PP.string ppstrm (concat[name2, ": ", pad2]);
		    ppType ppstrm ty2);
	     if kindname="" then ()
	     else (
		PP.newline ppstrm;
		PP.string ppstrm(concat["in ", kindname, ":"]);
		PP.break ppstrm {nsp=1,offset=2};
		kind ppstrm (phrase,!printDepth));
             ppModeErrorMsg ppstrm mode
	 end));
       false)

val _ = debugmsg (">>decType: toplev = " ^ Bool.toString toplev)
val _ = ppDecDebug(">>decType: dec = ",dec)

fun generalizeTy(VALvar{typ,path,btvs,...}, userbound: tyvar list,
		 occ:occ, generalize: bool, region) : tyvar list =
    let val _ = debugmsg (">>>generalizeTy: "^SymPath.toString path)
	val _ = debugmsg ("userbound: ")
        val _ = debugmsg ("generalize: "^Bool.toString generalize)
        val _ = debugmsg ("occ: "^Int.toString(lamdepth occ)^", "^Bool.toString(toplevel occ))
	val _ = List.app ppTyvarDebug userbound

	val failure = ref false
	val mkDummy = if toplevel occ
	              then TypesUtil.dummyTyGen()
		      else mkDummy0 (* shouldn't be called *)

	val index = ref 0  (* counts number of type variables bound *)
	fun next() = !index before (index := !index + 1)
	val sign = ref([]: Types.polysign)
	fun localUbound tv = List.exists (fn tv' => eqTyvar(tv,tv')) userbound

	(* menv: a reference to an association list environment mapping
	 *   generalized tyvars to the corresponding IBOUND type.
	 * ASSERT: there are no duplicate tyvars in domain of menv. *)
	val menv = ref([]: (tyvar*ty) list)
	fun lookup tv =
	    let fun find [] = raise NotThere
		  | find((tv',ty)::rest) = if eqTyvar(tv,tv') then ty
							      else find rest
	     in find(!menv)
	    end
	fun bind(tv,ty) = menv := (tv,ty) :: !menv

	fun gen(ty) =
	    case ty
	     of VARty(ref(INSTANTIATED ty)) => gen ty
	      | VARty(tv as ref(OPEN{depth,eq,kind})) =>
		  (case kind
		     of FLEX[(lab,_)] =>
                         if ((depth > lamdepth occ) andalso
                             (generalize orelse (toplevel occ)))
                            orelse ((toplevel occ) andalso (depth=0))
                         then
			   (err region COMPLAIN (String.concat
			     ["unresolved flex record\n\
			      \   (can't tell what fields there are besides #",
			      Symbol.name lab, ")"])
			     nullErrorBody;
                            tv := INSTANTIATED WILDCARDty;
			    WILDCARDty)
                         else ty
		      | FLEX _ =>
                         if ((depth > lamdepth occ) andalso
                             (generalize orelse (toplevel occ)))
                            orelse ((toplevel occ) andalso (depth=0))
                         then
  			   (err region COMPLAIN
			        "unresolved flex record (need to know the \
			        \names of ALL the fields\n in this context)"
			    (fn ppstrm =>
			       (PPType.resetPPType();
				PP.newline ppstrm;
				PP.string ppstrm "type: ";
				ppType ppstrm ty));
                            tv := INSTANTIATED WILDCARDty;
			    WILDCARDty)
                         else ty
		      | META =>
			  if depth > lamdepth occ
			  then if generalize then
				  lookup tv handle NotThere =>
				    let val new = IBOUND(next())
				     in sign := eq :: !sign;
				        bind(tv,new);
					new
				    end
			       else (if toplevel occ
				     then let val new = mkDummy()
					   in failure := true;
                                              tv := INSTANTIATED new;
					      new
					  end
				     else (if !ElabControl.valueRestrictionLocalWarn
					   then err region WARN
				             ("type variable not generalized\
                                              \ in local decl (value restriction): "
                                              ^ (tyvarPrintname tv))
				             nullErrorBody
					   else ();
					   (* reset depth to prevent later
					      incorrect generalization inside
					      a lambda expression. See typechecking
					      test 5.sml *)
					   tv := OPEN{depth = lamdepth occ,
						      eq = eq, kind = kind};
					   ty))
			  else if toplevel occ andalso depth = 0
			   (* ASSERT: failed generalization at depth 0.
			      see bug 1066. *)
			    then lookup tv handle NotThere =>
				 let val new = mkDummy()
				  in failure := true;
                                     tv := INSTANTIATED new;
				     new
				 end
			  else ty) (* raise SHARE *)
	      | VARty(tv as ref(UBOUND{name,depth,eq})) =>
		 (debugmsg ("UBOUND:" ^Symbol.name name);
		  if localUbound tv
		  then (debugmsg "is local";
		       if depth > lamdepth occ andalso generalize
		       then (debugmsg "is generalized";
			     lookup tv handle NotThere =>
			      let val new = IBOUND(next())
			       in sign := eq :: !sign;
				  bind(tv,new);
				  new
			      end)
		       else (err region COMPLAIN
			     ("explicit type variable cannot be \
			       \generalized at its binding \
			       \declaration: " ^
			       (tyvarPrintname tv))
			      nullErrorBody;
			     tv := INSTANTIATED WILDCARDty;
			     WILDCARDty))
		  else (debugmsg "is not local"; ty))
	      | VARty(ref(OVLDV _ | OVLDI _ | OVLDW _)) => ty
	      | CONty(tyc,args) => CONty(tyc, map gen args) (*shareMap*)
	      | WILDCARDty => WILDCARDty
	      | MARKty(ty, region) =>
	      	let val () = ppTypeDebug (">> Markty", ty)
		in gen ty
		end
	      | _ => bug "generalizeTy -- bad arg"

	val _ = ppTypeDebug (">>gen: before: ",!typ)
	val ty = gen(!typ)
	val _ = ppTypeDebug (">>gen: after: ",ty)

        val generalizedTyvars = map #1 (rev(!menv))

        (* a hack to eliminate all user bound type variables --zsh *)
	(* ZHONG?: is this still necessary? [dbm] *)
        (* DBM: are ubound tyvars redefined by indexBoundTyvars in
         * generalizePat below? *)
	fun elimUbound(tv as ref(UBOUND{depth,eq,...})) =
              (tv := OPEN{depth=depth,eq=eq,kind=META})
          | elimUbound _ = ()

        (* turn ubound tyvars into ordinary META tyvars *)
        val _ = app elimUbound generalizedTyvars

     in if !failure andalso !ElabControl.valueRestrictionTopWarn
	  then err region WARN
	        "type vars not generalized because of\n\
                 \   value restriction are instantiated to dummy types (X1,X2,...)"
		nullErrorBody
          else ();
	debugmsg "generalizeTy returning";
	typ := (if !index > 0 then
                   POLYty{sign = rev(!sign),
		          tyfun = TYFUN{arity=(!index),body=ty}}
               else ty);
	btvs := generalizedTyvars;
	generalizedTyvars  (* return the tyvars that were generalized *)
    end

  | generalizeTy _ = bug "generalizeTy - bad arg"


(* the VARpat case seems designed to ensure that only one variable in a pattern
 * can have generalized type variables: either x or !tvs must be nil or a bug
 * message is generated.  Why is this? [dbm] *)
fun generalizePat(pat: pat, userbound: tyvar list, occ: occ, tdepth,
                  generalize: bool, region) =
    let val tvs : tyvar list ref = ref []
	fun union ([],tvs) = tvs
	  | union (tv::rest,tvs) = if List.exists (fn tv' => (tv = tv')) tvs then union(rest,tvs)
				   else tv :: (union(rest,tvs))
        fun gen(VARpat v) =
	      tvs := union(generalizeTy(v,userbound,occ,generalize,region), !tvs)
	  | gen(RECORDpat{fields,...}) = app (gen o #2) fields
	  | gen(APPpat(_,_,arg)) = gen arg
	  | gen(CONSTRAINTpat(pat,_)) = gen pat
	  | gen(LAYEREDpat(varPat,pat)) = (gen varPat; gen pat)
          | gen(MARKpat(pat,reg)) = gen pat
	  | gen _ = ()
     in gen pat;
        (* indexBoundTyvars(tdepth,!tvs); *)
	!tvs
    end

fun applyType(ratorTy: ty, randTy: ty) : ty =
  let val resultType = mkMETAty()
   in unifyTy(ratorTy, (randTy --> resultType), tyToLoc ratorTy, tyToLoc randTy);
      resultType
  end

fun stripMarksVar (MARKpat(p as VARpat _, reg)) = p
  | stripMarksVar (MARKpat(p,reg)) = stripMarksVar p
  | stripMarksVar (CONSTRAINTpat (p, ty)) =
      CONSTRAINTpat(stripMarksVar p, ty)
  | stripMarksVar p = p

fun patType(pat: pat, depth, region) : pat * ty =
    case pat
      of WILDpat => (pat,mkMETAtyBounded depth)
       | MARKpat(p,region') => patType(p,depth,region')
       | VARpat(VALvar{typ as ref UNDEFty,...}) =>
	      (typ := mkMETAtyBounded depth; (pat,MARKty(!typ, region)))
			             (* multiple occurrence due to or-pat *)
       | VARpat(VALvar{typ, ...}) => (pat, MARKty(!typ, region))
       | NUMpat(src, {ival, ty}) => (pat, oll_push(ival, src, ty, err region))
       | STRINGpat _ => (pat,MARKty(stringTy, region))
       | CHARpat _ => (pat,MARKty(charTy, region))
       | RECORDpat{fields,flex,typ} =>
	   (* fields assumed already sorted by label *)
	   let fun fieldType(lab,pat') =
                 let val (npat,nty) = patType(pat',depth,region)
                  in ((lab,npat), (lab,nty))
                 end
               val (fields',labtys) = mapUnZip fieldType fields
               val npat = RECORDpat{fields=fields',flex=flex,typ=typ}
	    in if flex
	       then let val tv = mkTyvar(mkFLEX(labtys,depth))
                        val ty = VARty(tv)
		     in registerFlex(tv,region);
                        typ := ty; (npat,ty)
		    end
	       else (npat,MARKty(recordTy(labtys), region))
	   end
       | VECTORpat(pats,_) =>
          (let val (npats,ntys) =
                     mapUnZip (fn pat => patType(pat,depth,region)) pats
               val nty =
	       foldr (fn (a,b) => (unifyTy(a, b, tyToLoc a, tyToLoc b); b))
		     (mkMETAtyBounded depth) ntys
            in (VECTORpat(npats,nty),
	    	MARKty(CONty(vectorTycon,[nty]), region))
           end handle Unify(mode) => (
	     err region COMPLAIN
		 (message("vector pattern type failure",mode)) nullErrorBody;
	     (pat,WILDCARDty)))
       | ORpat(p1, p2) =>
           let val (p1, ty1) = patType(p1, depth, region)
  	       val (p2, ty2) = patType(p2, depth, region)
	    in unifyErr{ty1=ty1,ty2=ty2,name1="expected",name2="found",
			message="or-patterns do not agree",region=region,
			kind=ppPat,kindname="pattern",phrase=pat};
	       (ORpat(p1, p2), MARKty(ty1, region))
	   end
       | CONpat(dcon as DATACON{typ,...},_) =>
           let val (ty, insts) = instantiatePoly typ
               (* the following unification is used to set the correct depth information
                * for the type variables in ty. (ZHONG)  It cannot fail.
                *)
               val nty = mkMETAtyBounded depth
               val _ = unifyTy(nty, ty, nullRegion, nullRegion)
            in (CONpat(dcon, insts), MARKty(ty, region))
           end
       | APPpat(dcon as DATACON{typ,rep,...},_,arg) =>
	   let val (argpat,argty) = patType(arg,depth,region)
               val (ty1,ndcon) = case rep
                                  of DA.REF => (refPatType,refNewDcon dcon)
                                   | _ => (typ,dcon)
               val (ty2,insts) = instantiatePoly ty1
               val npat = APPpat(ndcon,insts,argpat)
            in (npat,MARKty(applyType(ty2,argty), region))
	       handle Unify(mode) =>
		(err region COMPLAIN
                  (message("constructor and argument do not agree in pattern",mode))
		  (fn ppstrm =>
		   (PPType.resetPPType();
		    PP.newline ppstrm;
		    PP.string ppstrm "constructor: ";
		    ppType ppstrm typ; PP.newline ppstrm;
		    PP.string ppstrm "argument:    ";
		    ppType ppstrm argty; PP.newline ppstrm;
		    PP.string ppstrm "in pattern:"; PP.break ppstrm {nsp=1,offset=2};
		    ppPat ppstrm (pat,!printDepth)));
		 (pat,WILDCARDty))
	   end
       | CONSTRAINTpat(pat',ty) =>
	   let val (npat,patTy) = patType(pat',depth,region)
	    in if unifyErr{ty1=patTy,name1="pattern",ty2=ty,name2="constraint",
			   message="pattern and constraint do not agree",
			   region=region,kind=ppPat,kindname="pattern",phrase=pat}
		then (CONSTRAINTpat(npat,MARKty(ty, region)),
					(MARKty(ty, region)))
		else (pat,WILDCARDty)
	   end
       | LAYEREDpat(vpat,pat') =>
	   (case stripMarksVar vpat
              of VARpat(VALvar{typ,...}) =>
		 let val (npat,patTy) = patType(pat',depth,region)
		     val _ = (typ := patTy)
		  in (LAYEREDpat(vpat,npat),MARKty(patTy, region))
		 end
	       | (cpat as CONSTRAINTpat(VARpat(VALvar{typ,...}),ty)) =>
		 let val (npat,patTy) = patType(pat',depth,region)
		  in if unifyErr{ty1=patTy,name1="pattern",ty2=ty,name2="constraint",
				 message="pattern and constraint do not agree",
				 region=region,kind=ppPat,kindname="pattern",phrase=pat}
		     then (typ := ty; (LAYEREDpat(cpat,npat),MARKty(ty, region)))
		     else (pat,WILDCARDty)
		 end)
       | p => bug "patType -- unexpected pattern"

fun expType(exp: exp, occ: occ, tdepth: DI.depth, region) : exp * ty =
let fun boolUnifyErr { ty, name, message } =
	unifyErr { ty1 = ty, name1 = name, ty2 = boolTy, name2 = "",
		   message = message, region = region, kind = ppExp,
		   kindname = "expression", phrase = exp }
    fun boolshortcut (con, what, e1, e2) =
	let val (e1', t1) = expType (e1, occ, tdepth, region)
	    val (e2', t2) = expType (e2, occ, tdepth, region)
	    val m = String.concat ["operand of ", what, " is not of type bool"]
	in
	    if boolUnifyErr { ty = t1, name = "operand", message = m }
	    andalso boolUnifyErr { ty = t2, name = "operand", message = m }
	    then (con (e1', e2'), MARKty(boolTy, region))
	    else (exp, WILDCARDty)
	end
in
     case exp
      of VARexp(r as ref(v as VALvar{typ, ...}), _) =>
	   let val (ty, insts) = instantiatePoly(!typ)
	    in (VARexp(r, insts), MARKty(ty, region))
	   end
       | VARexp(varref as ref(OVLDvar _),_) =>
 	   (exp, olv_push (varref, region, err region))
       | VARexp(r as ref ERRORvar, _) => (exp, WILDCARDty)
       | CONexp(dcon as DATACON{typ,...},_) =>
           let val (ty,insts) = instantiatePoly typ
            in (CONexp(dcon, insts), MARKty(ty, region))
           end
       | NUMexp(src, {ival, ty}) => (exp, oll_push(ival, src, ty, err region))
(* REAL32: overload real literals *)
       | REALexp _ => (exp,MARKty(realTy, region))
       | STRINGexp _ => (exp,MARKty(stringTy, region))
       | CHARexp _ => (exp,MARKty(charTy, region))
       | RECORDexp fields =>
           let fun h(l,exp') =
                    let val (nexp,nty) = expType(exp',occ,tdepth,region)
                     in ((l,nexp),(l,nty))
                    end
               fun extract(LABEL{name,...},t) = (name,t)
               val (fields',tfields) = mapUnZip h fields
               val rty = map extract (sortFields tfields)
            in (RECORDexp fields',MARKty(recordTy(rty), region))
           end
       | SELECTexp (l, e) =>
           let val (nexp, nty) = expType(e, occ, tdepth, region)
               val res = mkMETAty ()
               val labtys = [(EU.labsym l, res)]
               val tv = mkTyvar(mkFLEX(labtys,infinity))
               val pt = VARty tv
               val _ = registerFlex(tv,region)
            in (unifyTy(pt, nty, region, tyToLoc nty);
		(SELECTexp(l, nexp), MARKty(res, region)))
               handle Unify(mode) =>
                 (err region COMPLAIN
                  (message("selecting a non-existing field from a record",mode))
                  (fn ppstrm =>
                   (PPType.resetPPType();
                    PP.newline ppstrm;
                    PP.string ppstrm "the field name: ";
                    (case l of LABEL{name,...} => ppSym ppstrm name);
                    PP.newline ppstrm;
                    PP.string ppstrm "the record type:    ";
                    ppType ppstrm nty; PP.newline ppstrm;
                    PP.string ppstrm "in expression:";
                    PP.break ppstrm {nsp=1,offset=2};
                    ppExp ppstrm (exp,!printDepth)));
                    (exp, WILDCARDty))
           end
       | VECTORexp(exps,_) =>
          (let val (exps',nty) = mapUnZip (fn e => expType(e,occ,tdepth,region)) exps
               val vty = foldr (fn (a,b) => (unifyTy(a,b,tyToLoc a, tyToLoc b); b))
			       (mkMETAty()) nty
            in (VECTORexp(exps',vty),
	    	MARKty(CONty(vectorTycon,[vty]), region))
           end handle Unify(mode) =>
	   (err region COMPLAIN
	     (message("vector expression type failure",mode))
             nullErrorBody; (exp,WILDCARDty)))
       | SEQexp exps =>
	   let fun scan nil = (nil,unitTy)
	         | scan [e] =
                     let val (e',ety) = expType(e,occ,tdepth,region)
                      in ([e'],ety)
                     end
		 | scan (e::rest) =
                     let val (e',_) = expType(e,occ,tdepth,region)
                         val (el',ety) = scan rest
                      in (e'::el',ety)
                     end
               val (exps',expty) = scan exps
            in (SEQexp exps',MARKty(expty, region))
	   end
       | APPexp(rator, rand) =>
	   let val (rator',ratorTy) = expType(rator,occ,tdepth,region)
	       val (rand',randTy) = expType(rand,occ,tdepth,region)
               val exp' = APPexp(rator',rand')
	    in (exp',applyType(ratorTy,MARKty(randTy, region)))
	       handle Unify(mode) =>
	       let val ratorTy = prune ratorTy
		   val reducedRatorTy = headReduceType ratorTy
		in PPType.resetPPType();
		   if isArrowType(reducedRatorTy)
		   then (err region COMPLAIN
			  (message("operator and operand do not agree",mode))
			  (fn ppstrm =>
			   (PP.newline ppstrm;
			    PP.string ppstrm "operator domain: ";
			    ppType ppstrm (domain reducedRatorTy);
			    PP.newline ppstrm;
			    PP.string ppstrm "operand:         ";
			    ppType ppstrm randTy; PP.newline ppstrm;
			    PP.string ppstrm "in expression:";
			    PP.break ppstrm {nsp=1,offset=2};
			    ppExp ppstrm (exp,!printDepth);
			    ppModeErrorMsg ppstrm mode));
			 (exp,WILDCARDty))
		   else (err region COMPLAIN
			  (message("operator is not a function",mode))
			  (fn ppstrm =>
			    (PP.newline ppstrm;
			     PP.string ppstrm "operator: ";
			     ppType ppstrm (ratorTy); PP.newline ppstrm;
			     PP.string ppstrm "in expression:";
			     PP.break ppstrm {nsp=1,offset=2};
			     ppExp ppstrm (exp,!printDepth);
			     ppModeErrorMsg ppstrm mode));
			 (exp,WILDCARDty))
	       end
	   end
       | CONSTRAINTexp(e,ty) =>
	   let val (e',ety) = expType(e,occ,tdepth,region)
	    in if unifyErr{ty1=ety,name1="expression", ty2=ty, name2="constraint",
			message="expression does not match constraint",
			region=region,kind=ppExp,kindname="expression",
			phrase=exp}
		then (CONSTRAINTexp(e',MARKty(ty, region)),
			MARKty(ty, region))
		else (exp,WILDCARDty)
	   end
       | HANDLEexp(e, (rules, _)) =>
	   let val (e',ety) = expType(e,occ,tdepth,region)
	       and (rules',rty,hty) = matchType (rules, occ, region)
               val exp' = HANDLEexp(e', (rules', rty))
	    in (unifyTy(hty, exnTy --> ety, region, tyToLoc ety);
		(exp', MARKty(ety, region)))
	       handle Unify(mode) =>
		 (if unifyErr{ty1=domain(prune hty), name1="handler domain",
			     ty2=exnTy, name2="",
			     message="handler domain is not exn",
			     region=region,kind=ppExp,kindname="expression",
			     phrase=exp}
		     then unifyErr{ty1=ety, name1="body",
				   ty2=range(prune hty), name2="handler range",
				   message="expression and handler do not agree",
				   region=region,
				   kind=ppExp,kindname="expression",phrase=exp}
		     else false;
		  (exp,WILDCARDty))
	   end
       | RAISEexp(e,_) =>
	   let val (e',ety) = expType(e,occ,tdepth,region)
               val newty = mkMETAty()
	    in unifyErr{ty1=ety, name1="raised", ty2=exnTy, name2="",
			message="argument of raise is not an exception",
			region=region,kind=ppExp,kindname="expression",phrase=exp};
	       (RAISEexp(e',newty),MARKty(newty, region))
	   end
       | LETexp(d,e) =>
           let val d' = decType0(d,LetDef(occ),tdepth,region)
               val (e',ety) = expType(e,occ,tdepth,region)
            in (LETexp(d',e'),MARKty(ety, region))
           end
       | CASEexp(e,rules,isMatch) =>
	   let val (e',ety) = expType(e,occ,tdepth,region)
	       val (rules',_,rty) = matchType(rules,occ,region)
               val exp' = CASEexp(e',rules', isMatch)
	    in (exp',MARKty(applyType(rty,ety), region))
	       handle Unify(mode) =>
	       (if isMatch then
		    unifyErr{ty1=domain rty, name1="rule domain",
			     ty2=ety, name2="object",
			     message="case object and rules do not agree",
			     region=region,kind=ppExp,kindname="expression",phrase=exp}
                else
                 let val decl = case rules
                                 of (RULE(pat,_))::_ =>
				    VB{pat=pat,exp=exp,tyvars=ref[],boundtvs=[]}
                                  | _ => bug "unexpected rule list 456"
		  in unifyErr{ty1=domain rty, name1="pattern",
			      ty2=ety, name2="expression",
			      message="pattern and expression in val dec do not agree",
			      region=region,kind=ppVB,kindname="declaration",
			      phrase=decl}
                 end;
	        (exp,WILDCARDty))
	   end
		 (* this causes case to behave differently from let, i.e.
		    bound variables do not have generic types *)
       | IFexp { test, thenCase, elseCase } =>
	   let val (test', tty) = expType (test,occ,tdepth,region)
	       val (thenCase', tct) = expType (thenCase, occ, tdepth, region)
	       val (elseCase', ect) = expType (elseCase, occ, tdepth, region)
	   in
	       if boolUnifyErr
		      { ty = tty, name = "test expression",
			message="test expression in if is not of type bool" }
	       andalso
	          unifyErr { ty1 = tct, name1 = "then branch",
			     ty2 = ect, name2 = "else branch",
			     message="types of if branches do not agree",
			     region = region, kind = ppExp,
			     kindname = "expression", phrase = exp }
	       then
		   (IFexp { test = test', thenCase = thenCase',
			    elseCase = elseCase' },
		    MARKty(tct, region))
	       else
		   (exp, WILDCARDty)
	   end
       | ANDALSOexp (e1, e2) =>
	   boolshortcut (ANDALSOexp, "andalso", e1, e2)
       | ORELSEexp (e1, e2) =>
	   boolshortcut (ORELSEexp, "orelse", e1, e2)
       | WHILEexp { test, expr } =>
	   let val (test', tty) = expType (test, occ, tdepth, region)
	       val (expr', _) = expType (expr, occ, tdepth, region)
	   in
	       if boolUnifyErr { ty = tty, name = "test expression",
				 message = "test expression in while is not of type bool" }
	       then
		   (WHILEexp { test = test', expr = expr' }, MARKty(unitTy, region))
	       else
		   (exp, WILDCARDty)
	   end
       | FNexp(rules,_) =>
           let val (rules',ty,rty) = matchType(rules,occ,region)
            in (FNexp(rules',ty),MARKty(rty, region))
           end
       | MARKexp(e,region) =>
           let val (e',et) = expType(e,occ,tdepth,region)
            in (MARKexp(e',region),MARKty(et, region))
           end
end

and ruleType(RULE(pat,exp),occ,region) =
 let val occ = Abstr occ
     val (pat',pty) = patType(pat,lamdepth occ,region)
     val (exp',ety) = expType(exp,occ,tdepth,region)
  in (RULE(pat',exp'),pty,pty --> ety)
 end

and matchType(l,occ,region) =
    case l
      of [] => bug "empty rule list in typecheck.matchType"
       | [rule] =>
	    let val (rule0,argt,rty) = ruleType(rule,occ,region)
	     in ([rule0],argt,rty)
	    end
       | rule::rest =>
	    let val (rule0,argt,rty) = ruleType(rule,occ,region)
		fun checkrule rule' =
		   let val (rule1,argt',rty') = ruleType(rule',occ,region)
		    in unifyErr{ty1=rty,ty2=rty', name1="earlier rule(s)",
				name2="this rule",
				message="types of rules do not agree",
				region=region,
				kind=ppRule,kindname="rule",phrase=rule'};
		       rule1
		   end
	     in (rule0::(map checkrule rest),argt,rty)
	    end

and decType0(decl,occ,tdepth,region) : dec =
     case decl
      of VALdec vbs =>
	   let fun vbType(vb as VB{pat, exp, tyvars=(tv as (ref tyvars)), boundtvs}) =
	        let val (pat',pty) = patType(pat,infinity,region)
		    val (exp',ety) = expType(exp,occ,DI.next tdepth,region)
                    val generalize = TypesUtil.isValue exp
				     andalso not(TypesUtil.refutable pat)
		                     (* orelse isVarTy ety *)
		    val _ = unifyErr{ty1=pty,ty2=ety, name1="pattern", name2="expression",
			     message="pattern and expression in val dec do not agree",
			     region=region,kind=ppVB,kindname="declaration",
			     phrase=vb};
                   val vb = VB{pat=pat',exp=exp',tyvars=tv,
                      boundtvs=generalizePat(pat,tyvars,occ,tdepth,generalize,region)}
		in
                   debugPrint ("VB: ", ppVB, (vb,100));
                   debugmsg ("generalize: "^Bool.toString generalize);
		   vb
                end
	       val _ = debugmsg ">>decType0: VALdec"
	    in VALdec(map vbType vbs)
	   end

       | VALRECdec(rvbs) =>
 	   let val occ = Abstr occ

	       (* First go through and type-check all the patterns and
		  result-constraints, unifying with each other and with
		  the specified result type.
	       *)
	       fun setType(rvb as RVB{var=VALvar{typ,...},exp,resultty,...}) =
                   let val domainty = mkMETAtyBounded(lamdepth occ)
		       val rangety = mkMETAtyBounded(lamdepth occ)
                                      (* depth should be infinity? *)
		       val funty = domainty --> rangety

		       val _ =
			   case resultty
			     of NONE => true
			      | SOME ty =>
				 unifyErr{ty1=funty,ty2=ty,
					  name1="",name2="constraint",
					  message="type constraint of val rec dec\
					           \ is not a function type",
					  region=region,kind=ppRVB,
					  kindname="declaration", phrase=rvb}

		       fun f(FNexp(rules,_), region, funty) =
		             let fun unify a =
				  (unifyErr{ty1=a,name1="this clause",
				    ty2=funty,name2="previous clauses",
				    message="parameter or result constraints\
			                     \ of clauses do not agree",
					   region=region,kind=ppRVB,
					   kindname="declaration", phrase=rvb};
                                  ())

				 fun approxRuleTy(RULE(pat,e)) =
				     let val (pat',pty) =
					     patType(pat,lamdepth occ,region)
				      in case e
					  of CONSTRAINTexp(e,ty) =>
					      (pat',pty-->ty,(e,region))
					   | e => (pat',pty-->rangety,(e,region))
				     end

				 val patTyExps = map approxRuleTy rules
				 val pats = map #1 patTyExps
				 val tys = map #2 patTyExps
				 val exps = map #3 patTyExps

				 fun doExp (e,region) =
				     let val (exp', ety) = expType(e,occ,tdepth,region)
				      in unifyErr{ty1=ety, name1="expression",
					  ty2=rangety, name2="result type",
					  message="right-hand-side of clause\
					\ does not agree with function result type",
					  region=region,kind=ppRVB,
					  kindname="declaration",phrase=rvb};
					 exp'
				     end

                              in app unify tys;
				 typ := funty;
				 fn()=>
				   FNexp(ListPair.map RULE (pats, map doExp exps),
						domain(prune(funty)))
			     end
		         | f(MARKexp(e,region),_,funty) =
			     let val build = f(e,region,funty)
			      in fn()=> MARKexp(build(), region)
			     end
                         | f(CONSTRAINTexp(e,ty),region,funty) =
			     let val _ =
				   unifyErr{ty1=ty, name1="this constraint",
					    ty2=funty, name2="outer constraints",
					    message="type constraints on val rec\
					             \ declaraction disagree",
					    region=region,kind=ppRVB,
					    kindname="declaration", phrase=rvb}
				 val build = f(e,region,funty)
			     in fn()=> CONSTRAINTexp(build(), ty)
			    end
			| f _ = bug "typecheck.823"
                   in f(exp,region,funty)
                  end
		 | setType _ = bug "setType"

	      (* Second, go through and type-check the right-hand-side
	         expressions (function bodies) *)
	       fun rvbType(RVB{var=v,resultty,tyvars,boundtvs,...}, build) =
                      RVB{var=v,exp=build(), resultty=resultty,tyvars=tyvars,
			  boundtvs=boundtvs}

	       val _ = debugmsg ">>decType0: VALRECdec"
               val builders = map setType rvbs
               val rvbs' = ListPair.map rvbType (rvbs,builders)
               (* No need to generalize here, because every VALRECdec is
                  wrapped in a VALdec, and the generalization occurs at the
                  outer level.  Previously: val rvbs'' = map genType rvbs' *)
	    in EU.recDecs rvbs'
	   end

       | DOdec exp => let
	  val (exp',ety) = expType(exp,occ,DI.next tdepth,region)
	  val _ = unifyErr{
		    ty1=unitTy, ty2=ety, name1="", name2="expression",
		    message="do expression does not have type unit",
		    region=region, kind=ppDec, kindname="declaration",
		    phrase=decl
		  }
	  in
	    DOdec exp'
	  end

       | EXCEPTIONdec(ebs) =>
	   let fun check(VARty(ref(UBOUND _))) =
		     err region COMPLAIN
		         "type variable in top level exception type"
			 nullErrorBody
		 | check(CONty(_,args)) =
		     app check args
		 | check _ = ()
	       fun ebType(EBgen{etype=SOME ty,...}) = check ty
	         | ebType _ = ()
	       val _ = debugmsg ">>decType0: EXCEPTIONdec"
            in if TypesUtil.lamdepth occ < 1 then app ebType ebs else ();
               decl
	   end
       | LOCALdec(decIn,decOut) =>
	   let val decIn' = decType0(decIn,LetDef occ,tdepth,region)
               val decOut' = decType0(decOut,occ,tdepth,region)
	       val _ = debugmsg ">>decType0: LOCALdec"
            in LOCALdec(decIn',decOut')
           end
       | SEQdec(decls) =>
           SEQdec(map (fn decl => decType0(decl,occ,tdepth,region)) decls)
       | ABSTYPEdec{abstycs,withtycs,body} =>
	   let fun makeAbstract(GENtyc { eq, ... }) = eq := ABS
		 | makeAbstract _ = bug "makeAbstract"
	       fun redefineEq(DATATYPEdec{datatycs,...}) =
		   let fun setDATA (GENtyc { eq, ... }) = eq := DATA
			 | setDATA _ = ()
		   in
		       app setDATA datatycs;
		       EqTypes.defineEqProps(datatycs,nil,EntityEnv.empty)
		   end
	         | redefineEq(SEQdec decs) = app redefineEq decs
	         | redefineEq(LOCALdec(din,dout)) =
		    (redefineEq din; redefineEq dout)
	         | redefineEq(MARKdec(dec,_)) = redefineEq dec
	         | redefineEq _ = ()
	       val body'= decType0(body,occ,tdepth,region)
	       val _ = debugmsg ">>decType0: ABSTYPEdec"
	    in app makeAbstract abstycs;
	       redefineEq body';
	       ABSTYPEdec{abstycs=abstycs,withtycs=withtycs,body=body'}
	   end
       | MARKdec(dec,region) => MARKdec(decType0(dec,occ,tdepth,region),region)

      (*
       * The next several declarations will never be seen ordinarily;
       * they are for re-typechecking after the instrumentation phase
       * of debugger or profiler.
       *)
       | STRdec strbs => STRdec(map (strbType(occ,tdepth,region)) strbs)
       | FCTdec fctbs => FCTdec(map (fctbType(occ,tdepth,region)) fctbs)
       | _ => decl

and fctbType (occ,tdepth,region) (FCTB{fct,def,name}) =
      let fun fctexpType(FCTfct{param, argtycs, def}) =
  	        FCTfct{param=param, def=strexpType (occ,DI.next tdepth,region) def,
	               argtycs=argtycs}
 	    | fctexpType(LETfct(dec,e)) =
	        LETfct(decType0(dec,LetDef occ,tdepth,region), fctexpType e)
	    | fctexpType(MARKfct(f,region)) = MARKfct(fctexpType f,region)
            | fctexpType v = v
       in FCTB{fct=fct,def=fctexpType def,name=name}
      end

and strexpType (occ,tdepth,region) (se as (APPstr{oper,arg,argtycs})) = se
  | strexpType (occ,tdepth,region) (LETstr(dec,e)) =
      LETstr(decType0(dec,LetDef occ,tdepth,region), strexpType (occ,tdepth,region) e)
  | strexpType (occ,tdepth,_) (MARKstr(e,region)) =
      MARKstr(strexpType (occ,tdepth,region) e, region)
  | strexpType _ v = v

and strbType (occ,tdepth,region) (STRB{str,def,name}) =
    STRB{str=str,def=strexpType (occ,tdepth,region) def,name=name}

val _ = debugmsg ">>decType: calling decType0"
val dec' = decType0(dec, if toplev then Root else (LetDef Root), tdepth, region);
in
(*    oll_resolve ();  -- literal overloading resolution merged with operator resolution *)
    ol_resolve env;
    checkFlex ();
    debugmsg "<<decType: returning";
    dec'
end (* function decType *)

val decType = Stats.doPhase (Stats.makePhase "Compiler 035 typecheck") decType

end (* local *)
end (* structure Typecheck *)
