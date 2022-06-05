(* transtypes.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TRANSTYPES =
  sig

    val genTT  : unit -> {
	    tpsKnd : Types.tycpath -> PLambdaType.tkind,
	    tpsTyc : DebIndex.depth -> Types.tycpath -> PLambdaType.tyc,
	    toTyc  : DebIndex.depth -> Types.ty -> PLambdaType.tyc,
	    toLty  : DebIndex.depth -> Types.ty -> PLambdaType.lty,
	    strLty : Modules.Structure * DebIndex.depth * ElabUtil.compInfo -> PLambdaType.lty,
	    fctLty : Modules.Functor * DebIndex.depth * ElabUtil.compInfo -> PLambdaType.lty
	  }

  end (* signature TRANSTYPES *)

structure TransTypes : TRANSTYPES =
  struct

    structure BT = BasicTypes
    structure DA = Access
    structure DI = DebIndex
    structure EE = EntityEnv
    structure EM = ErrorMsg
    structure EPC = EntPathContext
    structure EV = EvalEntity
    structure INS = Instantiate
    structure IP = InvPath
    structure LT = PLambdaType
    structure PT = PrimTyc
    structure MU = ModuleUtil
    structure SE = StaticEnv
    structure TU = TypesUtil
    structure PP = PrettyPrint
    open Types Modules ElabDebug

    fun bug msg = ErrorMsg.impossible ("TransTypes: " ^ msg)
    val say = Control.Print.say
    val debugging = FLINT_Control.tmdebugging
    fun debugmsg (msg: string) =
	  if !debugging then (say msg; say "\n") else ()
    val debugPrint = (fn x => debugPrint debugging x)
    val defaultError =
	  EM.errorNoFile(EM.defaultConsumer(),ref false) SourceMap.nullRegion

    val env = StaticEnv.empty

    fun ppType x =
     ((PP.with_pp (EM.defaultConsumer())
	       (fn ppstrm => (PP.string ppstrm "find: ";
			      PPType.resetPPType();
			      PPType.ppType env ppstrm x)))
      handle _ => say "fail to print anything")

    fun ppTycon x =
	((PP.with_pp (EM.defaultConsumer())
	    (fn ppstrm => (PP.string ppstrm "find: ";
			   PPType.resetPPType();
			   PPType.ppTycon env ppstrm x)))
	handle _ => say "fail to print anything")


    fun ppLtyc ltyc =
	PP.with_default_pp (fn ppstrm => PPLty.ppTyc 20 ppstrm ltyc)


  (****************************************************************************
   *               TRANSLATING ML TYPES INTO FLINT TYPES                      *
   ****************************************************************************)
local
  val recTyContext = ref [~1]
in
  fun enterRecTy (a) = (recTyContext := (a::(!recTyContext)))
  fun exitRecTy () = (recTyContext := tl (!recTyContext))
  fun recTyc (i) =
	let val x = hd(!recTyContext)
	    val base = DI.innermost
	 in if x = 0 then LT.tcc_var(base, i)
	    else if x > 0 then LT.tcc_var(DI.di_inner base, i)
		 else bug "unexpected RECtyc"
	end
  fun freeTyc (i) =
	let val x = hd(!recTyContext)
	    val base = DI.di_inner (DI.innermost)
	 in if x = 0 then LT.tcc_var(base, i)
	    else if x > 0 then LT.tcc_var(DI.di_inner base, i)
		 else bug "unexpected RECtyc"
	end
end (* end of recTyc and freeTyc hack *)

fun tpsKnd (TP_VAR x) = TransTKind.trans(#kind x)
  | tpsKnd _ = bug "unexpected tycpath parameters in tpsKnd"

(* ---------------- begin genTT ---------------- *)
fun genTT () = let

fun tpsTyc d tp =
  let fun h (TP_VAR x, cur) =
	  let val { tdepth, num, ... } = x
	  in
              LT.tcc_var(DI.getIndex(cur, tdepth), num)
	  end
        | h (TP_TYC tc, cur) = tycTyc(tc, cur)
        | h (TP_SEL (tp, i), cur) = LT.tcc_proj(h(tp, cur), i)
        | h (TP_APP (tp, ps), cur) =
              LT.tcc_app(h(tp, cur), map (fn x => h(x, cur)) ps)
        | h (TP_FCT (ps, ts), cur) =
              let val ks = map tpsKnd ps
                  val cur' = DI.next cur
                  val ts' = map (fn x => h(x, cur')) ts
               in LT.tcc_fn(ks, LT.tcc_seq ts')
              end

   in h(tp, d)
  end (* tpsTyc *)

(*
and tycTyc x =
  Stats.doPhase(Stats.makePhase "Compiler 043 1-tycTyc") tycTyc0 x
*)

and tycTyc (tc, d) =
  let fun dtsTyc nd ({dcons: dconDesc list, arity=i, ...} : dtmember) =
            let val nnd = if i=0 then nd else DI.next nd
                fun f ({domain=NONE, rep, name}, r) = (LT.tcc_unit)::r
                  | f ({domain=SOME t, rep, name}, r) = (toTyc nnd t)::r

                val _ = enterRecTy i
                val core = LT.tcc_sum(foldr f [] dcons)
                val _ = exitRecTy()

                val resTyc = if i=0 then core
                             else (let val ks = LT.tkc_arg i
                                    in LT.tcc_fn(ks, core)
                                   end)
             in (LT.tkc_int i, resTyc)
            end

      fun dtsFam (freetycs, fam as { members, ... } : dtypeFamily) =
	  case ModulePropLists.dtfLtyc fam of
	      SOME (tc, od) =>
              LT.tc_adj(tc, od, d) (* invariant: tc contains no free variables
				    * so tc_adj should have no effects *)
	    | NONE =>
              let fun ttk (GENtyc { arity, ... }) = LT.tkc_int arity
                    | ttk (DEFtyc{tyfun=TYFUN{arity=i, ...},...}) =
		      LT.tkc_int i
                    | ttk _ = bug "unexpected ttk in dtsFam"
                  val ks = map ttk freetycs
                  val (nd, hdr) =
                      case ks of [] => (d, fn t => t)
                               | _ => (DI.next d, fn t => LT.tcc_fn(ks, t))
                  val mbs = Vector.foldr (op ::) nil members
                  val mtcs = map (dtsTyc (DI.next nd)) mbs
                  val (fks, fts) = ListPair.unzip mtcs
                  val nft = case fts of [x] => x | _ => LT.tcc_seq fts
                  val tc = hdr(LT.tcc_fn(fks, nft))
                  val _ = ModulePropLists.setDtfLtyc (fam, SOME(tc, d))
              in tc
              end

      and g (tycon as GENtyc { arity, kind, ... }) =
	  (case kind
	     of PRIMITIVE => (* translation defined in FLINT/kernel/primtyc.sml *)
		  LT.tcc_prim(PrimTyc.pt_fromtyc tycon)
              | DATATYPE {index, family, freetycs, stamps, ...} =>
		if TU.eqTycon(tycon, BT.refTycon) then LT.tcc_prim (PT.ptc_ref)
		else let val tc = dtsFam (freetycs, family)
			 val n = Vector.length stamps
			 val names = Vector.map
				       (fn ({tycname,...}: dtmember) => Symbol.name tycname)
				       (#members family)
                          (* invariant: n should be the number of family members *)
		     in LT.tcc_fix((n, names, tc, (map g freetycs)), index)
		     end
              | ABSTRACT tc => (g tc) (*>>> LT.tcc_abs(g tc) <<<*)
              | FLEXTYC tp => tpsTyc d tp
              | FORMAL => bug "unexpected FORMAL kind in tycTyc-h"
              | TEMP => bug "unexpected TEMP kind in tycTyc-h")
        | g (DEFtyc{tyfun, ...}) = tfTyc(tyfun, d)
        | g (RECtyc i) = recTyc i
        | g (FREEtyc i) = freeTyc i
        | g (RECORDtyc _) = bug "unexpected RECORDtyc in tycTyc-g"
        | g (PATHtyc{arity, path=InvPath.IPATH ss, entPath}) =
              ((* say "*** Warning for compiler writers: PATHtyc ";
               app (fn x => (say (Symbol.name x); say ".")) ss;
               say " in translate: ";
               say (EntPath.entPathToString entPath);
               say "\n"; *)
               if arity > 0 then LT.tcc_fn(LT.tkc_arg arity, LT.tcc_void)
               else LT.tcc_void)
        | g (ERRORtyc) = bug "unexpected tycon in tycTyc-g"

   in (g tc)
  end (* tycTyc *)

and tfTyc (TYFUN{arity=0, body}, d) = toTyc d body
  | tfTyc (TYFUN{arity, body}, d) =
      let val ks = LT.tkc_arg arity
       in LT.tcc_fn(ks, toTyc (DI.next d) body)
      end

(* toTyc : DI.depth -> ty -> LT.tyc *)
and toTyc d t =
  let val tvDict : (tyvar * LT.tyc) list ref = ref []
      fun lookTv tv =
        let val tv_alist = !tvDict
            fun lookup ((a,x)::rest) =
                 if a = tv then x else lookup rest
              | lookup nil =  (* tv not found in tvDict *)
		 let val tyc = trMTyvarKind (!tv)
                  in tvDict := (tv, tyc) :: tv_alist;  (* bind new tyc in tvDict *)
		     tyc
                 end
         in lookup tv_alist
        end

      and trMTyvarKind (INSTANTIATED t) = trTy t
        | trMTyvarKind (LBOUND{depth,index,...}) =
             LT.tcc_var(DI.getIndex(d, depth), index)
        | trMTyvarKind (UBOUND _) = LT.tcc_void
            (* dbm: a user-bound type variable that didn't get generalized;
               treat the same as an uninstantiated metatyvar.
	       E.g. val x = ([]: 'a list; 1) *)
        | trMTyvarKind (OPEN _) = LT.tcc_void
            (* dbm: a metatyvar that was neither instantiated nor
	       generalized.  E.g. val x = ([],1); -- the metatyvar
               introduced by the generic instantiation of the type of [] is
               neither instantiated nor generalized. *)
        | trMTyvarKind _ = bug "toTyc:h" (* OVLD should have been resolved *)

      and trTy (VARty tv) = (* h(!tv) *) lookTv tv
        | trTy (CONty(RECORDtyc _, [])) = LT.tcc_unit
        | trTy (CONty(RECORDtyc _, tys)) = LT.tcc_tuple (map trTy tys)
        | trTy (CONty(tyc, [])) = tycTyc(tyc, d)
        | trTy (CONty(DEFtyc{tyfun,...}, args)) = trTy (TU.applyTyfun(tyfun,args))
	| trTy (CONty (tc as GENtyc { kind, ... }, ts)) =
	  (case (kind, ts) of
	       (ABSTRACT _, ts) =>
	       LT.tcc_app(tycTyc(tc, d), map trTy ts)
             | (_, [t1, t2]) =>
               if TU.eqTycon(tc, BT.arrowTycon) then LT.tcc_parrow(trTy t1, trTy t2)
               else LT.tcc_app(tycTyc(tc, d), [trTy t1, trTy t2])
	     | _ => LT.tcc_app (tycTyc (tc, d), map trTy ts))
        | trTy (CONty(tyc, ts)) = LT.tcc_app(tycTyc(tyc, d), map trTy ts)
        | trTy (IBOUND i) = LT.tcc_var(DI.innermost, i)
			 (* [KM] IBOUNDs are encountered when toTyc
                          * is called on the body of a POLYty in
                          * toLty (see below). *)
	| trTy (MARKty (t, _)) = trTy t
        | trTy (POLYty _) = bug "unexpected poly-type in toTyc"
        | trTy (UNDEFty) = (* mkVB kluge!!! *) LT.tcc_void
	    (* bug "unexpected undef-type in toTyc" *)
        | trTy (WILDCARDty) = bug "unexpected wildcard-type in toTyc"
   in trTy t
  end

and toLty d (POLYty {tyfun=TYFUN{arity=0, body}, ...}) = toLty d body
  | toLty d (POLYty {tyfun=TYFUN{arity, body},...}) =
      let val ks = LT.tkc_arg arity
       in LT.ltc_poly(ks, [toLty (DI.next d) body])
      end

  | toLty d x = LT.ltc_tyc (toTyc d x)

(****************************************************************************
 *               TRANSLATING ML MODULES INTO FLINT TYPES                    *
 ****************************************************************************)

fun specLty (elements, entEnv, depth, compInfo) =
  let fun g ([], entEnv, ltys) = rev ltys
        | g ((sym, (TYCspec _ ))::rest, entEnv, ltys) =
              g(rest, entEnv, ltys)
        | g ((sym, STRspec {sign, entVar, ...})::rest, entEnv, ltys) =
              let val rlzn = EE.lookStrEnt(entEnv,entVar)
                  val lt = strRlznLty(sign, rlzn, depth, compInfo)
               in g(rest, entEnv, lt::ltys)
              end
        | g ((sym, FCTspec {sign, entVar, ...})::rest, entEnv, ltys) =
              let val rlzn = EE.lookFctEnt(entEnv,entVar)
                  val lt = fctRlznLty(sign, rlzn, depth, compInfo)
               in g(rest, entEnv, lt::ltys)
              end
        | g ((sym, spec)::rest, entEnv, ltys) =
              let val _ = debugmsg ">>specLtyElt"
                  fun transty ty =
                    ((MU.transType entEnv ty)
                      handle EE.Unbound =>
                         (debugmsg "$specLty";
                          withInternals(fn () =>
                           debugPrint("entEnv: ",
                                 (fn pps => fn ee =>
                                  PPModules.ppEntityEnv pps (ee,SE.empty,12)),
                                  entEnv));
                          debugmsg ("$specLty: should have printed entEnv");
                          raise EE.Unbound))

                  fun mapty t = toLty depth (transty t)

               in case spec
                   of VALspec{spec=typ,...} =>
                        g(rest, entEnv, (mapty typ)::ltys)
                    | CONspec{spec=DATACON{rep=DA.EXN _,
                                           typ, ...}, ...} =>
                        let val argt =
                              if BT.isArrowType typ then
                                   #1(LT.ltd_parrow (mapty typ))
                              else LT.ltc_unit
                         in g(rest, entEnv, (LT.ltc_etag argt)::ltys)
                        end
                    | CONspec{spec=DATACON _, ...} =>
                        g(rest, entEnv, ltys)
                    | _ => bug "unexpected spec in specLty"
              end

   in g (elements, entEnv, [])
  end

(*
and signLty (sign, depth, compInfo) =
  let fun h (SIG {kind=SOME _, lambdaty=ref (SOME(lt, od)), ...}) = lt
             (* LT.lt_adj(lt, od, depth) *)
        | h (sign as SIG{kind=SOME _, lambdaty as ref NONE, ...}) =
          (* Invariant: we assum that all Named signatures (kind=SOME _) are
           * defined at top-level, outside any functor definitions. (ZHONG)
           *)
             let val {rlzn=rlzn, tycpaths=tycpaths} =
                   INS.instParam {sign=sign, entEnv=EE.empty, depth=depth,
                                  rpath=InvPath.IPATH[], compInfo=compInfo,
                                  region=SourceMap.nullRegion}
                 val nd = DI.next depth
                 val nlty = strMetaLty(sign, rlzn, nd, compInfo)

                 val ks = map tpsKnd tycpaths
                 val lt = LT.ltc_poly(ks, nlty)
              in lambdaty := SOME (lt, depth); lt
             end
        | h _ = bug "unexpected sign in signLty"
   in h sign
  end
*)
and strMetaLty (sign, rlzn as { entities, ... }: strEntity, depth, compInfo) =
    case (sign, ModulePropLists.strEntityLty rlzn) of
	(_, SOME (lt, od)) => LT.lt_adj(lt, od, depth)
      | (SIG { elements, ... }, NONE) =>
	let val ltys = specLty (elements, entities, depth, compInfo)
            val lt = (* case ltys of [] => LT.ltc_int
                                   | _ => *) LT.ltc_str(ltys)
        in
	    ModulePropLists.setStrEntityLty (rlzn, SOME(lt, depth));
	    lt
        end
      | _ => bug "unexpected sign and rlzn in strMetaLty"

and strRlznLty (sign, rlzn : strEntity, depth, compInfo) =
    case (sign, ModulePropLists.strEntityLty rlzn) of
	(sign, SOME (lt,od)) => LT.lt_adj(lt, od, depth)

(* Note: the code here is designed to improve the "toLty" translation;
   by translating the signature instead of the structure, this can
   potentially save time on strLty. But it can increase the cost of
   other procedures. Thus we turn it off temporarily. (ZHONG)

      | (SIG{kind=SOME _, ...}, {lambdaty, ...}) =>
             let val sgt = signLty(sign, depth, compInfo)
                 (* Invariant: we assum that all Named signatures
                  * (kind=SOME _) are defined at top-level, outside any
                  * functor definitions. (ZHONG)
                  *)
                 val argtycs = INS.getTycPaths{sign=sign, rlzn=rlzn,
                         entEnv=EE.empty, compInfo=compInfo}
                 val lt = LT.lt_inst(sgt, map (tpsTyc depth) argtycs)
              in lambdaty := SOME(lt, depth); lt
             end
*)
      | _ => strMetaLty(sign, rlzn, depth, compInfo)

and fctRlznLty (sign, rlzn, depth, compInfo) =
    case (sign, ModulePropLists.fctEntityLty rlzn, rlzn) of
	(sign, SOME (lt, od), _) => LT.lt_adj(lt, od, depth)
      | (FSIG{paramsig, bodysig, ...}, _,
         {closure as CLOSURE{env,...}, ...}) =>
        let val {rlzn=argRlzn, tycpaths=tycpaths} =
                INS.instParam {sign=paramsig, entEnv=env, tdepth=depth,
                               rpath=InvPath.IPATH[], compInfo=compInfo,
                               region=SourceMap.nullRegion}
            val nd = DI.next depth
            val paramLty = strMetaLty(paramsig, argRlzn, nd, compInfo)
            val ks = map tpsKnd tycpaths
            val bodyRlzn =
                EV.evalApp(rlzn, argRlzn, nd, EPC.initContext,
                           IP.empty, compInfo)
            val bodyLty = strRlznLty(bodysig, bodyRlzn, nd, compInfo)

            val lt = LT.ltc_poly(ks, [LT.ltc_fct([paramLty],[bodyLty])])
        in
	    ModulePropLists.setFctEntityLty (rlzn, SOME (lt, depth));
	    lt
        end
      | _ => bug "fctRlznLty"

and strLty (str as STR { sign, rlzn, ... }, depth, compInfo) =
    (case ModulePropLists.strEntityLty rlzn of
	 SOME (lt, od) => LT.lt_adj(lt, od, depth)
       | NONE =>
         let val lt = strRlznLty(sign, rlzn, depth, compInfo)
         in
	     ModulePropLists.setStrEntityLty (rlzn, SOME(lt, depth));
	     lt
         end)
  | strLty _ = bug "unexpected structure in strLty"

and fctLty (fct as FCT { sign, rlzn, ... }, depth, compInfo) =
    (case ModulePropLists.fctEntityLty rlzn of
	 SOME (lt,od) => LT.lt_adj(lt, od, depth)
       | NONE =>
         let val lt = fctRlznLty(sign, rlzn, depth, compInfo)
	 in
	     ModulePropLists.setFctEntityLty (rlzn, SOME(lt,depth));
	     lt
         end)
  | fctLty _ = bug "unexpected functor in fctLty"

(****************************************************************************
 *           A HASH-CONSING VERSION OF THE ABOVE TRANSLATIONS               *
 ****************************************************************************)

(*
structure MIDict = RedBlackMapFn(struct type ord_key = ModuleId.modId
                                     val compare = ModuleId.cmp
                              end)
*)

(*
      val m1 = ref (MIDict.mkDict())   (* modid (tycon) -> LT.tyc *)
      val m2 = ref (MIDict.mkDict())   (* modid (str/fct) -> LT.lty *)

      fun tycTycLook (t as (GENtyc _ | DEFtyc _), d) =
            let tid = MU.tycId t
             in (case MIDict.peek(!m1, tid)
                  of SOME (t', od) => LT.tc_adj(t', od, d)
                   | NONE =>
                       let val x = tycTyc (t, d)
                           val _ = (m1 := TcDict.insert(!m1, tid, (x, d)))
                        in x
                       end)
            end
        | tycTycLook x = tycTyc tycTycLook x

(*
      val toTyc = toTyc tycTycLook
      val toLty = toTyc tycTycLook
*)
      val coreDict = (toTyc, toLty)

      fun strLtyLook (s as STR _, d) =
            let sid = MU.strId s
             in (case MIDict.peek(!m2, sid)
                  of SOME (t', od) => LT.lt_adj(t', od, d)
                   | NONE =>
                       let val x = strLty (coreDict, strLtyLook,
                                           fctLtyLook) (s, d)
                           val _ = (m2 := TcDict.insert(!m2, sid, (x, d)))
                        in x
                       end)
            end
        | strLtyLook x = strLty (coreDict, strLtyLook, fctLtyLook)

      and fctLtyLook (f as FCT _, d) =
            let fid = fctId f
             in (case MIDict.peek(!m2, fid)
                  of SOME (t', od) => LT.lt_adj(t', od, d)
                   | NONE =>
                       let val x = fctLty (tycTycLook, strLtyLook,
                                           fctLtyLook) (s, d)
                           val _ = (m2 := TcDict.insert(!m2, fid, (x, d)))
                        in x
                       end)
            end
        | fctLtyLook x = fctLty (coreDict, strLtyLook, fctLtyLook)
*)

   in {tpsKnd=tpsKnd, tpsTyc=tpsTyc,
       toTyc=toTyc, toLty=toLty, strLty=strLty, fctLty=fctLty}
  end (* function genTT *)
(* ---------------- end genTT ---------------- *)

end (* structure TransTypes *)
