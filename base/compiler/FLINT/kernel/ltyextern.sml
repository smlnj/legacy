(* ltyextern.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure LtyExtern : LTYEXTERN =
struct

local structure PT = PrimTyc
      structure DI = DebIndex
      structure LT = Lty
      structure LKC = LtyKindChk
      structure LK = LtyKernel
      structure PO = Primop     (* really should not refer to this *)
      structure FL = FLINT

      fun bug msg = ErrorMsg.impossible("LtyExtern: "^msg)
      val say = Control.Print.say

      (** common utility functions *)
      val tk_inj = LK.tk_inj
      val tk_out = LK.tk_out

      val tc_inj = LK.tc_inj
      val tc_out = LK.tc_out

      val lt_inj = LK.lt_inj
      val lt_out = LK.lt_out

      val tcc_env = LK.tcc_env
      val ltc_env = LK.ltc_env
      val tc_whnm = LK.tc_whnm
      val lt_whnm = LK.lt_whnm
      val tc_norm = LK.tc_norm
      val lt_norm = LK.lt_norm

in

structure PP = PrettyPrint
structure PU = PPUtil
val with_pp = PP.with_default_pp

open LtyBasic

(** tkind constructors *)
val tkc_mono = Lty.tkc_mono
val tkc_box = Lty.tkc_box
val tkc_seq = Lty.tkc_seq
val tkc_fun = Lty.tkc_fun

(** tkind deconstructors *)
val tkd_mono = Lty.tkd_mono
val tkd_box = Lty.tkd_box
val tkd_seq = Lty.tkd_seq
val tkd_fun = Lty.tkd_fun

(** tkind predicates *)
val tkp_mono = Lty.tkp_mono
val tkp_box = Lty.tkp_box
val tkp_seq = Lty.tkp_seq
val tkp_fun = Lty.tkp_fun

(** tkind one-arm switch *)
val tkw_mono = Lty.tkw_mono
val tkw_box = Lty.tkw_box
val tkw_seq = Lty.tkw_seq
val tkw_fun = Lty.tkw_fun

val tkc_int = Lty.tkc_int
val tkc_arg = Lty.tkc_arg

fun tc_bug tc msg =
    (with_pp (fn ppstm =>
      (PU.pps ppstm msg; PP.newline ppstm;
       PPLty.ppTyc 20 ppstm tc; PP.newline ppstm));
     bug "LtyExtern.tc_bug")

fun lt_bug lt msg =
    (with_pp (fn ppstm =>
      (PU.pps ppstm msg; PP.newline ppstm;
       PPLty.ppLty 20 ppstm lt; PP.newline ppstm));
     bug "LtyExtern.lt_bug")

val ltKindChk = LtyKindChk.ltKindCheckGen ()
val (tcKindChk,tcKindVer,teKindChk) = LtyKindChk.tcteKindCheckGen ()

val tkc_mono = LT.tkc_mono

(** instantiating a polymorphic type or an higher-order constructor *)
fun lt_inst (lt : lty, ts : tyc list) =
  let val nt = lt_whnm lt
   in case ((* lt_outX *) lt_out nt, ts)
        of (LT.LT_POLY(ks, b), ts) =>
             if length ks <> length ts
             then (with_pp (fn ppstm =>
                     (PU.pps ppstm "### arity error in lt_inst:\n|ks| = ";
                      PU.ppi ppstm (length ks);
                      PU.pps ppstm ", |ts| = "; PU.ppi ppstm (length ts);
                      PP.newline ppstm;
                      PU.pps ppstm "lt: ";
                      PP.openHOVBox ppstm (PP.Rel 0);
                      PPLty.ppLty 20 ppstm lt;
                      PP.closeBox ppstm;
                      PP.newline ppstm;
                      PU.pps ppstm "nt: ";
                      PP.openHOVBox ppstm (PP.Rel 0);
                      PPLty.ppLty 20 ppstm nt;
                      PP.closeBox ppstm;
                      PP.newline ppstm;
                      PU.pps ppstm "ts: ";
                      PPLty.ppList ppstm
                        {sep = ",",pp=PPLty.ppTyc 20}
                        ts;
                      PP.newline ppstm));
                   bug "lt_inst - arity mismatch")
             else
             let val nenv = LT.teCons(LT.Beta(0,ts,ks), LT.teEmpty)
(* (no kind env)                val _ = teKindChk(nenv,0,Lty.initTkEnv) *)
              in map (fn x => ltc_env(x, 1, 0, nenv)) b
             end
         | (_, []) => [nt]   (* this requires further clarifications !!! *)
         | (lt,ts) =>
           (with_pp (fn ppstm =>
              (PU.pps ppstm "lt_inst arg:"; PP.newline ppstm;
               PPLty.ppLty 20 ppstm (lt_inj lt); PP.newline ppstm;
               PU.pps ppstm "ts length: ";
               PU.ppi ppstm (length ts); PP.newline ppstm;
               PU.pps ppstm "ts head: ";
               PPLty.ppTyc 20 ppstm (hd ts); PP.newline ppstm));
            bug "incorrect lty instantiation in lt_inst")
  end

fun lt_pinst (lt : lty, ts : tyc list) =
  (case lt_inst (lt, ts) of [y] => y | _ => bug "unexpected lt_pinst")


exception KindChk = LtyKindChk.KindChk
exception LtyAppChk

exception TCENV = LK.TCENV
exception TeUnbound = LK.teUnbound2

val tcKindCheckGen : unit -> (tkindEnv -> tyc -> tkind) = LKC.tcKindCheckGen
val tcKindVerifyGen : unit -> (tkindEnv -> (tkind * tyc) -> unit)
    = LKC.tcKindVerifyGen
val ltKindCheckGen : unit -> (tkindEnv -> lty -> tkind) = LKC.ltKindCheckGen

(* lty application with kind-checking (exported) *)
fun lt_inst_chk_gen() = let
    val tkChk = LKC.tcKindVerifyGen()
    fun lt_inst_chk (lt : lty, ts : tyc list, kenv : tkindEnv) =
        let val nt = lt_whnm lt
        in (case ((* lt_outX *) lt_out nt, ts)
              of (LT.LT_POLY(ks, b), ts) =>
                 let val _ = ListPair.app (tkChk kenv) (ks, ts)
                     fun h x = ltc_env(x, 1, 0, teCons(Beta(0,ts,ks),teEmpty))
                 in map h b
                 end
               | (_, []) => [nt]    (* ? problematic *)
               | _ => raise LtyAppChk)
        end
in
    lt_inst_chk
end

(** a special lty application --- used inside the translate/specialize.sml *)
fun lt_sp_adj(ks, lt, ts, dist, bnl) =
    let fun h(abslevel, ol, nl, tenv) =
          if abslevel = 0 then ltc_env(lt, ol, nl, tenv)
          else if abslevel > 0 then
                 h(abslevel-1, ol+1, nl+1,
                   teCons(Lamb(nl,ks (* dbm ??? *)), tenv))
               else bug "unexpected cases in ltAdjSt"
        val btenv = teCons(Beta(0,ts,ks (* dbm ??? *)),teEmpty)
        val nt = h(dist, 1, bnl, btenv)
     in nt (* was lt_norm nt *)
    end

(** a special tyc application --- used in translate/specialize.sml *)
fun tc_sp_adj(ks, tc, ts, dist, bnl) =
    let fun h(abslevel, ol, nl, tenv) =
          if abslevel = 0 then tcc_env(tc, ol, nl, tenv)
          else if abslevel > 0 then
                 h(abslevel-1, ol+1, nl+1,
                   teCons(Lamb(nl,ks (* dbm ??? *)), tenv))
               else bug "unexpected cases in tcAdjSt"
        val btenv = teCons(Beta(0,ts,ks (* dbm ??? *)), teEmpty)
        val nt = h(dist, 1, bnl, btenv)
     in nt (* was tc_norm nt *)
    end

(** sinking the lty one-level down --- used in specialize.sml *)
fun lt_sp_sink (ks, lt, d, nd) =
    let fun h(abslevel, ol, nl, tenv) =
          if abslevel = 0 then ltc_env(lt, ol, nl, tenv)
          else if abslevel > 0 then
                 h(abslevel-1, ol+1, nl+1,
                   teCons(Lamb(nl,ks (* dbm ??? *)), tenv))
               else bug "unexpected cases in ltSinkSt"
        val nt = h(nd-d, 0, 1, teEmpty)
     in nt (* was lt_norm nt *)
    end

(** sinking the tyc one-level down --- used in specialize.sml *)
fun tc_sp_sink (ks, tc, d, nd) =
    let fun h(abslevel, ol, nl, tenv) =
          if abslevel = 0 then tcc_env(tc, ol, nl, tenv)
          else if abslevel > 0 then
                 h(abslevel-1, ol+1, nl+1, teCons(Lamb(nl,ks), tenv))
               else bug "unexpected cases in ltSinkSt"
        val nt = h(nd-d, 0, 1, teEmpty)
     in nt (* was tc_norm nt *)
    end

(** utility functions used in CPS *)
fun lt_iscont lt =
      (case lt_out lt
        of LT.LT_CONT _ => true
         | LT.LT_TYC tc =>
             (case tc_out tc of LT.TC_CONT _ => true | _ => false)
         | _ => false)

fun ltw_iscont (lt, f, g, h) =
      (case lt_out lt
        of LT.LT_CONT t => f t
         | LT.LT_TYC tc =>
             (case tc_out tc of LT.TC_CONT x => g x | _ => h lt)
         | _ => h lt)

(** other misc utility functions *)
fun tc_select(tc, i) =
  (case tc_out tc
    of LT.TC_TUPLE (_,zs) =>
         ((List.nth(zs, i)) handle _ => bug "wrong TC_TUPLE in tc_select")
     | _ => tc_bug tc "wrong TCs in tc_select")

fun lt_select(t, i) =
  (case lt_out t
    of LT.LT_STR ts =>
         ((List.nth(ts, i)) handle _ => bug "incorrect LT_STR in lt_select")
     | LT.LT_TYC tc => ltc_tyc(tc_select(tc, i))
     | _ => bug "incorrect lambda types in lt_select")

fun tc_swap t =
  (case (tc_out t)
    of LT.TC_ARROW (LT.FF_VAR (r1,r2), [s1], [s2]) =>
         tcc_arrow(LT.FF_VAR (r2,r1), [s2], [s1])
     | LT.TC_ARROW (LT.FF_FIXED, [s1], [s2]) =>
         tcc_arrow(LT.FF_FIXED, [s2], [s1])
     | _ => bug "unexpected tycs in tc_swap")

fun lt_swap t =
  (case (lt_out t)
    of (LT.LT_POLY (ks, [x])) => ltc_poly(ks, [lt_swap x])
     | (LT.LT_TYC x) => ltc_tyc(tc_swap x)
     | _ => bug "unexpected type in lt_swap")

(** functions that manipulate the FLINT function and record types *)
fun ltc_fkfun ({cconv=FL.CC_FCT, ...}: FL.fkind, atys, rtys) =
      ltc_fct (atys, rtys)
  | ltc_fkfun ({cconv=FL.CC_FUN fixed, ...}, atys, rtys) =
      ltc_arrow(fixed, atys, rtys)

fun ltd_fkfun (lty: lty) : lty list * lty list =
  if ltp_fct lty then ltd_fct lty
  else let val (_, atys, rtys) = ltd_arrow lty
        in (atys, rtys)
       end

fun ltc_rkind (FL.RK_TUPLE _, lts) = ltc_tuple lts
  | ltc_rkind (FL.RK_STRUCT, lts) = ltc_str lts
  | ltc_rkind (FL.RK_VECTOR t, _) = ltc_vector (ltc_tyc t)

fun ltd_rkind (lt, i) = lt_select (lt, i)

(****************************************************************************
 *             UTILITY FUNCTIONS USED BY POST-REPRESENTATION ANALYSIS       *
 ****************************************************************************)
(** find out what is the appropriate primop given a tyc *)
fun tc_upd_prim tc =
  let fun h(LT.TC_PRIM pt) = if PT.ubxupd pt then PO.UNBOXEDUPDATE else PO.UPDATE
        | h(LT.TC_TUPLE _ | LT.TC_ARROW _) = PO.UPDATE
        | h(LT.TC_FIX{family={size=1,gen=tc,params=ts,...},index=0}) =
            let val ntc = case ts of [] => tc
                                   | _ => tcc_app(tc, ts)
             in (case (tc_out ntc)
                  of LT.TC_FN([k],b) => h (tc_out b)
                   | _ => PO.UPDATE)
            end
        | h(LT.TC_SUM tcs) =
            let fun g (a::r) = if tc_eqv(a, tcc_unit) then g r else false
                  | g [] = true
             in if (g tcs) then PO.UNBOXEDUPDATE else PO.UPDATE
            end
        | h _ = PO.UPDATE
   in h(tc_out tc)
  end

(** tk_lty : tkind -> lty --- finds out the corresponding type for a tkind *)
fun tk_lty tk =
  (case tk_out tk
    of LT.TK_MONO => ltc_int
     | LT.TK_BOX => ltc_int
     | LT.TK_SEQ ks => ltc_tuple (map tk_lty ks)
     | LT.TK_FUN (ks, k) =>
         ltc_arrow(ffc_fixed, [ltc_tuple(map tk_lty ks)], [tk_lty k]))


(* tnarrow_gen : unit -> ((tyc -> tyc) * (lty -> lty) * (unit->unit)) *)
fun tnarrow_gen () =
  let fun tcNarrow tcf t =
        (case (tc_out t)
          of LT.TC_PRIM pt =>
               if PT.isvoid pt then tcc_void else t
           | LT.TC_TUPLE (_, tcs) => tcc_tuple (map tcf tcs)
           | LT.TC_ARROW (r, ts1, ts2) =>
               tcc_arrow(ffc_fixed, map tcf ts1, map tcf ts2)
           | _ => tcc_void)

      fun ltNarrow (tcf, ltf) t =
        (case lt_out t
          of LT.LT_TYC tc => ltc_tyc (tcf tc)
           | LT.LT_STR ts => ltc_str (map ltf ts)
           | LT.LT_FCT (ts1, ts2) => ltc_fct(map ltf ts1, map ltf ts2)
           | LT.LT_POLY (ks, xs) =>
               ltc_fct([ltc_str (map tk_lty ks)], map ltf xs)
           | LT.LT_CONT _ => bug "unexpected CNTs in ltNarrow"
           | LT.LT_IND _ => bug "unexpected INDs in ltNarrow"
           | LT.LT_ENV _ => bug "unexpected ENVs in ltNarrow")

      val {tc_map, lt_map} = LtyDict.tmemo_gen {tcf=tcNarrow, ltf=ltNarrow}
   in (tc_map o tc_norm, lt_map o lt_norm, fn ()=>())
  end (* function tnarrow_gen *)

(* twrap_gen   : bool -> ((tyc -> tyc) * (lty -> lty) *
 *                        (tyc -> tyc) * (lty -> lty) * (unit -> unit))
 *)
fun twrap_gen bbb =
  let fun tc_wmap (w, u) t =
        (case (tc_out t)
          of (LT.TC_VAR _ | LT.TC_NVAR _) => t
           | LT.TC_PRIM pt => if PT.unboxed pt then tcc_wrap t else t
           | LT.TC_FN (ks, tc) => tcc_fn(ks, w tc) (* impossible case *)
           | LT.TC_APP (tc, tcs) => tcc_app(w tc, map w tcs)
           | LT.TC_SEQ tcs => tcc_seq(map w tcs)
           | LT.TC_PROJ (tc, i) => tcc_proj(w tc, i)
           | LT.TC_SUM tcs => tcc_sum (map w tcs)
           | LT.TC_FIX{family={size=n,names,gen=tc,params=ts},index=i} =>
               tcc_fix((n, names, tc_norm (u tc), map w ts), i)

           | LT.TC_TUPLE (_, ts) => tcc_wrap(tcc_tuple (map w ts)) (* ? *)
           | LT.TC_ARROW (LT.FF_VAR(b1,b2), ts1, ts2) =>
               let val nts1 =    (* too specific ! *)
                     (case ts1 of [t11,t12] => [w t11, w t12]
                                | _ => [w (LK.tc_autotuple ts1)])
                   val nts2 = [w (LK.tc_autotuple ts2)]
                   val nt = tcc_arrow(ffc_fixed, nts1, nts2)
                in if b1 then nt else tcc_wrap nt
               end
           | LT.TC_ARROW (LT.FF_FIXED, _, _) =>
                bug "unexpected TC_FIXED_ARROW in tc_umap"
           | LT.TC_TOKEN (k, t) => bug "unexpected token tyc in tc_wmap"
           | LT.TC_BOX _ => bug "unexpected TC_BOX in tc_wmap"
           | LT.TC_ABS _ => bug "unexpected TC_ABS in tc_wmap"
           | _ => bug "unexpected other tycs in tc_wmap")

      fun tc_umap (u, w) t =
        (case (tc_out t)
          of (LT.TC_VAR _ | LT.TC_NVAR _ | LT.TC_PRIM _) => t
           | LT.TC_FN (ks, tc) => tcc_fn(ks, u tc) (* impossible case *)
           | LT.TC_APP (tc, tcs) => tcc_app(u tc, map w tcs)
           | LT.TC_SEQ tcs => tcc_seq(map u tcs)
           | LT.TC_PROJ (tc, i) => tcc_proj(u tc, i)
           | LT.TC_SUM tcs => tcc_sum (map u tcs)
           | LT.TC_FIX{family={size=n,names,gen=tc,params=ts},index=i} =>
               tcc_fix((n, names, tc_norm (u tc), map w ts), i)

           | LT.TC_TUPLE (rk, tcs) => tcc_tuple(map u tcs)
           | LT.TC_ARROW (LT.FF_VAR(b1,b2), ts1, ts2) =>
               tcc_arrow(ffc_fixed, map u ts1, map u ts2)
           | LT.TC_ARROW (LT.FF_FIXED, _, _) =>
               bug "unexpected TC_FIXED_ARROW in tc_umap"
           | LT.TC_PARROW _ => bug "unexpected TC_PARROW in tc_umap"

           | LT.TC_BOX _ => bug "unexpected TC_BOX in tc_umap"
           | LT.TC_ABS _ => bug "unexpected TC_ABS in tc_umap"
           | LT.TC_TOKEN (k, t) =>
               if LK.token_eq(k, LK.wrap_token) then
                 bug "unexpected TC_WRAP in tc_umap"
               else tc_inj (LT.TC_TOKEN (k, u t))

           | _ => bug "unexpected other tycs in tc_umap")

      fun lt_umap (tcf, ltf) t =
        (case (lt_out t)
          of LT.LT_TYC tc => ltc_tyc (tcf tc)
           | LT.LT_STR ts => ltc_str (map ltf ts)
           | LT.LT_FCT (ts1, ts2) => ltc_fct(map ltf ts1, map ltf ts2)
           | LT.LT_POLY (ks, xs) => ltc_poly(ks, map ltf xs)
           | LT.LT_CONT _ => bug "unexpected CNTs in lt_umap"
           | LT.LT_IND _ => bug "unexpected INDs in lt_umap"
           | LT.LT_ENV _ => bug "unexpected ENVs in lt_umap")

      val {tc_wmap=tcWrap, tc_umap=tcMap, lt_umap=ltMap, cleanup} =
        LtyDict.wmemo_gen{tc_wmap=tc_wmap, tc_umap=tc_umap, lt_umap=lt_umap}

      fun ltWrap x =
        ltw_tyc (x, (fn tc => ltc_tyc (tcWrap tc)),
                     fn _ => bug "unexpected case in ltWrap")

   in (tcWrap o tc_norm, ltWrap o lt_norm,
       tcMap o tc_norm, ltMap o lt_norm, cleanup)
  end


(************************************************************************
 *            SUBSTITION OF NAMED VARS IN A TYC/LTY                     *
 ************************************************************************)
structure TcDict = BinaryMapFn
                     (struct
                        type ord_key = tyc
                        val compare = LT.tc_cmp
		      end)

structure LtDict = BinaryMapFn
                       (struct
                           type ord_key = lty
                           val compare = Lty.lt_cmp
                       end)

fun tc_nvar_elim_gen() = let
    val dict = ref (TcDict.empty)

    fun tc_nvar_elim s d tyc =
        case LK.tc_nvars tyc of
            [] => tyc                   (* nothing to elim *)
          | _ =>
    let
        (* encode the tyc and the depth for memoization
         * using tcc_proj *)
        val tycdepth = tcc_proj (tyc, d)
    in
        case TcDict.find(!dict, tycdepth) of
            SOME t => t                 (* hit! *)
          | NONE => let                 (* must recompute *)
                val r = tc_nvar_elim s d (* default recursive invoc. *)
                val rs = map r          (* recursive invocation on list *)
                val t =
                    case tc_out tyc of
                        LT.TC_NVAR tvar =>
                            (case s (tvar, d) of
                                 SOME t => t
                               | NONE => tyc)
                      | LT.TC_VAR _ => tyc
                      | LT.TC_PRIM _ => tyc
                      | LT.TC_FN (tks, t) =>
                            tcc_fn (tks, tc_nvar_elim s (DI.next d) t)
                      | LT.TC_APP (t, ts) =>
                            tcc_app (r t, rs ts)
                      | LT.TC_SEQ ts =>
                            tcc_seq (rs ts)
                      | LT.TC_PROJ (t, i) =>
                            tcc_proj (r t, i)
                      | LT.TC_SUM ts =>
                            tcc_sum (rs ts)
                      | LT.TC_FIX {family={size,names,gen,params},index} =>
                            tcc_fix ((size,names,r gen,rs params),index)
                      | LT.TC_TUPLE (rf,ts) =>
                            tcc_tuple (rs ts)
                      | LT.TC_ARROW (ff, ts, ts') =>
                            tcc_arrow (ff, rs ts, rs ts')
                      | LT.TC_PARROW (t, t') =>
                            tcc_parrow (r t, r t')
                      | LT.TC_BOX t =>
                            tcc_box (r t)
                      | LT.TC_ABS t =>
                            tcc_abs (r t)
                      | LT.TC_TOKEN (tok, t) =>
                            tc_inj (LT.TC_TOKEN (tok, r t))
                      | LT.TC_CONT ts =>
                            tcc_cont (rs ts)
                      | LT.TC_IND _ =>
                            bug "unexpected TC_IND in tc_nvar_elim"
                      | LT.TC_ENV _ =>
                            bug "unexpected TC_ENV in tc_nvar_elim"
            in
                dict := TcDict.insert(!dict, tycdepth, t);
                t
            end
    end (* tc_nvar_elim *)
in
    tc_nvar_elim
end

fun lt_nvar_elim_gen() = let
    val dict = ref (LtDict.empty)
    val tc_nvar_elim = tc_nvar_elim_gen()

    fun lt_nvar_elim s d lty =
        case LK.lt_nvars lty
          of [] => lty                   (* nothing to elim *)
           | _ =>
    let
        (* encode the lty and depth info using LT_ENV
         * (only first 2 args are useful) *)
        val ltydepth = lt_inj (LT.LT_ENV (lty, d, 0, LT.teEmpty))
    in
        case LtDict.find(!dict, ltydepth) of
            SOME t => t                 (* hit! *)
          | NONE => let                 (* must recompute *)
                val r = lt_nvar_elim s d (* default recursive invoc. *)
                val rs = map r          (* recursive invocation on list *)
                val t =
                    case lt_out lty of
                        LT.LT_TYC t =>
                            ltc_tyc (tc_nvar_elim s d t)
                      | LT.LT_STR ts =>
                            ltc_str (rs ts)
                      | LT.LT_FCT (ts, ts') =>
                            ltc_fct (rs ts, rs ts')
                      | LT.LT_POLY (tks, ts) =>
                            ltc_poly (tks,
                                      map (lt_nvar_elim s (DI.next d)) ts)
                      | LT.LT_CONT ts =>
                            ltc_cont (rs ts)
                      | LT.LT_IND _ =>
                            bug "unexpected LT_IND in lt_nvar_elim"
                      | LT.LT_ENV _ =>
                            bug "unexpected LT_ENV in lt_nvar_elim"
            in
                dict := LtDict.insert(!dict, ltydepth, t);
                t
            end
    end (* lt_nvar_elim *)
in
    lt_nvar_elim
end (* lt_nvar_elim_gen *)

(************************************************************)

type smap = (tvar * tyc) list

(* is the intersection of two sorted lists non-nil? *)
fun intersectionNonEmpty(nil,_:tvar list) = false
  | intersectionNonEmpty(_,nil) = false
  | intersectionNonEmpty(s1 as (h1:tvar,_)::t1, s2 as h2::t2) =
        case LambdaVar.compare (h1, h2) of
            LESS => intersectionNonEmpty(t1, s2)
          | GREATER => intersectionNonEmpty(s1, t2)
          | EQUAL => true

fun searchSubst (tv:tvar, s) =
    let fun h [] = NONE
          | h ((tv':tvar,tyc)::s) =
                case LambdaVar.compare (tv, tv') of
                    LESS => NONE
                  | GREATER => h s
                  | EQUAL => SOME tyc
    in h s
    end

fun tc_nvar_subst_gen() = let
    val dict = ref (TcDict.empty)

    fun tc_nvar_subst subst = let
        fun loop tyc =
        (* check if substitution overlaps with free vars list *)
        (case intersectionNonEmpty(subst, LK.tc_nvars tyc) of
             false => tyc               (* nothing to subst *)
           | true =>
             (* next check the memoization table *)
             (case TcDict.find(!dict, tyc) of
                  SOME t => t           (* hit! *)
                | NONE =>
              let                       (* must recompute *)
                  val t =
                    case tc_out tyc of
                        LT.TC_NVAR tv =>
                            (case searchSubst(tv,subst) of
                                 SOME t => t
                               | NONE => tyc
                                 )
                      | LT.TC_VAR _ => tyc
                      | LT.TC_PRIM _ => tyc
                      | LT.TC_FN (tks, t) =>
                            tcc_fn (tks, loop t)
                      | LT.TC_APP (t, ts) =>
                            tcc_app (loop t, map loop ts)
                      | LT.TC_SEQ ts =>
                            tcc_seq (map loop ts)
                      | LT.TC_PROJ (t, i) =>
                            tcc_proj (loop t, i)
                      | LT.TC_SUM ts =>
                            tcc_sum (map loop ts)
                      | LT.TC_FIX{family={size,names,gen,params},index} =>
                            tcc_fix ((size, names, loop gen, map loop params),index)
                      | LT.TC_TUPLE (rf,ts) =>
                            tcc_tuple (map loop ts)
                      | LT.TC_ARROW (ff, ts, ts') =>
                            tcc_arrow (ff, map loop ts, map loop ts')
                      | LT.TC_PARROW (t, t') =>
                            tcc_parrow (loop t, loop t')
                      | LT.TC_BOX t =>
                            tcc_box (loop t)
                      | LT.TC_ABS t =>
                            tcc_abs (loop t)
                      | LT.TC_TOKEN (tok, t) =>
                            tc_inj (LT.TC_TOKEN (tok, loop t))
                      | LT.TC_CONT ts =>
                            tcc_cont (map loop ts)
                      | LT.TC_IND _ =>
                            bug "unexpected TC_IND in substTyc"
                      | LT.TC_ENV _ =>
                            bug "unexpected TC_ENV in substTyc"
              in
                  (* update memoization table *)
                  dict := TcDict.insert(!dict, tyc, t);
                  t
              end
                  )) (* end cases *)
    in loop
    end (* tc_nvar_subst *)
in tc_nvar_subst
end (* tc_nvar_subst_gen *)

fun lt_nvar_subst_gen() = let
    val dict = ref (LtDict.empty)
    val tc_nvar_subst' = tc_nvar_subst_gen()

    fun lt_nvar_subst subst = let
        val tc_nvar_subst = tc_nvar_subst' subst

        fun loop lty =
        (* check if there are any free type variables first *)
        (case intersectionNonEmpty(subst, LK.lt_nvars lty) of
             false => lty                  (* nothing to subst *)
           | true =>
             (* next check the memoization table *)
             (case LtDict.find(!dict, lty) of
                  SOME t => t           (* hit! *)
                | NONE =>
              let                       (* must recompute *)
                  val t =
                    case lt_out lty of
                        LT.LT_TYC t =>
                            ltc_tyc (tc_nvar_subst t)
                      | LT.LT_STR ts =>
                            ltc_str (map loop ts)
                      | LT.LT_FCT (ts, ts') =>
                            ltc_fct (map loop ts, map loop ts')
                      | LT.LT_POLY (tks, ts) =>
                            ltc_poly (tks, map loop ts)
                      | LT.LT_CONT ts =>
                            ltc_cont (map loop ts)
                      | LT.LT_IND _ =>
                            bug "unexpected LT_IND in lt_nvar_elim"
                      | LT.LT_ENV _ =>
                            bug "unexpected LT_ENV in lt_nvar_elim"
              in
                  (* update memoization table *)
                  dict := LtDict.insert(!dict, lty, t);
                  t
              end
                  )) (* end cases *)
    in loop
    end (* lt_nvar_subst *)
in lt_nvar_subst
end (* lt_nvar_subst_gen *)

(************************************************************)

(** building up a polymorphic type by abstracting over a
 ** list of named vars
 **)
type tvoffs = (tvar * int) list

fun intersect(nil, _:tvar list) = nil
  | intersect(_, nil) = nil
  | intersect(s1 as (h1:tvar,n)::t1, s2 as h2::t2) =
        case LambdaVar.compare (h1, h2) of
            LESS => intersect(t1, s2)
          | GREATER => intersect(s1, t2)
          | EQUAL => (h1,n) :: intersect(t1, t2)

(* val s_iter = Stats.makeStat "Cvt Iterations" *)
(* val s_hits = Stats.makeStat "Cvt Hits in dict" *)
(* val s_cuts = Stats.makeStat "Cvt Freevar cutoffs" *)

(* val s_tvoffs = Stats.makeStat "Cvt tvoffs length" *)
(* val s_nvars = Stats.makeStat "Cvt free nvars length" *)

fun tc_nvar_cvt_gen() = let
    val dict = ref (TcDict.empty)

    fun tc_nvar_cvt (tvoffs:tvoffs) d tyc =
        ((* Stats.addStat s_iter 1; *)
         (* Stats.addStat s_tvoffs (length tvoffs); *)
         (* Stats.addStat s_nvars (length (LK.tc_nvars tyc)); *)
        (* check if substitution overlaps with free vars list *)
        case intersect(tvoffs, LK.tc_nvars tyc) of
            [] => ((* Stats.addStat s_cuts 1; *)
                   tyc           (* nothing to cvt *)
                   )
          | tvoffs =>
    let
        (* encode the tyc and the depth for memoization
         * using tcc_proj *)
        val tycdepth = tcc_proj (tyc, d)
    in
        case TcDict.find(!dict, tycdepth) of
            SOME t => ((* Stats.addStat s_hits 1; *)
                       t                 (* hit! *)
                       )
          | NONE => let                 (* must recompute *)
                val r = tc_nvar_cvt tvoffs d (* default recursive invoc. *)
                val rs = map r          (* recursive invocation on list *)
                val t =
                    case tc_out tyc of
                        LT.TC_NVAR tvar =>
                            (case searchSubst(tvar,tvoffs) of
                                 SOME i => tcc_var (d, i)
                               | NONE => tyc)
                      | LT.TC_VAR _ => tyc
                      | LT.TC_PRIM _ => tyc
                      | LT.TC_FN (tks, t) =>
                            tcc_fn (tks, tc_nvar_cvt tvoffs (DI.next d) t)
                      | LT.TC_APP (t, ts) =>
                            tcc_app (r t, rs ts)
                      | LT.TC_SEQ ts =>
                            tcc_seq (rs ts)
                      | LT.TC_PROJ (t, i) =>
                            tcc_proj (r t, i)
                      | LT.TC_SUM ts =>
                            tcc_sum (rs ts)
                      | LT.TC_FIX{family={size,names,gen,params},index} =>
                            tcc_fix ((size, names, r gen, rs params), index)
                      | LT.TC_TUPLE (rf,ts) =>
                            tcc_tuple (rs ts)
                      | LT.TC_ARROW (ff, ts, ts') =>
                            tcc_arrow (ff, rs ts, rs ts')
                      | LT.TC_PARROW (t, t') =>
                            tcc_parrow (r t, r t')
                      | LT.TC_BOX t =>
                            tcc_box (r t)
                      | LT.TC_ABS t =>
                            tcc_abs (r t)
                      | LT.TC_TOKEN (tok, t) =>
                            tc_inj (LT.TC_TOKEN (tok, r t))
                      | LT.TC_CONT ts =>
                            tcc_cont (rs ts)
                      | LT.TC_IND _ =>
                            bug "unexpected TC_IND in tc_nvar_cvt"
                      | LT.TC_ENV _ =>
                            bug "unexpected TC_ENV in tc_nvar_cvt"
            in
                dict := TcDict.insert(!dict, tycdepth, t);
                t
            end
    end (* tc_nvar_cvt *)
        )
in
    tc_nvar_cvt
end (* tc_nvar_cvt_gen *)


fun lt_nvar_cvt_gen() = let
    val dict = ref (LtDict.empty)
    val tc_nvar_cvt = tc_nvar_cvt_gen()

    fun lt_nvar_cvt tvoffs d lty =
        (* check if substitution overlaps with free vars list *)
        case intersect(tvoffs, LK.lt_nvars lty) of
            [] => lty                (* nothing to cvt *)
          | tvoffs =>
    let
        (* encode the lty and depth info using LT_ENV
         * (only first 2 args are useful) *)
        val ltydepth = lt_inj (LT.LT_ENV (lty, d, 0, LT.teEmpty))
    in
        case LtDict.find(!dict, ltydepth) of
            SOME t => t                 (* hit! *)
          | NONE => let                 (* must recompute *)
                val r = lt_nvar_cvt tvoffs d (* default recursive invoc. *)
                val rs = map r          (* recursive invocation on list *)
                val t =
                    case lt_out lty of
                        LT.LT_TYC t =>
                            ltc_tyc (tc_nvar_cvt tvoffs d t)
                      | LT.LT_STR ts =>
                            ltc_str (rs ts)
                      | LT.LT_FCT (ts, ts') =>
                            ltc_fct (rs ts, rs ts')
                      | LT.LT_POLY (tks, ts) =>
                            ltc_poly (tks,
                                      map (lt_nvar_cvt tvoffs (DI.next d)) ts)
                      | LT.LT_CONT ts =>
                            ltc_cont (rs ts)
                      | LT.LT_IND _ =>
                            bug "unexpected LT_IND in lt_nvar_cvt"
                      | LT.LT_ENV _ =>
                            bug "unexpected LT_ENV in lt_nvar_cvt"
            in
                dict := LtDict.insert(!dict, ltydepth, t);
                t
            end
    end (* lt_nvar_cvt *)
in
    lt_nvar_cvt
end (* lt_nvar_cvt_gen *)

(* make a type abstraction from nvar to lty *)
fun lt_nvpoly(tvks, lt) =
    let
	fun frob ((tv,k)::tvks, n, ks, tvoffs) =
	    frob (tvks, n+1, k::ks, (tv,n)::tvoffs)
	  | frob ([], _, ks, tvoffs) =
	    (rev ks, rev tvoffs)

	val (ks, tvoffs) = frob (tvks, 0, [], [])
	fun gt ((tvar1,_), (tvar2,_)) = LambdaVar.>(tvar1, tvar2)
	val tvoffs = ListMergeSort.sort gt tvoffs

	(* temporarily gen() *)
	val ltSubst = lt_nvar_cvt_gen() tvoffs (DI.next DI.top)
    in ltc_poly(ks, map ltSubst lt)
    end

end (* top-level local *)
end (* structure LtyExtern *)
