(* typeoper.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TYPEOPER =
sig
  type kenv
  type tkind = LtyDef.tkind
  type tyc   = LtyDef.tyc
  type lty   = LtyDef.lty
  type tvar  = LtyDef.tvar
  type lvar  = LambdaVar.lvar
  type lexp  = FLINT.lexp
  type value = FLINT.value

  val initKE : kenv

  val tkAbs  : kenv * (tvar * tkind) list * lvar ->
                  (kenv * (lexp * lexp -> lexp))
  val tcLexp : kenv -> tyc -> lexp
  val tsLexp : kenv * tyc list -> lexp

  val utgc   : tyc * kenv * tyc -> value -> lexp
  val utgd   : tyc * kenv * tyc -> value -> lexp
  val tgdc   : int * tyc * kenv * tyc -> value -> lexp
  val tgdd   : int * tyc * kenv * tyc -> value -> lexp

  val mkwrp  : tyc * kenv * bool * tyc -> lexp -> lexp
  val mkuwp  : tyc * kenv * bool * tyc -> lexp -> lexp

end (* signature TYPEOPER *)

signature Outcome =
sig
    datatype outcome = YES
		     | NO
		     | MAYBE of FLINT.lexp
end

structure OT:Outcome = RuntimeType

structure TypeOper : TYPEOPER =
struct

local structure DI = DebIndex
      structure LT = LtyExtern
      structure LV = LambdaVar
      structure PO = Primop
      structure PT = PrimTyc
      structure BT = BasicTypes
      structure TP = Types
      structure RT = RuntimeType
      open Lty LtyKernel FLINT OT
in

type tkind = tkind
type tyc   = tyc
type lty   = lty
type tvar  = LtyDef.tvar
type lvar  = LV.lvar
type lexp  = lexp
type value = value
type kenv = RT.kenv

fun bug s = ErrorMsg.impossible ("TypeOper: " ^ s)
fun say (s : string) = Control.Print.say s
fun mkv _ = LV.mkLvar()
val ident = fn le => le
val fkfun = {isrec=NONE, known=false, inline=IH_ALWAYS, cconv=CC_FUN LT.ffc_fixed}

fun mkarw(ts1, ts2) = LT.tcc_arrow(LT.ffc_fixed, ts1, ts2)

val lt_arw = LT.ltc_tyc o LT.tcc_arrow

fun wty tc = (NONE, PO.WRAP, lt_arw(LT.ffc_fixed, [tc], [LT.tcc_void]), [])
fun uwty tc = (NONE, PO.UNWRAP, lt_arw(LT.ffc_fixed, [LT.tcc_void], [tc]), [])

fun FU_WRAP(tc, vs, v, e) = PRIMOP(wty tc, vs, v, e)
fun FU_UNWRAP(tc, vs, v, e) = PRIMOP(uwty tc, vs, v, e)
val FU_rk_tuple = FlintUtil.rk_tuple

fun WRAP(t, u) =
  let val v = mkv()
   in FU_WRAP(t, [u], v, RET[VAR v])
  end

fun UNWRAP(t, u) =
  let val v = mkv()
   in FU_UNWRAP(t, [u], v, RET[VAR v])
  end

(****************************************************************************
 *                  UTILITY FUNCTIONS AND CONSTANTS                         *
 ****************************************************************************)

fun split(RET [v]) = (v, ident)
  | split x = let val v = mkv()
               in (VAR v, fn z => LET([v], x, z))
              end

fun SELECTg(i, e) =
  let val (v, hdr) = split e
      val x = mkv()
   in hdr(SELECT(v, i, x, RET [VAR x]))
  end

fun FNg(vts, e) =
  let val f = mkv()
   in FIX([(fkfun, f, vts, e)], RET[VAR f])
  end

fun SELECTv(i, u) =
  let val x = mkv()
   in SELECT(u, i, x, RET [VAR x])
  end

fun APPg(e1, e2) =
  let val (v1, h1) = split e1
      val (v2, h2) = split e2
   in h1(h2(APP(v1, [v2])))
  end

fun RECORDg es =
  let fun f ([], vs, hdr) =
               let val x = mkv()
                in hdr(RECORD(FU_rk_tuple, rev vs, x, RET[VAR x]))
               end
        | f (e::r, vs, hdr) =
              let val (v, h) = split e
               in f(r, v::vs, hdr o h)
              end
   in f(es, [], ident)
  end

fun SRECORDg es =
  let fun f ([], vs, hdr) =
               let val x = mkv()
                in hdr(RECORD(RK_STRUCT, rev vs, x, RET[VAR x]))
               end
        | f (e::r, vs, hdr) =
              let val (v, h) = split e
               in f(r, v::vs, hdr o h)
              end
   in f(es, [], ident)
  end

fun WRAPg (z, b, e) =
  let val (v, h) = split e
   in h(WRAP(z, v))
  end

fun UNWRAPg (z, b, e) =
  let val (v, h) = split e
   in h(UNWRAP(z, v))
  end

fun WRAPcast (z, b, e) =
  let val (v, h) = split e
      val pt = LT.ltc_arrow(LT.ffc_fixed, [LT.ltc_tyc z], [LT.ltc_void])
      val pv = (NONE,PO.CAST,pt,[])
      val x = mkv()
   in h(PRIMOP(pv, [v], x, RET[VAR x]))
  end

fun UNWRAPcast (z, b, e) =
  let val (v, h) = split e
      val pt = LT.ltc_arrow(LT.ffc_fixed, [LT.ltc_void], [LT.ltc_tyc z])
      val pv = (NONE,PO.CAST,pt,[])
      val x = mkv()
   in h(PRIMOP(pv, [v], x, RET[VAR x]))
  end

fun SWITCHg (e, s, ce, d) =
  let val (v, h) = split e
   in h(SWITCH(v, s, ce, d))
  end

fun COND(u,e1,e2) = u(e1,e2)


(****************************************************************************
 *                           KIND ENVIRONMENTS                              *
 ****************************************************************************)

fun addKE(kenv, vs, ks) = RT.addKE


(****************************************************************************
 *                            MAIN FUNCTIONS                                *
 ****************************************************************************)

(* val tkAbsGen : kenv * lvar list * tkind list * lvar * fkind
                  -> kenv * ((lexp *lexp) -> lexp) *)
(* val tkAbsGen = RT.tkAbsGen *)


(* val tkAbs: kenv * (tvar * tkind) list -> kenv * (lexp * lexp -> lexp) *)
val tkAbs = RT.tkAbs

(* val tkTfn: kenv * tkind list -> kenv * (lexp -> lexp) *)
val tkTfn = RT.tkTfn

val ieqLexp = RT.ieqLexp

val iaddLexp = RT.iaddLexp


val tovalue = RT.tovalue
val tcode_void = RT.tcode_void
val tcode_record = RT.tcode_record
val tcode_pair = RT.tcode_pair
val tcode_fpair = RT.tcode_fpair
val tcode_real = RT.tcode_real
val tcode_realN = RT.tcode_realN


(* tcLexp maps TC_VAR to proper lvars, TC_PRIM to proper constants *)
(* val tcLexp : kenv -> tyc -> lexp *)

val initKE = RT.initKE

val tcLexp = RT.rtLexp
val tsLexp = RT.tsLexp

val isFloat  = RT.isFloat

val isPair = RT.isPair

fun tagInt i = INT{ival = IntInf.fromInt i, ty = Target.defaultIntSz}

(****************************************************************************
 *                      TYPED INTERPRETATION OF UNTAGGED                    *
 ****************************************************************************)

(** tc is of kind Omega; this function tests whether tc can be a tagged int ? *)
fun tcTag (kenv, tc) =
  let fun loop x =     (* a lot of approximations in this function *)
	(case (tc_out x)
	  of (TC_PRIM pt) => if PT.unboxed pt then NO else YES
                (* if PT.ubxupd pt then YES else NO *)
		    (* this is just an approximation *)
	   | (TC_TUPLE (_, [])) => YES
	   | (TC_TUPLE (_, ts)) => NO
	   | (TC_ARROW (_,tc1,tc2)) => YES (* NO *)
	   | (TC_ABS tx) => loop tx
	   | (TC_TOKEN(_,tx)) => loop tx
	   | (TC_FIX _) => YES
	   | (TC_APP(tx, _)) =>
		(case tc_out tx
		  of (TC_APP _ | TC_PROJ _ | TC_VAR _) =>
		       MAYBE (tcLexp kenv x)
		   | _ => YES)
	   | _ => (MAYBE (tcLexp kenv x)))
   in loop tc
  end (* function tcTag *)

(* val utgc : tyc * kenv * tyc -> value -> lexp *)
fun utgc (tc, kenv, rt) =
  (case tcTag(kenv, tc)
    of YES => (fn u => let val v = mkv()
                        in RECORD(FU_rk_tuple, [u], v,
                             WRAP(LT.tcc_tuple[rt], VAR v))
                       end)
     | NO => (fn u => WRAP(rt, u))
     | MAYBE ne =>
	 (fn u => let val v = mkv()
                      val hh = ieqLexp(ne, tcode_void)
                   in COND(hh, RECORD(FU_rk_tuple, [u], v,
                                      WRAP(LT.tcc_tuple[rt], VAR v)),
                               WRAP(rt, u))
           	  end))

(* val utgd : tyc * kenv * tyc -> value -> lexp *)
fun utgd (tc, kenv, rt) =
  (case tcTag(kenv, tc)
    of YES => (fn u => let val v = mkv() and z = mkv()
                        in FU_UNWRAP(LT.tcc_tuple [rt], [u], v,
                               SELECT(VAR v, 0, z, RET[VAR z]))
                       end)
     | NO => (fn u => UNWRAP(rt, u))
     | MAYBE ne =>
          (fn u => let val v = mkv() and z = mkv()
                       val hh = ieqLexp(ne, tcode_void)
                    in COND(hh, FU_UNWRAP(LT.tcc_tuple [rt], [u], v,
                               SELECT(VAR v, 0, z, RET[VAR z])),
                            UNWRAP(rt, u))
                   end))

(* val tgdc : int * tyc * kenv * tyc -> value -> lexp *)
fun tgdc (i, tc, kenv, rt) =
  let val nt = LT.tcc_tuple [LT.tcc_int, rt]
   in fn u => let val x = mkv()
               in RECORD(FU_rk_tuple, [tagInt i, u], x, WRAP(nt, VAR x))
              end
  end

(* val tgdd : int * tyc * kenv * tyc -> value -> lexp *)
fun tgdd (i, tc, kenv, rt) =
  let val nt = LT.tcc_tuple [LT.tcc_int, rt]
   in fn u => let val x = mkv() and v = mkv()
               in FU_UNWRAP(nt, [u], x, SELECT(VAR x, 1, v, RET[VAR v]))
              end
  end

(****************************************************************************
 *                      TYPED INTERPRETATION OF FP RECORD                   *
 ****************************************************************************)
(** tc is a ground tyc of kind Omega, only record types and arrow types are
    interesting for the time being. *)
(** all of these wrappers probably should be lifted to the top of the
    program, otherwise we may run into space blow-up ! *)
(* val tcCoerce : kenv * tyc * bool * bool -> (lexp -> lexp) option *)
fun tcCoerce (kenv, tc, nt, wflag, b) =
  (case (tc_out tc, tc_out nt)
    of (TC_TUPLE (_, ts), _) =>
	 let fun h([], i, e, el, 0) = NONE
	       | h([], i, e, el, res) =
		   let val w = mkv()
		       val wx = VAR w
		       fun g(i, NONE) =  SELECTv(i, wx)
			 | g(i, SOME _) =
			     if wflag then
			       UNWRAPg(LT.tcc_real, b, SELECTv(i, wx))
			     else WRAPg(LT.tcc_real, b, SELECTv(i, wx))

		       val ntc = LT.tcc_tuple(map (fn _ => LT.tcc_real) ts)

		       val ne = RECORDg (map g (rev el))
		       val test = ieqLexp(e, tcode_realN res)

		       fun hdr0 xe =
			 if wflag then
			   COND(test, LET([w], xe, WRAPcast(ntc, b, ne)),
				      WRAPcast(nt, b, xe))
			 else COND(test, LET([w], UNWRAPcast(ntc, b, xe), ne),
					 UNWRAPcast(nt, b, xe))

		       fun hdr (xe as (RET[(VAR _)])) = hdr0 xe
			 | hdr xe = let val z = mkv()
				     in LET([z], xe, hdr0 (RET[VAR z]))
				    end
		    in SOME hdr
		   end
	       | h(a::r, i, e, el, res) =
		   (case isFloat(kenv, a)
		     of NO => NONE
		      | YES => h(r, i+1, e, (i,NONE)::el, res)
		      | MAYBE z => h(r, i+1, iaddLexp(e, z),
				     (i, SOME a)::el, res+1))

	  in h(ts, 0, RET[tagInt 0], [], 0)
	 end
     | (TC_ARROW _, _) => (* (tc1, tc2) => *)
        let val (tc1, _) = LT.tcd_parrow tc
            val (_, tc2) = LT.tcd_parrow nt
         in (case isPair(kenv, tc1)
              of (YES | NO) => NONE
               | (MAYBE e) =>
                 let val w = mkv()
                     val test1 = ieqLexp(RET[(VAR w)], tcode_pair)
                     val test2 = ieqLexp(RET[(VAR w)], tcode_fpair)
                     val m = mkv() and m2 = mkv()
                     val n = mkv() and n2 = mkv()

                     val tc_real = LT.tcc_real
                     val tc_breal = LT.tcc_void (* LT.tcc_wrap tc_real *)
                     val lt_breal = LT.ltc_tyc tc_breal
                     val tc_void = LT.tcc_void
                     val lt_void = LT.ltc_void
                     val tc_pair = LT.tcc_tuple [tc_void, tc_void]
                     val tc_fpair = LT.tcc_tuple [tc_real, tc_real]
                     val tc_bfpair = LT.tcc_tuple [tc_breal, tc_breal]
                     val lt_pair = LT.ltc_tyc tc_pair
                     val lt_fpair = LT.ltc_tyc tc_fpair
                     val lt_bfpair = LT.ltc_tyc tc_bfpair
                     val ident = fn le => le

                     val (argt1, body1, hh1) =
                       if wflag then (* wrapping *)
                         ([(m,lt_void),(m2,lt_void)],
                          fn sv =>
                            let val xx = mkv() and yy = mkv()
                             in RECORD(FU_rk_tuple, [VAR m, VAR m2], xx,
                                  FU_WRAP(tc_pair, [VAR xx], yy,
                                    APP(sv, [VAR yy])))
                            end,
                          fn le =>
                            WRAPcast(mkarw([tc_void,tc_void],[tc2]),
                                     true, le))
                       else (* unwrapping *)
                         let val x = mkv() and y = mkv() and z = mkv()
                          in ([(m, lt_void)],
                              fn sv =>
                                let val xx = mkv()
                                 in LET([xx],
                                      UNWRAPcast(
                                         mkarw([tc_void, tc_void], [tc2]),
                                              true, RET[sv]),
                                        FU_UNWRAP(tc_pair, [VAR m], x,
                                         SELECT(VAR x, 0, y,
                                         SELECT(VAR x, 1, z,
                                          APP(VAR xx, [VAR y, VAR z])))))
                                end,
                             ident)
                         end

                     val (argt2, body2, hh2) =
                       if wflag then  (* wrapping *)
                         ([(n,lt_breal),(n2,lt_breal)],
                          fn sv =>
                            let val xx = mkv() and yy = mkv()
                             in LET ([xx],
                                   RECORDg [UNWRAP(tc_real, VAR n),
                                            UNWRAP(tc_real, VAR n2)],
                                FU_WRAP(tc_fpair, [VAR xx], yy,
                                   APP(sv, [VAR yy])))
                            end,
                          fn le => WRAPcast(mkarw([tc_breal,tc_breal],[tc2]),
                                            true, le))
                       else  (* unwrapping *)
                         let val x = mkv() and y = mkv() and z = mkv()
                             val q0 = mkv() and q1 = mkv()
                          in ([(n, lt_void)],
                              fn sv =>
                                let val xx = mkv()
                                 in LET([xx],
                                      UNWRAPcast(
                                         mkarw([tc_breal, tc_breal], [tc2]),
                                            true, RET[sv]),
                                      FU_UNWRAP(tc_fpair, [VAR n], x,
                                        SELECT(VAR x, 0, y,
                                          FU_WRAP(tc_real, [VAR y], q0,
                                        SELECT(VAR x, 1, z,
                                          FU_WRAP(tc_real, [VAR z], q1,
                                         APP(VAR xx, [VAR q0, VAR q1])))))))
                                end,
                            ident)
                         end

                     val hh3 = if wflag then fn le => WRAPcast(nt, true, le)
                               else fn le => UNWRAPcast(nt, true, le)

                     (*** NEEDS MORE WORK TO DO THE RIGHT COERCIONS ***)
                     fun hdr0(sv) =
                       LET([w], e,
                         COND(test1, hh1(FNg(argt1, body1 sv)),
                           COND(test2, hh2(FNg(argt2, body2 sv)),
                                hh3(RET[sv]))))

                     fun hdr (xe as RET [sv]) = hdr0 sv
                       | hdr xe = let val z = mkv()
                                   in LET([z], xe, hdr0(VAR z))
                                  end
                  in SOME hdr
                 end)
        end
     | _ => NONE)

(* val mkwrp : tyc * kenv * bool * tyc -> lexp -> lexp *)
fun mkwrp (tc, kenv, b, nt) =
  (case tcCoerce(kenv, tc, nt, true, b)
    of NONE => (fn le => WRAPg(nt, b, le))
     | SOME hdr => hdr)

(* val mkuwp  : tyc * kenv * bool * tyc -> lexp -> lexp *)
fun mkuwp (tc, kenv, b, nt) =
  (case tcCoerce(kenv, tc, nt, false, b)
    of NONE => (fn le => UNWRAPg(nt, b, le))
     | SOME hdr => hdr)

end (* toplevel local *)
end (* structure TypeOper *)

