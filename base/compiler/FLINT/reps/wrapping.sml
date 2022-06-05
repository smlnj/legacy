(* wrapping.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature WRAPPING =
sig
  val wrapping : FLINT.prog -> FLINT.prog

end (* signature WRAPPING *)

structure Wrapping : WRAPPING =
struct

local structure CO = Coerce
      structure LT = LtyExtern
      structure DI = DebIndex
      structure PO = Primop
      structure DA = Access
      open FLINT
in

fun bug s = ErrorMsg.impossible ("Wrapping: " ^ s)
val say = Control_Print.say
fun mkv _ = LambdaVar.mkLvar()
val fkfun = {isrec=NONE,known=false,inline=IH_ALWAYS, cconv=CC_FUN LT.ffc_fixed}
val ident = fn le => le
fun option f NONE = NONE
  | option f (SOME x) = SOME (f x)

(****************************************************************************
 *                   MISC UTILITY FUNCTIONS                                 *
 ****************************************************************************)
local val lt_upd =
        let val x = LT.ltc_array (LT.ltc_tv 0)
         in LT.ltc_poly([LT.tkc_mono],
              [LT.ltc_arrow(LT.ffc_rrflint, [x, LT.ltc_int, LT.ltc_tv 0],
                                            [LT.ltc_unit])])
        end
      val lt_sub =
        let val x = LT.ltc_array (LT.ltc_tv 0)
         in LT.ltc_poly([LT.tkc_mono],
              [LT.ltc_arrow(LT.ffc_rrflint, [x, LT.ltc_int], [LT.ltc_tv 0])])
        end
in

fun isArraySub t = LT.lt_eqv(t, lt_sub)
fun isArrayUpd t = LT.lt_eqv(t, lt_upd)
val f64sub = PO.NUMSUBSCRIPT(PO.FLOAT 64)
val f64upd = PO.NUMUPDATE(PO.FLOAT 64)

(* Function classPrim : primop -> primop * bool * bool takes a primop
 * and classifies its kind. It returns a new primop, a flag indicates
 * if this primop has been specialized.
 * [2019-06-25] removed run-time type specialization
 *)
fun classPrim (px as (d, p, lt, ts)) =
  (case (p, ts)
    of ((PO.NUMSUBSCRIPT _ | PO.NUMUPDATE _), _) =>   (* overloaded primops *)
         ((d, p, LT.lt_pinst(lt, ts), []), true)
     | (PO.ASSIGN, [tc]) =>			      (* special *)
	if (LT.tc_upd_prim tc = PO.UNBOXEDUPDATE)
	  then ((d, PO.UNBOXEDASSIGN, lt, ts), false) (* avoid store-list allocation *)
	  else ((d, p, lt, ts), false)
     | (PO.UPDATE, [tc]) => ((d, LT.tc_upd_prim tc, lt, ts), false)
     | _ => (px, false))

val argbase = fn vs => (vs, ident)
val resbase = fn v => (v, ident)

end (* utility functions *)

(****************************************************************************
 * The "wrapping" function does the following several things:               *
 *                                                                          *
 *   (1) representation coercions are inserted at TAPP, BRANCH, PRIMOP,     *
 *       CON, SWITCH, and RECORD(RK_VECTOR _, _). Where CON and SWITCH      *
 *       only wrap/unwrap the arguments of a datatype constuctor while      *
 *       RK_VECTOR just wraps the vector elements only.                     *
 *   (2) all primops in PRIM are given type-specific meanings;              *
 *   (3) all conreps in CON and SWITCH are given type-specific meanings ??  *
 *                                                                          *
 ****************************************************************************)
fun wrapping fdec =
let (* In pass1, we calculate the old type of each variables in the FLINT
     * expression. We do this for the sake of having simpler wrapping code.
     *)
    val {getLty=getlty, cleanUp, ...} = Recover.recover (fdec, false)

    (** generate a set of new wrappers *)
    val (tcWrap, ltWrap, tcf, ltf, cleanup2) = LT.twrap_gen true

    fun fixDconTy lt =
      if LT.ltp_ppoly lt then
        let val (ks, t) = LT.ltd_ppoly lt
         in LT.ltc_ppoly(ks, ltWrap t)
        end
      else ltWrap lt

    (* transform : CO.wpEnv * DI.depth -> (lexp -> lexp) *)
    fun transform (wenv, d) =
      let
          fun lpfd ({isrec,known,inline,cconv}, v, vts, e) =
	      let val nisrec = case isrec of SOME(ts,l) => SOME(map ltf ts, l)
					   | NONE => NONE
		  val ncconv = case cconv of CC_FUN fixed => CC_FUN LT.ffc_fixed
					   | CC_FCT => cconv
	      in ({isrec=nisrec, known=known,
		   cconv=ncconv, inline=inline},
		  v,
		  map (fn (x,t) => (x, ltf t)) vts,
		  loop e)
	      end

          (* lpdc : dcon * tyc list * value * bool ->
                       (dcon * tyc list * (lexp -> lexp) * value)  *)
          and lpdc (dc as (name,rep,lt), ts, u, wflag) =
            let (*** fixing the potential mismatch in the type *)
                val ndc = (name, rep, fixDconTy lt)

                val aty = case LT.ltd_arrow (LT.lt_pinst(lt, ts))
                           of (_, [x], _) => x
                            | _ => bug "unexpected case in lpdc"
                val (naty, oaty) = (ltWrap aty, ltf aty)

                val hdr = if wflag then CO.wrapOp(wenv,[naty],[oaty],d)
                          else CO.unwrapOp(wenv,[naty],[oaty],d)


                val nts = map tcWrap ts
             in case hdr
                 of NONE => (ndc, nts, ident, u)
                  | SOME hhh =>
                      let val z = mkv()
                          val nu = VAR z
                       in if wflag then  (* CON *)
                            (ndc, nts, fn xe => LET([z], hhh([u]), xe), nu)
                          else           (* DECON *)
                            let val x = case u of VAR q => q
                                          | _ => bug "unexpected case in lpdc"
                             in (ndc, nts,
                                 fn xe => LET([x], hhh([nu]), xe), nu)
                            end
                      end
            end (* function lpdc *)

          (* lpsw : con * lexp -> con * lexp *)
          and lpsw (DATAcon(dc, ts, v), e) =
                let val (ndc, nts, hdr, u) = lpdc(dc, ts, VAR v, false)
                 in (case u
                      of VAR nv => (DATAcon(ndc, nts, nv), hdr(loop e))
                       | _ => bug "unexpected case in lpsw")
                end
            | lpsw (c, e) = (c, loop e)


          (* lprim : primop -> (primop *
           *                    (value list -> value list * (lexp -> lexp))
           *                    (lvar -> lvar * (lexp -> lexp)))
           *)
          and lprim (dict, p, lt, []) =
                ((dict, p, ltf lt, []), argbase, resbase)
            | lprim px =
                let val ((dict, np, lt, ts), issp) = classPrim px
                    val nlt = ltf lt
                    val wts = map tcWrap ts
                 in if issp (* primop has been specialized *)
                    then ((dict, np, nlt, wts), argbase, resbase)
                    else (* still a polymorphic primop *)
                     (let val nt = LT.lt_pinst(nlt, wts)
                          val (_, nta, ntr) = LT.ltd_arrow nt
                          val ot = ltf(LT.lt_pinst(lt, ts))
                          val (_, ota, otr) = LT.ltd_arrow ot
                          val arghdr =
                            (case CO.wrapOp(wenv, nta, ota, d)
                              of NONE => argbase
                               | SOME hhh =>
                                   (fn vs =>
                                     let val nvs = map mkv vs
                                      in (map VAR nvs,
                                          fn le => LET(nvs, hhh(vs), le))
                                     end))
                          val reshdr =
                            (case CO.unwrapOp(wenv, ntr, otr, d)
                              of NONE => resbase
                               | SOME hhh =>
                                   (fn v =>
                                     let val nv = mkv()
                                      in (nv,
                                          fn le => LET([v], hhh([VAR nv]), le))
                                     end))
                          val npx' = (dict, np, nt, [])
                       in (npx', arghdr, reshdr)
                      end)
                end (* function lprim *)

          and loop le =
            (case le
              of RET _ => le
               | LET (vs, e1, e2) => LET (vs, loop e1, loop e2)
               | FIX (fdecs, e) => FIX(map lpfd fdecs, loop e)
               | APP _ => le
               | TFN ((tfk, v, tvks, e1), e2) =>  (* put down all wrappers *)
                   let val nwenv = CO.wpNew(wenv, d)
                       val ne1 = transform (nwenv, DI.next d) e1
                    in TFN((tfk, v, tvks, CO.wpBuild(nwenv, ne1)), loop e2)
                   end
               | TAPP (v, ts) =>
                   let val olt = getlty v
                       val nts = map tcWrap ts
                       val nlts = LT.lt_inst(ltf olt, nts)
                       val olts = map ltf (LT.lt_inst(olt, ts))
                       val hdr = CO.unwrapOp (wenv, nlts, olts, d)
                    in case hdr
                        of NONE => TAPP(v, nts)
                         | SOME hhh =>
                             let val nvs = map mkv nlts
                              in LET(nvs, TAPP(v, nts), hhh(map VAR nvs))
                             end
                   end
               | CON (dc, ts, u, v, e) =>
                   let val (ndc, nts, hdr, nu) = lpdc(dc, ts, u, true)
                    in hdr (CON(ndc, nts, nu, v, loop e))
                   end
               | SWITCH (v, csig, cases, opp) =>
                   SWITCH(v, csig, map lpsw cases, option loop opp)

               | RECORD(RK_VECTOR t, vs, v, e) =>
                   let val (otc, ntc) = (tcf t, tcWrap t)
                       val ot = LT.ltc_tyc otc
                       val nt = LT.ltc_tyc ntc
                    in (case CO.wrapOp(wenv, [nt], [ot], d)
                         of NONE => RECORD(RK_VECTOR ntc, vs, v, loop e)
                          | SOME hhh =>
                              let val f = mkv() and x = mkv()
                                  fun mh xe =
                                    FIX([(fkfun,f,[(x,ot)],hhh([VAR x]))], xe)

                                  fun pass([], nvs, h) =
                                        h(RECORD(RK_VECTOR ntc,
                                                rev nvs, v, loop e))
                                    | pass(u::r, nvs, h) =
                                        let val z = mkv()
                                            fun h0 xe =
                                              LET([z], APP(VAR f, [u]), xe)
                                         in pass(r, (VAR z)::nvs, h o h0)
                                        end
                               in pass(vs, [], mh)
                              end)
                   end
               | RECORD (rk, vs, v, e) => RECORD(rk, vs, v, loop e)
               | SELECT (u, i, v, e) => SELECT(u, i, v, loop e)

               | RAISE (u, lts) => RAISE(u, map ltf lts)
               | HANDLE (e, v) => HANDLE (loop e, v)

               (* resolving the polymorphic equality in a special way *)
               | BRANCH (p as (_, PO.POLYEQL, _, _), vs, e1, e2) =>
                   loop(Equal.equal_branch (p, vs, e1, e2))
               | PRIMOP (p as (_, PO.POLYEQL, _, _), vs, v, e) =>
                   bug "unexpected POLYEQL in wrapping"
               | PRIMOP ((_, PO.INLMKARRAY, _, _), vs, v, e) =>
                   bug "unexpected INLMKARRAY in wrapping"

               (* resolving the usual primops *)
               | BRANCH (p, vs, e1, e2) =>
                   let val (np, hg, _) = lprim p
                       val (nvs, nh) = hg vs
                    in nh(BRANCH(np, nvs, loop e1, loop e2))
                   end
               | PRIMOP (p, vs, v, e) =>
                   let val (np, hg1, hg2) = lprim p
                       val (nvs, nh1) = hg1 vs
                       val (nv, nh2) = hg2 v
                    in nh1(PRIMOP(np, nvs, nv, nh2(loop e)))
                   end)
       in loop
      end (* function transform *)

    val (fk, f, vts, e) = fdec
    val nvts = map (fn (v, t) => (v, ltf t)) vts
    val wenv = CO.initWpEnv()
    val ne = transform (wenv, DI.top) e
 in (fk, f, nvts, CO.wpBuild(wenv, ne)) before (cleanup2(); cleanUp())
end (* function wrapping *)

end (* toplevel local *)
end (* structure Wrapping *)

