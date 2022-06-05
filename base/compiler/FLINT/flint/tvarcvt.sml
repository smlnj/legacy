(* tvarcvt.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * converting between different representations of
 * type variables in a FLINT program.
 *)

signature TVARCVT =
  sig
    val debIndex2names : FLINT.prog -> FLINT.prog
    val names2debIndex : FLINT.prog -> FLINT.prog
  end (* TVARCVT *)

structure TvarCvt :> TVARCVT =
  struct
    (* local abbreviations *)
    structure F = FLINT
    structure DI = DebIndex
    structure LT = LtyExtern
    structure LK = LtyKernel

    (* `debIndex2names' converts all variables bound by the
     * term-language TFN (capital lambda) construct into named
     * variables.  This is primarily to experiment with the cost of
     * named variables, should we introduce them during translate or
     * other phases.
     *)
    val debIndex2names = let

        fun extendEnv env d _ tvtks = let
            val (tvars,ks) = ListPair.unzip tvtks
        in
            Lty.teCons(Lty.Beta(0,map LT.tcc_nvar tvars,ks), env)
        end

        fun cvtExp (env: Lty.tycEnv) (d: int) = let
            fun tcSubst tyc = LK.tcc_env (tyc, d, d, env)
            fun ltSubst lty = LK.ltc_env (lty, d, d, env)

            fun cvtCon (F.DATAcon ((sym,cr,lty),ts,lv)) =
                F.DATAcon ((sym, cr, ltSubst lty),
                           map tcSubst ts, lv)
              | cvtCon c = c

            fun cvtDict {default, table} = let
                fun f (ts,lv) = ((map tcSubst ts), lv)
            in
                {default = default,
                 table = map f table
                 } : F.dict
            end (* cvtDict *)

            fun cvtPrimop (dictOpt, po, lty, tycs) =
                (Option.map cvtDict dictOpt,
                 po,
                 ltSubst lty,
                 map tcSubst tycs
                 ) : F.primop

            fun r exp =
                case exp of
                    F.RET _ => exp              (* no processing required *)

                  | F.LET (lvs, e1, e2) =>      (* recursion only *)
                    F.LET (lvs, r e1, r e2)

                  | F.FIX (fundecs, e) =>       (* recursion only *)
                    F.FIX (map (cvtFundec env d) fundecs,
                           r e)

                  | F.APP _ => exp              (* no processing required *)

                  | F.TFN ((tfk,v,tvtks,e1),e2) =>
                    F.TFN ((tfk, v, tvtks,
                            cvtExp (extendEnv env d 0 tvtks) (DI.next d) e1),
                           r e2)

                  | F.TAPP (v, ts) =>           (* subst ts *)
                    F.TAPP (v, map tcSubst ts)

                  | F.SWITCH (v, cs, conlexps, lexpO) =>
                    F.SWITCH (v, cs,
                              (map (fn (con,lexp) => (cvtCon con, r lexp))
                                   conlexps),
                              Option.map r lexpO)

                  | F.CON ((sym,cr,lty), ts, v, lv, e) =>
                    F.CON ((sym, cr, ltSubst lty),
                           map tcSubst ts,
                           v, lv, r e)

                  | F.RECORD (rk, vs, lv, e) =>
                    F.RECORD ((case rk of
                                   F.RK_VECTOR t =>
                                   F.RK_VECTOR (tcSubst t)
                                 | _ => rk),
                              vs, lv, r e)

                  | F.SELECT (v, i, lv, e) =>
                    F.SELECT (v, i, lv, r e)

                  | F.RAISE (v, ltys) =>
                    F.RAISE (v, map ltSubst ltys)

                  | F.HANDLE (e, v) =>
                    F.HANDLE (r e, v)

                  | F.BRANCH (po, vs, e1, e2) =>
                    F.BRANCH (cvtPrimop po,
                              vs, r e1, r e2)

                  | F.PRIMOP (po, vs, lv, e) =>
                    F.PRIMOP (cvtPrimop po,
                              vs, lv, r e)
        in
            r
        end (* cvtExp *)

        and cvtFundec env d (fkind, lvar, lvlts, e) =
            let fun ltSubst lty = LK.ltc_env (lty, d, d, env)

                fun cvtFkind {isrec = SOME(ltys,lk),
                              cconv, known, inline} =
                    {isrec = SOME (map ltSubst ltys, lk),
		     cconv = cconv,
		     known = known,
		     inline = inline}
                  | cvtFkind fk = fk

                fun cvtLvLt (lvar, lty) = (lvar, ltSubst lty)
            in (cvtFkind fkind,
                lvar,
                map cvtLvLt lvlts,
                cvtExp env d e
               ) : F.fundec
            end (* cvtFundec *)
    in
        cvtFundec Lty.teEmpty DI.top
    end

    (* `names2debIndex' removes all named variables (`TC_NVAR')
     * from a FLINT program, replacing them with deBruijn-indexed
     * variables.  It expects, of course, that named variables are
     * only bound by the term-language TFN (capital lambda), and not
     * by the LT_POLY (forall) or TC_FN (lowercase lambda) in the
     * type language.
     *)
    fun names2debIndex_gen() = let

        fun extendEnv env d i [] = env
          | extendEnv env d i ((tv,_)::tvtks) =
            extendEnv (LambdaVar.Map.insert (env, tv, (d,i)))
                      d (i+1) tvtks

        fun queryEnv env (tvar, currDepth) =
	  (case LambdaVar.Map.find(env, tvar)
	    of NONE => NONE
	     | SOME(defnDepth, i) =>
	         SOME (LT.tcc_var (DI.getIndex (currDepth, defnDepth), i))
          (*esac*))

        val tc_nvar_elim = LT.tc_nvar_elim_gen()
        val lt_nvar_elim = LT.lt_nvar_elim_gen()

        fun cvtExp env d = let
            val q = queryEnv env
            (* instantiate a new subst dictionary on each invocation..
             * clean this up later.
             *)
            val tcSubst = tc_nvar_elim q d
            val ltSubst = lt_nvar_elim q d

            fun cvtCon (F.DATAcon ((sym,cr,lty),ts,lv)) =
                F.DATAcon ((sym, cr, ltSubst lty),
                           map tcSubst ts, lv)
              | cvtCon c = c

            fun cvtDict {default, table} = let
                fun f (ts,lv) = ((map tcSubst ts), lv)
            in
                {default = default,
                 table = map f table
                 } : F.dict
            end (* cvtDict *)

            fun cvtPrimop (dictOpt, po, lty, tycs) =
                (Option.map cvtDict dictOpt,
                 po,
                 ltSubst lty,
                 map tcSubst tycs
                 ) : F.primop

            fun r exp =                 (* default recursive invocation *)
                case exp of
                    F.RET _ => exp              (* no processing required *)

                  | F.LET (lvs, e1, e2) =>      (* recursion only *)
                    F.LET (lvs, r e1, r e2)

                  | F.FIX (fundecs, e) =>       (* recursion only *)
                    F.FIX (map (cvtFundec env d) fundecs,
                           r e)

                  | F.APP _ => exp              (* no processing required *)

                  | F.TFN ((tfk,v,tvtks,e1),e2) =>
                    F.TFN ((tfk, v, tvtks,
                            cvtExp (extendEnv env d 0 tvtks) (DI.next d) e1),
                           r e2)

                  | F.TAPP (v, ts) =>           (* subst ts *)
                    F.TAPP (v, map tcSubst ts)

                  | F.SWITCH (v, cs, conlexps, lexpO) =>
                    F.SWITCH (v, cs,
                              (map (fn (con,lexp) => (cvtCon con, r lexp))
                                   conlexps),
                              Option.map r lexpO)

                  | F.CON ((sym,cr,lty), ts, v, lv, e) =>
                    F.CON ((sym, cr, ltSubst lty),
                           map tcSubst ts,
                           v, lv, r e)

                  | F.RECORD (rk, vs, lv, e) =>
                    F.RECORD ((case rk of
                                   F.RK_VECTOR t =>
                                   F.RK_VECTOR (tcSubst t)
                                 | _ => rk),
                              vs, lv, r e)

                  | F.SELECT (v, i, lv, e) =>
                    F.SELECT (v, i, lv, r e)

                  | F.RAISE (v, ltys) =>
                    F.RAISE (v, map ltSubst ltys)

                  | F.HANDLE (e, v) =>
                    F.HANDLE (r e, v)

                  | F.BRANCH (po, vs, e1, e2) =>
                    F.BRANCH (cvtPrimop po,
                              vs, r e1, r e2)

                  | F.PRIMOP (po, vs, lv, e) =>
                    F.PRIMOP (cvtPrimop po,
                              vs, lv, r e)
        in
            r
        end (* cvtExp *)

        and cvtFundec env d (fkind, lvar, lvlts, e) = let
            val q = queryEnv env
            (* instantiate a new subst dictionary on each invocation..
             * clean this up later.
             *)
            val tcSubst = tc_nvar_elim q d
            val ltSubst = lt_nvar_elim q d

            fun cvtFkind ({isrec = SOME (ltys,lk),
			   cconv, known, inline}) =
                {isrec = SOME (map ltSubst ltys, lk),
		 cconv = cconv,
		 known = known,
		 inline = inline}
              | cvtFkind fk = fk

            fun cvtLvLt (lvar, lty) = (lvar, ltSubst lty)
        in
            (cvtFkind fkind,
             lvar,
             map cvtLvLt lvlts,
             cvtExp env d e
             ) : F.fundec
        end (* cvtFundec *)
    in
        cvtFundec LambdaVar.Map.empty DI.top
    end (* names2debIndex_gen *)

    (* generate tables once per invocation
     * ie, once per compilation unit.
     *)
    fun names2debIndex prog = names2debIndex_gen() prog

  end (* TvarCvt *)
