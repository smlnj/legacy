(* split.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* monnier@cs.yale.edu *)

signature FSPLIT =
sig
    type flint = FLINT.prog
    val split: flint * int option -> flint * flint option
end

structure FSplit :> FSPLIT =
struct

local
    structure F  = FLINT
    structure S  = LambdaVar.Set
    structure M  = LambdaVar.Map
    structure O  = Option
    structure OU = OptUtils
    structure FU = FlintUtil
    structure LT = LtyExtern
    structure PO = Primop
    structure PP = PPFlint
    structure CTRL = FLINT_Control
in

val say = Control_Print.say
fun bug msg = ErrorMsg.impossible ("FSplit: "^msg)
fun buglexp (msg,le) = (say "\n"; PP.printLexp le; say " "; bug msg)
fun bugval (msg,v) = (say "\n"; PP.printSval v; say " "; bug msg)
fun assert p = if p then () else bug ("assertion failed")

type flint = F.prog
val mklv = LambdaVar.mkLvar
val cplv = LambdaVar.dupLvar

fun S_rmv(x, s) = S.delete(s, x) handle NotFound => s

fun addv (s,F.VAR lv) = S.add(s, lv)
  | addv (s,_) = s
fun addvs (s,vs) = foldl (fn (v,s) => addv(s, v)) s vs
fun rmvs (s,lvs) = foldl (fn (l,s) => S_rmv(l, s)) s lvs

exception Unknown

fun split (fdec, NONE) = (fdec, NONE)
  | split (fdec as (fk,f,args,body), SOME aggressiveness) = let
    val {getLty,addLty,...} = Recover.recover (fdec, false)

    val m = LambdaVar.Tbl.mkTable(64, Unknown)
    fun addpurefun f = LambdaVar.Tbl.insert m (f, false)
    fun funeffect f = (LambdaVar.Tbl.lookup m f) handle Uknown => true

(* sexp: env -> lexp -> (leE, leI, fvI, leRet)
 * - env: IntSetF.set	current environment
 * - lexp: lexp		expression to split
 * - leRet: lexp	the core return expression of lexp
 * - leE: lexp -> lexp	recursively split lexp:  leE leRet == lexp
 * - leI: lexp option	inlinable part of lexp (if any)
 * - fvI: IntSetF.set	free variables of leI:   FU.freevars leI == fvI
 *
 * sexp splits the lexp into an expansive part and an inlinable part.
 * The inlinable part is guaranteed to be side-effect free.
 * The expansive part doesn't bother to eliminate unused copies of
 *   elements copied to the inlinable part.
 * If the inlinable part cannot be constructed, leI is set to F.RET[].
 *   This implies that fvI == S.empty, which in turn prevents us from
 *   mistakenly adding anything to leI.
 *)
fun sexp env lexp =			(* fixindent *)
    let
	(* non-side effecting binds are copied to leI if exported *)
	fun let1 (le,lewrap,lv,vs,effect) =
	    let val (leE,leI,fvI,leRet) = sexp (S.add(env, lv)) le
		val leE = lewrap o leE
	    in if effect orelse not (S.member(fvI, lv))
	       then (leE, leI, fvI, leRet)
	       else (leE, lewrap leI, addvs(S_rmv(lv, fvI), vs), leRet)
	    end

    in case lexp
	(* we can completely move both RET and TAPP to the I part *)
	of F.RECORD (rk,vs,lv,le as F.RET [F.VAR lv']) =>
	   if lv' = lv
	   then (fn e => e, lexp, addvs(S.empty, vs), lexp)
	   else (fn e => e, le, S.singleton lv', le)
	 | F.RET vs =>
	   (fn e => e, lexp, addvs(S.empty, vs), lexp)
	 | F.TAPP (F.VAR tf,tycs) =>
	   (fn e => e, lexp, S.singleton tf, lexp)

	 (* recursive splittable lexps *)
	 | F.FIX (fdecs,le) => sfix env (fdecs, le)
	 | F.TFN (tfdec,le) => stfn env (tfdec, le)

	 (* binding-lexps *)
	 | F.CON (dc,tycs,v,lv,le) =>
	   let1(le, fn e => F.CON(dc, tycs, v, lv, e), lv, [v], false)
	 | F.RECORD (rk,vs,lv,le) =>
	   let1(le, fn e => F.RECORD(rk, vs, lv, e), lv, vs, false)
	 | F.SELECT (v,i,lv,le) =>
	   let1(le, fn e => F.SELECT(v, i, lv, e), lv, [v], false)
	 | F.PRIMOP (po,vs,lv,le) =>
	   let1(le, fn e => F.PRIMOP(po, vs, lv, e), lv, vs, PrimopUtil.effect(#2 po))

	 (* IMPROVEME: lvs should not be restricted to [lv] *)
	 | F.LET(lvs as [lv],body as F.TAPP (v,tycs),le) =>
	   let1(le, fn e => F.LET(lvs, body, e), lv, [v], false)
	 | F.LET (lvs as [lv],body as F.APP (v as F.VAR f,vs),le) =>
	   let1(le, fn e => F.LET(lvs, body, e), lv, v::vs, funeffect f)

	 | F.SWITCH (v,ac,[(dc as F.DATAcon(_,_,lv),le)],NONE) =>
	   let1(le, fn e => F.SWITCH(v, ac, [(dc, e)], NONE), lv, [v], false)

	 | F.LET (lvs,body,le) =>
	   let val (leE,leI,fvI,leRet) = sexp (S.union(S.addList(S.empty, lvs), env)) le
	   in (fn e => F.LET(lvs, body, leE e), leI, fvI, leRet)
	   end

	 (* useless sophistication *)
	 | F.APP (F.VAR f,args) =>
	   if funeffect f
	   then (fn e => e, F.RET[], S.empty, lexp)
	   else (fn e => e, lexp, addvs(S.singleton f, args), lexp)

	 (* other non-binding lexps result in unsplittable functions *)
	 | (F.APP _ | F.TAPP _) => bug "strange (T)APP"
	 | (F.SWITCH _ | F.RAISE _ | F.BRANCH _ | F.HANDLE _) =>
	   (fn e => e, F.RET[], S.empty, lexp)
    end

(* Functions definitions fall into the following categories:
 * - inlinable:  if exported, copy to leI
 * - (mutually) recursive:  don't bother
 * - non-inlinable non-recursive:  split recursively *)
and sfix env (fdecs,le) =
    let val nenv = S.union(S.addList(S.empty, map #2 fdecs), env)
	val (leE,leI,fvI,leRet) = sexp nenv le
	val nleE = fn e => F.FIX(fdecs, leE e)
    in case fdecs
	of [({inline=inl as (F.IH_ALWAYS | F.IH_MAYBE _),...},f,args,body)] =>
	   let val min = case inl of F.IH_MAYBE(n,_) => n | _ => 0
	   in if not(S.member(fvI, f)) orelse min > aggressiveness (* !CTRL.splitThreshold *)
	      then (nleE, leI, fvI, leRet)
	      else (nleE, F.FIX(fdecs, leI),
		    rmvs(S.union(fvI, FU.freevars body),
			 f::(map #1 args)),
		    leRet)
	   end
	 | [fdec as (fk as {cconv=F.CC_FCT,...},_,_,_)] =>
	   sfdec env (leE,leI,fvI,leRet) fdec

	 | _ => (nleE, leI, fvI, leRet)
    end

and sfdec env (leE,leI,fvI,leRet) (fk,f,args,body) =
    let val benv = S.union(S.addList(S.empty, map #1 args), env)
	val (bodyE,bodyI,fvbI,bodyRet) = sexp benv body
    in case bodyI
	of F.RET[] =>
	   (fn e => F.FIX([(fk, f, args, bodyE bodyRet)], e),
	    leI, fvI, leRet)
	 | _ =>
	   let val fvbIs = S.listItems(S.difference(fvbI, benv))
	       val (nfk,fkE) = OU.fk_wrap(fk, NONE)

	       (* fdecE *)
	       val fE = cplv f
	       val fErets = (map F.VAR fvbIs)
	       val bodyE = bodyE(F.RET fErets)
	       (* val tmp = mklv()
	       val bodyE = bodyE(F.RECORD(F.RK_STRUCT, map F.VAR fvbIs,
					  tmp, F.RET[F.VAR tmp])) *)
	       val fdecE = (fkE, fE, args, bodyE)
	       val fElty = LT.ltc_fct(map #2 args, map getLty fErets)
	       val _ = addLty(fE, fElty)

	       (* fdecI *)
	       val fkI = {inline=F.IH_ALWAYS, cconv=F.CC_FCT,
			  known=true, isrec=NONE}
	       val argsI =
		(map (fn lv => (lv, getLty(F.VAR lv))) fvbIs) @ args
	       (* val argI = mklv()
	       val argsI = (argI, LT.ltc_str(map (getLty o F.VAR) fvbIs))::args

	       val (_,bodyI) = foldl (fn (lv,(n,le)) =>
				      (n+1, F.SELECT(F.VAR argI, n, lv, le)))
				     (0, bodyI) fvbIs *)
	       val fdecI as (_,fI,_,_) = FU.copyfdec(fkI,f,argsI,bodyI)
	       val _ = addpurefun fI

	       (* nfdec *)
	       val nargs = map (fn (v,t) => (cplv v, t)) args
	       val argsv = map (fn (v,t) => F.VAR v) nargs
	       val nbody =
		   let val lvs = map cplv fvbIs
		   in F.LET(lvs, F.APP(F.VAR fE, argsv),
			    F.APP(F.VAR fI, (map F.VAR lvs)@argsv))
		   end
		   (* let val lv = mklv()
		   in F.LET([lv], F.APP(F.VAR fE, argsv),
			    F.APP(F.VAR fI, (F.VAR lv)::argsv))
		   end *)
	       val nfdec = (nfk, f, nargs, nbody)

	       (* and now, for the whole F.FIX *)
	       fun nleE e =
		   F.FIX([fdecE], F.FIX([fdecI], F.FIX([nfdec], leE e)))

	   in if not(S.member(fvI, f)) then (nleE, leI, fvI, leRet)
	      else (nleE,
		    F.FIX([fdecI], F.FIX([nfdec], leI)),
		    S.add(S.union(S_rmv(f, fvI), S.intersection(env, fvbI)), fE),
		    leRet)
	   end
    end

(* TFNs are kinda like FIX except there's no recursion *)
and stfn env (tfdec as (tfk,tf,args,body),le) =
    let val (bodyE,bodyI,fvbI,bodyRet) =
	    if #inline tfk = F.IH_ALWAYS
	    then (fn e => body, body, FU.freevars body, body)
	    else sexp env body
	val nenv = S.add(env, tf)
	val (leE,leI,fvI,leRet) = sexp nenv le
    in case (bodyI, S.listItems(S.difference(fvbI, env)))
	of (_,[]) =>
	   (* everything was split out *)
	   let val ntfdec = ({inline=F.IH_ALWAYS}, tf, args, bodyE bodyRet)
	       val nlE = fn e => F.TFN(ntfdec, leE e)
	   in if not(S.member(fvI, tf)) then (nlE, leI, fvI, leRet)
	      else (nlE, F.TFN(ntfdec, leI),
		    S_rmv(tf, S.union(fvI, fvbI)), leRet)
	   end
	 | ((F.RET _ | F.RECORD(_,_,_,F.RET _)),_) =>
	   (* split failed *)
	   (fn e => F.TFN((tfk, tf, args, bodyE bodyRet), leE e),
	    leI, fvI, leRet)
	 | (_,fvbIs) =>
	   let (* tfdecE *)
	       val tfE = cplv tf
	       val tfEvs = map F.VAR fvbIs
	       val bodyE = bodyE(F.RET tfEvs)
	       val tfElty = LT.lt_nvpoly(args, map getLty tfEvs)
	       val _ = addLty(tfE, tfElty)

	       (* tfdecI *)
	       val tfkI = {inline=F.IH_ALWAYS}
	       val argsI = map (fn (v,k) => (cplv v, k)) args
	       val tmap = ListPair.map (fn (a1,a2) =>
					(#1 a1, LT.tcc_nvar(#1 a2)))
				       (args, argsI)
	       val bodyI = FU.copy tmap M.empty
				   (F.LET(fvbIs, F.TAPP(F.VAR tfE, map #2 tmap),
					  bodyI))
	       (* F.TFN *)
	       fun nleE e =
		   F.TFN((tfk, tfE, args, bodyE),
			 F.TFN((tfkI, tf, argsI, bodyI), leE e))

	   in if not(S.member(fvI, tf)) then (nleE, leI, fvI, leRet)
	      else (nleE,
		    F.TFN((tfkI, tf, argsI, bodyI), leI),
		    S.add(S.union(S_rmv(tf, fvI), S.intersection(env, fvbI)), tfE),
		    leRet)
	   end
    end

(* here, we use B-decomposition, so the args should not be
 * considered as being in scope *)
val (bodyE,bodyI,fvbI,bodyRet) = sexp S.empty body
in case (bodyI, bodyRet)
    of (F.RET _,_) => ((fk, f, args, bodyE bodyRet), NONE)
     | (_,F.RECORD (rk,vs,lv,F.RET[lv'])) =>
       let val fvbIs = S.listItems fvbI

	   (* fdecE *)
	   val bodyE = bodyE(F.RECORD(rk, vs@(map F.VAR fvbIs), lv, F.RET[lv']))
	   val fdecE as (_,fE,_,_) = (fk, cplv f, args, bodyE)

	   (* fdecI *)
	   val argI = mklv()
	   val argLtys = (map getLty vs) @ (map (getLty o F.VAR) fvbIs)
	   val argsI = [(argI, LT.ltc_str argLtys)]
	   val (_,bodyI) = foldl (fn (lv,(n,le)) =>
				  (n+1, F.SELECT(F.VAR argI, n, lv, le)))
				 (length vs, bodyI) fvbIs
	   val fdecI as (_,fI,_,_) = FU.copyfdec (fk, f, argsI, bodyI)

	   val nargs = map (fn (v,t) => (cplv v, t)) args
       in
	   (fdecE, SOME fdecI)
	   (* ((fk, f, nargs,
	     F.FIX([fdecE],
		   F.FIX([fdecI],
			 F.LET([argI],
			       F.APP(F.VAR fE, map (F.VAR o #1) nargs),
			       F.APP(F.VAR fI, [F.VAR argI]))))),
	    NONE) *)
       end

     | _ => (fdec, NONE)		(* sorry, can't do that *)
     (* (PPFlint.printLexp bodyRet; bug "couldn't find the returned record") *)

end

end
end
