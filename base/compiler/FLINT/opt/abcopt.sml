(* abcopt.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * NOTE: this optimization is not enabled, so we no longer include
 * it in the compile [JHR; 2017-10-16]
 *)

(* yichen.xie@yale.edu *)

signature ABCOPT =
sig
    val abcOpt : FLINT.prog -> FLINT.prog
end

structure ABCOpt :> ABCOPT =
struct
local
    structure LV  = LambdaVar
    structure M = IntRedBlackMap
    structure S = IntRedBlackSet
    structure F = FLINT
    structure PO = Primop
    structure PP = PPFlint
    structure LT = LtyExtern
    structure CTRL = FLINT_Control

in
fun bug msg = ErrorMsg.impossible ("ABCOpt: "^msg)

val mklv = LV.mkLvar
val lvname = !PP.LVarString

val pDebug = ref false

val say = Control_Print.say

fun sayABC s =
    (* (if !CTRL.printABC then say s
     *  else ()) *) ()

fun debug s =
    (* (if !CTRL.printABC andalso !pDebug then
     * 	 say s
     *  else ()) *) ()

fun printVals nil = say "\n"
  | printVals (x::xs) = (PP.printSval x; say ", "; printVals xs)

fun abcOpt (pgm as (progkind, progname, progargs, progbody)) = let

    val lt_len = LT.ltc_tyc (LT.tcc_arrow (LT.ffc_fixed,
					   [LT.tcc_void],
					   [LT.tcc_int]))

    fun cse lmap rmap lexp = let

	fun substVar x =
	    (case (M.find (rmap, x))
	      of SOME y => (sayABC ("replacing: "^
				    (lvname x)^
				    " with "^
				    (lvname y)^
				    "\n");
			    y)
	       | NONE => x)

	fun substVal (F.VAR x) = (F.VAR (substVar x))
	  | substVal x = x

	fun substVals vals = map substVal vals

	fun g (F.PRIMOP (p as (d, PO.LENGTH, lty, tycs),
			 [F.VAR arrayVar], dest, body)) =
	    (case (M.find (lmap, arrayVar))
	      of SOME x =>
		 cse lmap (M.insert (rmap, dest, x)) body
	       | NONE =>
		 (F.PRIMOP
		      (p, [F.VAR arrayVar], dest,
		       cse (M.insert (lmap, arrayVar, dest))
			   rmap body)))

	  | g (F.RET x) = F.RET (substVals x)

	  | g (F.LET (vars, lexp, body)) =
	    F.LET (vars, g lexp, g body)

	  | g (F.FIX (fundecs, body)) =
	    F.FIX (map h fundecs, g body)

	  | g (F.APP (v, vs)) = F.APP (substVal v, substVals vs)

	  | g (F.TFN (tfundec as (tfkind, lv, tvtks, tfnbody), body)) =
	    F.TFN ((tfkind, lv, tvtks, g tfnbody), g body)

	  | g (F.TAPP (v, tycs)) = F.TAPP (substVal v, tycs)

	  | g (F.SWITCH (v, consig, cel, lexpOpt)) =
	    let
		fun hh (c, e) = (c, g e)

		val cel' = map hh cel

		fun gg (SOME x) = SOME (g x)
		  | gg NONE = NONE
	    in
		F.SWITCH (substVal v, consig, cel', gg lexpOpt)
	    end

	  | g (F.CON (dcon, tycs, v, lv, body)) =
	    F.CON (dcon, tycs, substVal v, lv, g body)

	  | g (F.RECORD (rk, vals, lv, body)) =
	    F.RECORD (rk, substVals vals, lv, g body)

	  | g (F.SELECT (v, field, lv, body)) =
	    F.SELECT (substVal v, field, lv, g body)

	  | g (F.RAISE (v, ty)) = F.RAISE (substVal v, ty)

	  | g (F.HANDLE (body, v)) = F.HANDLE (g body, substVal v)

	  | g (F.BRANCH (p, vals, body1, body2)) =
	    F.BRANCH (p, substVals vals, g body1, g body2)

	  | g (F.PRIMOP (p, vals, lv, body)) =
	    F.PRIMOP (p, substVals vals, lv, g body)

	and h (fk, lvar, lvty, body) = (fk, lvar, lvty, g body)

    in
	g lexp
    end

    fun lenOp (src, mm, body) =
	(sayABC ("hoisting: length of "^(lvname src)^"\n");
	 case M.find(mm, src)
	  of NONE => bug "strange bug!"
	   | SOME lty =>
	     F.PRIMOP((NONE, PO.LENGTH, lty, []),
		      [F.VAR src],
		      mklv(),
		      body))

    val agressiveHoist = ref true
    val mapUnion = M.unionWith (fn (a, b) => a)
    val mapIntersect = M.intersectWith (fn (a, b) => a)
    fun remove' (m, k) = let val (m', _) = M.remove(m, k) in m' end

    fun sayVars nil = ()
      | sayVars (x::nil) = sayABC (lvname x)
      | sayVars (x::xs) =
	(sayABC (lvname x);
	 sayABC ", ";
	 sayVars xs)

    fun hoist (F.RET x)= (M.empty, (F.RET x))

      | hoist (F.LET (vars, lexp, body)) =
	let
	    val (m1, lexp') = hoist lexp
	    val (m2, body') = hoist body
	    fun ft x = M.inDomain(m2, x)
	    val hlist = List.filter ft vars

	    fun h nil mm b = (mm, b)
	      | h (x::xs) mm b =
		h xs (remove' (mm, x)) (lenOp(x, mm, b))

	    val (m2', body'') = h hlist m2 body'
	in
	    (mapUnion (m1, m2'), F.LET (vars, lexp', body''))
	end

      | hoist (F.FIX (fundecs, body)) =
	let
	    fun hoistFundec (fk, lv,
			     lvtys : (F.lvar*F.lty) list,
			     body) =
		let
		    val varList = map #1 lvtys

		    val (m, b) = hoist body

		    fun ft x = M.inDomain (m, x)

		    val toHoist = List.filter ft varList

		    fun h mm nil b = (mm, b)
		      | h mm (v::vs) b =
			h (remove' (mm, v)) vs (lenOp(v, mm, b))

		    val (m', body') = h m toHoist b

		in
		    (*
		    sayABC ("List of extern vars in "^(lvname lv)^" (FIX): [");
		    sayVars (S.listItems set);
		    sayABC ("]\n");
		     *)
		    sayABC ("List of hoisted vars in "^
			    (lvname lv)^" (FIX): [");
		    sayVars (toHoist);
		    sayABC ("]\n");
		    (m', (fk, lv, lvtys, body'))
		end


	    (* fundec sets and bodys *)
	    val fsbody = map hoistFundec fundecs
	    val fsets = map #1 fsbody
	    val fbody = map #2 fsbody

	    val (bmap, newbody) = hoist body

	    val mmm = foldl mapUnion bmap fsets

	in
	    (mmm, F.FIX (fbody, newbody))
	end

      | hoist (F.APP x) = (M.empty, F.APP x)

      | hoist (F.TFN (tfundec as (tfkind, lv, tvtks, tfnbody), body)) =
	let
	    val (mtfn, btfn) = hoist tfnbody
	    val (m, b) = hoist body
	in
	    (mapUnion (mtfn, m), F.TFN (tfundec, b))
	end

      | hoist (F.TAPP (v, tl)) = (M.empty, F.TAPP (v, tl))

      (* if agressive, use union; otherwise use intersect *)
      (* no var defined, so no hoisting *)
      | hoist (F.SWITCH (v, consig, clexps, lexp)) =
	let
	    val lexps = map #2 clexps

	    val sblist = (map hoist lexps)

	    val maps = map #1 sblist
	    val bodys = map #2 sblist

	    val (defMap, defBody) =
		case lexp
		 of SOME l =>
		    let
			val (m, b) = hoist l
		    in
			(SOME m, SOME b)
		    end
		  | NONE => (NONE, NONE)

	    (* agressive may not always be benificial *)
	    (* it's turned off by default *)
	    val mapOper = if !agressiveHoist then mapUnion
			  else mapIntersect

	    val resSet = (foldl mapOper (hd maps) (tl maps))

	    fun helper nil nil = nil
	      | helper ((c, le)::xs) (le'::ys) =
		(c, le')::(helper xs ys)
	      | helper _ _ = bug "no!!!! help!!!!\n"

	    val resClexps = helper clexps bodys

	in
	    ((case defMap
	       of SOME m => mapOper(m, resSet)
		| NONE => resSet),
	     F.SWITCH (v, consig, resClexps, defBody))
	end

      (* there prob. isn't anything interesting here *)
      (* but anyways... *)
      | hoist (F.CON (d, tl, v, lv, le)) =
	let
	    val (m, b) = hoist le
	in
	    if M.inDomain (m, lv) then
		(remove' (m, lv),
		 F.CON (d, tl, v, lv, lenOp(lv, m, b)))
	    else (m, F.CON (d, tl, v, lv, b))
	end

      (* there probably isn't anything interesting here either *)
      (* but anyways... *)
      | hoist (F.RECORD (rk, vals, lv, le)) =
	let
	    val (m, b) = hoist le
	in
	    if M.inDomain (m, lv) then
		(remove' (m, lv),
		 F.RECORD (rk, vals, lv, lenOp(lv, m, b)))
	    else (m, F.RECORD (rk, vals, lv, b))
	end

      | hoist (F.SELECT (v, f, lv, le)) =
	let
	    val (m, b) = hoist le
	in
	    if (M.inDomain (m, lv)) then
		(remove' (m, lv),
		 F.SELECT (v, f, lv, lenOp(lv, m, b)))
	    else (m, F.SELECT (v, f, lv, b))
	end

      | hoist (F.RAISE (v, ltys)) =
	(M.empty, F.RAISE (v, ltys))

      | hoist (F.HANDLE (le, v)) =
	let
	    val (m, b) = hoist le
	in
	    (m, F.HANDLE (b, v))
	end

      (* what's used is just intersection of that of
       * the two branches
       *)
      | hoist (F.BRANCH (po, vals, le1, le2)) =
	let
	    val (m1, b1) = hoist le1
	    val (m2, b2) = hoist le2
	    val mapOper =
		if (!agressiveHoist) then mapUnion
		else mapIntersect
	in
	    (*
	    sayABC "for this branch: [";
	    sayVars (S.listItems (S.union (s1, s2)));
	    sayABC "]\n";
	     *)
	    (mapOper (m1, m2), F.BRANCH (po, vals, b1, b2))
	end

      (* the use site *)
      | hoist (F.PRIMOP(p as (d, PO.LENGTH, lty, tycs),
			vals, dest, body)) =
	let
	    val (m, b) = hoist body
	in
	    sayABC "got one!\n";
	    (case vals
	      of [F.VAR x] => (M.insert(m, x, lty),
			       F.PRIMOP(p, vals, dest, b))
	       | _ => (m, F.PRIMOP(p, vals, dest, b)))
	end

      (* the result of a primop is unlikely to be an
       * array, but anyways...
       *)

      | hoist (F.PRIMOP (p, vals, dest, body)) =
	let
	    val (m, b) = hoist body
	in
	    if M.inDomain (m, dest) then
		(remove' (m, dest),
		 F.PRIMOP (p, vals, dest, lenOp(dest, m, b)))
	    else (m, F.PRIMOP (p, vals, dest, b))
	end

    fun elimSwitches cmpsVV cmpsIV lexp = let

	val lt_cmp =
	    LT.ltc_tyc
		(LT.tcc_arrow (LT.ffc_fixed,
			       [LT.tcc_int, LT.tcc_int],
			       [LT.tcc_void]))

	fun g (F.LET ([lv],
		      br as
			 (F.BRANCH (p as (NONE,
					  PO.CMP {oper=PO.LTU,
						  kind=PO.UINT 31}, (* 64BIT: FIXME *)
					  lt_cmp,
					  nil),
				    [val1, val2],
				    tbr,
				    (* just to make sure it's an ABC *)
				    fbr as
					(F.RECORD
					     (_,_,_,
					      F.RECORD
						  (_,_,_,
						   F.PRIMOP
						       ((_,PO.WRAP,_,_), _, _,
							F.PRIMOP
							    ((_,PO.MARKEXN,_,_), _, _,
							     F.RAISE _))))))),
			 body)) =
	    let
		fun decide (F.VAR v1, F.VAR v2) =
		    let
			fun lookup (v1, v2) =
			    (sayABC ("cmp: looking for "^(lvname v1)^
				     " and "^(lvname v2)^"\n");

			     case (M.find (cmpsVV, v2))
			      of SOME set => S.member(set, v1)
			       | NONE => false)

			fun add (v1, v2) =
			    (sayABC ("cmp: entering "^(lvname v1)^
				     " and "^(lvname v2)^"\n");

			     case (M.find (cmpsVV, v2))
			      of SOME set =>
				 M.insert (cmpsVV, v2, S.add(set, v1))
			       | NONE =>
				 M.insert (cmpsVV, v2, S.singleton v1))
		    in
			if lookup (v1, v2) then (true, cmpsVV, cmpsIV)
			else (false, add(v1, v2), cmpsIV)
		    end

		  | decide (F.INT n, F.VAR v) = let
		      fun lookup (n, v) = (
			    sayABC (concat["looking for (", IntInf.toString n,"<"^
				     (lvname v)^")\n");
(* JHR
		  | decide (F.INT n, F.VAR v) =
		    let
			fun lookup (n, v) =
			    (sayABC ("looking for ("^
				     (Int.toString n)^"<"^
				     (lvname v)^")\n");
			     if n = 0 then true
			     else
				 (case M.find (cmpsIV, v)
				   of SOME x => (n <= x)
				    | NONE => false))

			fun add (n, v) =
			    M.insert(cmpsIV, v, n)

		    in
			if lookup(n, v) then (true, cmpsVV, cmpsIV)
			else (false, cmpsVV, add(n, v))
		    end
*)

		  | decide _ = (false, cmpsVV, cmpsIV)

		val (toElim, newVV, newIV) = decide (val1, val2)

	    in
		if toElim then
		    (case tbr
		      of F.PRIMOP (p, vals, lv1, F.RET [F.VAR lv2]) =>
			 if (lv1 = lv2) then F.PRIMOP (p, vals, lv, g body)
			 else F.LET ([lv], g tbr, g body)
		       | _ => F.LET ([lv], g tbr, g body))
		else
		    (F.LET ([lv],
			    F.BRANCH
				(p,
				 [val1, val2],
				 elimSwitches newVV newIV tbr,
				 g fbr),
				elimSwitches newVV newIV body))
	    end

	  | g (F.RET x) = F.RET x

	  | g (F.LET (vars, lexp, body)) =
	    F.LET (vars, g lexp, g body)

	  | g (F.FIX (fundecs, body)) =
	    F.FIX (map h fundecs, g body)

	  | g (F.APP (v, vs)) = F.APP (v, vs)

	  | g (F.TFN (tfundec, body)) =
	    F.TFN (tfundec, g body)

	  | g (F.TAPP (v, tycs)) = F.TAPP(v, tycs)

	  | g (F.SWITCH (v, consig, cel, lexpopt)) =
	    let
		fun hh (c, e) = (c, g e)

		val cel' = map hh cel

		fun gg (SOME x) = SOME (g x)
		  | gg NONE = NONE

	    in
		F.SWITCH (v, consig, cel', gg lexpopt)
	    end

	  | g (F.CON (dcon, tycs, v, lv, body)) =
	    F.CON (dcon, tycs, v, lv, g body)

	  | g (F.RECORD (rk, vals, lv, body)) =
	    F.RECORD (rk, vals, lv, g body)

	  | g (F.SELECT (v, field, lv, body)) =
	    F.SELECT (v, field, lv, g body)

	  | g (F.RAISE (v, ty)) = F.RAISE (v, ty)

	  | g (F.HANDLE (body, v)) = F.HANDLE (g body, v)

	  | g (F.BRANCH (p, vals, body1, body2)) =
	    F.BRANCH (p, vals, g body1, g body2)

	  | g (F.PRIMOP (p, vals, lv, body)) =
	    F.PRIMOP (p, vals, lv, g body)

	and h (fk, lvar, lvty, body) = (fk, lvar, lvty, g body)

    in
	g lexp
    end

    val (s, hoisted) = hoist progbody

    val csed = cse M.empty M.empty hoisted

    val elimed = elimSwitches M.empty M.empty csed

    (*		val optimized = (progkind, progname, progargs, elimed)*)
    val optimized = (progkind, progname, progargs, elimed)
in
    (* some advertising stuff! *)
    (* if !CTRL.printABC then
     * 	(say "\nhello! This is ABCOpt!\n";
     *
     * 	 (say "[Before ABCOpt...]\n\n";
     * 	  PP.printProg pgm);
     *
     * 	 (say "\n[After Hoisting...]\n\n";
     * 	  PP.printProg (progkind, progname, progargs, hoisted));
     *
     * 	 (say "\n[After CSE...]\n\n";
     * 	  PP.printProg (progkind, progname, progargs, csed));
     *
     * 	 (say "\n[After Elim...]\n\n";
     * 	  PP.printProg (progkind, progname, progargs, elimed));
     *
     * 	 say "\nbyebye! i'm done!\n\n")
     * else (); *)

    (* can eventually be removed after testing *)
    (*
    case (S.listItems s)
     of nil => ()
      | _ => bug "should be nil!!!";
     *)
    optimized
end
end
end
