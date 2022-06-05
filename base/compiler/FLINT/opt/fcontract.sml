(* fcontract.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Author: monnier@cs.yale.edu
 *)

signature FCONTRACT =
  sig

    type options = {etaSplit : bool, tfnInline : bool}

    (* needs Collect to be setup properly *)
    val contract : options -> FLINT.prog -> FLINT.prog

  end

(* All kinds of beta-reductions.  In order to do as much work per pass as
 * possible, the usage counts of each variable (maintained by the Collect
 * module) is kept as much uptodate as possible.  For instance as soon as a
 * variable becomes dead, all the variables that were referenced have their
 * usage counts decremented correspondingly.  This means that we have to
 * be careful to make sure that a dead variable will indeed not appear
 * in the output lexp since it might else reference other dead variables *)

(* things that fcontract does:
 * - several things not mentioned
 * - elimination of Con(Decon x)
 * - update counts when selecting a SWITCH alternative
 * - contracting RECORD(R.1,R.2) => R  (only if the type is easily available)
 * - dropping of dead arguments
 *)

(* things that lcontract.sml does that fcontract doesn't do (yet):
 * - inline across DeBruijn depths (will be solved by named-tvar)
 * - elimination of let [dead-vs] = pure in body
 *)

(* things that cpsopt/eta.sml did that fcontract doesn't do:
 * - let f vs = select(v,i,g,g vs)
 *)

(* things that cpsopt/contract.sml did that fcontract doesn't do:
 * - IF-idiom (I still don't know what it is)
 * - unifying branches
 * - Handler operations
 * - primops expressions
 * - branch expressions
 *)

(* things that could also be added:
 * - elimination of dead vars in let
 * - elimination of constant arguments
 *)

(* things that would require some type info:
 * - dropping foo in LET vs = RAISE v IN foo
 *)

(* eta-reduction is tricky:
 * - recognition of eta-redexes and introduction of the corresponding
 *   substitution in the table has to be done at the very beginning of
 *   the processing of the FIX
 * - eta-reduction can turn a known function into an escaping function
 * - fun f (g,v2,v3) = g(g,v2,v3) looks tremendously like an eta-redex
 *)

(* order of contraction is important:
 * - the body of a FIX is contracted before the functions because the
 *   functions might end up being inlined in the body in which case they
 *   could be contracted twice.
 *)

(* When creating substitution f->g (as happens with eta redexes or with
 * code like `LET [f] = RET[g]'), we need to make sure that the usage cout
 * of f gets properly transfered to g.  One way to do that is to make the
 * transfer incremental:  each time we apply the substitution, we decrement
 * f's count and increment g's count.  But this can be tricky since the
 * elimination of the eta-redex (or the trivial binding) eliminates one of the
 * references to g and if this is the only one, we might trigger the killing
 * of g even though its count would be later incremented.  Similarly, inlining
 * of g would be dangerous as long as some references to f exist.
 * So instead we do the transfer once and for all when we see the eta-redex,
 * which frees us from those two problems but forces us to make sure that
 * every existing reference to f will be substituted with g.
 * Also, the transfer of counts from f to g is not quite straightforward
 * since some of the references to f might be from inside g and without doing
 * the transfer incrementally, we can't easily know which of the usage counts
 * of f should be transfered to the internal counts of g and which to the
 * external counts.
 *)

(* Preventing infinite inlining:
 * - inlining a function in its own body amounts to unrolling which has
 *   to be controlled (you only want to unroll some number of times).
 *   It's currently simply not allowed.
 * - inlining a recursive function outside of tis body amounts to `peeling'
 *   one iteration. Here also, since the inlined body will have yet another
 *   call, the inlining risks non-termination.  It's hence also
 *   not allowed.
 * - inlining a mutually recursive function is just a more general form
 *   of the problem above although it can be safe and desirable in some cases.
 *   To be safe, you simply need that one of the functions forming the
 *   mutual-recursion loop cannot be inlined (to break the loop).  This cannot
 *   be trivially checked.  So we (foolishly?) trust the `inline' bit in
 *   those cases.  This is mostly used to inline wrappers inside the
 *   function they wrap.
 * - even if one only allows inlining of funtions showing no sign of
 *   recursion, we can be bitten by a program creating its own Y combinator:
 *       datatype dt = F of dt -> int -> int
 *       let fun f (F g) x = g (F g) x in f (F f) end
 *   To solve this problem, `cexp' has an `ifs' parameter containing the set
 *   of funtions that we are inlining in order to detect (and break) cycles.
 * - funnily enough, if we allow inlining recursive functions the cycle
 *   detection will ensure that the unrolling (or peeling) will only be done
 *   once.  In the future, maybe.
 *)

(* Dropping useless arguments.
 * Arguments whose value is constant (i.e. the function is known and each
 * call site provides the same value for that argument (or the argument
 * itself in the case of recursive calls) can be safely removed and replaced
 * inside the body by a simple let binding.  The only problem is that the
 * constant argument might be out of scope at the function definition site.
 * It is obviously always possible to move the function to bring the argument
 * in scope, but since we don't do any code motion here, we're stuck.
 * If it wasn't for this little problem, we could do the cst-arg removal in
 * collect (we don't gain anything from doing it here).
 * The removal of dead arguments (args not used in the body) on the other
 * hand can quite well be done in collect, the only problem being that it
 * is convenient to do it after the cst-arg removal so that we can rely
 * on deadarg to do the actual removal of the cst-arg.
 *)

(* Simple inlining (inlining called-once functions, which doesn't require
 * alpha-renaming) seems inoffensive enough but is not always desirable.
 * The typical example is wrapper functions introduced by eta-expand: they
 * usually (until inlined) contain the only call to the main function,
 * but inlining the main function in the wrapper defeats the purpose of the
 * wrapper.
 * cpsopt dealt with this problem by adding a `NO_INLINE_INTO' hint to the
 * wrapper function.  In this file, the idea is the following:
 * If you have a function declaration like `let f x = body in exp', first
 * contract `exp' and only contract `body' afterwards.  This ensures that
 * the eta-wrapper gets a chance to be inlined before it is (potentially)
 * eta-reduced away.  Interesting details:
 * - all functions (even the ones that would have a `NO_INLINE_INTO') are
 *   contracted, because the "aggressive usage count maintenance" makes any
 *   alternative painful (the collect phase has already assumed that dead code
 *   will be eliminated, which means that fcontract should at the very least
 *   do the dead-code elimination, so you can only avoid fcontracting a
 *   a function if you can be sure that the body doesn't contain any dead-code,
 *   which is generally  not known).
 * - once a function is fcontracted, its inlinable status is re-examined.
 *   More specifically, if no inlining occured during its fcontraction, then
 *   we assume that the code has just become smaller and should hence
 *   still be considered inlinable.  On another hand, if inlining took place,
 *   then we have to reset the inline-bit because the new body might
 *   be completely different (i.e. much bigger) and inlining it might be
 *   undesirable.
 *   This means that in the case of
 *       let fwrap x = body1 and f y = body2 in exp
 *   if fwrap is fcontracted before f and something gets inlined into it,
 *   then fwrap cannot be inlined in f.
 *   To minimize the impact of this problem, we make sure that we fcontract
 *   inlinable functions only after fcontracting other mutually recursive
 *   functions.  One way to solve the problem more thoroughly would be
 *   to keep the uncontracted fwrap around until f has been contracted.
 *   Such a trick hasn't seemed necessary yet.
 * - at the very end of the optimization phase, cpsopt had a special pass
 *   that ignored the `NO_INLINE_INTO' hint (since at this stage, inlining
 *   into it doesn't have any undesirable side effects any more).  The present
 *   code doesn't need such a thing.  On another hand, the cpsopt approach
 *   had the advantage of keeping the `inline' bit from one contract phase to
 *   the next.  If this ends up being important, one could add a global
 *   "noinline" flag that could be set to true whenever fcontracting an
 *   inlinable function (this would ensure that fcontracting such an inlinable
 *   function can only reduce its size, which would allow keeping the `inline'
 *   bit set after fcontracting).
 *)

structure FContract :> FCONTRACT =
  struct

    structure F  = FLINT
    structure M  = LambdaVar.Map
    structure S  = LambdaVar.Set
    structure C  = Collect
    structure O  = Option
    structure DI = DebIndex
    structure PP = PPFlint
    structure FU = FlintUtil
    structure LT = LtyExtern
    structure LK = LtyKernel
    structure OU = OptUtils
    structure PO = Primop
    structure CTRL = FLINT_Control

    fun say s = (Control_Print.say s; Control_Print.flush())
    fun bug msg = ErrorMsg.impossible ("FContract: "^msg)
    fun buglexp (msg,le) = (say "\n"; PP.printLexp le; bug msg)
    fun bugval (msg,v) = (say "\n"; PP.printSval v; bug msg)

(* fun sayexn e = app say (map (fn s => s^" <- ") (SMLofNJ.exnHistory e)) *)

    val cplv = LambdaVar.dupLvar
    val mklv = LambdaVar.mkLvar

    fun tagInt n = F.INT{ival = IntInf.fromInt n, ty = Target.defaultIntSz}

    type options = {etaSplit : bool, tfnInline : bool}

    datatype sval
      = Val    of F.value			(* F.value should never be F.VAR lv *)
      | Fun    of F.lvar * F.lexp * (F.lvar * F.lty) list * F.fkind * sval list list ref
      | TFun   of F.lvar * F.lexp * (F.tvar * F.tkind) list * F.tfkind
      | Record of F.lvar * sval list
      | Con    of F.lvar * sval * F.dcon * F.tyc list
      | Decon  of F.lvar * sval * F.dcon * F.tyc list
      | Select of F.lvar * sval * int
      | Var    of F.lvar * F.lty option		(* cop out case *)

  (* a map that tracks the bindings of the lvars *)
    type bindings = sval M.map

    fun sval2lty (Var(_,x)) = x
      | sval2lty (Decon(_,_,(_,_,lty),tycs)) =
	  SOME(hd(#2 (LT.ltd_arrow (hd(LT.lt_inst(lty, tycs))))))
      | sval2lty (Select(_,sv,i)) =
	  (case sval2lty sv of SOME lty => SOME(LT.lt_select(lty, i)) | _ => NONE)
      | sval2lty _ = NONE

    fun tycs_eq ([],[]) = true
      | tycs_eq (tyc1::tycs1,tyc2::tycs2) =
	  LT.tc_eqv(tyc1,tyc2) andalso tycs_eq(tycs1,tycs2)
      | tycs_eq _ = false

  (* calls `code' to append a lexp to each leaf of `le'.
   * Typically used to transform `let lvs = le in code' so that
   * `code' is now copied at the end of each branch of `le'.
   * `lvs' is a list of lvars that should be used if the result of `le'
   * needs to be bound before calling `code'.
   *)
    fun append lvs code le =
	let fun l (F.RET vs) = code vs
	      | l (le as (F.APP _ | F.TAPP _ | F.RAISE _ | F.HANDLE _)) =
		let val lvs = map (fn lv => let val nlv = cplv lv
					    in C.new NONE nlv; nlv end)
				  lvs
		in F.LET(lvs, le, code(map F.VAR lvs))
		end
	      | l (F.LET (lvs,body,le)) = F.LET(lvs,body, l le)
	      | l (F.FIX (fdecs,le)) = F.FIX(fdecs, l le)
	      | l (F.TFN (tfdec,le)) = F.TFN(tfdec, l le)
	      | l (F.SWITCH (v,ac,arms,def)) =
		let fun larm (con,le) = (con, l le)
		in F.SWITCH(v, ac, map larm arms, O.map l def)
		end
	      | l (F.CON (dc,tycs,v,lv,le)) = F.CON(dc, tycs, v, lv, l le)
	      | l (F.RECORD (rk,vs,lv,le)) = F.RECORD(rk, vs, lv, l le)
	      | l (F.SELECT (v,i,lv,le)) = F.SELECT(v, i, lv, l le)
	      | l (F.BRANCH (po,vs,le1,le2)) = F.BRANCH(po, vs, l le1, l le2)
	      | l (F.PRIMOP (po,vs,lv,le)) = F.PRIMOP(po, vs, lv, l le)
	in l le
	end

  (* `extract' extracts the code of a switch arm into a function
   * and replaces it with a call to that function
   *)
    fun extract (con,le) =
	let val f = mklv()
	    val fk = {isrec=NONE,known=true,inline=F.IH_SAFE,
		      cconv=F.CC_FUN(Lty.FF_FIXED)}
	in case con of
	    F.DATAcon(dc as (_,_,lty),tycs,lv) =>
	    let val nlv = cplv lv
		val _ = C.new (SOME[lv]) f
		val _ = C.use false (C.new NONE nlv)
		val (lty,_) = LT.ltd_parrow(hd(LT.lt_inst(lty, tycs)))
	    in ((F.DATAcon(dc, tycs, nlv),
		 F.APP(F.VAR f, [F.VAR nlv])),
		(fk, f, [(lv, lty)], le))
	    end
	  | con =>
	    let val _ = C.new (SOME[]) f
	    in ((con, F.APP(F.VAR f, [])),
		(fk, f, [], le))
	    end
	end

    fun inScope m lv = Option.isSome(M.find(m,lv))

    fun click s c = (if !CTRL.misc = 1 then say s else ();
		     c := !c + 1 (* Stats.addCounter c 1 *) )

    fun contract {etaSplit,tfnInline} (fdec as (_,f,_,_)) = let

	  val c_dummy = ref 0 (* Stats.newCounter[] *)
	  val c_miss = ref 0 (* Stats.newCounter[] *)

	  val counter = c_dummy

	  fun click_deadval  () = (click "d" counter)
	  fun click_deadlexp () = (click "D" counter)
	  fun click_select   () = (click "s" counter)
	  fun click_record   () = (click "r" counter)
	  fun click_con      () = (click "c" counter)
	  fun click_switch   () = (click "s" counter)
	  fun click_eta      () = (click "e" counter)
	  fun click_etasplit () = (click "E" counter)
	  fun click_branch   () = (click "b" counter)
	  fun click_dropargs () = (click "a" counter)

	  fun click_lacktype () = (click "t" c_miss)

	  (* this counters is actually *used* by fcontract.
	   * It's  not used just for statistics. *)
	  val c_inline	 = ref 0 (* Stats.newCounter[counter] *)
	  fun click_simpleinline () = (click "i" c_inline)
	  fun click_copyinline   () = (click "I" c_inline)
	  fun click_unroll       () = (click "u" c_inline)
	  fun inline_count () = (* Stats.getCounter *) !c_inline

	  fun used lv = (C.usenb(C.get lv) > 0)
			    (* handle x =>
			    (say("while in FContract.used "^(C.LVarString lv)^"\n");
			     raise x) *)

	  fun eqConV (F.INTcon i1,	F.INT i2)	= (#ival i1 = #ival i2)
	    | eqConV (F.WORDcon i1,	F.WORD i2)	= (#ival i1 = #ival i2)
	    | eqConV (F.STRINGcon s1,	F.STRING s2)	= s1 = s2
	    | eqConV (con, v) = bugval("unexpected comparison with val", v)

	  exception Lookup
	  fun lookup m lv = (case M.find(m,lv)
		 of NONE => (
		      say "\nlooking up unbound ";
		      say (!PP.LVarString lv);
		      raise Lookup)
		  | SOME x => x
		(*esac*))

	  fun sval2val sv = (case sv
		 of (Fun{1=lv,...} | TFun{1=lv,...} | Record{1=lv,...} | Decon{1=lv,...}
		  | Con{1=lv,...} | Select{1=lv,...} | Var{1=lv,...}) => F.VAR lv
		  | Val v => v
		(*esac*))

	  fun val2sval m (F.VAR ov) =
	      ((lookup m ov) (* handle x =>
	       (say("val2sval "^(C.LVarString ov)^"\n"); raise x) *) )
	    | val2sval m v = Val v

	  fun bugsv (msg,sv) = bugval(msg, sval2val sv)

	  fun subst m ov = sval2val (lookup m ov)
	  fun substval m = sval2val o (val2sval m)
	  fun substvar m lv =
	      case substval m (F.VAR lv)
	       of F.VAR lv => lv
		| v => bugval ("unexpected val", v)

	  (* called when a variable becomes dead.
	   * it simply adjusts the use-counts *)
	  fun undertake m lv = let
		val undertake = undertake m
		in case lookup m lv
		    of Var {1=nlv,...}	 => ()
		     | Val v		 => ()
		     | Fun (lv,le,args,_,_) =>
		       C.unuselexp undertake
				   (F.LET(map #1 args,
					  F.RET (map (fn _ => tagInt 0) args),
					  le))
		     | TFun{1=lv,2=le,...} =>
		       C.unuselexp undertake le
		     | (Select {2=sv,...} | Con {2=sv,...}) => unusesval m sv
		     | Record {2=svs,...} => app (unusesval m) svs
		     (* decon's are implicit so we can't get rid of them *)
		     | Decon _ => ()
		end
		    handle
			Lookup =>
			  (say("Unable to undertake "^(C.LVarString lv)^"\n"))
		      | x =>
			  (say("while undertaking "^(C.LVarString lv)^"\n");
			   raise x)

	  and unusesval m sv = unuseval m (sval2val sv)
	  and unuseval m (F.VAR lv) =
	      if (C.unuse false (C.get lv)) then undertake m lv else ()
	    | unuseval f _ = ()
	  fun unusecall m lv =
	      if (C.unuse true (C.get lv)) then undertake m lv else ()

	  fun addbind (m,lv,sv) = M.insert(m, lv, sv)

	  (* substitute a value sv for a variable lv and unuse value v. *)
	  fun substitute (m, lv1, sv, v) =
	      (case sval2val sv of F.VAR lv2 => C.transfer(lv1,lv2) | v2 => ();
	       unuseval m v;
	       addbind(m, lv1, sv)) (* handle x =>
		   (say ("while substituting "^
			 (C.LVarString lv1)^
			 " -> ");
		    PP.printSval (sval2val sv);
		    raise x) *)

	  (* common code for primops *)
	  fun cpo m (SOME{default,table},po,lty,tycs) =
	      (SOME{default=substvar m default,
		    table=map (fn (tycs,lv) => (tycs, substvar m lv)) table},
	       po,lty,tycs)
	    | cpo _ po = po

	  fun cdcon m (s,Access.EXN(Access.LVAR lv),lty) =
	      (s, Access.EXN(Access.LVAR(substvar m lv)), lty)
	    | cdcon _ dc = dc

	(* The set of functions which are apparently non-recursive but which seem
	 * to recurse nevertheless, typically with the help of a type-level
	 * recursion.
	 *)
	  val recursive_funs = ref S.empty

	(* ifs (inlined functions): records which functions we're currently inlining
	 *     in order to detect loops
	 * m: is a map lvars to their defining expressions (svals)
	 * le : expression to contract
	 * cont: context continuation
	 *)
	  fun fcexp (ifs : S.set) (m : bindings) (le : F.lexp) (cont : bindings * F.lexp -> F.lexp) = let
		val loop = fcexp ifs
		val substval = substval m
		val cdcon = cdcon m
		val cpo = cpo m

		fun fcLet (lvs, le, body) = let
		      fun fcbody (nm,nle) = let
			    fun cbody () = let
				  val nm = (foldl (fn (lv,m) => addbind(m, lv, Var(lv, NONE))) nm lvs)
				  in case loop nm body cont
				      of F.RET vs =>
					  if ListPair.allEq (fn (v, lv) => FU.sameValue(v, F.VAR lv)) (vs, lvs)
					    then nle
					    else F.LET(lvs, nle, F.RET vs)
				       | nbody => F.LET(lvs, nle, nbody)
				  end
			    in case nle
				of F.RET vs =>
				   let fun simplesubst (lv,v,m) =
					   let val sv = val2sval m v
					   in substitute(m, lv, sv, sval2val sv)
					   end
				       val nm = (ListPair.foldl simplesubst nm (lvs, vs))
				   in loop nm body cont
				   end
				 | F.TAPP _ =>
				   if List.all (C.dead o C.get) lvs
				   then loop nm body cont
				   else cbody()
				 | _ => cbody()
			    end (* fcbody *)

		    (* this is a hack originally meant to cleanup the BRANCH
		     * mess introduced in flintnm (where each branch returns
		     * just true or false which is generally only used as
		     * input to a SWITCH).
		     * The present code does more than clean up this case.
		     *)
		      fun cassoc (lv,F.SWITCH(F.VAR v,ac,arms,NONE),wrap) =
			    if lv <> v orelse C.usenb(C.get lv) > 1 then loop m le fcbody else
			    let val (narms,fdecs) =
				    ListPair.unzip (map extract arms)
				fun addswitch [v] =
				    C.copylexp
					M.empty
					(F.SWITCH(v,ac,narms,NONE))
				  | addswitch _ = bug "prob in addswitch"
				(* replace each leaf `ret' with a copy
				 * of the switch *)
				val nle = append [lv] addswitch le
				(* decorate with the functions extracted
				 * from the switch arms *)
				val nle =
				    foldl (fn (f,le) => F.FIX([f],le))
					  (wrap nle) fdecs
				in
				    click_branch();
				    loop m nle cont
				end
			| cassoc _ = loop m le fcbody

		      in case (lvs, le, body)
			  of ([lv],(F.BRANCH _ | F.SWITCH _),F.SWITCH _) =>
			     cassoc(lv, body, fn x => x)
			   | ([lv],(F.BRANCH _ | F.SWITCH _),F.LET(lvs,body as F.SWITCH _,rest)) =>
			     cassoc(lv, body, fn le => F.LET(lvs,le,rest))
			   | _ =>
			     loop m le fcbody
		      end (* fcLet *)

		fun fcFix (fs, le) = let
		    (* merge actual arguments to extract the constant subpart *)
		      fun merge_actuals ((lv,lty),[],m) = addbind(m, lv, Var(lv, SOME lty))
			| merge_actuals ((lv,lty),a::bs,m) = addbind(m, lv, Var(lv, SOME lty))
			    (* FIXME:  there's a bug here, but it's not caught by chkflint
			       let fun f (b::bs) =
				    if sval2val a = sval2val b then f bs
				    else addbind(m, lv, Var(lv, SOME lty))
				  | f [] =
				    (click "C" c_cstarg;
				     case sval2val a
				      of v as F.VAR lv' =>
					 (* FIXME: this inScope check is wrong for non-recursive
					  * functions.  But it only matters if the function is
					  * passed itself as a parameter which cannot happen
					  * with the current type system I believe. *)
					 if inScope m lv' then
					     let val sv =
						     case a of Var (v,NONE) => Var(v, SOME lty)
							     | _ => a
					     in substitute(m, lv, sv, v)
					     end
					 else (click "O" c_outofscope;

					       addbind(m, lv, Var(lv, SOME lty)))
				       | v => substitute(m, lv, a, v))
			    in f bs
			    end *)
		    (* The actual function contraction *)
		      fun fcFun (application, (m, fs)) = let
			    val (f, body, args, fk as {inline,cconv,known,isrec}, actuals) = application
			    val fi = C.get f
			    in if C.dead fi then (m,fs)
			       else if C.iusenb fi = C.usenb fi then
				   (* we need to be careful that undertake not be called
				    * recursively *)
				   (C.use false fi; undertake m f; (m,fs))
			       else
				   let (*val _ = say (concat["Entering ", C.LVarString f, "\n"]) *)
				       val saved_ic = inline_count()
				       (* make up the bindings for args inside the body *)
				       val actuals = if isSome isrec orelse
							C.escaping fi orelse
							null(!actuals)
						     then map (fn _ => []) args
						     else OU.transpose(!actuals)
				       val nm = ListPair.foldl merge_actuals m (args, actuals)
				       (* contract the body and create the resulting fundec.
					* Temporarily remove f's definition from the
					* environment while we're rebuilding it to avoid
					* nasty problems. *)
				       val nbody = fcexp (S.add(ifs, f))
							 (addbind(nm, f, Var(f, NONE)))
							 body #2
				       (* if inlining took place, the body might be completely
					* changed (read: bigger), so we have to reset the
					* `inline' bit *)
				       val nfk = {isrec=isrec, cconv=cconv,
						  known=known orelse not(C.escaping fi),
						  inline=if inline_count() = saved_ic
							 then inline
							 else F.IH_SAFE}
				       (* update the binding in the map.  This step is
					* not just a mere optimization but is necessary
					* because if we don't do it and the function
					* gets inlined afterwards, the counts will reflect the
					* new contracted code while we'll be working on the
					* the old uncontracted code *)
				       val nm = addbind(m, f, Fun(f, nbody, args, nfk, ref []))
				   in (nm, (nfk, f, args, nbody)::fs)
				   (* before say (concat["Exiting ", C.LVarString f, "\n"]) *)
				   end
			    end (* fcFun *)

		    (* check for eta redex *)
		      fun fcEta (fdec as (f,F.APP(F.VAR g,vs),args,_,_), (m,fs,hs)) =
			    if List.length args = List.length vs andalso
				ListPair.allEq (fn (v,(lv,t)) =>
						 case v of F.VAR v => v = lv andalso lv <> g
							 | _ => false)
						(vs, args)
			    then
				let val svg = lookup m g
				    val g = case sval2val svg
					     of F.VAR g => g
					      | v => bugval("not a variable", v)
				(* NOTE: we don't want to turn a known function into an
				 * escaping one.  It's dangerous for optimisations based
				 * on known functions (elimination of dead args, f.ex)
				 * and could generate cases where call>use in collect.
				 * Of course, if g is not a locally defined function (it's
				 * bound by a LET or as an argument), then knownness is
				 * irrelevant. *)
				in if f = g orelse
				    ((C.escaping(C.get f)) andalso
				     not(C.escaping(C.get g)) andalso
				     (case svg of Fun _ => true | _ => false))
				   (* the default case could ensure the inline *)
				   then (m, fdec::fs, hs)
				   else let
				       (* if an earlier function h has been eta-reduced
					* to f, we have to be careful to update its
					* binding to not refer to f any more since f
					* will disappear *)
				      fun add (h, m) =
					    if FU.sameValue(sval2val(lookup m h), F.VAR f)
					      then addbind(m, h, svg)
					      else m
				      val m = foldl add m hs
				      in
				       (* I could almost reuse `substitute' but the
					* unuse in substitute assumes the val is escaping *)
				        click_eta();
				        C.transfer(f, g);
				        unusecall m g;
				        (addbind(m, f, svg), fs, f::hs)
				      end
				end
			    else (m, fdec::fs, hs)
			| fcEta (fdec,(m,fs,hs)) = (m,fdec::fs,hs)

		    (* add wrapper for various purposes *)
		      fun wrap (f as (fk as {isrec,inline,...},g,args,body):F.fundec, fs) =
			    let val gi = C.get g
				fun dropargs filter = let
				      val (nfk,nfk') = OU.fk_wrap(fk, O.map #1 isrec)
				      val args' = filter args
				      val ng = cplv g
				      val nargs = map (fn (v,t) => (cplv v, t)) args
				      val nargs' = map #1 (filter nargs)
				      val appargs = (map F.VAR nargs')
				      val nf = (nfk, g, nargs, F.APP(F.VAR ng, appargs))
				      val nf' = (nfk', ng, args', body)
				      val ngi = C.new (SOME(map #1 args')) ng
				      in
					C.ireset gi;
					app (ignore o (C.new NONE) o #1) nargs;
					C.use true ngi;
					app (C.use false o C.get) nargs';
					nf'::nf::fs
				      end
			    in
				(* Don't introduce wrappers for escaping-only functions.
				 * This is debatable since although wrappers are useless
				 * on escaping-only functions, some of the escaping uses
				 * might turn into calls in the course of fcontract, so
				 * by not introducing wrappers here, we avoid useless work
				 * but we also postpone useful work to later invocations. *)
				if C.dead gi then fs
				else if inline=F.IH_ALWAYS then f::fs else
				    let val used = map (used o #1) args
				    in if C.called gi then
					(* if some args are not used, let's drop them *)
					if not (List.all (fn x => x) used) then
					    (click_dropargs();
					     dropargs (fn xs => OU.filter used xs))

					(* eta-split: add a wrapper for escaping uses *)
					else if etaSplit andalso C.escaping gi then
					    (* like dropargs but keeping all args *)
					    (click_etasplit(); dropargs (fn x => x))

					else f::fs
				       else f::fs
				    end
			    end (* wrap *)

		      (* add various wrappers *)
		      val fs = foldl wrap [] fs

		      (* register the new bindings (uncontracted for now) *)
		      val (nm,fs) = foldl (fn (fdec as (fk,f,args,body),(m,fs)) =>
					   let val nf = (f, body, args, fk, ref [])
					   in (addbind(m, f, Fun nf), nf::fs) end)
					  (m,[]) fs
		      (* check for eta redexes *)
		      val (nm,fs,_) = foldl fcEta (nm,[],[]) fs

		      val (wrappers,funs) =
			  List.partition (fn (_,_,_,{inline=F.IH_ALWAYS,...},_) => true
					   | _ => false) fs
		      val (maybes,funs) =
			  List.partition (fn (_,_,_,{inline=F.IH_MAYBE _,...},_) => true
					   | _ => false) funs

		      (* First contract the big inlinable functions.  This might make them
		       * non-inlinable and we'd rather know that before we inline them.
		       * Then we inline the body (so that we won't go through the inline-once
		       * functions twice), then the normal functions and finally the wrappers
		       * (which need to come last to make sure that they get inlined if
		       * at all possible) *)
		      val fs = []
		      val (nm,fs) = foldl fcFun (nm,fs) maybes
		      val nle = loop nm le cont
		      val (nm,fs) = foldl fcFun (nm,fs) funs
		      val (nm,fs) = foldl fcFun (nm,fs) wrappers
		      (* junk newly unused funs *)
		      val fs = List.filter (used o #2) fs
		      in
			case fs
			 of [] => nle
			  | [f1 as ({isrec=NONE,...},_,_,_),f2] =>
			    (* gross hack: `wrap' might have added a second
			     * non-recursive function.  we need to split them into
			     * 2 FIXes.  This is _very_ ad-hoc. *)
			    F.FIX([f2], F.FIX([f1], nle))
			  | _ => F.FIX(fs, nle)
		      end (* fcFix *)

		fun fcApp (f as F.VAR f', vs) = let
		      val svs = map (val2sval m) vs
		      val svf = val2sval m f
		    (* F.APP inlining (if any) *)
		      in case svf
			  of Fun(g,body,args,{inline,...},actuals) =>
			     (* eta contraction could cause f to map to a different function g,
			      * which, in turn, might have already been contracted, so we need
			      * to make sure that we get the latest version of the function.
			      *)
			       if (f' <> g) then fcApp (F.VAR g, vs)
			       else let
			         val gi = C.get g
				 fun noinline () =
				     (actuals := svs :: (!actuals);
				      cont(m, F.APP(sval2val svf, map sval2val svs)))
				 fun simpleinline () =
				     (* simple inlining:  we should copy the body and then
				      * kill the function, but instead we just move the body
				      * and kill only the function name.
				      * This inlining strategy looks inoffensive enough,
				      * but still requires some care: see comments at the
				      * begining of this file and in cfun *)
				     (* FIXME: this may cause body to be optimized a second
				      * time.  Usually, this is not a big deal, but it can
				      * lead to infinite inlining in the following case:
				      * inlining a call to F generates a function G which
				      * contains a call to F (not inlined this time around
				      * thanks to ifs), later on G gets simpleinlined at which
				      * point the new call to F does get inlined, ...
				      * This particular case is handled by recursive_funs,
				      * but there might be other problematic scenarios.  *)
				     (click_simpleinline();
				      (* say(concat["fcApp.simpleinline ", PPFlint.toStringValue f, " -> ", C.LVarString g, "\n"]); *)
				      ignore(C.unuse true gi);
				      loop m (F.LET(map #1 args, F.RET vs, body)) cont)
				 fun copyinline () =
				     (* aggressive inlining.  We allow pretty much
				      * any inlinling, but we detect and reject inlining
				      * recursively which would else lead to infinite loop *)
				     if S.member(ifs, g) orelse S.member(!recursive_funs, g) then
				       (* We're trying to inline a function we're already in
					* the process of inlining (or which we found earlier
					* to be recursive).  This means this function is
					* declared as non-recursive, whereas it does recurse in
					* practice.  Record it in recursive_funs to make sure
					* we won't try to inline it ever again, even after
					* we're done inlining it.  *)
				       (recursive_funs := S.add(!recursive_funs, g);
					noinline()) else
				     (* Do the actual inlining.  Random half-related note:
				      * unrolling is not as straightforward as it seems:
				      * if you inline the function you're currently
				      * fcontracting, you're asking for trouble: there is a
				      * hidden assumption in the counting that the old code
				      * will be replaced by the new code (and is hence dead).
				      * If the function to be unrolled has the only call to
				      * function f, then f might get simpleinlined before
				      * unrolling, which means that unrolling will introduce
				      * a second occurence of the `only call' but at that point
				      * f has already been killed. *)
				     let val nle = (F.LET(map #1 args, F.RET vs, body))
					 val nle = C.copylexp M.empty nle
				     in
					 click_copyinline();
					 (app (unuseval m) vs);
					 unusecall m g;
					 (* say("copyinline "^(C.LVarString g)^"\n"); *)
					 fcexp (S.add(ifs, g)) m nle cont
				     end

			         in if C.usenb gi = 1
				   (* Not sure why/if this is needed.  *)
				   andalso not(S.member(ifs, g))
				    then simpleinline()
				    else case inline of
					F.IH_SAFE => noinline()
				      | F.IH_UNROLL => noinline()
				      | F.IH_ALWAYS => copyinline()
				      | F.IH_MAYBE(min,ws) =>
					let fun value w _ (Val _ | Con _ | Record _) = w
					      | value w v (Fun (f,_,args,_,_)) =
						if C.usenb(C.get v) = 1 then w * 2 else w
					      | value w _ _ = 0
					    val s = (OU.foldl3 (fn (sv,w,(v,t),s) => value w v sv + s)
							       0 (svs,ws,args))
							handle OU.Unbalanced => 0
					in if s > min then copyinline() else noinline()
					end
			         end
			   | sv => cont(m, F.APP(sval2val svf, map sval2val svs))
		      end (* fcApp *)
		  | fcApp (f, vs) = let
		      val svs = map (val2sval m) vs
		      val svf = val2sval m f
		      in
			cont (m, F.APP(sval2val svf, map sval2val svs))
		      end
		fun fcTfn ((tfk,f,args,body),le) = let
		      val fi = C.get f
		      in if C.dead fi then (click_deadlexp(); loop m le cont) else
			  let val saved_ic = inline_count()
			      val nbody = fcexp ifs m body #2
			      val ntfk =
				  if inline_count() = saved_ic then tfk else {inline=F.IH_SAFE}
			      val nm = addbind(m, f, TFun(f, nbody, args, tfk))
			      val nle = loop nm le cont
			  in
			      if C.dead fi then nle else F.TFN((tfk, f, args, nbody), nle)
			  end
		      end (* fcTfn *)

		fun fcTapp (f,tycs) = let
		      val svf = val2sval m f
		    (* F.TAPP inlining (if any) *)
		      fun noinline () = (cont(m, F.TAPP(sval2val svf, tycs)))
		      fun specialize (g,tfk,args,body,tycs) = let
			    val prog =
				  ({cconv=F.CC_FCT,inline=F.IH_SAFE,isrec=NONE,known=false},
				   mklv(), [],
				   F.TFN((tfk, g, args, body), F.TAPP(F.VAR g, tycs)))
			    in
			      case #4(Specialize.specialize prog) of
				  F.LET(_,nprog,F.RET _) => (PP.printLexp nprog; nprog)
				| _ => bug "specialize"
			    end

		      in case (tfnInline,svf)
			  of (true,TFun(g,body,args,tfk as {inline,...})) =>
			     let val gi = C.get g
				 fun simpleinline () =
				     (* simple inlining:  we should copy the body and then
				      * kill the function, but instead we just move the body
				      * and kill only the function name.
				      * This inlining strategy looks inoffensive enough,
				      * but still requires some care: see comments at the
				      * begining of this file and in cfun *)
				     (click_simpleinline();
				      (* say(concat["fcTapp.simpleinline ", C.LVarString g, "\n"]); *)
				      ignore(C.unuse true gi);
				      loop m (specialize(g, tfk, args, body, tycs)) cont)
				 fun copyinline () =
				     (* aggressive inlining.  We allow pretty much
				      * any inlinling, but we detect and reject inlining
				      * recursively which would else lead to infinite loop *)
				     let val nle = (F.TFN((tfk, g, args, body),
							  F.TAPP(F.VAR g, tycs)))
					 val nle = C.copylexp M.empty nle
				     in
					 click_copyinline();
					 (*  say("copyinline "^(C.LVarString g)^"\n"); *)
					 unusecall m g;
					 fcexp (S.add(ifs, g)) m nle cont
				     end

			     in if C.usenb gi = 1 andalso not(S.member(ifs, g))
				then noinline() (* simpleinline() *)
				else case inline of
				    F.IH_ALWAYS =>
				    if S.member(ifs, g) then noinline() else copyinline()
				  | _ => noinline()
			     end
			   | sv => noinline()
		      end (* fcTapp *)

		fun fcSwitch (v,ac,arms,def) = let
		      fun fcsCon (lvc,svc,dc1:F.dcon,tycs1) = let
			    fun killle le = C.unuselexp (undertake m) le
			    fun kill lv le =
				C.unuselexp (undertake (addbind(m,lv,Var(lv,NONE)))) le
			    fun killarm (F.DATAcon(_,_,lv),le) = kill lv le
			      | killarm _ = buglexp("bad arm in switch(con)", le)

			    fun carm ((F.DATAcon(dc2,tycs2,lv),le)::tl) =
				(* sometimes lty1 <> lty2 :-( so this doesn't work:
				 *  FU.dcon_eq(dc1, dc2) andalso tycs_eq(tycs1,tycs2) *)
				if #2 dc1 = #2 (cdcon dc2) then
				    (map killarm tl; (* kill the rest *)
				     O.map killle def; (* and the default case *)
				     loop (substitute(m, lv, svc, F.VAR lvc))
				     le cont)
				else
				    (* kill this arm and continue with the rest *)
				    (kill lv le; carm tl)
			      | carm [] = loop m (O.valOf def) cont
			      | carm _ = buglexp("unexpected arm in switch(con,...)", le)
			    in click_switch(); carm arms
			    end (* fcsCon *)

		      fun fcsVal v = let
			    fun kill le = C.unuselexp (undertake m) le
			    fun carm ((con,le)::tl) =
				if eqConV(con, v) then
				    (map (kill o #2) tl;
				     O.map kill def;
				     loop m le cont)
				else (kill le; carm tl)
			      | carm [] = loop m (O.valOf def) cont
			    in click_switch(); carm arms
			    end

		      fun fcsDefault (sv,lvc) = (case (arms,def)
			     of ([(F.DATAcon(dc,tycs,lv),le)],NONE) =>
				(* this is a mere DECON, so we can push the let binding
				 * (hidden in cont) inside and maybe even drop the DECON *)
				let val ndc = cdcon dc
				    val slv = Decon(lv, sv, ndc, tycs)
				    val nm = addbind(m, lv, slv)
				    (* see below *)
				    (* val nm = addbind(nm, lvc, Con(lvc, slv, ndc, tycs)) *)
				    val nle = loop nm le cont
				    val nv = sval2val sv
				in
				    if used lv then
					F.SWITCH(nv,ac,[(F.DATAcon(ndc,tycs,lv),nle)],NONE)
				    else (unuseval m nv; nle)
				end
			      | (([(_,le)],NONE) | ([],SOME le)) =>
				(* This should never happen, but we can optimize it away *)
				(unuseval m (sval2val sv); loop m le cont)
			      | _ =>
				let fun carm (F.DATAcon(dc,tycs,lv),le) =
					let val ndc = cdcon dc
					    val slv = Decon(lv, sv, ndc, tycs)
					    val nm = addbind(m, lv, slv)
					(* we can rebind lv to a more precise value
					 * !!BEWARE!!  This rebinding is misleading:
					 * - it gives the impression that `lvc' is built
					 *   from`lv' although the reverse is true:
					 *   if `lvc' is undertaken, `lv's count should
					 *   *not* be updated!
					 *   Luckily, `lvc' will not become dead while
					 *   rebound to Con(lv) because it's used by the
					 *   SWITCH. All in all, it works fine, but it's
					 *   not as straightforward as it seems.
					 * - it seems to be a good idea, but it can hide
					 *   other opt-opportunities since it hides the
					 *   previous binding. *)
					(* val nm = addbind(nm, lvc, Con(lvc,slv,ndc,tycs)) *)
					in (F.DATAcon(ndc, tycs, lv), loop nm le #2)
					end
				      | carm (con,le) = (con, loop m le #2)
				    val narms = map carm arms
				    val ndef = Option.map (fn le => loop m le #2) def
				in cont(m, F.SWITCH(sval2val sv, ac, narms, ndef))
				end)

		      in case val2sval m v
			  of sv as Con x => fcsCon x
			   | sv as Val v => fcsVal v
			   | sv as (Var{1=lvc,...} | Select{1=lvc,...} | Decon{1=lvc, ...}
			   | (* will probably never happen *) Record{1=lvc,...}) =>
			     fcsDefault(sv, lvc)
			   | sv as (Fun _ | TFun _) =>
			     bugval("unexpected switch arg", sval2val sv)
		      end (* fcSwitch *)

		fun fcCon (dc1,tycs1,v,lv,le) = let
		      val lvi = C.get lv
		      in if C.dead lvi then (click_deadval(); loop m le cont) else
			  let val ndc = cdcon dc1
			      fun ccon sv =
				  let val nm = addbind(m, lv, Con(lv, sv, ndc, tycs1))
				      val nle = loop nm le cont
				  in if C.dead lvi then nle
				     else F.CON(ndc, tycs1, sval2val sv, lv, nle)
				  end
			  in case val2sval m v
			      of sv as (Decon (lvd,sv',dc2,tycs2)) =>
				 if FU.dcon_eq(dc1, dc2) andalso tycs_eq(tycs1,tycs2) then
				     (click_con();
				      loop (substitute(m, lv, sv', F.VAR lvd)) le cont)
				 else ccon sv
			       | sv => ccon sv
			  end
		      end (* fcCon *)

		fun fcRecord (rk,vs,lv,le) = let
		    (* g: check whether the record already exists *)
		      val lvi = C.get lv
		      in if C.dead lvi then (click_deadval(); loop m le cont) else
			  let fun g (Select(_,sv,0)::ss) =
				  let fun g' (n,Select(_,sv',i)::ss) =
					  if n = i
					  andalso FU.sameValue(sval2val sv, sval2val sv')
					    then g'(n+1,ss)
					    else NONE
					| g' (n,[]) =
					  (case sval2lty sv
					    of SOME lty =>
					       let val ltd =
						       case (rk, LT.ltp_tyc lty)
							of (F.RK_STRUCT,false) => LT.ltd_str
							 | (F.RK_TUPLE _,true) => LT.ltd_tuple
							 (* we might select out of a struct
							  * into a tuple or vice-versa *)
							 | _ => (fn _ => [])
					       in if length(ltd lty) = n
						  then SOME sv else NONE
					       end
					     | _ => (click_lacktype(); NONE)) (* sad *)
					| g' _ = NONE
				  in g'(1,ss)
				  end
				| g _ = NONE
			      val svs = map (val2sval m) vs
			  in case g svs
			      of SOME sv => (click_record();
					     loop (substitute(m, lv, sv, tagInt 0)) le cont
						  before app (unuseval m) vs)
			       | _ =>
				 let val nm = addbind(m, lv, Record(lv, svs))
				     val nle = loop nm le cont
				 in if C.dead lvi then nle
				    else F.RECORD(rk, map sval2val svs, lv, nle)
				 end
			  end
		      end (* fcRecord *)

		fun fcSelect (v,i,lv,le) = let
		      val lvi = C.get lv
		      in if C.dead lvi then (click_deadval(); loop m le cont) else
			  (case val2sval m v
			    of Record (lvr,svs) =>
			       let val sv = List.nth(svs, i)
			       in click_select();
				   loop (substitute(m, lv, sv, F.VAR lvr)) le cont
			       end
			     | sv =>
			       let val nm = addbind (m, lv, Select(lv, sv, i))
				   val nle = loop nm le cont
			       in if C.dead lvi then nle
				  else F.SELECT(sval2val sv, i, lv, nle)
			       end)
		      end (* fcSelect *)

		fun fcBranch (po,vs,le1,le2) = let
		      val nvs = map substval vs
		      val npo = cpo po
		      val nle1 = loop m le1 #2
		      val nle2 = loop m le2 #2
		      in cont(m, F.BRANCH(npo, nvs, nle1, nle2))
		      end (* fcBranch *)

		fun fcPrimop (po,vs,lv,le) = let
		      val lvi = C.get lv
		      val pure = not(PrimopUtil.effect(#2 po))
		      in if pure andalso C.dead lvi then (click_deadval();loop m le cont) else
			  let val nvs = map substval vs
			      val npo = cpo po
			      val nm = addbind(m, lv, Var(lv,NONE))
			      val nle = loop nm le cont
			  in
			      if pure andalso C.dead lvi then nle
			      else F.PRIMOP(npo, nvs, lv, nle)
			  end
		      end (* fcPrimop *)

		in case le
		    of F.RET vs => cont(m, F.RET(map substval vs))
		     | F.LET x => fcLet x
		     | F.FIX x => fcFix x
		     | F.APP x => fcApp x
		     | F.TFN x => fcTfn x
		     (* | F.TAPP (f,tycs) => cont(m, F.TAPP(substval f, tycs)) *)
		     | F.TAPP x => fcTapp x
		     | F.SWITCH x => fcSwitch x
		     | F.CON x => fcCon x
		     | F.RECORD x => fcRecord x
		     | F.SELECT x => fcSelect x
		     | F.RAISE (v,ltys) => cont(m, F.RAISE(substval v, ltys))
		     | F.HANDLE (le,v) => cont(m, F.HANDLE(loop m le #2, substval v))
		     | F.BRANCH x => fcBranch x
		     | F.PRIMOP x => fcPrimop x
		end (* fcexp *)

	  in
	  (*  C.collect fdec; *)
	    case fcexp S.empty M.empty (F.FIX([fdec], F.RET[F.VAR f])) #2
	     of F.FIX([fdec], F.RET[F.VAR f]) => fdec
	      | fdec => bug "invalid return fundec"
	  end (* contract *)

  end (* FContract *)
