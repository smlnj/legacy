(* cpsBranchProb.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implements the following Ball-Larus heuristic estimates
 * for branch prediction.
 *
 * PH (pointer heuristic)
 *    boxed and unboxed tests
 *
 * OH (op-code heuristic)
 *    comparisons of <=0, =0, =constant will fail.
 *
 * RH (return heuristic)
 *    block containing a return is unlikely
 *    block with a goto is likely.
 *
 * Unlikely:
 *    bounds check, raising an exception, <any others>
 *)

signature CPS_BRANCH_PROB =
  sig
    exception CpsProbTbl

    val branchProb : CPS.function list -> (CPS.lvar -> Probability.prob option)

  end

structure CpsBranchProb : CPS_BRANCH_PROB = struct

  structure Prob = Probability
  structure P = CPS.P

  val disableF =
      MLRiscControl.mkFlag
        ("disable-cps-branch-prob",
	 "Turn CPS branch probability computation off")

 (* keep track of variables that hold a:
  *	object length,
  *	continuation, or
  *     handler/handler-code-pointer
  *)
  datatype info
     = OBJLEN 				(* object length *)
     | CONT 				(* continuation *)
     | HANDLER				(* exception handler *)
     | HDLR_CODEPTR			(* exception handler code pointer *)


 (* condensed CPS flow graph *)
  datatype condensed
    = BLOCK				(* ordinary code block *)
    | RETURN				(* calls a continuation *)
    | ESCAPE				(* makes a function call  *)
    | GOTO				(* call to known function *)
    | RAISE				(* raises an exception *)
    | BRANCH of CPS.P.branch * CPS.value list * CPS.lvar * condensed * condensed
    | SWITCH of condensed list

  exception InfoTbl
  exception CpsProbTbl

  fun error msg = MLRiscErrorMsg.error ("CpsBranchProb", msg)

  fun branchProb fs = let
    val infoTbl : info LambdaVar.Tbl.hash_table = LambdaVar.Tbl.mkTable(32, InfoTbl)
    val insertInfo = LambdaVar.Tbl.insert infoTbl
    val findInfo = LambdaVar.Tbl.find infoTbl

    val brProbTbl : Prob.prob LambdaVar.Tbl.hash_table = LambdaVar.Tbl.mkTable(32, CpsProbTbl)


    fun buildInfo(fk, f, args, tys, e) = let
      (* record how the function returns *)
      fun return () =
	case fk
	 of CPS.CONT =>
	     (case args
	      of _::stdcont::_ => insertInfo (stdcont, CONT)
	       | _ => error "return"
	     (*esac*))
	  | CPS.ESCAPE =>
	     (case args
	      of _::_::stdcont::_ => insertInfo(stdcont, CONT)
	       | _ => error "escape"
	     (*esac*))
	  | _  =>
	     (* check if any of the arguments has a CPS.CNTt -- continuation *)
	     ListPair.app
		 (fn (x, CPS.CNTt) => insertInfo(x, CONT) | _ => ())
		 (args, tys)
	(*esac*)
      fun cexp(CPS.RECORD(_, _, _, e)) = cexp e
	| cexp(CPS.SELECT(0, CPS.VAR v, x, _, e)) =
	    (case findInfo v
	      of SOME HANDLER => (insertInfo(x, HDLR_CODEPTR); cexp e)
	       | _ => cexp e
	    (*esac*))
	| cexp(CPS.SELECT(_, _, _, _, e)) = cexp e
	| cexp(CPS.OFFSET(_, _, _, e)) = cexp e
	| cexp(CPS.APP(v, _)) =
	   (case v
	    of CPS.VAR v =>
		(case findInfo v
		  of SOME CONT => RETURN
		   | SOME HDLR_CODEPTR => RAISE
		   | _ => ESCAPE
		(*esac*))
	     | CPS.LABEL _ => GOTO
	     | _ => BLOCK
	   (*esac*))

	| cexp(CPS.SWITCH(_, _, cexps)) = SWITCH(List.map cexp cexps)
	| cexp(CPS.BRANCH(cc, args, x, t, f)) =
	    BRANCH(cc, args, x, cexp t, cexp f)
	| cexp(CPS.SETTER(_, _, e)) = cexp e
	| cexp(CPS.LOOKER(P.GETHDLR, [], x, _, e)) = (insertInfo (x, HANDLER); cexp e)
	| cexp(CPS.LOOKER(_, _, _, _, e)) = cexp e
	| cexp(CPS.ARITH(_, _, _, _, e)) = cexp e
	| cexp(CPS.PURE(pure, _, x, _, e)) =
	   (case pure
	    of P.OBJLENGTH => insertInfo(x, OBJLEN)
	     | P.LENGTH => insertInfo(x, OBJLEN)
	     | _ => ()
	    (*esac*);
	    cexp e)
	| cexp(CPS.RCC(_, _, _, _, _, e)) = cexp e
	| cexp(FIX_) = error "cexp:FIX"

    in	 return ();  cexp e
    end

    (* PH = 80 means that 80% of the time the prediction was a hit.
     *  ... and similarly for the others.
     *)
    val PH = Prob.percent 80    val notPH = Prob.not(PH)
    val OH = Prob.percent 84    val notOH = Prob.not(OH)
    val RH = Prob.percent 72	val notRH = Prob.not(RH)
    val unlikely = Prob.prob(1,100)
    val likely = Prob.not(Prob.likely)

    fun assign(SWITCH cs) = List.app assign cs
      | assign(BRANCH(test, args, x, c1, c2)) = let
	    (* pointer heuristic *)
	    fun ph() =
	      (case test
		of P.BOXED => SOME PH
		 | P.UNBOXED => SOME notPH
		 | P.PEQL => SOME notPH
		 | P.PNEQ => SOME PH
		 | _ => NONE
	      (*esac*))

	    (* opcode heuristic *)
	    fun oh () = let
	      datatype num = Zero | Num | Other
	      fun number(CPS.NUM{ival=0, ...}) = Zero
		| number(CPS.NUM _) = Num
		| number(CPS.REAL{rval, ...}) = if RealLit.isZero rval then Zero else Num
		| number _ = Other

	    in
	      case (test, args)
		of (P.CMP{oper, kind}, [v1, v2]) =>
		   (case (oper, number v1, number v2)
		     of (P.LT, _, Zero) => SOME notOH
		      | (P.LTE, _, Zero) => SOME notOH
		      | (P.EQL, _, Num) => SOME notOH

		      | (P.LT, Zero, _)  => SOME OH
		      | (P.LTE, Zero, _)  => SOME OH
		      | (P.EQL, Num, _)  => SOME notOH

		      | (P.GT, _, Zero)  => SOME OH
		      | (P.GTE, _, Zero) => SOME OH
		      | (P.NEQ, _, Num) => SOME OH

		      | (P.GT, Zero, _)  =>  SOME notOH
		      | (P.GTE, Zero, _) => SOME notOH
		      | (P.NEQ, Num, _) => SOME OH
		      | _ => NONE
		   (*esac*))

		 | (P.FCMP{oper, size}, [v1, v2]) =>
		    (* The wu-larus paper does not menetion floating point,
		     * but what the hey ...
		     * Note that the negation of LT is UGL, so we wont
		     * bother with all those.
		     *)

		    (case (oper, number v1, number v2)
		     of (P.F_LT, _, Zero) => SOME notOH
		      | (P.F_LE, _, Zero) => SOME notOH
		      | (P.F_EQ, _, Num) => SOME notOH

		      | (P.F_LT, Zero, _)  => SOME OH
		      | (P.F_LE, Zero, _)  => SOME OH
		      | (P.F_EQ, Num, _)  => SOME notOH

		      | _ => NONE
		   (*esac*))

		 | _ => NONE
	      (*esac*)
	    end

	    (* return heuristic *)
	    fun rh() =
	      (case (c1, c2)
	       of (RETURN, RETURN) => NONE
		| (RETURN, _) => SOME notRH
		| (_, RETURN) => SOME RH
		| _ => NONE
	      (*esac*))

	    fun raiseExn() =
	      (case (c1, c2)
	       of (RAISE, _) => SOME unlikely
		| (_, RAISE) => SOME likely
		| _ => NONE
	      (*esac*))

	    fun boundsCheck() = (case (test, args)
	           of (P.CMP{oper= P.LT, kind=P.UINT _}, [v1,CPS.VAR v2]) => (
		  	case findInfo v2
			 of SOME OBJLEN => SOME likely
			  | _ => NONE)
		    | _ => NONE
	          (*esac*))

	    fun combine(f, trueProb) =
	      (case (f(), trueProb)
		of (NONE, NONE) => NONE
		 | (NONE, p as SOME _) => p
		 | (p as SOME _, NONE) => p
		 | (SOME takenP, SOME trueP) =>
		      (SOME(#t(Probability.combineProb2{trueProb=trueP, takenProb=takenP})))
		        handle e =>
			       (print (Format.format "true=%s, taken=%s\n"
				       [Format.STR(Probability.toString trueP),
					Format.STR(Probability.toString takenP)]);
				raise e)

	      (*esac*))

	in
	   case List.foldl combine NONE [ph, oh, rh, raiseExn, boundsCheck]
	    of NONE => ()
	     | SOME prob => LambdaVar.Tbl.insert brProbTbl (x, prob)
	   (*esac*);
	   assign(c1);
	   assign(c2)
	end
      | assign _ = ()


  in
      if !disableF then (fn _ => NONE)
      else let val condensed = List.map buildInfo fs
        in
	  List.app assign condensed;
	  LambdaVar.Tbl.find brProbTbl
        end
  end
end
