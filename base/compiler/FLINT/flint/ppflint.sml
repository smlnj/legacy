(* ppflint.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Pretty printer for Flint IL.
 *)

structure PPFlint :> PPFLINT =
struct
    (** frequently used structures *)
    structure F = FLINT
    structure FU = FlintUtil
    structure S = Symbol
    structure LV = LambdaVar
    structure LT = LtyExtern
    structure PO = Primop
    structure PU = PrintUtil
    structure CTRL = Control.FLINT

    (** some print utilities **)
    val say = Control_Print.say
    val margin = ref 0
    exception Undent
    fun indent n = margin := !margin + n
    fun undent n = (margin := !margin - n;
		    if !margin < 0 then raise Undent
		    else ())
    fun dent () = PU.tab(!margin)
    val newline = PU.newline

    infix &
    fun op& (f1,f2) () = (f1(); f2())

    fun toStringFFlag ff =
      let fun h b = if b then "r" else "c"
       in LT.ffw_var (ff, fn (b1,b2) => (h b1)^(h b2), fn _ => "f")
      end

    fun toStringFKind ({isrec,cconv,inline,...}:F.fkind) =
	(case inline of F.IH_ALWAYS => "(i)"
		      | F.IH_UNROLL => "(u)"
		      | F.IH_MAYBE(s,ws) => "(i:"^(Int.toString s)^")"
		      | F.IH_SAFE => "")^
	     (case isrec
	       of SOME(_,F.LK_UNKNOWN) => "R"
		| SOME(_,F.LK_LOOP) => "LR"
		| SOME(_,F.LK_TAIL) => "TR"
		| NONE => "")^
		  (case cconv
		    of F.CC_FCT => "FCT"
		     | F.CC_FUN fixed => ("FUN "^(toStringFFlag fixed)))

(*
    fun toStringFKind F.FK_ESCAPE  = "FK_ESCAPE"
      | toStringFKind F.FK_KNOWN   = "FK_KNOWN"
      | toStringFKind F.FK_KREC    = "FK_KREC"
      | toStringFKind F.FK_KTAIL   = "FK_KTAIL"
      | toStringFKind F.FK_NOINL   = "FK_NOINL"
      | toStringFKind F.FK_HANDLER = "FK_HANDLER"
*)

    val printFKind = say o toStringFKind

    (** classifications of various kinds of records *)
    fun toStringRKind (F.RK_VECTOR tyc) = "VECTOR[" ^ LT.tc_print tyc ^ "]"
      | toStringRKind F.RK_STRUCT = "STRUCT"
      | toStringRKind (F.RK_TUPLE _) = "RECORD"

    val printRKind = say o toStringRKind

    (** con: used to specify all possible switching statements. *)
    fun toStringCon (F.DATAcon((symbol,_,_),_,_))   = S.name symbol
      | toStringCon (F.INTcon{ival, ty}) =
	  concat["(I", Int.toString ty, ")", IntInf.toString ival]
      | toStringCon (F.WORDcon{ival, ty}) =
	  concat["(W", Int.toString ty, ")", IntInf.toString ival]
      | toStringCon (F.STRINGcon s) = PrintUtil.formatString s
      | toStringCon (F.VLENcon n)   = Int.toString n

    val printCon = say o toStringCon

    (** simple values, including variables and static constants. *)
    fun toStringValue (F.VAR v) = LV.lvarName v
      | toStringValue (F.INT{ival, ty}) =
	  concat["(I", Int.toString ty, ")", IntInf.toString ival]
      | toStringValue (F.WORD{ival, ty}) =
	  concat["(W", Int.toString ty, ")", IntInf.toString ival]
      | toStringValue (F.REAL{rval, ty}) =
	  concat["(R", Int.toString ty, ")", RealLit.toString rval]
      | toStringValue (F.STRING s) = PrintUtil.formatString s

    val printSval = say o toStringValue
    val LVarString = ref LV.lvarName

    fun printVar v = say (!LVarString v)
    val printTyc = say o LT.tc_print
    val printLty = say o LT.lt_print
    fun printTvTk (tv:LT.tvar,tk) =
	say ((LV.lvarName tv)^":"^(LT.tk_print tk))

    val parenCommaSep = ("(", ",", ")")
    val printValList = PU.printClosedSequence ("[",",","]") printSval
    val printVarList = PU.printClosedSequence ("[",",","]") printVar
    val printTycList = PU.printClosedSequence ("[",",","]") printTyc
    val printLtyList = PU.printClosedSequence ("[",",","]") printLty
    val printTvTkList = PU.printClosedSequence ("[",",","]") printTvTk

    fun printDecon (F.DATAcon((_,Access.CONSTANT _,_),_,_)) = ()
        (* WARNING: a hack, but then what about constant exceptions ? *)
      | printDecon (F.DATAcon((symbol,conrep,lty),tycs,lvar)) =
	(* <lvar> = DECON(<symbol>,<conrep>,<lty>,[<tycs>]) *)
	(printVar lvar;
	 say " = DECON(";
	 say (S.name symbol); say ",";
	 say (Access.prRep conrep); say ",";
	 printLty lty; say ",";
	 printTycList tycs; say ")";
	 newline(); dent())
      | printDecon _ = ()

    fun appPrint prfun sepfun [] = ()
      | appPrint prfun sepfun (x::xs) =
	(prfun x;  app (fn y => (sepfun(); prfun y)) xs)

    (** the definitions of the lambda expressions *)

    fun complex (F.LET _) = true
      | complex (F.FIX _) = true
      | complex (F.TFN _) = true
      | complex (F.SWITCH _) = true
      | complex (F.CON _) = true
      | complex (F.HANDLE _) = true
      | complex _ = false

    fun pLexp (F.RET values) =
	(* RETURN [values] *)
	(say "RETURN "; printValList values)

      | pLexp (F.APP (f, args)) =
	(* APP(f, [args]) *)
	(say "APP(";
	 printSval f;
	 say ",";
	 printValList args;
	 say ")")

      | pLexp (F.TAPP (tf, tycs)) =
	(* TAPP(tf, [tycs]) *)
	(say "TAPP(";
	 printSval tf;
	 say ",";
	 printTycList tycs;
	 say ")")

      | pLexp (F.LET (vars, lexp, body)) =
	(* [vars] = lexp   OR   [vars] =
	 *   body                 lexp
	 *                      body
	 *)
	(printVarList vars; say " = ";
	 if complex lexp then
	     (indent 2; newline(); dent(); pLexp lexp; undent 2)
	 else
	     let val len = (3		(* for the " = " *)
			    + 2		(* for the "[]" *)
			    + (length vars) (* for each comma *)
			    + (foldl	(* sum of varname lengths *)
			       (fn (v,n) => n + (size (!LVarString v)))
			       0 vars))
	     in
		 indent len;  pLexp lexp;  undent len
	     end;
	 newline();  dent();  pLexp body)

      | pLexp (F.FIX (fundecs, body)) =
      (* FIX(<fundec1>,
       *     <fundec2>,
       *     <fundec3>)
       * <body>
       *)
	(say "FIX(";
	 indent 4;
	 appPrint printFundec (newline & dent) fundecs;
	 undent 4;  say ")";  newline();
	 dent();
	 pLexp body)

      | pLexp (F.TFN ((tfk as {inline,...}, lvar, tv_tk_list, tfnbody), body)) =
	(* v =
	 *   TFN([tk],lty,
	 *     <tfnbody>)
	 * <body>
	 *)
	(printVar lvar; say " = "; newline();
	 indent 2; dent();
	 if inline = F.IH_SAFE then () else say "i"; say "TFN(";
	 printTvTkList tv_tk_list; say ",";
	 (*** printLty lty; say ","; *** lty no longer available ***)
         newline();
	 indent 2;
	 dent();
	 pLexp tfnbody;
	 undent 4; say ")"; newline();
	 dent();
	 pLexp body)

      (** NOTE: I'm ignoring the consig here **)
      | pLexp (F.SWITCH (value, consig, con_lexp_list, lexpOption)) =
	(* SWITCH <value>
	 *   <con> =>
	 *       <lexp>
	 *   <con> =>
	 *       <lexp>
	 *)
	 (say "SWITCH "; printSval value; newline();
	  indent 2;  dent();
	  appPrint printCase (newline & dent) con_lexp_list;
	  case  lexpOption of
	      NONE => ()
	    | SOME lexp =>		(* default case *)
		  (newline(); dent(); say "_ => ";
		      indent 4;  newline();  dent();
		      pLexp lexp;  undent 4);
		      undent 2)

      | pLexp (F.CON ((symbol,_,_), tycs, value, lvar, body)) =
	 (* <lvar> = CON(<symbol>, <tycs>, <value>)
	  * <body>
	  *)
	 (printVar lvar; say " = CON(";
	  say (S.name symbol); say ", ";
	  printTycList tycs;  say ", ";
	  printSval value;  say ")";
	  newline();  dent();  pLexp body)

      | pLexp (F.RECORD (rkind, values, lvar, body)) =
	 (* <lvar> = RECORD(<rkind>, <values>)
	  * <body>
	  *)
	 (printVar lvar;  say " = ";
	  printRKind rkind; say " ";
	  printValList values;
	  newline();  dent();  pLexp body)

      | pLexp (F.SELECT (value, int, lvar, body)) =
	 (* <lvar> = SELECT(<value>, <int>)
	  * <body>
	  *)
	 (printVar lvar;  say " = SELECT(";
	  printSval value;  say ", ";
	  say (Int.toString int);  say ")";
	  newline();  dent();  pLexp body)

      | pLexp (F.RAISE (value, ltys)) =
	 (* NOTE: I'm ignoring the lty list here. It is the return type
	  * of the raise expression. (ltys temporarily being printed --v)
	  *)
	 (* RAISE(<value>) *)
	 (say "RAISE(";
	  printSval value; say ") : "; printLtyList ltys)

      | pLexp (F.HANDLE (body, value)) =
	 (* <body>
	  * HANDLE(<value>)
	  *)
	 (pLexp body;
	  newline();  dent();
	  say "HANDLE(";  printSval value;  say ")")

      | pLexp (F.BRANCH ((d, primop, lty, tycs), values, body1, body2)) =
	 (* IF PRIM(<primop>, <lty>, [<tycs>]) [<values>]
          * THEN
	  *   <body1>
          * ELSE
	  *   <body2>
	  *)
	 ((case d of NONE => say "IF PRIMOP("
                   | _ => say "IF GENOP(");
	  say (PrimopUtil.toString primop);  say ", ";
	  printLty lty;  say ", ";
	  printTycList tycs;  say ") ";
	  printValList values; newline();
          dent();
          appPrint printBranch (newline & dent)
              [("THEN", body1), ("ELSE", body2)])

      | pLexp (F.PRIMOP (p as (_, PO.MKETAG, _, _), [value], lvar, body)) =
	 (* <lvar> = ETAG(<value>[<tyc>])
	  * <body>
	  *)
	 (printVar lvar;  say " = ETAG(";
	  printSval value;  say "[";
	  printTyc (FU.getEtagTyc p);  say "])";
	  newline();  dent();  pLexp body)

      | pLexp (F.PRIMOP (p as (_, PO.WRAP, _, _), [value], lvar, body)) =
	 (* <lvar> = WRAP(<tyc>, <value>)
	  * <body>
	  *)
	 (printVar lvar;  say " = WRAP(";
	  printTyc (FU.getWrapTyc p);  say ", ";
	  printSval value;  say ")";
	  newline();  dent();  pLexp body)

      | pLexp (F.PRIMOP (p as (_, PO.UNWRAP, _, []), [value], lvar, body)) =
	 (* <lvar> = UNWRAP(<tyc>, <value>)
	  * <body>
	  *)
	 (printVar lvar;  say " = UNWRAP(";
	  printTyc (FU.getUnWrapTyc p);  say ", ";
	  printSval value;  say ")";
	  newline();  dent();  pLexp body)

      | pLexp (F.PRIMOP ((d, primop, lty, tycs), values, lvar, body)) =
	 (* <lvar> = PRIM(<primop>, <lty>, [<tycs>]) [<values>]
	  * <body>
	  *)
	 (printVar lvar;
          (case d of NONE => say " = PRIMOP("
                   | _ => say " = GENOP(");
	  say (PrimopUtil.toString primop);  say ", ";
	  printLty lty;  say ", ";
	  printTycList tycs;  say ") ";
	  printValList values;
	  newline();  dent();  pLexp body)

    and printFundec (fkind as {cconv,...}, lvar, lvar_lty_list, body) =
	(*  <lvar> : (<fkind>) <lty> =
	 *    FN([v1 : lty1,
	 *        v2 : lty2],
	 *      <body>)
	 *)
	(printVar lvar; say " : ";
	 say "("; printFKind fkind; say ") ";
	 (*** the return-result lty no longer available ---- printLty lty; **)
         say " = "; newline();
	 indent 2;
	 dent();
	 say "FN([";
	 indent 4;
	 (case lvar_lty_list of
	      [] => ()
	    | ((lvar,lty)::L) =>
		  (printVar lvar; say " : ";
		   if !CTRL.printFctTypes orelse cconv <> F.CC_FCT
		   then printLty lty else say "???";
		   app (fn (lvar,lty) =>
			(say ","; newline(); dent();
			 printVar lvar; say " : "; printLty lty)) L));
	      say "],"; newline();
	      undent 2;  dent();
	      pLexp body; say ")";
	      undent 4)

    and printCase (con, lexp) =
	(printCon con;
	 say " => ";
         indent 4; newline(); dent();
	 printDecon con;
	 pLexp lexp; undent 4)

    and printBranch (s, lexp) =
	(say s;
         indent 4; newline(); dent();
	 pLexp lexp; undent 4)

    fun printLexp lexp = pLexp lexp before (newline(); newline())

    fun printProg prog = (printFundec prog; newline())


end (* structure PPFlint *)
