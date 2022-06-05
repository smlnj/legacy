(* ppabsyn.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PPABSYN =
sig
  val ppPat  : StaticEnv.staticEnv -> PrettyPrint.stream
               -> Absyn.pat * int -> unit
  val ppExp  : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrint.stream -> Absyn.exp * int -> unit
  val ppRule : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrint.stream -> Absyn.rule * int -> unit
  val ppVB   : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrint.stream -> Absyn.vb * int -> unit
  val ppRVB  : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrint.stream -> Absyn.rvb * int -> unit
  val ppDec  : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrint.stream -> Absyn.dec * int -> unit

  val ppStrexp : StaticEnv.staticEnv * Source.inputSource option
                 -> PrettyPrint.stream -> Absyn.strexp * int -> unit

  val lineprint : bool ref

  val debugging : bool ref

end (* signature PPABSYN *)


structure PPAbsyn: PPABSYN =
struct

local structure EM = ErrorMsg
      structure M = Modules
      structure B = Bindings
      structure S = Symbol
      structure PP = PrettyPrint
      structure PU = PPUtil

      open Absyn Tuples Fixity VarCon Types PPType PPVal
in

(* debugging *)
val say = Control_Print.say
val debugging = ref false
fun debugmsg (msg: string) =
      if !debugging then (say msg; say "\n") else ()
fun bug msg = ErrorMsg.impossible("PPAbsyn: "^msg)

val internals = ElabDataControl.absynInternals

val lineprint = ref false

fun C f x y = f y x

val nullFix = INfix(0,0)
val infFix = INfix(1000000,100000)
fun strongerL(INfix(_,m),INfix(n,_)) = m >= n
  | strongerL _ = false			(* should not matter *)
fun strongerR(INfix(_,m),INfix(n,_)) = n > m
  | strongerR _ = true			(* should not matter *)

fun prpos(ppstrm: PP.stream,
          source: Source.inputSource, charpos: int) =
    if (!lineprint) then
      let val {line,column,...} = Source.filepos source charpos
       in PU.ppi ppstrm line;
	  PU.pps ppstrm ".";
	  PU.ppi ppstrm column
      end
    else PU.ppi ppstrm charpos


fun checkpat (n,nil) = true
  | checkpat (n, (sym,_)::fields) =
    S.eq(sym, numlabel n) andalso checkpat(n+1,fields)

fun checkexp (n,nil) = true
  | checkexp (n, (LABEL{name=sym,...},_)::fields) =
	S.eq(sym, numlabel n) andalso checkexp(n+1,fields)

fun isTUPLEpat (RECORDpat{fields=[_],...}) = false
  | isTUPLEpat (RECORDpat{flex=false,fields,...}) = checkpat(1,fields)
  | isTUPLEpat _ = false

fun isTUPLEexp (RECORDexp [_]) = false
  | isTUPLEexp (RECORDexp fields) = checkexp(1,fields)
  | isTUPLEexp (MARKexp(a,_)) = isTUPLEexp a
  | isTUPLEexp _ = false

fun lookFIX (env,sym) =
    Lookup.lookFix (env,S.fixSymbol(S.name sym))

fun stripMark (MARKexp(a,_)) = stripMark a
  | stripMark x = x

fun ppRpath ppstrm rpath = PP.string ppstrm (InvPath.toString rpath)

fun ppStr ppstrm str =
    (case str
      of M.STR{access,rlzn={rpath,...},...} =>
	 (ppRpath ppstrm rpath;
	  PP.string ppstrm "[";
	  ppAccess ppstrm access;
	  PP.string ppstrm "]")
       | M.STRSIG _ => PP.string ppstrm "SIGSTR"
       | M.ERRORstr => PP.string ppstrm "ERRORstr")

fun ppFct ppstrm fct =
    (case fct
      of M.FCT{access,rlzn={rpath,...},...} =>
	 (ppRpath ppstrm rpath;
	  PP.string ppstrm "[";
	  ppAccess ppstrm access;
	  PP.string ppstrm "]")
       | M.ERRORfct => PP.string ppstrm "ERRORfct")

fun ppPat env ppstrm =
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = PU.en_pp ppstrm
	fun ppPat' (_,0) = pps "<pat>"
	  | ppPat' (VARpat v,_) = ppVar ppstrm v
	  | ppPat' (WILDpat,_) = pps "_"
          | ppPat' (NUMpat(src, _), _) = pps src
	  | ppPat' (STRINGpat s,_) = PU.ppString ppstrm s
	  | ppPat' (CHARpat s,_) = (pps "#"; PU.ppString ppstrm s)
	  | ppPat' (LAYEREDpat (v,p),d) =
	      (openHVBox 0;
	       ppPat'(v,d); pps " as "; ppPat'(p,d-1);
	       closeBox ())
		    (* Handle 0 length case specially to avoid {,...}: *)
	  | ppPat' (RECORDpat{fields=[],flex,...},_) =
	      if flex then pps "{...}"
	      else pps "()"
	  | ppPat' (r as RECORDpat{fields,flex,...},d) =
	      if isTUPLEpat r
	      then PU.ppClosedSequence ppstrm
		     {front=(C PP.string "("),
		      sep = PU.sepWithCut ",",
		      back=(C PP.string ")"),
		      pr=(fn _ => fn (sym,pat) => ppPat'(pat,d-1)),
		      style=PU.INCONSISTENT}
		     fields
	      else PU.ppClosedSequence ppstrm
		     {front=(C PP.string "{"),
		      sep = PU.sepWithCut ",",
		      back=(fn ppstrm => if flex then PP.string ppstrm ",...}"
				         else PP.string ppstrm "}"),
		      pr=(fn ppstrm => fn (sym,pat) =>
			  (PU.ppSym ppstrm sym; PP.string ppstrm "=";
			   ppPat'(pat,d-1))),
		      style=PU.INCONSISTENT}
		     fields
	  | ppPat' (VECTORpat(nil,_), d) = pps "#[]"
	  | ppPat' (VECTORpat(pats,_), d) =
	      let fun pr _ pat = ppPat'(pat, d-1)
	       in PU.ppClosedSequence ppstrm
		    {front=(C PP.string "#["),
		     sep= PU.sepWithCut ",",
		     back=(C PP.string "]"),
		     pr=pr,
		     style=PU.INCONSISTENT}
		    pats
	      end
	  | ppPat' (pat as (ORpat _), d) = let
	      fun mkList (ORpat(hd, tl)) = hd :: mkList tl
		| mkList p = [p]
	      fun pr _ pat = ppPat'(pat, d-1)
	      in
		PU.ppClosedSequence ppstrm {
		    front = (C PP.string "("),
		    sep = fn ppstrm => (PP.space ppstrm 1;
                                        PP.string ppstrm "|";
					PP.space ppstrm 1),
		    back = (C PP.string ")"),
		    pr = pr,
		    style = PU.INCONSISTENT
		  } (mkList pat)
	      end
	  | ppPat' (CONpat(e,_),_) = ppDcon ppstrm e
	  | ppPat' (p as APPpat _, d) =
	      ppDconPat (env,ppstrm) (p,nullFix,nullFix,d)
	  | ppPat' (CONSTRAINTpat (p,t),d) =
	     (openHOVBox 0;
	      ppPat'(p,d-1); pps " :";
	      PP.break ppstrm {nsp=1,offset=2};
	      ppType env ppstrm t;
	      closeBox ())
          | ppPat' (MARKpat(p,region), d) = ppPat' (p,d)
	  | ppPat' _ = bug "ppPat'"
     in ppPat'
    end

and ppDconPat(env,ppstrm) =
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = PU.en_pp ppstrm
	fun lpcond(atom) = if atom then pps "(" else ()
	fun rpcond(atom) = if atom then pps ")" else ()
	fun ppDconPat'(_,_,_,0) = pps "<pat>"
	  | ppDconPat'(MARKpat(p,_),l:fixity,r:fixity,d:int) =
              ppDconPat'(p,l,r,d)
	  | ppDconPat'(CONpat(DATACON{name,...},_),l,r,_) =
	      PU.ppSym ppstrm name
	  | ppDconPat'(CONSTRAINTpat(p,t),l,r,d) =
	     (openHOVBox 0;
	      pps "("; ppPat env ppstrm (p,d-1); pps " :";
	      PP.break ppstrm {nsp=1,offset=2};
	      ppType env ppstrm t; pps ")";
	      closeBox ())
	  | ppDconPat'(LAYEREDpat(v,p),l,r,d) =
	     (openHOVBox 0;
	      pps "("; ppPat env ppstrm (v,d); PP.break ppstrm {nsp=1,offset=2};
	      pps " as "; ppPat env ppstrm (p,d-1); pps ")";
	      closeBox ())
	  | ppDconPat'(APPpat(DATACON{name,...},_,p),l,r,d) =
	      let val dname = S.name name
		      (* should really have original path, like for VARexp *)
		  val thisFix = lookFIX(env,name)
		  val effFix = case thisFix of NONfix => infFix | x => x
		  val atom = strongerR(effFix,r) orelse strongerL(l,effFix)
	       in openHOVBox 2;
		  lpcond(atom);
		  case (thisFix,p)
		    of (INfix _, RECORDpat{fields=[(_,pl),(_,pr)],...}) =>
			 let val (left,right) =
				 if atom then (nullFix,nullFix)
				 else (l,r)
			  in ppDconPat' (pl,left,thisFix,d-1);
			     PP.break ppstrm {nsp=1,offset=0};
			     pps dname;
			     PP.break ppstrm {nsp=1,offset=0};
			     ppDconPat' (pr,thisFix,right,d-1)
			 end
		     | _ =>
		        (pps dname; PP.break ppstrm {nsp=1,offset=0};
			 ppDconPat'(p,infFix,infFix,d-1));
		  rpcond(atom);
		  closeBox ()
	      end
	  | ppDconPat' (p,_,_,d) = ppPat env ppstrm (p,d)
     in ppDconPat'
    end

fun trim [x] = []
  | trim (a::b) = a::trim b
  | trim [] = []

fun ppExp (context as (env,source_opt)) ppstrm =
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = PU.en_pp ppstrm
	fun lparen () = pps "("
	fun rparen () = pps ")"
	fun lpcond atom = if atom then pps "(" else ()
	fun rpcond atom = if atom then pps ")" else ()
	fun ppExp' (_,_,0) = pps "<exp>"
	  | ppExp' (VARexp(ref var,_),_,_) = ppVar ppstrm var
	  | ppExp' (CONexp(con,_),_,_) = ppDcon ppstrm con
          | ppExp' (NUMexp(src, _), _, _) = pps src
	  | ppExp' (REALexp(src, _),_,_) = pps src
	  | ppExp' (STRINGexp s,_,_) = PU.ppString ppstrm s
	  | ppExp' (CHARexp s,_,_) = (pps "#"; PU.ppString ppstrm s)
	  | ppExp' (r as RECORDexp fields,_,d) =
	      if isTUPLEexp r
	      then PU.ppClosedSequence ppstrm
		     {front=(C PP.string "("),
		      sep=PU.sepWithCut ",",
		      back=(C PP.string ")"),
		      pr=(fn _ => fn (_,exp) => ppExp'(exp,false,d-1)),
		      style=PU.INCONSISTENT}
		     fields
	      else PU.ppClosedSequence ppstrm
		     {front=(C PP.string "{"),
		      sep=PU.sepWithCut ",",
		      back=(C PP.string "}"),
		      pr=(fn ppstrm => fn (LABEL{name,...},exp) =>
			  (PU.ppSym ppstrm name; pps "=";
			   ppExp'(exp,false,d))),
		      style=PU.INCONSISTENT}
		     fields
	  | ppExp' (SELECTexp (LABEL{name,...},exp),atom,d) =
	      (openHVBox 0;
	        lpcond(atom);
	        pps "#"; PU.ppSym ppstrm name;
	        ppExp'(exp,true,d-1); pps ">";
		rpcond(atom);
	       closeBox ())
	  | ppExp' (VECTORexp(nil,_),_,d) = pps "#[]"
	  | ppExp' (VECTORexp(exps,_),_,d) =
	      let fun pr _ exp = ppExp'(exp,false,d-1)
	      in  PU.ppClosedSequence ppstrm
		    {front=(C PP.string "#["),
		     sep=PU.sepWithSpc ",",	(* should this be sepWithCut like tuples? *)
		     back=(C PP.string "]"),
		     pr=pr,
		     style=PU.INCONSISTENT}
		    exps
	      end
	  | ppExp' (SEQexp exps,_,d) =
	      PU.ppClosedSequence ppstrm
	        {front=(C PP.string "("),
		 sep=PU.sepWithSpc ",",
		 back=(C PP.string ")"),
		 pr=(fn _ => fn exp => ppExp'(exp,false,d-1)),
		 style=PU.INCONSISTENT}
		exps
	  | ppExp' (e as APPexp _,atom,d) =
	      let val infix0 = INfix(0,0)
	       in lpcond(atom);
		  ppAppExp(e,nullFix,nullFix,d);
		  rpcond(atom)
	      end
	  | ppExp' (CONSTRAINTexp(e, t),atom,d) =
	     (openHOVBox 0;
	       lpcond(atom);
	       ppExp'(e,false,d); pps ":";
	       PP.break ppstrm {nsp=1,offset=2};
	       ppType env ppstrm t;
	       rpcond(atom);
	      closeBox ())
	  | ppExp' (HANDLEexp(exp, (rules,_)),atom,d) =
	     (openHVBox 0;
	       lpcond(atom);
	       ppExp'(exp,atom,d-1); PP.newline ppstrm; pps "handle ";
	       PU.nl_indent ppstrm 2;
	       PU.ppvlist ppstrm ("  ","| ",
		  (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)), rules);
	       rpcond(atom);
	      closeBox ())
	  | ppExp' (RAISEexp(exp,_),atom,d) =
	      (openHVBox 0;
	       lpcond(atom);
	       pps "raise "; ppExp'(exp,true,d-1);
	       rpcond(atom);
	       closeBox ())
	  | ppExp' (LETexp(dec, exp),_,d) =
	      (openHVBox 0;
		pps "let ";
		openHVBox 0;
		 ppDec context ppstrm (dec,d-1);
		closeBox ();
		PP.break ppstrm {nsp=1,offset=0};
		pps "in ";
		openHVBox 0;
		 ppExp'(exp,false,d-1);
		closeBox ();
		PP.break ppstrm {nsp=1,offset=0};
		pps "end";
	       closeBox ())
	  | ppExp' (CASEexp(exp, rules, _),_,d) =
	      (openHVBox 0;
	       pps "(case "; ppExp'(exp,true,d-1); PU.nl_indent ppstrm 2;
	       PU.ppvlist ppstrm ("of ","   | ",
		 (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)),
                  trim rules);
	       rparen();
	       closeBox ())
	  | ppExp' (IFexp { test, thenCase, elseCase },atom,d) =
	      (openHVBox 0;
	       lpcond(atom);
	       pps "if ";
	       openHVBox 0;
	        ppExp' (test, false, d-1);
	       closeBox ();
	       PP.break ppstrm {nsp=1,offset= 0};
	       pps "then ";
	       openHVBox 0;
	        ppExp' (thenCase, false, d-1);
	       closeBox ();
	       PP.break ppstrm {nsp=1,offset= 0};
	       pps "else ";
	       openHVBox 0;
	        ppExp' (elseCase, false, d-1);
	       closeBox ();
	       rpcond(atom);
	       closeBox ())
	  | ppExp' (ANDALSOexp (e1, e2),atom,d) =
	      (openHVBox 0;
	       lpcond(atom);
	       openHVBox 0;
	       ppExp' (e1,true,d-1);
	       closeBox ();
	       PP.break ppstrm {nsp=1,offset= 0};
	       pps "andalso ";
	       openHVBox 0;
	       ppExp' (e2,true,d-1);
	       closeBox ();
	       rpcond(atom);
	       closeBox ())
	  | ppExp' (ORELSEexp (e1, e2),atom,d) =
	      (openHVBox 0;
	       lpcond(atom);
	       openHVBox 0;
	       ppExp' (e1,true,d-1);
	       closeBox ();
	       PP.break ppstrm {nsp=1,offset= 0};
	       pps "orelse ";
	       openHVBox 0;
	       ppExp' (e2,true,d-1);
	       closeBox ();
	       rpcond(atom);
	       closeBox ())
	  | ppExp' (WHILEexp { test, expr },atom,d) =
	      (openHVBox 0;
	       pps "while ";
	       openHVBox 0;
	        ppExp'(test,false,d-1);
	       closeBox ();
	       PP.break ppstrm {nsp=1,offset= 0};
	       pps "do ";
	       openHVBox 0;
	         ppExp'(expr,false,d-1);
	       closeBox ();
	       closeBox ())
	  | ppExp' (FNexp(rules,_),_,d) =
	      (openHVBox 0;
	       PU.ppvlist ppstrm ("(fn ","  | ",
			       (fn ppstrm => fn r =>
				  ppRule context ppstrm (r,d-1)),
			       trim rules);
	       rparen();
	       closeBox ())
	  | ppExp' (MARKexp (exp,(s,e)),atom,d) =
	      (case source_opt
		of SOME source =>
		     if !internals
		     then (pps "<MARK(";
			   prpos(ppstrm,source,s); pps ",";
			   prpos(ppstrm,source,e); pps "): ";
			   ppExp'(exp,false,d); pps ">")
		     else ppExp'(exp,atom,d)
	         | NONE => ppExp'(exp,atom,d))

	and ppAppExp (_,_,_,0) = PP.string ppstrm "<exp>"
	  | ppAppExp arg =
	    let val pps = PP.string ppstrm
		fun fixitypp(name,rand,leftFix,rightFix,d) =
		    let val dname = SymPath.toString(SymPath.SPATH name)
			val thisFix = case name
					of [id] => lookFIX(env,id)
					 | _ => NONfix
			fun prNon exp =
			    (openHOVBox 2;
			     pps dname; PP.break ppstrm {nsp=1,offset=0};
			     ppExp'(exp,true,d-1);
			     closeBox ())
		     in case thisFix
			  of INfix _ =>
			     (case stripMark rand
				of RECORDexp[(_,pl),(_,pr)] =>
				    let val atom = strongerL(leftFix,thisFix)
					     orelse strongerR(thisFix,rightFix)
					val (left,right) =
					    if atom then (nullFix,nullFix)
					    else (leftFix,rightFix)
				     in (openHOVBox 2;
					  lpcond(atom);
					  ppAppExp (pl,left,thisFix,d-1);
					  PP.break ppstrm {nsp=1,offset=0};
					  pps dname;
					  PP.break ppstrm {nsp=1,offset=0};
					  ppAppExp (pr,thisFix,right,d-1);
					  rpcond(atom);
					 closeBox ())
				    end
				 | e' => prNon e')
			   | NONfix => prNon rand
		    end
		fun appPrint(_,_,_,0) = pps "#"
		  | appPrint(APPexp(rator,rand),l,r,d) =
		    (case stripMark rator
		       of CONexp(DATACON{name,...},_) =>
		           fixitypp([name],rand,l,r,d)
		        | VARexp(v,_) =>
			   let val path =
			           case !v
				     of VALvar{path=SymPath.SPATH p,...} => p
				      | OVLDvar{name,...} => [name]
				      | ERRORvar => [S.varSymbol "<errorvar>"]
			    in fixitypp(path,rand,l,r,d)
			   end
		        | rator =>
			   (openHOVBox 2;
			     ppExp'(rator,true,d-1); PP.break ppstrm {nsp=1,offset=2};
			     ppExp'(rand,true,d-1);
			    closeBox ()))
		  | appPrint(MARKexp(exp,(s,e)),l,r,d) =
		      (case source_opt
			of SOME source =>
			     if !internals
			     then (pps "<MARK(";
				   prpos(ppstrm,source,s); pps ",";
				   prpos(ppstrm,source,e); pps "): ";
				   ppExp'(exp,false,d); pps ">")
			     else appPrint(exp,l,r,d)
			 | NONE => appPrint(exp,l,r,d))
		  | appPrint (e,_,_,d) = ppExp'(e,true,d)
	     in appPrint arg
	    end
     in (fn (exp,depth) => ppExp'(exp,false,depth))
    end

and ppRule (context as (env,source_opt)) ppstrm (RULE(pat,exp),d) =
    if d>0
    then (PP.openHVBox ppstrm (PP.Rel 0);
	  ppPat env ppstrm (pat,d-1);
	  PP.string ppstrm " =>"; PP.break ppstrm {nsp=1,offset=2};
	  ppExp context ppstrm (exp,d-1);
	  PP.closeBox ppstrm)
    else PP.string ppstrm "<rule>"

and ppVB (context as (env,source_opt)) ppstrm (VB{pat,exp,...},d) =
    if d>0
    then (PP.openHVBox ppstrm (PP.Rel 0);
	  ppPat env ppstrm (pat,d-1); PP.string ppstrm " =";
	  PP.break ppstrm {nsp=1,offset=2}; ppExp context ppstrm (exp,d-1);
	  PP.closeBox ppstrm)
    else PP.string ppstrm "<binding>"

and ppRVB context ppstrm (RVB{var, exp, ...},d) =
    if d>0
    then (PP.openHOVBox ppstrm (PP.Rel 0);
	  ppVar ppstrm var; PP.string ppstrm " =";
	  PP.break ppstrm {nsp=1,offset=2}; ppExp context ppstrm (exp,d-1);
	  PP.closeBox ppstrm)
    else PP.string ppstrm "<rec binding>"

and ppDec (context as (env,source_opt)) ppstrm =
  let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = PU.en_pp ppstrm

      fun ppDec'(_,0) = pps "<dec>"
        | ppDec'(VALdec vbs,d) =
	  (openHVBox 0;
	   PU.ppvlist ppstrm ("val ","and ",
	     (fn ppstrm => fn vb => ppVB context ppstrm (vb,d-1)),vbs);
	   closeBox ())
        | ppDec'(VALRECdec rvbs,d) =
	  (openHVBox 0;
	   PU.ppvlist ppstrm ("val rec ","and ",
	     (fn ppstrm => fn rvb => ppRVB context ppstrm (rvb,d-1)),rvbs);
	   closeBox ())
	| ppDec'(DOdec exp, d) =
	  (openHVBox 0;
	   pps "do";
	   PP.break ppstrm {nsp=1,offset=2}; ppExp context ppstrm (exp,d-1);
	   closeBox ())
        | ppDec'(TYPEdec tycs,d) = let
	      fun f ppstrm (DEFtyc{path, tyfun=TYFUN{arity,body},...}) =
		  (case arity
		    of 0 => ()
		     | 1 => (pps "'a ")
		     | n => (PU.ppTuple ppstrm PP.string (typeFormals n);
                             pps " ");
		   PU.ppSym ppstrm (InvPath.last path);
		   pps " = "; ppType env ppstrm body)
		| f _ _ = bug "ppDec'(TYPEdec)"
	  in
	      openHVBox 0;
	      PU.ppvlist ppstrm ("type "," and ", f, tycs);
	      closeBox ()
	  end
        | ppDec'(DATATYPEdec{datatycs,withtycs},d) = let
	      fun ppDATA ppstrm (GENtyc { path, arity, kind, ... }) =
		  (case kind
		     of DATATYPE(_) =>
		       (case arity
			 of 0 => ()
			  | 1 => (pps "'a ")
			  | n => (PU.ppTuple ppstrm PP.string (typeFormals n);
				  pps " ");
			PU.ppSym ppstrm (InvPath.last path); pps " = ..."(*;
		        PU.ppSequence ppstrm
			{sep=(fn ppstrm => (PP.string ppstrm " |";
					    PP.break ppstrm {nsp=1,offset=0})),
			 pr=(fn ppstrm => fn (DATACON{name,...}) =>
					     PU.ppSym ppstrm name),
			 style=PU.INCONSISTENT}
			dcons*))
		     | _ => bug "ppDec'(DATATYPEdec) 1.1")
		| ppDATA _ _ = bug "ppDec'(DATATYPEdec) 1.2"
	      fun ppWITH ppstrm (DEFtyc{path, tyfun=TYFUN{arity,body},...}) =
		  (case arity
		    of 0 => ()
		     | 1 => (pps "'a ")
		     | n => (PU.ppTuple ppstrm PP.string (typeFormals n);
                             pps " ");
		   PU.ppSym ppstrm (InvPath.last path);
		   pps " = "; ppType env ppstrm body)
		| ppWITH _ _ = bug "ppDec'(DATATYPEdec) 2"
	  in
	      (* could call PPDec.ppDec here *)
	      openHVBox 0;
	      PU.ppvlist ppstrm ("datatype ","and ", ppDATA, datatycs);
	      PP.newline ppstrm;
	      PU.ppvlist ppstrm ("withtype ","and ", ppWITH, withtycs);
	      closeBox ()
	  end
        | ppDec'(ABSTYPEdec _,d) = pps "abstype"

        | ppDec'(EXCEPTIONdec ebs,d) = let
	      fun f ppstrm (EBgen{exn=DATACON{name,...},etype,...}) =
		  (PU.ppSym ppstrm name;
		   case etype
		    of NONE => ()
		     | SOME ty' => (pps " of "; ppType env ppstrm ty'))
		| f ppstrm (EBdef{exn=DATACON{name,...},
				  edef=DATACON{name=dname,...}}) =
		  (PU.ppSym ppstrm name; pps "="; PU.ppSym ppstrm dname)
	  in
	      openHVBox 0;
	      PU.ppvlist ppstrm ("exception ","and ", f, ebs);
	      closeBox ()
	  end
        | ppDec'(STRdec sbs,d) = let
	      fun f ppstrm (STRB{name, str=M.STR { access, ... }, def}) =
		  (PU.ppSym ppstrm name;
		   ppAccess ppstrm access;
		   pps " = ";
		   PP.break ppstrm {nsp=1,offset=2};
		   ppStrexp context ppstrm (def,d-1))
		| f _ _ = bug "ppDec:STRdec:STRB"
	  in
	      openHVBox 0;
	      PU.ppvlist ppstrm ("structure ","and ", f, sbs);
	      closeBox ()
	  end
        | ppDec'(FCTdec fbs,d) = let
	      fun f ppstrm (FCTB{name=fname, fct=M.FCT { access, ... }, def}) =
                  (PU.ppSym ppstrm fname;
		   ppAccess ppstrm access;
		   pps " = ";
		   PP.break ppstrm {nsp=1,offset= 2};
		   ppFctexp context ppstrm (def,d-1))
		| f _ _ = bug "ppDec':FCTdec"
	  in
	      openHVBox 0;
	      PU.ppvlist ppstrm ("functor ","and ", f, fbs);
              closeBox ()
	  end
        | ppDec'(SIGdec sigvars,d) = let
	      fun f ppstrm (M.SIG { name, ... }) =
		  (pps "signature ";
		   case name of
		       SOME s => PU.ppSym ppstrm s
                     | NONE => pps "ANONYMOUS")
		| f _ _ = bug "ppDec':SIGdec"
	  in
	      openHVBox 0;
	      PU.ppSequence ppstrm {sep=PP.newline, pr=f,
				 style=PU.CONSISTENT} sigvars;
	      closeBox ()
	  end
        | ppDec'(FSIGdec sigvars,d) = let
	      fun f ppstrm (M.FSIG{kind, ...}) =
		  (pps "funsig ";
                   case kind of SOME s => PU.ppSym ppstrm s
                              | NONE => pps "ANONYMOUS")
		| f _ _ = bug "ppDec':FSIGdec"
	  in
	      openHVBox 0;
	      PU.ppSequence ppstrm
			 {sep=PP.newline, pr = f, style = PU.CONSISTENT} sigvars;
	      closeBox ()
	  end
        | ppDec'(LOCALdec(inner,outer),d) =
	  (openHVBox 0;
	   pps "local"; PU.nl_indent ppstrm 2;
	   ppDec'(inner,d-1); PP.newline ppstrm;
	   pps "in ";
	   ppDec'(outer,d-1); PP.newline ppstrm;
	   pps "end";
	   closeBox ())
        | ppDec'(SEQdec decs,d) =
	  (openHVBox 0;
	   PU.ppSequence ppstrm
	     {sep=PP.newline,
	      pr=(fn ppstrm => fn dec => ppDec'(dec,d)),
	      style=PU.CONSISTENT}
	     decs;
	   closeBox ())
        | ppDec'(FIXdec {fixity,ops},d) =
	  (openHVBox 0;
	   case fixity
	     of NONfix => pps "nonfix "
	      | INfix (i,_) =>
		    (if i mod 2 = 0 then
		       pps "infix "
		     else pps "infixr ";
		     if i div 2 > 0 then
		       (pps(Int.toString(i div 2));
			pps " ")
		     else ());
	   PU.ppSequence ppstrm
	     {sep=(fn ppstrm => PP.break ppstrm {nsp=1,offset=0}),
	      pr=PU.ppSym,style=PU.INCONSISTENT}
	     ops;
	   closeBox ())

        | ppDec'(OVLDdec ovldvar,d) =
	  (pps "overload "; ppVar ppstrm ovldvar)

        | ppDec'(OPENdec strbs,d) =
	  (openHVBox 0;
	   pps "open ";
	   PU.ppSequence ppstrm
	     {sep=(fn ppstrm => PP.break ppstrm {nsp=1,offset=0}),
	      pr=(fn ppstrm => fn (sp,_) =>
                        pps (SymPath.toString sp)),
	      style=PU.INCONSISTENT}
            strbs;
	   closeBox ())

        | ppDec'(MARKdec(dec,(s,e)),d) =
	  (case source_opt
	    of SOME source =>
	       (pps "MARKdec(";
		ppDec'(dec,d); pps ",";
		prpos(ppstrm,source,s); pps ",";
		prpos(ppstrm,source,e); pps ")")
	     | NONE => ppDec'(dec,d))

     in ppDec'
    end

and ppStrexp (context as (statenv,source_opt)) ppstrm =
    let val pps = PP.string ppstrm

      fun ppStrexp'(_,0) = pps "<strexp>"

	| ppStrexp'(VARstr str, d) = (ppStr ppstrm str)

	| ppStrexp'(APPstr{oper, arg, ...}, d) =
	  (ppFct ppstrm oper; pps"("; ppStr ppstrm arg; pps")")
        | ppStrexp'(STRstr bindings, d) =
              (PP.openVBox ppstrm (PP.Abs 0);
	       pps "struct";
	       PU.ppvseq ppstrm 2 ""
		 (fn ppstrm => fn binding =>
		     PPModules.ppBinding ppstrm
			(Bindings.bindingSymbol binding,binding,statenv,d-1))
		 bindings;
	       pps "end";
               PP.closeBox ppstrm)
	| ppStrexp'(LETstr(dec,body),d) =
	      (PP.openHVBox ppstrm (PP.Abs 0);
	       pps "let "; ppDec context ppstrm (dec,d-1);
               PP.cut ppstrm;
	       pps " in "; ppStrexp'(body,d-1);
	       PP.cut ppstrm;
	       pps "end";
	       PP.closeBox ppstrm)
        | ppStrexp'(MARKstr(body,(s,e)),d) =
	      (case source_opt
		of SOME source =>
	           (pps "MARKstr(";
		    ppStrexp'(body,d); pps ",";
		    prpos(ppstrm,source,s); pps ",";
		    prpos(ppstrm,source,e); pps ")")
	         | NONE => ppStrexp'(body,d))

   in ppStrexp'
  end

and ppFctexp (context as (_,source_opt)) ppstrm =
  let val pps = PP.string ppstrm

      fun ppFctexp'(_, 0) = pps "<fctexp>"

        | ppFctexp'(VARfct fct, d) = ppFct ppstrm fct

        | ppFctexp'(FCTfct{param, def, ...}, d) =
            (pps "FCT(";
	     ppStr ppstrm param;
	     pps ") => ";
	     PP.openHVBox ppstrm (PP.Abs 2);
	       PP.cut ppstrm;
 	       ppStrexp context ppstrm (def,d-1);
	     PP.closeBox ppstrm)

        | ppFctexp'(LETfct(dec,body),d) =
	    (PP.openHVBox ppstrm (PP.Abs 0);
	     pps "let "; ppDec context ppstrm (dec,d-1);
             PP.cut ppstrm;
	     pps " in "; ppFctexp'(body,d-1);
	     PP.cut ppstrm;
	     pps "end";
	     PP.closeBox ppstrm)

	| ppFctexp'(MARKfct(body,(s,e)),d) =
	    (case source_opt
	      of SOME source =>
	           (pps "MARKfct(";
		    ppFctexp'(body,d); pps ",";
		    prpos(ppstrm,source,s); pps ",";
		    prpos(ppstrm,source,e); pps ")")
               | NONE => ppFctexp'(body,d))

   in ppFctexp'
  end

end (* top-level local *)
end (* structure PPAbsyn *)
