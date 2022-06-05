(* pplty.sml
 *
 * (c) 2006 SML/NJ Fellowship
 *
 * Pretty Printer for PLambda types using the new SMLNJ-lib new pretty printer
 *
 *)

signature PPLTY =
sig
  (* printing flags *)
  val dtPrintNames : bool ref
  val printIND : bool ref

  val ppList : PrettyPrint.stream ->
               {sep: string, pp : PrettyPrint.stream -> 'a -> unit} ->
               'a list -> unit
  val ppTKind : int -> PrettyPrint.stream -> Lty.tkind -> unit
  val ppTyc : int -> PrettyPrint.stream -> Lty.tyc -> unit
  val ppLty : int -> PrettyPrint.stream -> Lty.lty -> unit
end

structure PPLty (* : PPLTY *) =
struct

local
    structure PT = PrimTyc
    structure PP = PrettyPrint
    open PPUtil
in

val dtPrintNames : bool ref = ref true
val printIND : bool ref = ref false

fun ppSeq ppstrm {sep: string, pp : PP.stream -> 'a -> unit} (list: 'a list) =
    ppSequence ppstrm
      {sep = fn ppstrm => (PP.string ppstrm sep;
			   PP.break ppstrm {nsp=1, offset=0}),
       style = INCONSISTENT,
       pr = pp}
      list

fun ppList ppstrm {sep: string, pp : PP.stream -> 'a -> unit} (list: 'a list) =
    ppClosedSequence ppstrm
      {front = fn ppstrm => (PP.string ppstrm "["),
       back = fn ppstrm => (PP.string ppstrm "]"),
       sep = PPUtil.sepWithCut sep,
       style = INCONSISTENT,
       pr = pp}
      list

(* ppTKind : tkind -> unit
 * Print a hashconsed representation of the kind *)
fun ppTKind pd ppstrm (tk : Lty.tkind) =
    if pd < 1 then pps ppstrm "<tk>" else
    let val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
        val ppTKind' = ppTKind (pd-1) ppstrm
	val ppList' = ppList ppstrm
	fun ppTKindI(Lty.TK_MONO) = pps "M"
	  | ppTKindI(Lty.TK_BOX) = pps "B"
	  | ppTKindI(Lty.TK_FUN (argTkinds, resTkind)) =
	      (* resTkind may be a TK_SEQ wrapping some tkinds
	       * These are produced by Elaborate/modules/instantiate.sml
	       *)
	     (openHOVBox 1;
	       pps "(";
	       ppList' {sep=",", pp=ppTKind (pd-1)} argTkinds;
	       pps "=>"; ppTKind' resTkind;
	       pps ")";
	      closeBox())
	  | ppTKindI(Lty.TK_SEQ tkinds) =
	     (openHOVBox 1;
	       pps "KSEQ";
	       ppList' {sep=",", pp=ppTKind (pd-1)} tkinds;
	      closeBox())
     in ppTKindI (Lty.tk_outX tk)
    end (* ppTKind *)

fun tycEnvFlatten(tycenv) =
    (case Lty.teDest(tycenv)
       of NONE => []
        | SOME(elem, rest) => elem::tycEnvFlatten(rest))

fun ppKeFrame pd ppstrm ks =
    ppList ppstrm {sep=",", pp=ppTKind pd} ks

fun ppKindEnv pd ppstrm kenv =
    if pd < 1 then pps ppstrm "<tkenv>" else
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
     in openHOVBox 1;
        ppList ppstrm {sep=",",pp=ppKeFrame (pd-1)} kenv;
        closeBox ()
    end

fun ppTEBinder pd ppstrm (binder: Lty.teBinder) =
    if pd < 1 then pps ppstrm "<teBinder>" else
    let val {openHOVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
    in openHOVBox 1;
       (case binder
         of Lty.Lamb (level, ks) =>
            (pps "L"; ppi level;
             pps ": ";
             ppKeFrame (pd-1) ppstrm ks)
          | Lty.Beta (level, args, ks) =>
            (pps "B"; ppi level; pps "(";
             ppList ppstrm {sep=",", pp=ppTyc (pd-1)} args;
             pps ": ";
             ppKeFrame (pd-1) ppstrm ks;
             pps ")"));
       closeBox()
    end (* function ppTEBinder *)

and ppTyc pd ppstrm (tycon : Lty.tyc) =
    (* FLINT variables are represented using deBruijn indices *)
    if pd < 1 then pps ppstrm "<tyc>" else
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, break, ...} =
            en_pp ppstrm

        (* partially applied versions of functions *)
	val ppList' : {pp: PP.stream -> 'a -> unit, sep: string}
                      -> 'a list -> unit =
              fn x => ppList ppstrm x
            (* eta-expand (ppList ppstrm) to avoid value restriction *)

	val ppTKind' = ppTKind (pd-1) ppstrm
	val ppTyc' = ppTyc (pd-1) ppstrm

	fun ppTycI (Lty.TC_VAR(depth, cnt)) =
	    (pps "TV(";
	     (* depth is a deBruijn index set in elabmod.sml/instantiate.sml *)
	     pps (DebIndex.di_print depth);
	     pps ",";
	     (* cnt is computed in instantiate.sml sigToInst or
	        alternatively may be simply the IBOUND index *)
	     ppi cnt;
	     pps ")")
	  (* Named tyc VAR; is actually an lvar *)
	  | ppTycI (Lty.TC_NVAR tvar) =
	    (pps "NTV(v"; pps(LambdaVar.prLvar tvar); pps ")")
	  | ppTycI (Lty.TC_PRIM primtycon) =
	    (pps "PRIM(";
	     pps (PT.pt_print primtycon);
	     pps ")")
	  | ppTycI (Lty.TC_FN (argTkinds, resultTyc)) =
	    (openHOVBox 1;
	     pps "TCFN(";
	     ppList' {sep=",", pp=ppTKind (pd-1)} argTkinds;
	     pps ",";
	     break {nsp=1,offset=0};
	     ppTyc' resultTyc;
	     pps ")";
	     closeBox())
	  | ppTycI (Lty.TC_APP(contyc, tys)) =
	    (openHOVBox 0;
	     pps "TCAP(";
             PP.openHVBox ppstrm (PP.Rel 0);
	     ppTyc' contyc;
	     pps ","; break {nsp=1,offset=0};
	     ppList' {sep=",", pp=ppTyc (pd-1)} tys;
	     pps ")";
             closeBox();
	     closeBox())
	  | ppTycI (Lty.TC_SEQ tycs) =
	    (openHOVBox 1;
	     pps "SEQ(";
	     ppList' {sep=",", pp=ppTyc (pd-1)} tycs;
	     pps ")";
	     closeBox())
	  | ppTycI (Lty.TC_PROJ(tycon, index)) =
	    (openHOVBox 1;
	     pps "PROJ(";
	     ppTyc' tycon;
	     pps ",";
	     break {nsp=1,offset=0};
	     pps (Int.toString index);
	     pps ")";
	     closeBox())
	  | ppTycI (Lty.TC_SUM(tycs)) =
	    (pps "SUM(";
	     ppList' {sep=",", pp=ppTyc (pd-1)} tycs;
	     pps ")")
	    (* TC_FIX is a recursive datatype constructor
	       from a (mutually-)recursive family *)
	  | ppTycI (Lty.TC_FIX{family={size,names,gen,params},index}) =
            if !dtPrintNames then pps (Vector.sub(names,index))
            else
	    (openHOVBox 0;
              pps "FIX(";
              openHVBox 0;
               pps "size = "; ppi size; break {nsp=1,offset=0};
               pps "index = "; ppi index; break {nsp=1,offset=0};
               pps "gen = ";
               openHOVBox 2;
                ppTyc' gen;
               closeBox ();
               break {nsp=1,offset=0};
               pps "prms = ";
               openHOVBox 2;
                ppList' {sep = ",", pp = ppTyc (pd-1)} params;
               closeBox ();
               pps ")";
              closeBox();
	     closeBox())
	  | ppTycI (Lty.TC_ABS tyc) =
	    (pps "ABS(";
	     ppTyc' tyc;
	     pps ")")
	  | ppTycI (Lty.TC_BOX tyc) =
	    (pps "BOX(";
	     ppTyc' tyc;
	     pps ")")
	    (* rflag is a tuple kind template, a singleton datatype RF_TMP *)
	  | ppTycI (Lty.TC_TUPLE(rflag, tycs)) =
	    (ppClosedSequence ppstrm
                {front = (fn s => PP.string s "{"),
                 sep = PPUtil.sepWithCut ",",
                 back = (fn s => PP.string s "}"),
                 pr = ppTyc (pd-1),
                 style = INCONSISTENT}
	        tycs)
	    (* fflag records the calling convention: either FF_FIXED or FF_VAR *)
	  | ppTycI (Lty.TC_ARROW (fflag, argTycs, resTycs)) =
	    ((case fflag
                of Lty.FF_FIXED => pps "AR("
		 | Lty.FF_VAR(b1, b2) =>
                    (pps "AR[";
                     pps(case (b1,b2)
                           of (true, true) => "rr]("
                            | (true, false) => "rc]("
                            | (false, true) => "cr]("
                            | (false, false) => "cc](")));
             openHOVBox 0;
	     ppList' {sep=",", pp=ppTyc (pd-1)} argTycs;
	     pps ",";
	     break {nsp=1,offset=0};
	     ppList' {sep=",", pp=ppTyc (pd-1)} resTycs;
             closeBox ();
	     pps ")")
	    (* According to ltykernel.sml comment, this arrow tyc is not used *)
	  | ppTycI (Lty.TC_PARROW (argTyc, resTyc)) =
	    (pps "PAR(";
	     ppTyc' argTyc;
	     pps ",";
	     break {nsp=1,offset=0};
	     ppTyc' resTyc;
	     pps ")")
	  | ppTycI (Lty.TC_TOKEN (tok, tyc)) =
	    (pps "TOK(";
	     pps (Lty.token_name tok);
	     pps ",";
	     break {nsp=1,offset=0};
	     ppTyc' tyc;
	     pps ")")
	  | ppTycI (Lty.TC_CONT tycs) =
	    (pps "CONT(";
	     ppList' {sep=", ", pp=ppTyc (pd-1)} tycs;
	     pps ")")
	  | ppTycI (Lty.TC_IND (tyc, tycI)) =
            if !printIND then
              (openHOVBox 1;
               pps "IND(";
               openHOVBox 0;
               ppTyc' tyc;
               pps ",";
               break {nsp=1,offset=0};
               ppTycI tycI;
               closeBox();
               pps ")";
               closeBox())
            else ppTyc' tyc
	  | ppTycI (Lty.TC_ENV (tyc, ol, nl, tenv)) =
	    (openHVBox 1;
	     pps "ENV(";
	     pps "ol=";
	     pps (Int.toString ol);
	     pps ", ";
	     pps "nl=";
	     pps (Int.toString nl);
	     pps ",";
	     break {nsp=1,offset=0};
	     ppTyc' tyc;
	     pps ",";
	     break {nsp=1,offset=0};
	     ppList' {sep=",", pp=ppTEBinder (pd-1)} (tycEnvFlatten tenv);
             pps ")";
	     closeBox())
    in ppTycI (Lty.tc_outX tycon)
    end (* ppTyc *)


fun ppTycEnv pd ppstrm (tycEnv : Lty.tycEnv) =
    if pd < 1 then pps ppstrm "<tycEnv>" else
    let val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
     in openHOVBox 1;
	 pps "TycEnv(";
	 ppList ppstrm {sep=", ", pp=ppTEBinder (pd-1)} (tycEnvFlatten tycEnv);
	 pps ")";
	closeBox()
    end (* ppTycEnv *)


fun ppLty pd ppstrm (lty: Lty.lty) =
    if pd < 1 then pps ppstrm "<tyc>" else
    let val {openHOVBox, openHVBox, openVBox, closeBox, pps, ppi, break, newline} =
            en_pp ppstrm
	val ppList' : {pp:PP.stream -> 'a -> unit, sep: string} -> 'a list -> unit =
              fn x => ppList ppstrm x
	       (* eta-expansion of ppList to avoid value restriction *)

	val ppTKind' = ppTKind (pd-1) ppstrm
	val ppLty' = ppLty (pd-1) ppstrm

        fun ppLtyI (Lty.LT_TYC tc) =
            (pps "TYC("; ppTyc pd ppstrm tc; pps ")")
          | ppLtyI (Lty.LT_STR ltys) =
            (pps "STR("; ppList' {sep=",",pp=ppLty (pd-1)} ltys; pps ")")
          | ppLtyI (Lty.LT_FCT (args,res)) =
            (pps "FCT("; ppList' {sep=",",pp=ppLty (pd-1)} args; pps ",";
             break {nsp=1,offset=0};
             ppList' {sep=",",pp=ppLty (pd-1)} res; pps ")")
          | ppLtyI (Lty.LT_POLY (ks,ltys)) =
	    (openHOVBox 1;
	     pps "POL(";
	     ppList' {sep=",", pp=ppTKind (pd-1)} ks;
	     pps ",";
	     break {nsp=1,offset=0};
	     ppList' {sep=",",pp=ppLty (pd-1)} ltys;
	     pps ")";
	     closeBox())
          | ppLtyI (Lty.LT_CONT ltys) =
            (pps "CONT("; ppList' {sep=",",pp=ppLty (pd-1)} ltys; pps ")")
          | ppLtyI (Lty.LT_IND(nt,ot)) =
            if !printIND then
              (pps "IND(";
               openHOVBox 0;
               ppLty' nt; pps ",";
               break {nsp=1,offset=0};
               ppLtyI ot;
               closeBox();
               pps ")")
            else ppLty pd ppstrm nt
	  | ppLtyI (Lty.LT_ENV (lty, ol, nl, tenv)) =
	    (openHVBox 1;
	     pps "LT_ENV(";
	     pps "ol=";
	     pps (Int.toString ol);
	     pps ", ";
	     pps "nl=";
	     pps (Int.toString nl);
	     pps ",";
	     break {nsp=1,offset=0};
	     ppLty' lty;
	     pps ",";
	     break {nsp=1,offset=0};
	     ppList' {sep=",", pp=ppTEBinder (pd-1)} (tycEnvFlatten tenv);
             pps ")";
	     closeBox())
    in ppLtyI (Lty.lt_outX lty)
    end (* ppLty *)

end (* local *)

end (* structure PPLty *)
