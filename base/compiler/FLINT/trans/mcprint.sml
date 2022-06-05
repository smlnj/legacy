(* mcprint.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* pretty printing for match compiler (MC) internal structures *)

signature PPMC =
sig
  val debugMsg : bool ref -> string -> unit
  val debugPrint : bool ref
                   -> (string *
		       (PrettyPrint.stream -> 'a -> unit) *
		       'a)
                   -> unit
end (* signature PPMC *)


structure PPMatchComp (* : PPMC *) =
struct

local
   structure PP = PrettyPrint
   open MCCommon
   open PP
   open PPUtil
in

fun bug s = ErrorMsg.impossible ("PPMC: "^s)

				fun debugMsg (debugging: bool ref) (msg: string) =
    if (!debugging)
    then with_default_pp
	  (fn ppstrm =>
	    (openHVBox ppstrm (PP.Rel 0);
	     PP.string ppstrm msg;
	     closeBox ppstrm;
	     newline ppstrm;
	     PP.flushStream ppstrm))
    else ()

fun debugPrint (debugging: bool ref)
               (msg: string, printfn: PP.stream -> 'a -> unit, arg: 'a) =
    if (!debugging)
    then with_default_pp
	  (fn ppstrm =>
	    (openHVBox ppstrm (PP.Rel 0);
	     PP.string ppstrm msg;
	     newline ppstrm;
	     PP.nbSpace ppstrm 2;
	     openHVBox ppstrm (PP.Rel 0);
	     printfn ppstrm arg;
	     closeBox ppstrm;
	     newline ppstrm;
	     closeBox ppstrm;
	     PP.flushStream ppstrm))
    else ()

fun ppPcon (pd: int) ppstrm (c : pcon) : unit =
    let val {openHOVBox, openHVBox, closeBox, break, newline, pps, ppi, ...} =
            en_pp ppstrm
     in case c
	 of DATApcon (datacon, _) => ppSym ppstrm (TypesUtil.dconName datacon)
	  | INTpcon (intConst) => pps (IntConst.toString intConst)
	  | WORDpcon (intConst) => pps (IntConst.toString intConst)
	  | STRINGpcon s => ppString ppstrm s
	  | VLENpcon (n,_) => pps "VLENpcon"
    end

fun ppPath (pd: int) ppstrm (p: path) : unit =
    let val {openHOVBox, openHVBox, closeBox, break, newline, pps, ppi, ...} =
            en_pp ppstrm
     in case p
	 of PIPATH (n, path) =>
	    (pps "PIPATH("; ppi n; pps ","; ppPath (pd-1) ppstrm path; pps ")")
	  | VPIPATH (n, ty, path) =>
	    (pps "VPIPATH("; ppi n; pps ","; ppPath (pd-1) ppstrm path; pps ")")
	  | VLENPATH (ty, path) =>
    	    (pps "VLENPATH("; ppPath (pd-1) ppstrm p; pps ")")
	  | DELTAPATH (pcon, path) =>
	    (pps "DELTAPATH(";
	     ppPcon (pd - 1) ppstrm pcon; pps ",";
             ppPath (pd-1) ppstrm path;
	     pps ")")
	  | ROOTPATH => pps "ROOTPATH"
    end

fun ppList ppstrm ppfn elems =
    ppClosedSequence ppstrm
      {front = (fn strm => PP.string strm "["),
       back =  (fn strm => PP.string strm "]"),
       sep = PPUtil.sepWithSpc ",",
       pr = ppfn,
       style = CONSISTENT}
      elems

fun ppOption ppstrm ppfn elemOp =
    case elemOp
     of NONE => PP.string ppstrm "<<>>"
      | SOME e => (PP.string ppstrm "<< "; ppfn ppstrm e; PP.string ppstrm " >>")

fun ppDectree (pd: int) ppstrm (dt: dectree) : unit =
    let val {openHOVBox, openHVBox, closeBox, break, newline, pps, ppi, ...} =
            en_pp ppstrm
	fun ppdt (pd: int) dt =
            case dt
	     of CASETEST (path, consig, caseTrees, deftreeOp) =>
		(openHOVBox 4;
		 pps "CASETEST"; nl_indent ppstrm 4;
		 openHOVBox 0;
		 pps "path: "; ppPath (pd-1) ppstrm path; newline();
		 pps "sign: ";
		 (case consig
		    of Access.CSIG(n,m) =>
		       (pps "CSIG("; ppi n; pps ","; ppi m; pps ")")
		     | Access.CNIL => pps "CNIL");
		 newline();
		 pps "branches:"; nl_indent ppstrm 4;
		   openHOVBox 2;
		   ppList ppstrm
			(fn strm =>
			    (fn (pcon,dt0) =>
				(PP.string strm "("; ppPcon (pd-1) strm pcon;
				 PP.string strm ", ";
				 ppDectree (pd-1) strm dt0;
				 PP.string strm ")")))
			caseTrees;
		   closeBox(); newline();
		 pps "deftreeOp: ";
		 ppOption ppstrm
  		    (fn strm => (fn dt0 => ppDectree (pd-1) strm dt0))
		    deftreeOp;
		 closeBox();
		 closeBox())
	      | RHS r =>
		   (pps "RHS("; ppi r; pps ")")
	      | BIND (path, dectree) =>
		   pps "BIND"

    in ppdt pd dt
    end

fun ppAndor (pd: int) ppstrm (ao: andor) : unit =
    let val {openHOVBox, openHVBox, closeBox, break, newline, pps, ppi, ...} =
            en_pp ppstrm
     in case ao
	 of AND {bindings, subtrees} =>
            (pps "AND")
	  | CASE {bindings, sign, cases} =>
            (pps "CASE")
	  | LEAF {bindings} =>
	    (pps "LEAF")
    end

fun ppDecision (pd: int) ppstrm (decn: decision) : unit =
    let val {openHOVBox, openHVBox, closeBox, break, newline, pps, ppi, ...} =
            en_pp ppstrm
     in case decn
	 of CASEDEC (path, consig, cases, defaults) =>
            (pps "CASEDEC")
	  | BINDDEC (path, rules) =>
            (pps "BINDDEC")
    end

end (* top local *)

end (* structure PPMatchComp *)
