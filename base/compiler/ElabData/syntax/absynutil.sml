(* absynutil.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * More stuff from ElabUtil should be moved here eventually.
 *)
structure AbsynUtil : sig

    val unitExp : Absyn.exp

    val TUPLEexp : Absyn.exp list -> Absyn.exp
    val TUPLEpat : Absyn.pat list -> Absyn.pat
    val stripPatMarks : Absyn.pat -> Absyn.pat

  end =

struct

  local open Absyn in

  val unitExp = RECORDexp []

  fun TUPLEexp l =
      let fun build (_, []) = []
	    | build (i, e :: es) =
	      (LABEL { number = i-1, name = Tuples.numlabel i }, e)
	      :: build (i+1, es)
       in RECORDexp (build (1, l))
      end

  fun TUPLEpat l =
      let fun build (_, []) = []
	    | build (i, e :: es) = (Tuples.numlabel i, e) :: build (i+1, es)
       in RECORDpat { fields = build (1, l), flex = false,
		      typ = ref Types.UNDEFty }
      end

fun stripPatMarks pat =
    case pat 
      of (MARKpat(p,_)) => stripPatMarks p
       | RECORDpat{fields, flex, typ} =>
	 RECORDpat{fields = map (fn (l,p) => (l, stripPatMarks p)) fields, flex = flex, typ = typ}
       | APPpat (dc, tvs, p) => 
         APPpat (dc, tvs, stripPatMarks p)
       | CONSTRAINTpat (p, ty) => CONSTRAINTpat (stripPatMarks p, ty) 
       | LAYEREDpat(pat1, pat2) => 
	 LAYEREDpat(stripPatMarks pat1, stripPatMarks pat2)
       | ORpat (pat1, pat2) =>
         ORpat (stripPatMarks pat1, stripPatMarks pat2)
       | VECTORpat (pats,ty) => 
	 VECTORpat (map stripPatMarks pats, ty)
       | p => p

end (* local *)
end (* structure AbsynUtil *)
