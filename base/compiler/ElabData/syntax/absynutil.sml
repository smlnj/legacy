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

    (* strip MARKexp and CONSTRAINTexp head constructors. Used to access the
     * RECORDexp (pair) argument of an infix constructor in an APPexp
     * (see PPAbsyn.ppAppExp)
     *)
    val headStripExp : Absyn.exp -> Absyn.exp

    (* strip MARKpat and CONSTRAINTpat head constructors. Used to access
     * the RECORDpat (pair) argument of an infix constructor in an APPpat
     * (see PPAbsyn.ppDconPat)
     *)
    val headStripPat : Absyn.pat -> Absyn.pat

    val stripPatMarks : Absyn.pat -> Absyn.pat

  end = struct

    datatype exp = datatype Absyn.exp
    datatype pat = datatype Absyn.pat

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

    fun headStripExp (MARKexp(exp,_)) = headStripExp exp
      | headStripExp (CONSTRAINTexp(exp,_)) = headStripExp exp
      | headStripExp exp = exp

    fun headStripPat (MARKpat (p,_)) = headStripPat p
      | headStripPat (CONSTRAINTpat (p, ty)) = headStripPat p
      | headStripPat pat = pat

    fun stripPatMarks pat = (case pat
           of (MARKpat(p,_)) => stripPatMarks p
            | RECORDpat{fields, flex, typ} => RECORDpat{
                  fields = map (fn (l,p) => (l, stripPatMarks p)) fields,
                  flex = flex, typ = typ
                }
            | APPpat(dc, tvs, p) =>  APPpat (dc, tvs, stripPatMarks p)
            | APPpat(dc, tvs, p) =>  APPpat (dc, tvs, stripPatMarks p)
            | CONSTRAINTpat(p, ty) => CONSTRAINTpat (stripPatMarks p, ty)
            | LAYEREDpat(pat1, pat2) => LAYEREDpat(stripPatMarks pat1, stripPatMarks pat2)
            | ORpat(pat1, pat2) => ORpat(stripPatMarks pat1, stripPatMarks pat2)
            | VECTORpat (pats,ty) => VECTORpat(map stripPatMarks pats, ty)
            | p => p
          (* end case *))

  end (* structure AbsynUtil *)
