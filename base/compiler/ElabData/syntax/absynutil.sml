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

    structure A = Absyn

    val unitExp = A.RECORDexp []

    fun TUPLEexp l =
        let fun build (_, []) = []
              | build (i, e :: es) =
                (A.LABEL { number = i-1, name = Tuples.numlabel i }, e)
                :: build (i+1, es)
         in A.RECORDexp (build (1, l))
        end

    fun TUPLEpat l =
        let fun build (_, []) = []
              | build (i, e :: es) = (Tuples.numlabel i, e) :: build (i+1, es)
         in A.RECORDpat { fields = build (1, l), flex = false,
                        typ = ref Types.UNDEFty }
        end

    fun headStripExp (A.MARKexp(exp,_)) = headStripExp exp
      | headStripExp (A.CONSTRAINTexp(exp,_)) = headStripExp exp
      | headStripExp exp = exp

    fun headStripPat (A.MARKpat (p,_)) = headStripPat p
      | headStripPat (A.CONSTRAINTpat (p, ty)) = headStripPat p
      | headStripPat pat = pat

    fun stripPatMarks pat = (case pat
           of A.MARKpat(p,_) => stripPatMarks p
            | A.RECORDpat{fields, flex, typ} => A.RECORDpat{
                  fields = map (fn (l,p) => (l, stripPatMarks p)) fields,
                  flex = flex, typ = typ
                }
            | A.APPpat(dc, tvs, p) => A.APPpat (dc, tvs, stripPatMarks p)
            | A.CONSTRAINTpat(p, ty) => A.CONSTRAINTpat (stripPatMarks p, ty)
            | A.LAYEREDpat(pat1, pat2) => A.LAYEREDpat(stripPatMarks pat1, stripPatMarks pat2)
            | A.ORpat(pat1, pat2) => A.ORpat(stripPatMarks pat1, stripPatMarks pat2)
            | A.VECTORpat (pats,ty) => A.VECTORpat(map stripPatMarks pats, ty)
            | p => p
          (* end case *))

  end (* structure AbsynUtil *)
