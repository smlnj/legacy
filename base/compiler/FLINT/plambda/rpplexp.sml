(* rpplexp.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* _Real_ pretty printing for plambda lexp *)

signature PPLEXP =
sig

  val conToString : PLambda.con -> string
  val ppLexp : int -> PrettyPrint.ppstream -> PLambda.lexp -> unit
  val printMatch : StaticEnv.staticEnv ->
                       (Absyn.pat * PLambda.lexp) list -> unit
  val printFun : PLambda.lexp -> LambdaVar.lvar -> unit

  val stringTag : PLambda.lexp -> string

end (* signature PPLEXP *)


structure PPLexp : PPLEXP =
struct

local structure A = Absyn
      structure DA = Access
      structure S = Symbol
      structure PP = PrettyPrint
      structure PU = PPUtil
      structure LT = PLambdaType
      open PLambda PPUtil
in

fun bug s = ErrorMsg.impossible ("PPLexp: "^s)

fun sayrep rep = say (DA.prRep rep)
val lvarName = LambdaVar.lvarName

fun app2(f, [], []) = ()
  | app2(f, a::r, b::z) = (f(a, b); app2(f, r, z))
  | app2(f, _, _) = bug "unexpected list arguments in function app2"

fun conToString (DATAcon((sym, _, _), _, v)) = ((S.name sym) ^ "." ^ (lvarName v))
  | conToString (INTcon i) = Int.toString i
  | conToString (INT32con i) = "(I32)" ^ (Int32.toString i)
  | conToString (INTINFcon i) = "(II)" ^ IntInf.toString i
  | conToString (WORDcon i) = "(W)" ^ (Word.toString i)
  | conToString (WORD32con i) = "(W32)" ^ (Word32.toString i)
  | conToString (STRINGcon s) = PrintUtil.formatString s
  | conToString (VLENcon n) = Int.toString n

(** use of complex in printLexp may lead to stupid n^2 behavior. *)
fun complex le =
  let fun h l = List.exists g l

      and g (FN(_, _, b)) = g b
        | g (FIX(vl, _, ll, b)) = true
        | g (APP(FN _, _)) = true
        | g (APP(l, r)) = g l orelse g r

        | g (LET _) = true
        | g (TFN(_, b)) = g b
        | g (TAPP(l, [])) = g l
        | g (TAPP(l, _)) = true
        | g (GENOP(_,_,_,_)) = true
        | g (PACK(_, _, _, l)) = g l

        | g (RECORD l) = h l
        | g (SRECORD l) = h l
        | g (VECTOR (l, _)) = h l
        | g (SELECT(_, l)) = g l

        | g (SWITCH _) = true
        | g (CON(_, _, l)) = true
(*      | g (DECON(_, _, l)) = true *)

        | g (HANDLE _) = true
        | g (RAISE(l, _)) = g l
        | g (ETAG (l, _)) = g l

        | g (WRAP(_, _, l)) = g l
        | g (UNWRAP(_, _, l)) = g l
        | g _ = false

   in g le
  end

fun ppLexp (pd:int) ppstrm (l: lexp): unit =
    if pd < 1 then pps ppstrm "<tyc>" else
    let val {openHOVBox, openHVBox, closeBox, break, newline, pps, ppi, ...} =
            en_pp ppstrm
	val ppList' : {pp:PP.stream -> 'a -> unit, sep: string} -> 'a list -> unit =
              fn x => ppList ppstrm x
	       (* eta-expansion of ppList to avoid value restriction *)

        val ppLexp' = ppLexp (pd-1) ppstrm
        val ppLty' = PPLty.ppLty (pd-1) ppstrm
        val ppTyc' = PPLty.ppTyc (pd-1) ppstrm
        fun br0 n = PP.break {nsp=0,offset=n}
        fun br1 n = PP.break {nsp=1,offset=n}
        fun br(n,m) = PP.break {nsp=n,offset=m}
        fun ppClosedSeq (open,sep,close) ppfn elems =
            PU.ppClosedSequence
              {front = (fn s => PP.string s open),
               back = (fn s => PP.string s close),
               sep = PPUtil.sepWithCut sep,
               pr = ppfn,
               style = PU.INCONSISTENT}
              elems

        fun ppl (VAR v) = pps (lvarName v)
          | ppl (INT i) = ppi i
          | ppl (WORD i) = (pps "(W)"; pps (Word.toString i))
          | ppl (INT32 i) = (pps "(I32)"; pps(Int32.toString i))
          | ppl (WORD32 i) = (pps "(W32)"; pps(Word32.toString i))
          | ppl (REAL s) = pps s
          | ppl (STRING s) = PPUtil.ppString ppstrm s
          | ppl (ETAG (l,_)) = ppl l

          | ppl (RECORD l) =
              let val style = if complex l then PU.CONSISTENT else PU.INCONSISTENT
               in openHOVBox 3;
                  pps "RCD";
                  ppClosedSeq ("(",",",")" (ppLexp (pd-1)) l;
                  closeBox ()
              end
        | ppl (SRECORD l) =
              let val style = if complex l then PU.CONSISTENT else PU.INCONSISTENT
               in openHOVBox 4;
                  pps "SRCD";
                  ppClosedSeq ("(",",",")" (ppLexp (pd-1)) l;
                  closeBox ()
              end

        | ppl (VECTOR (l, _)) =
              let val style = if complex l then PU.CONSISTENT else PU.INCONSISTENT
               in openHOVBox 3;
                  pps "VEC";
                  ppClosedSeq ("(",",",")" (ppLexp (pd-1)) l;
                  closeBox ()
              end

        | ppl (PRIM(p,t,ts)) =
              (openHOVBox 4;
                pps "PRM(";
                openHOVBox 0;
                 pps(Primop.prPrimop p); pps ","; br1 0;
                 prLty' t; br1 0;
                 ppClosedSeq ("[",",","]") (ppTyc (pd-1)) ts
                closeBox ()
                pps ")";
               closeBox ())

        | ppl (l as SELECT(i, _)) =
            let fun gather(SELECT(i,l)) =
                      let val (more,root) = gather l
                       in  (i :: more,root)
                      end
                  | gather l = (nil, l)

                val (path,root) = gather l
                fun ipr (i:int) = pps(Int.toString i)
             in openHOVBox 2;
                ppl root;
                ppClosedSeq ("[",",","]") ppi (rev path);
                closeBox ((((((()
            end

        | ppl (FN(v,t,l)) =
            (openHOVBox 3; pps "FN(";
              pps(lvarName v); pps ":"; br1 0; prLty' t; pps ",";
              if complex l then
                 (newline(); (ppLexp' l; pps ")")
              else (ppl l; pps ")");
             closeBox())

        | ppl (CON((s, c, lt), ts, l)) =
            (openHOVBox 4;
              pps "CON(";
              openHOVBox 1; pps "("; pps(S.name s); pps ",";
               pps(DA.prRep c); pps ",";
               prLty' lt; pps ")";
              closeBox ();
              pps ","; br1 0;
              ppClosedSeq ("[",",","]") ppTyc' ts;
              pps ","; br1 0;
              ppl l; pps ")";
             closeBox())

(*
        | ppl (DECON((s, c, lt), ts, l)) =
            (pps "DECON(("; pps(S.name s); pps ","; ppsrep c; pps ",";
             prLty lt; pps "), ["; plist(prTyc, ts, ","); pps "], ";
             if complex l then (indent 4; ppl l; pps ")"; undent 4)
             else (g l; pps ")"))
*)
        | ppl (APP(FN(v,_,l),r)) =
            (openHOVBox 5;
             pps "(APP)";
             ppl (LET(v, r, l));
             closeBox())

        | ppl (LET(v, r, l)) =
            (openHVBox 2;
              openHOVBox 4;
               pps (lvarName v); br1 0; pps "="; br1 0; ppl r;
              closeBox();
              newline();
              ppl l;
             closeBox())

        | ppl (APP(l, r)) =
            (pps "APP(";
             openHVBox 0;
             ppl l; pps ","; br1 0; ppl r;
             closeBox();
             pps ")")

        | ppl (TFN(ks, b)) =
            (openHOVBox 0; pps "TFN(";
             openHVBox 0;
             ppClosedSeq ("(",",",")") PPLty.ppTKind ks; br1 0;
             ppl b;
             closeBox();
             pps ")";
             closeBox())

        | ppl (TAPP(l, ts)) =
            (openHOVBox 0;
              pps "TAPP(";
              openHVBox 0;
               ppl l; br1 0;
               ppClosedSeq ("[",",","]") ppTyc' ks;
              closeBox();
              pps ")";
             closeBox())

        | ppl (GENOP(dict, p, t, ts)) =
              (openHOVBox 4;
                pps "GEN(";
                openHOVBox 0;
                 pps(Primop.prPrimop p); pps ","; br1 0;
                 prLty' t; br1 0;
                 ppClosedSeq ("[",",","]") (ppTyc (pd-1)) ts
                closeBox ()
                pps ")";
               closeBox ())

        | ppl (PACK(lt, ts, nts, l)) =
            (openHOVBox 0;
              pps "PACK(";
              openHVBox 0;
               openHOVBox 0;
                app2 (fn (tc,ntc) =>
                        (pps "<"; ppTyc' tc; pps ","; ppTyc' ntc;
                         pps ">,"; br1 0),
                     ts, nts);
               closeBox(); br1 0;
               prLty' lt; pps ","; br1 0;
               ppl l;
              closeBox();
              pps ")";
             closeBox())

        | ppl (SWITCH (l,_,llist,default)) =
            let fun switch [(c,l)] =
                      (openHOVBox 2;
                       pps (conToString c); pps " =>"; br1 0; ppl l;
                       closeBox())
                  | switch ((c,l)::more) =
                      (openHOVBox 2;
                       pps (conToString c); pps " =>"; br1 0; ppl l;
                       closeBox();
                       newline();
                       switch more)
                  | switch [] = () (* bug "unexpected case in switch" *)

             in openHOVBox 3;
                pps "SWI";
                ppl l; newline();
                pps "of ";
                openHVBox 0;
                switch llist;
                case (default,llist)
                 of (NONE,_) => ()
                  | (SOME l,nil) => (openHOVBox 2; pps "_ =>"; br1 0; ppl l;
                                     closeBox())
                  | (SOME l,_) => (newline();
                                   openHOVBox 2;
                                   pps "_ =>"; br1 0; ppl l;
                                   closeBox());
                closeBox();
                closeBox()
            end

        | ppl (FIX(varlist,ltylist,lexplist,lexp)) =
            let fun flist([v],[t],[l]) =
                      let val lv = lvarName v
                          val len = size lv + 2
                       in pps lv; pps ": "; ppLty' t; pps " :: ";
                          ppl l
                      end
                  | flist(v::vs,t::ts,l::ls) =
                      let val lv = lvarName v
                          val len = size lv + 2
                       in pps lv; pps ": "; ppLty' t; pps " :: ";
                          ppl l; newline();
                          flist(vs,ts,ls)
                      end
                  | flist(nil,nil,nil) = ()
                  | flist _ = bug "unexpected cases in flist"

             in openHOVBox 0;
                pps "FIX(";
                openHVBox 0; flist(varlist,ltylist,lexplist); closeBox();
                newline(); pps "IN ";
                ppl lexp;
                pps ")";
                closeBox()
            end

        | ppl (RAISE(l,t)) =
            (openHOVBox 0;
              pps "RAISE(";
              openHVBox 0;
               ppLty t'; pps ","; br1 0; ppl l;
              closeBox();
              pps ")";
             closeBox())

        | ppl (HANDLE (lexp,withlexp)) =
            (openHOVBox 0;
             pps "HANDLE"; br1 0; ppl lexp;
             newline();
             pps "WITH"; br1 0; ppl withlexp;
             closeBox())

        | ppl (WRAP(t, _, l)) =
            (openHOVBox 0;
              pps "WRAP("; ppTyc' t; pps ",";
              newline();
              ppl l;
              pps ")";
             closeBox())

        | ppl (UNWRAP(t, _, l)) =
            (openHOVBox 0;
              pps "UNWRAP("; ppTyc' t; pps ",";
              newline();
              ppl l;
              pps ")";
             closeBox())

   in ppl l; newline(); newline()
  end

fun printMatch env ((p,r)::more) =
    let val pd = !Control.Print.printDepth)
    in (PP.with_pp (ErrorMsg.defaultConsumer())
         (fn ppstrm =>
          (PPAbsyn.ppPat env ppstrm (p,pd);
           PP.newline ppstrm));
         pps " => "; ppLexp pd ppstrm r; printMatch env more)
  | printMatch _ [] = ()

fun ppFun ppstrm l v =
  let fun last (DA.LVAR x) = x
        | last (DA.PATH(r,_)) = last r
        | last _ = bug "unexpected access in last"

      fun find le =
        case le
          of VAR w =>
               if (v=w)
               then (pps("VAR " ^ lvarName v ^ " is free in <lexp>\n");())
               else ()
           | l as FN(w,_,b) => if v=w then ppLexp 20 ppstrm l else find b
           | l as FIX(vl,_,ll,b) =>
             if List.exists (fn w => v=w) vl then ppLexp 20 ppstrm l
             else (app find ll; find b)
           | APP(l,r) => (find l; find r)
           | LET(w,l,r) => (if v=w then ppLexp 20 ppstrm l else find l; find r)
           | PACK(_,_,_,r) => find r
           | TFN(_, r) => find r
           | TAPP(l, _) => find l
           | SWITCH (l,_,ls,d) =>
             (find l; app (fn(_,l) => find l) ls;
              case d of NONE => () | SOME l => find l)
           | RECORD l => app find l
           | SRECORD l => app find l
           | VECTOR (l, t) => app find l
           | SELECT(_,l) => find l
           | CON((_, DA.EXN p, _), _, e) => (find(VAR(last p)); find e)
           | CON(_,_,e) => find e
(*
         | DECON((_, DA.EXN p, _), _, e) => (find(VAR(last p)); find e)
         | DECON(_,_,e) => find e
*)
           | HANDLE(e,h) => (find e; find h)
           | RAISE(l,_) => find l
           | INT _ => () | WORD _ => ()
           | INT32 _ => () | WORD32 _ => ()
           | STRING _ => () | REAL _ => ()
           | ETAG (e,_) => find e
           | PRIM _ => ()
           | GENOP ({default=e1,table=es}, _, _, _) =>
             (find e1; app (fn (_, x) => find x) es)
           | WRAP(_, _, e) => find e
           | UNWRAP(_, _, e) => find e

   in find l
  end

fun stringTag (VAR _) = "VAR"
  | stringTag (INT _) = "INT"
  | stringTag (INT32 _) = "INT32"
  | stringTag (WORD _) = "WORD"
  | stringTag (WORD32 _) = "WORD32"
  | stringTag (REAL _) = "REAL"
  | stringTag (STRING _) = "STRING"
  | stringTag (PRIM _) = "PRIM"
  | stringTag (GENOP _) = "GENOP"
  | stringTag (FN _) = "FN"
  | stringTag (FIX _) = "FIX"
  | stringTag (APP _) = "APP"
  | stringTag (LET _) = "LET"
  | stringTag (TFN _) = "TFN"
  | stringTag (TAPP _) = "TAPP"
  | stringTag (ETAG _) = "ETAG"
  | stringTag (RAISE _) = "RAISE"
  | stringTag (HANDLE _) = "HANDLE"
  | stringTag (CON _) = "CON"
  | stringTag (SWITCH _) = "SWITCH"
  | stringTag (VECTOR _) = "VECTOR"
  | stringTag (RECORD _) = "RECORD"
  | stringTag (SRECORD _) = "SRECORD"
  | stringTag (SELECT _) = "SELECT"
  | stringTag (PACK _) = "PACK"
  | stringTag (WRAP _) = "WRAP"
  | stringTag (UNWRAP _) = "UNWRAP"

end (* toplevel local *)
end (* struct PPLexp *)
