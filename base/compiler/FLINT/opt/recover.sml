(* recover.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* recover the type information of a closed FLINT program *)
signature RECOVER =
sig
  val recover : (FLINT.prog * bool) ->
                  {getLty: FLINT.value -> FLINT.lty,
                   cleanUp: unit -> unit,
		   addLty: (FLINT.lvar * FLINT.lty) -> unit}
end (* signature RECOVER *)

structure Recover : RECOVER =
struct

local structure LT = LtyExtern
      structure DI = DebIndex
      open FLINT
in

fun bug s = ErrorMsg.impossible ("Recover: "^s)

fun ltInst (lt, ts) =
  (case LT.lt_inst(lt, ts)
    of [x] => x
     | _ => bug "unexpected case in ltInst")

(** these two functions are applicable to the types of primops and data
    constructors only (ZHONG) *)
fun arglty (lt, ts) =
  let val (_, atys, _) = LT.ltd_arrow(ltInst(lt, ts))
   in case atys of [x] => x
                 | _ => bug "unexpected case in arglty"
  end
fun reslty (lt, ts) =
  let val (_, _, rtys) = LT.ltd_arrow(ltInst(lt, ts))
   in case rtys of [x] => x
                 | _ => bug "unexpected case in reslty"
  end

exception RecoverLty
fun recover (fdec, postRep) =
  let val ltyTable : lty LambdaVar.Tbl.hash_table =
	  LambdaVar.Tbl.mkTable(32, RecoverLty)
      fun get v = LambdaVar.Tbl.lookup ltyTable v
	    handle RecoverTy => (
	      print (concat["Recover.get: ", LambdaVar.prLvar v, "\n"]);
	      bug "Recover.recover.get")
      val addv = LambdaVar.Tbl.insert ltyTable
      fun addvs vts = app addv vts
      fun getlty (VAR v) = get v
        | getlty (INT{ty, ...}) = LT.ltc_num ty
        | getlty (WORD{ty, ...}) = LT.ltc_num ty
        | getlty (REAL _) = LT.ltc_real
        | getlty (STRING _) = LT.ltc_string

      val lt_nvar_cvt = LT.lt_nvar_cvt_gen()

      (* loop : depth -> lexp -> lty list *)
      fun loop e =
        let fun lpv u = getlty u
            fun lpvs vs = map lpv vs

            fun lpd (fk, f, vts, e) =
              (addvs vts; addv (f, LT.ltc_fkfun(fk, map #2 vts, lpe e)))
            and lpds (fds as ((fk as {isrec=SOME _, ...},_,_,_)::_)) =
                  let fun h ((fk as {isrec=SOME (rts,_), ...},
                             f, vts, _) : fundec) =
                            addv(f, LT.ltc_fkfun(fk, map #2 vts, rts))
                        | h _ = bug "unexpected case in lpds"
                      val _ = app h fds
                   in app lpd fds
                  end
              | lpds [fd] = lpd fd
              | lpds _ = bug "unexpected case 2 in lpds"

            and lpc (DATAcon((_,_,lt), ts, v), e) =
                  (addv (v, arglty(lt, ts)); lpe e)
              | lpc (_, e) = lpe e

            and lpe (RET vs) = lpvs vs
              | lpe (LET(vs, e1, e2)) =
                  (addvs (ListPair.zip(vs, lpe e1)); lpe e2)
              | lpe (FIX(fdecs, e)) = (lpds fdecs; lpe e)
              | lpe (APP(u, vs)) =
		  let val u' = lpv u
		  in (#2(LT.ltd_fkfun u')
		      handle LT.DeconExn =>
		       (print "\nError Application:\n";
			PPFlint.printLexp (APP(u, vs));
			raise LT.DeconExn))
		  end
              | lpe (TFN((tfk, v, tvks, e1), e2)) =
                  (addv(v, LT.lt_nvpoly(tvks, loop e1));
                   lpe e2)
              | lpe (TAPP(v, ts)) = LT.lt_inst (lpv v, ts)
              | lpe (RECORD(rk,vs,v,e)) =
                  (addv (v, LT.ltc_rkind(rk, lpvs vs)); lpe e)
              | lpe (SELECT(u,i,v,e)) =
                  (addv (v, LT.ltd_rkind(lpv u, i)); lpe e)
              | lpe (CON((_,_,lt),ts,_,v,e)) =
                  (addv (v, reslty(lt, ts)); lpe e)
              | lpe (SWITCH(_, _, ces, e)) =
                  let val lts = map lpc ces
                   in case e of NONE => hd lts
                              | SOME e => lpe e
                  end
              | lpe (RAISE (_, lts)) = lts
              | lpe (HANDLE(e, _)) = lpe e
              | lpe (BRANCH(p, _, e1, e2)) =
                  let val _ = lpe e1
                   in lpe e2
                  end
              | lpe (PRIMOP((_,Primop.WCAST, lt, []), _, v, e)) =
                  if postRep then
                     (case LT.ltd_fct lt
                       of ([_],[r]) => (addv(v, r); lpe e)
                        | _ => bug "unexpected case for WCAST")
                  else bug "unexpected primop WCAST in recover"
              | lpe (PRIMOP((_,_,lt,ts), _, v, e)) =
                  (addv (v, reslty (lt, ts)); lpe e)

         in lpe e handle LT.DeconExn => (print "\nWhole Expr:\n";
					 PPFlint.printLexp e; bug "ltd decon")
        end (* function transform *)

      val (fkind, f, vts, e) = fdec
      val _ = addvs vts
      val atys = map #2 vts
      (* val _ = PPFlint.printLexp e *)
      val rtys = loop e
      val _ = addv (f, LT.ltc_fkfun(fkind, atys, rtys))
  in {getLty=getlty, cleanUp=fn () => LambdaVar.Tbl.clear ltyTable, addLty=addv}
 end (* function recover *)

end (* local *)
end (* structure Recover *)
