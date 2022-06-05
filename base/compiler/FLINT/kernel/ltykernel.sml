(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* ltykernel.sml *)

structure LtyKernel :> LTYKERNEL =
struct

structure PT = PrimTyc
structure DI = DebIndex
open Lty

val debugging : bool ref = ref false
val dp : int ref = ref 20
fun bug s = ErrorMsg.impossible ("LtyKernel:" ^ s)

structure PP = PrettyPrint
structure PU = PPUtil
structure EM = ErrorMsg
open PPLty

val with_pp = PP.with_default_pp

fun dgPrint (msg: string, printfn: PP.stream -> 'a -> unit, arg: 'a) =
  if (!debugging)
  then with_pp
	(fn ppstrm =>
	  (PP.openHVBox ppstrm (PP.Rel 0);
	   PP.string ppstrm msg;
	   PP.newline ppstrm;
	   PP.nbSpace ppstrm 2;
	   PP.openHVBox ppstrm (PP.Rel 0);
	   printfn ppstrm arg;
	   PP.closeBox ppstrm;
	   PP.closeBox ppstrm))
  else ()

exception TCENV

(** utility functions for tc_env and lt_env *)
local

  (* tcc_env_chkd and ltc_env_chkd check for the r2 condition to avoid
   * constructing a closure, and check the invariants that ol and nl
   * are nonnegative are satisfied *)
  fun tcc_env_chkd(x, 0, 0, te) = x
    | tcc_env_chkd(x, ol, nl, te) =
     (* if ol < 0 then (print "tcc_env_chkd: negative ol\n"; raise TCENV)
      else if nl < 0 then (print "tcc_env_chkd: negative nl\n"; raise TCENV)
      else *) tc_injX(TC_ENV(x, ol, nl, te))

  fun ltc_env_chkd(x, 0, 0, te) = x
    | ltc_env_chkd(x, ol, nl, te) =
      (* if ol < 0 then (print "ltc_env_chkd: negative ol\n"; raise TCENV)
      else if nl < 0 then (print "ltc_env_chkd: negative nl\n"; raise TCENV)
      else *) lt_injX(LT_ENV(x, ol, nl, te))

  (* needsClosure : enc_tvar list * int * int * tycenv -> bool
   * checks to see whether any of a list of free variables need
   * the closure. This could be because they are bound in the
   * environment, or because they are subject to a depth adjustment
   * carried by the nl term [formally named withEff] *)
  fun needsClosure ([], ol, nl, tenv) = false
    | needsClosure (a::r, ol, nl, tenv) =
        let val (n, k) = tvDecode a  (* n is the deBruijn index *)
            val neweff =
              if n > ol then (ol <> nl)
              else true
                  (* case teLookup(n, tenv)
                       of SOME(Lamb(nl',_)) => (nl - nl') <> n
                        | SOME(Beta(nl',ts,_)) =>
                             (let val y = List.nth(ts, k)  (what is y???)
                               in (case tc_outX y
                                    of TC_VAR(ni, nj) =>
                                        ((nj <> k) orelse ((ni+nl-nl') <> n))
                                     | _ => true)
                              end) *)
         in neweff orelse (needsClosure(r, ol, nl, tenv))
        end

in

fun tcc_env(x, ol, nl, tenv) =
  (let fun checkTCVAR (tyc,ol,nl,tenv) =  (* GK -- debugging *)
           case (tc_outX tyc)
             of TC_VAR(d,k) =>
                 (case teLookup(tenv,d)
		   of SOME(Beta(_,ts,ks)) =>
                        if k >= length ts
			then (print "tcc_env TC_VAR [Beta]: ";
			      print (Int.toString k);
			      print ", ts length = ";
			      print (Int.toString (length ts));
                              print "\n";
			      bug "Bad TC_ENV TC_VAR [Beta]")
			else ()
                    | SOME(Lamb(_,ks)) =>
                        if k >= length ks
			then (print "tcc_env TC_VAR [Lamb]: ";
			      print (Int.toString k);
			      print ", ks length = ";
			      print (Int.toString (length ks));
                              print "\n";
			      bug "Bad TC_ENV TC_VAR [Lamb]")
			else ()
		    | NONE => (* (print "tcc_env TC_VAR: d out of bounds:\n";
                               print "d = "; print (Int.toString d); print "\n";
                               print "ol = "; print (Int.toString ol); print "\n";
                               print "length(tenv) = ";
                               print (Int.toString(teLength tenv)); print "\n"))
                               *) ())
              | TC_ENV(tc, ol', nl', tenv')  =>
                 ((* print "TC_ENV("; *)
                 checkTCVAR(tc,ol',nl',tenv')
                 (*;print ")\n" *))
              | _ => () (* print ("tcc_env OTHER " ^ tci_print tci ^"\n") *)
   in checkTCVAR(x,ol,nl,tenv);
   (* original body --- *)
   let val tvs = tc_vs x
    in case tvs
        of NONE => tcc_env_chkd(x, ol, nl, tenv)
         | SOME [] => x
         | SOME nvs =>  (* there are known free variables in body *)
             if needsClosure(nvs, ol, nl, tenv)
             then tcc_env_chkd(x, ol, nl, tenv)
             else x
   end
   end)

fun ltc_env(x, ol, nl, tenv) =
  let val tvs = lt_vs x
   in case tvs
       of NONE => ltc_env_chkd(x, ol, nl, tenv)
        | SOME [] => x
        | SOME nvs =>  (* there are known free variables in body *)
            if needsClosure (nvs, ol, nl, tenv)
            then ltc_env_chkd(x, ol, nl, tenv)
            else x
  end

end (* local -- utility functions for lt_env and tc_env *)


(***************************************************************************
 *            UTILITY FUNCTIONS ON REASONING ABOUT REDUCTIONS              *
 ***************************************************************************)

(** a list of constructor functions *)
val tcc_var = tc_injX o TC_VAR
val tcc_fn = tc_injX o TC_FN
val tcc_app = fn (fntyc, argtycs) =>
		 (* Check that parameter arity matches number of arguments
		    supplied because type application must be saturated *)
		 let fun checkParamArity (tc,tcs) =
			 let
			     fun getArity(tycEnv) =
				 (case (tc_outX tycEnv)
				   of TC_PRIM(ptyc) => PT.pt_arity ptyc
				    | TC_FN(params, _) => length params
				    | (TC_APP(tc, _)) =>
				      (case (tc_outX tc)
					of (TC_FN(_, tc')) => getArity tc'
					 | _ => 0)
				    | (TC_FIX{family={size,gen,params,...},index}) =>
				      (case (tc_outX gen)
					of (TC_FN (_,tc')) => (* generator function *)
					   (case (tc_outX tc')
				             of (TC_SEQ tycs) =>
                                                  getArity (List.nth (tycs, index))
					      | TC_FN (args, _) => length args
					      | _ => bug "Malformed generator range")
					 | _ =>
                                           (with_pp(fn s =>
                                              (PU.pps s "ERROR: checkParamArity - FIX";
                                               PP.newline s;
                                               ppTyc (!dp) s gen;
                                               PP.newline s));
                                            bug "FIX without generator!" ))
				    | _ => (with_pp (fn s =>
                                              (PU.pps s "getArity?:";
                                               PP.newline s;
                                               ppTyc (!dp) s tc;
                                               PP.newline s));
                                            0))  (* giving up! *)
			     val arity = getArity tc
			 in
			     if arity = (length tcs) then ()
			     else with_pp(fn s =>
                                    (PU.pps s "TC_APP arity mismatch"; PP.newline s;
                                     PU.pps s "arity: "; PU.ppi s arity; PP.newline s;
                                     PU.pps s "no. arguments: "; PU.ppi s (length tcs);
                                     PP.newline s;
                                     PU.pps s "operator:"; PP.newline s;
                                     ppTyc (!dp) s tc; PP.newline s) )
			 end
		 in
		     ((* checkParamArity(fntyc, argtycs); *)
		      (tc_injX o TC_APP) (fntyc, argtycs))
		 end
val tcc_seq = tc_injX o TC_SEQ
val tcc_proj = tc_injX o TC_PROJ
val tcc_fix =
    fn ((size:int,names: string vector,gen: tyc,params: tyc list),index:int) =>
       tc_injX(TC_FIX{family={size=size,names=names,gen=gen,params=params},index=index})
val tcc_abs = tc_injX o TC_ABS
val tcc_tup  = tc_injX o TC_TUPLE
val tcc_parw = tc_injX o TC_PARROW
val tcc_box = tc_injX o TC_BOX
val tcc_real = tc_injX (TC_PRIM PT.ptc_real)
val ltc_tyc = lt_injX o LT_TYC
val ltc_str = lt_injX o LT_STR
val ltc_fct = lt_injX o LT_FCT
val ltc_poly = lt_injX o LT_POLY
val tcc_sum = tc_injX o TC_SUM
val tcc_token = tc_injX o TC_TOKEN

(* The following functions decide on how to flatten the arguments
 * and results of an arbitrary FLINT function. The current threshold
 * is maintained by the "flatten_limit" parameter. This parameter
 * is designed as architecture independent, however, some implicit
 * constraints are:
 *     (1) flatten_limit <= numgpregs - numcalleesaves - 3
 *     (2) flatten_limit <= numfpregs - 2
 * Right now (2) is in general not true for x86; we inserted a
 * special hack at cpstrans phase to deal with this case. In the
 * long term, if the spilling phase in the backend can offer more
 * supports on large-number of arguments, then we can make this
 * flattening more aggressive. (ZHONG)
 *)
val flatten_limit = 9

(* teUnbound2 -- raised when second index of a deBruijn index pair is
 * out of bounds *)
exception teUnbound2

fun isKnown tc =
  (case tc_outX(tc_whnm tc)
    of (TC_PRIM _ | TC_ARROW _ | TC_BOX _ | TC_ABS _ | TC_PARROW _) => true
     | (TC_CONT _ | TC_FIX _ | TC_SUM _ | TC_TUPLE _) => true
     | TC_APP(tc, _) => isKnown tc
     | TC_PROJ(tc, _) => isKnown tc
     | TC_TOKEN(k, x) => token_isKnown(k, x)
     | _ => false)

and tc_autoflat tc =
  let val ntc = tc_whnm tc
   in (case tc_outX ntc
        of TC_TUPLE (_, [_]) => (* singleton record is not flattened to ensure
                              isomorphism btw plambdatype and flinttype *)
             (true, [ntc], false)
         | TC_TUPLE (_, []) =>  (* unit is not flattened to avoid coercions *)
             (true, [ntc], false)
         | TC_TUPLE (_, ts) =>
             if length ts <= flatten_limit then (true, ts, true)
             else (true, [ntc], false)  (* ZHONG added the magic number 10 *)
         | _ => if isKnown ntc then (true, [ntc], false)
                else (false, [ntc], false))
  end

and tc_autotuple [x] = x
  | tc_autotuple xs =
       if length xs <= flatten_limit then tcc_tup (RF_TMP, xs)
       else bug "fatal error with tc_autotuple"

and tcs_autoflat (flag, ts) =
  if flag then (flag, ts)
  else (case ts
         of [tc] => (let val ntc = tc_whnm tc
                         val (nraw, ntcs, _) = tc_autoflat ntc
                      in (nraw, ntcs)
                     end)
          | _ => bug "unexpected cooked multiples in tcs_autoflat")

and lt_autoflat lt =
  (case lt_outX(lt_whnm lt)
    of LT_TYC tc =>
         let val (raw, ts, flag) = tc_autoflat tc
          in (raw, map ltc_tyc ts, flag)
         end
     | _ => (true, [lt], false))

(** a special version of tcc_arw that does automatic flattening *)
and tcc_arw (x as (FF_FIXED, _, _)) = tc_injX (TC_ARROW x)
  | tcc_arw (x as (FF_VAR (true, true), _, _)) = tc_injX (TC_ARROW x)
  | tcc_arw (b as (FF_VAR (b1, b2)), ts1, ts2) =
      let val (nb1, nts1) = tcs_autoflat (b1, ts1)
          val (nb2, nts2) = tcs_autoflat (b2, ts2)
       in tc_injX (TC_ARROW(FF_VAR(nb1, nb2),  nts1, nts2))
      end

(** utility function to read the top-level of a tyc *)
and tc_lzrd(t: tyc) =
  let fun g x =
            (case tc_outX x
              of TC_IND (tc, _) => g tc
               | TC_ENV (tc, ol, nl, te) =>
                   let val ntc = g(h(tc, ol, nl, te))
                    in tyc_upd(x, ntc); ntc
                   end
               | _ => x)

      (* [KM ???] claim: h will not return a TC_IND nor a TC_ENV *)
      and h (x, 0, 0, _) = g x  (* [KM ???] redundant call to g here? *)
        | h (x, ol, nl, tenv) =
            let fun prop z = tcc_env(z, ol, nl, tenv)
		             handle TCENV =>
                               (with_pp(fn s =>
                                 (PU.pps s "tc_lzrd.prop:"; PP.newline s;
                                  ppTyc (!dp) s z; PP.newline s));
                                bug "tc_lzrd prop")
             in (case tc_outX x
                  of TC_VAR (n,k) =>
                       if (n <= ol) then  (* n is bound in tenv *)
                         (case teLookup(tenv, n)
                           of NONE => bug "tc_lzrd: short tenv"
                            | SOME(Lamb(nl',ks)) =>   (* rule r5 *)
                               (* ASSERT: nl > nl' *)
                                if nl' >= nl then
                                  (print ("ERROR: tc_lzrd (r5): nl ="^
                                          Int.toString nl ^ ", nl' = " ^
                                          Int.toString nl' ^ "\n");
                                   bug "tc_lzrd - nl' > nl")
                                else tcc_var(nl - nl', k)
                            | SOME(Beta(nl',ts,ks)) =>  (* rule r6 *)
                                let val y = List.nth(ts, k)
                                            handle Subscript =>
                                            (* kind/arity error! *)
                    (with_pp(fn s =>
                       let val {break,newline,openHVBox,openHOVBox,openVBox,
				closeBox, pps, ppi} = PU.en_pp s
                       in openHVBox 0;
                          pps "***Debugging***"; break{nsp=1,offset=0};
                          pps "tc_lzrd arg:"; newline();
                          PPLty.ppTyc (!dp) s t; break{nsp=1,offset=0};
		          pps "n = "; ppi n; pps ", k = "; ppi k;
			  newline();
                          pps "length(ts) = : "; ppi (length ts);
			  newline();
                          pps "ts elements: "; break{nsp=2,offset=2};
                          openHOVBox 2;
                          ppList s {sep=",",pp=ppTyc (!dp)} ts;
                          closeBox ();
                          closeBox ()
			end);
			raise teUnbound2)
                                 in (* ASSERT: nl >= nl' *)
                                    if nl' > nl then
                                        (print ("ERROR: tc_lzrd (r6): nl ="^
                                               Int.toString nl ^ ", nl' = " ^
                                               Int.toString nl' ^ "\n");
                                         bug "tc_lzrd - nl' > nl")
                                    else h(y, 0, nl - nl', teEmpty)  (* rule r6 *)
                                 end)
                       else tcc_var(n-ol+nl, k) (* rule r4 *)
                   | TC_NVAR _ => x
                   | TC_PRIM _ => x    (* rule r7 *)
                   | TC_FN (ks, tc) =>
                       let val tenv' = teCons(Lamb(nl,ks),tenv)
                        in tcc_fn(ks,
				  tcc_env(tc, ol+1, nl+1, tenv')  (* rule r10 *)
				  handle TCENV => bug "tc_lzrd TC_FN")
                       end
                   | TC_APP (tc, tcs) =>
                       tcc_app(prop tc, map prop tcs) (* rule r9 *)
                   | TC_SEQ tcs => tcc_seq (map prop tcs)
                   | TC_PROJ (tc, i) => tcc_proj(prop tc, i)
                   | TC_SUM tcs => tcc_sum (map prop tcs)
                   | TC_FIX{family={size,names,gen,params},index} =>
                        tcc_fix((size, names, prop gen, map prop params), index)
                   | TC_ABS tc => tcc_abs (prop tc)
                   | TC_BOX tc => tcc_box (prop tc)
                   | TC_TUPLE (rk, tcs) => tcc_tup (rk, map prop tcs)
                   | TC_ARROW (r, ts1, ts2) =>
                       tcc_arw (r, map prop ts1, map prop ts2)  (* rule r8 *)
                   | TC_PARROW (t1, t2) => tcc_parw (prop t1, prop t2)
                   | TC_TOKEN (k, t) => tcc_token(k, prop t)
                   | TC_CONT _ => bug "unexpected TC_CONT in tc_lzrd"
                   | TC_IND (tc, _) => h(tc, ol, nl, tenv)
                   | TC_ENV(tc, ol', nl', tenv') =>
                       if ol = 0 then h(tc, ol', nl+nl', tenv')  (* rule r11 *)
                       else h(g x, ol, nl, tenv))
            end (* function h *)
   in if tcp_norm(t) then t else g t
  end (* function tc_lzrd *)

(** utility function to read the top-level of an lty *)
and lt_lzrd t =
  let fun g x =
           (case lt_outX x
             of LT_IND (lt, _) => g lt
              | LT_ENV(lt, i, j, te) =>
                  let val nlt = g(h(lt, i, j, te))
                   in lty_upd(x, nlt); nlt
                  end
              | _ => x)

      and h (x, 0, 0, _) = g x
        | h (x, ol, nl, tenv) =
            let fun prop z = ltc_env(z, ol, nl, tenv)
             in (case lt_outX x
                  of LT_TYC tc => ltc_tyc (tcc_env(tc, ol, nl, tenv)
					   handle TCENV =>
						  bug "lt_lzrd LT_TYC")
                   | LT_STR ts => ltc_str (map prop ts)
                   | LT_FCT (ts1, ts2) => ltc_fct(map prop ts1, map prop ts2)
                   | LT_POLY (ks, ts) =>
                       let val tenv' = teCons(Lamb(nl,ks), tenv)
                        in ltc_poly(ks,
                             map (fn t => ltc_env(t, ol+1, nl+1, tenv')) ts)
                       end
                   | LT_CONT _ => bug "unexpected LT_CONT in lt_lzrd"
                   | LT_IND (t, _) => h(t, ol, nl, tenv)
                   | LT_ENV (lt, ol', nl', tenv') =>
                       if ol = 0 then h(lt, ol', nl+nl', tenv')
                       else h(g x, ol, nl, tenv))
            end (* function h *)
   in if ltp_norm(t) then t else g t
  end (* function lt_lzrd *)

(** taking out the TC_IND indirection *)
and stripInd t = (case tc_outX t of TC_IND (x,_) => stripInd x | _ => t)

(*
and printParamArgs (tc,tcs) =
    let fun getArity(tycEnv) =
	    (case (tc_outX tycEnv) of
		 TC_PRIM(ptyc) => PT.pt_arity ptyc
	       | TC_FN(params, _) => length params
	       | (TC_APP(tc, _)) =>
		 (case (tc_outX tc)
		   of (TC_FN(_, tc')) => getArity tc'
		    | _ => 0)
	       | (TC_FIX{family={size,gen,params,...},index}) =>
		 (case (tc_outX gen)
		   of (TC_FN (_,tc')) => (* generator function *)
		      (case (tc_outX tc')
			of (TC_SEQ tycs) => getArity (List.nth (tycs, index))
			 | TC_FN (args, _) => length args
			 | _ => bug "Malformed generator range")
		    | _ => bug "FIX without generator!" )
	       | _ => (with_pp (fn s => (PP.openHOVBox s (PP.Rel 2);
                                         PU.pps s "getArity on:";
                                         ppTyc (!dp) s tc; PP.newline s;
                                         PP.closeBox s));
                       0))
	val arity = getArity tc
    in
	if arity = (length tcs) then ()
	else with_pp(fn s =>
                        (PU.pps s "(TC_APP oper:"; PP.break s {nsp=1,offset=2};
                         ppTyc (!dp) s tc; PP.newline s;
                         PU.pps s "arity:"; PP.break s {nsp=1,offset=0};
		         PU.ppi s arity; PP.newline s;
		         PU.pps s "no. arguments:"; PP.break s {nsp=1,offset=0};
		         PU.ppi s (length tcs); PP.newline s))
    end
 *)

(** normalizing an arbitrary tyc into a simple weak-head-normal-form *)
and tc_whnm t = if tcp_norm(t) then t else
  let (* val _ = print ">>tc_whnm not norm\n" *)
      val nt = tc_lzrd t
   in case (tc_outX nt)
       of TC_APP(tc, tcs) =>
	    ((* print "\ntc_whnm: TC_APP\n"; *)
             let val tc' = tc_whnm tc
                           handle exn =>
                             (print "TC_APP in tc_whnm 1\n"; raise exn)
              in case (tc_outX tc')
                  of TC_FN(ks, b) =>
                       let fun base () =
                             (b, 1, 0, teCons(Beta(0,tcs,ks),teEmpty))  (* r1 *)
                           val sp =
                             (case tc_outX b
                               of TC_ENV(b', ol', nl', te') =>
                                    (case teDest te'
                                      of SOME(Lamb(n,ks'), te'') =>
                                           if (n = nl'-1) andalso (ol' > 0)
                                           then (* r12 *)
                                             (b', ol', n, teCons(Beta(n,tcs,ks),te''))
                                             (* dbm: ks and ks' should be the same *)
                                           else base()
                                       | _ => base())
                                | _ => base())
                           val res = tc_whnm(tcc_env sp)
			             handle exn =>
                                       (print "TC_APP in tc_whnm 2\n";
                                        raise exn)
                        in tyc_upd(nt, res); res
                       end
                   | ((TC_SEQ _) | (TC_TUPLE _) | (TC_ARROW _) | (TC_IND _)) =>
                       bug "unexpected tycs in tc_whnm-TC_APP"
                   | _ => stripInd(tcc_app(tc', tcs))
             end)
        | TC_PROJ(tc, i) =>
	    ((* print "\ntc_whnm: TC_PROJ\n"; *)
	     let val tc' = tc_whnm tc
              in case (tc_outX tc')
                   of (TC_SEQ tcs) =>
                        let val res = List.nth(tcs, i)
                                      handle Subscript => bug "TC_SEQ in tc_whnm"
                            val nres = tc_whnm res
                         in tyc_upd(nt, nres); nres
                        end
                    | ((TC_PRIM _) | (TC_NVAR _) | (TC_FIX _) | (TC_FN _) |
                       (TC_SUM _) | (TC_ARROW _) | (TC_ABS _) | (TC_BOX _) |
                       (TC_IND _) | (TC_TUPLE _)) =>
                         bug "unexpected tycs in tc_whnm-TC_PROJ"
                    | _ => stripInd(tcc_proj(tc', i))
             end)
        | TC_TOKEN(k, tc)  =>
	    ((* print "\ntc_whnm: TC_TOKEN\n"; *)
	    (let val tc' = tc_whnm tc
              in if token_whnm k tc'
                 then let val xx = tcc_token(k, tc') in stripInd xx end
                 else let val res = token_reduce(k, tc')
                          val nres = tc_whnm res
                       in tyc_upd(nt, nres); nres
                      end
             end))
        | TC_IND (tc, _) => ((*print "\ntc_whnm: TC_IND\n"; *) tc_whnm tc)
        | TC_ENV _ => bug "unexpected TC_ENV in tc_whnm"
        | _ => ((* print "\ntc_whnm: OTHER\n"; *) nt)
  end (* function tc_whnm *)

(** normalizing an arbitrary lty into the simple weak-head-normal-form *)
and lt_whnm t =
    if ltp_norm(t) then t
    else let val nt = lt_lzrd t
         in case (lt_outX nt)
             of LT_TYC tc => ltc_tyc(tc_whnm tc)
              | _ => nt
         end (* function lt_whnm *)

(** normalizing an arbitrary tyc into the standard normal form *)
fun tc_norm t = if (tcp_norm t) then t else
  let val nt = tc_whnm t
   in if (tcp_norm nt) then nt
      else
        (let val res =
              (case (tc_outX nt)
                of TC_FN (ks, tc) => tcc_fn(ks, tc_norm tc)
                 | TC_APP (tc, tcs) => tcc_app(tc_norm tc, map tc_norm tcs)
                 | TC_SEQ tcs => tcc_seq(map tc_norm tcs)
                 | TC_PROJ (tc, i) => tcc_proj(tc_norm tc, i)
                 | TC_SUM tcs => tcc_sum (map tc_norm tcs)
                 | TC_FIX{family={size,names,gen,params},index} =>
                     tcc_fix((size,names,tc_norm gen,map tc_norm params),index)
                 | TC_ABS tc => tcc_abs(tc_norm tc)
                 | TC_BOX tc => tcc_box(tc_norm tc)
                 | TC_TUPLE (rk, tcs) => tcc_tup(rk, map tc_norm tcs)
                 | TC_ARROW (r, ts1, ts2) =>
                     tcc_arw(r, map tc_norm ts1, map tc_norm ts2)
                 | TC_PARROW (t1, t2) => tcc_parw(tc_norm t1, tc_norm t2)
                 | TC_TOKEN (k, t) => tcc_token(k, tc_norm t)
                 | TC_IND (tc, _) => tc_norm tc
                 | TC_ENV _ => bug "unexpected tycs in tc_norm"
                 | _ => nt)
          in tyc_upd(nt, res); res
         end)
  end (* function tc_norm *)

(** normalizing an arbitrary lty into the standard normal form *)
fun lt_norm t = if (ltp_norm t) then t else
  let val nt = lt_lzrd t
   in if (ltp_norm nt) then nt
      else
        (let val res =
              (case lt_outX nt
                of LT_TYC tc => ltc_tyc(tc_norm tc)
                 | LT_STR ts => ltc_str(map lt_norm ts)
                 | LT_FCT (ts1, ts2) =>
                     ltc_fct(map lt_norm ts1, map lt_norm ts2)
                 | LT_POLY (ks, ts) => ltc_poly (ks, map lt_norm ts)
                 | LT_IND (lt, _) => lt_norm lt
                 | _ => bug "unexpected ltys in lt_norm")
          in lty_upd(nt, res); res
         end)
  end (* function lt_norm *)

(***************************************************************************
 *         REGISTER A NEW TOKEN TYC --- TC_WRAP                            *
 ***************************************************************************)

(** we add a new constructor named TC_WRAP through the token facility *)
local val name = "TC_WRAP"
      val abbrev = "WR"
      val is_known = fn _ => true      (* why is this ? *)
      fun tcc_tok k t = tcc_token(k, t)

      fun unknown tc =
        (case tc_outX tc
          of (TC_VAR _ | TC_NVAR _) => true
           | (TC_APP(tc, _)) => unknown tc
           | (TC_PROJ(tc, _)) => unknown tc
           | _ => false)

      fun flex_tuple ts =
        let fun hhh(x::r, ukn, wfree) =
                 let fun iswp tc =
                       (case tc_outX tc
                         of TC_TOKEN(k', t) => (* WARNING: need check k' *)
                              (case tc_outX t
                                of TC_PRIM pt => false
                                 | _ => true)
                          | _ => true)
                  in hhh(r, (unknown x) orelse ukn, (iswp x) andalso wfree)
                 end
              | hhh([], ukn, wfree) = ukn andalso wfree
         in hhh(ts, false, true)
        end

      fun is_whnm tc =
        (case tc_outX tc
          of (TC_ARROW(FF_FIXED, [t], _)) => (unknown t)
           | (TC_TUPLE(rf, ts)) => flex_tuple ts
           | (TC_PRIM pt) => PT.unboxed pt
           | _ => false)

      (* invariants: tc itself is in whnm but is_whnm tc = false *)
      fun reduce_one (k, tc) =
        (case tc_outX tc
          of TC_TUPLE (rk, ts) =>
               let fun hhh (x::r, nts, ukn) =
                         let val nx = tc_whnm x
                             val b1 = unknown nx
                             val nnx =
                               (case tc_outX nx
                                 of TC_TOKEN(k', t) =>
                                      if token_eq(k, k') then
                                        (case tc_outX t
                                          of TC_PRIM _ => t
                                           | _ => nx)
                                      else nx
                                  | _ => nx)
                          in hhh(r, nnx::nts, b1 orelse ukn)
                         end
                     | hhh ([], nts, ukn) =
                         let val nt = tcc_tup(rk, rev nts)
                          in if ukn then tcc_token(k, nt) else nt
                         end
                in hhh(ts, [], false)
               end
           | TC_ARROW (FF_FIXED, [_,_], [_]) => tc
           | TC_ARROW (FF_FIXED, [t1], ts2 as [_]) =>
               let val nt1 = tc_whnm t1
                   fun ggg z =
                     let val nz = tc_whnm z
                      in (case tc_outX nz
                           of TC_PRIM pt =>
                                if PT.unboxed pt then tcc_token(k, nz)
                                else nz
                            | _ => nz)
                     end
                   val (wp, nts1) =
                     (case tc_outX nt1
                       of TC_TUPLE(_, [x,y]) => (false, [ggg x, ggg y])
                        | TC_TOKEN(k', x) =>
                            if token_eq(k, k') then
                              (case (tc_outX x)
                                of TC_TUPLE(_, [y, z]) =>
                                    (false, [ggg y, ggg z])
                                 | _ => (false, [nt1]))
                            else (false, [nt1])
                        | _ => (unknown nt1, [nt1]))
                   val nt = tcc_arw(FF_FIXED, nts1, ts2)
                in if wp then tcc_token(k, nt) else nt
               end
           | TC_ARROW (FF_FIXED, _, _) =>
               bug "unexpected reduce_one on ill-formed FF_FIX arrow types"
           | TC_ARROW (FF_VAR(b1,b2), ts1, ts2) =>
               bug "calling reduce_one on FF_VAR arrow types"
           | TC_PRIM pt =>
               if PT.unboxed pt then
                 bug "calling reduce_one on an already-reduced whnm"
               else tc
           | TC_TOKEN(k', t) =>
               if token_eq(k, k') then tc
               else bug "unexpected token in reduce_one"
           | (TC_BOX _ | TC_ABS _ | TC_PARROW _) =>
               bug "unexpected tc_box/abs/parrow in reduce_one"
           | TC_ENV _ => bug "unexpected TC_ENV in reduce_one"
           | TC_IND _ => bug "unexpected TC_IND in reduce_one"
           | _ => tc)

in

val wrap_token =
  register_token {name=name, abbrev=abbrev, reduce_one=reduce_one,
                  is_whnm=is_whnm, is_known=is_known}

end (* end of creating the wrap token for "tcc_rbox" *)

(** testing if a tyc is a unknown constructor *)
fun tc_unknown tc = not (isKnown tc)

(***************************************************************************
 *         REBINDING THE INJECTION AND PROJECTION FUNCTIONS                *
 ***************************************************************************)
(** converting from the standard reps to the hash-consing reps *)
val tk_inj = tk_injX
val tc_inj = tc_injX
val lt_inj = lt_injX

(** converting from the hash-consing reps to the standard reps *)
val tk_out = tk_outX
val tc_out = tc_outX o tc_whnm
val lt_out = lt_outX o lt_whnm

(***************************************************************************
 *         UTILITY FUNCTIONS ON TESTING EQUIVALENCE                        *
 ***************************************************************************)

(** testing the equality of values of tkind, tyc, lty *)
fun eqlist p (x::xs, y::ys) = (p(x,y)) andalso (eqlist p (xs, ys))
  | eqlist p ([], []) = true
  | eqlist _ _ = false

(** testing the equivalence for arbitrary tkinds, tycs and ltys *)
val tk_eqv = tk_eq       (* all tkinds are normalized *)

local (* tyc equivalence utilities *)
(* The efficiency of checking FIX equivalence could probably be
 * improved somewhat, but it doesn't seem so bad for my purposes right
 * now.  Anyway, somebody might eventually want to do some profiling
 * and improve this.  --league, 24 March 1998
 *)

(* Profiling code, temporary?? *)
structure Click =
struct
    local
        val s_unroll = Stats.makeStat "FIX unrolls"
    in
        fun unroll() = Stats.addStat s_unroll 1
    end
end (* Click *)

(* In order to check equality of two FIXes, we need to be able to
 * unroll them once, and check equality on the unrolled version, with
 * an inductive assumption that they ARE equal.  The following code
 * supports making and checking these inductive assumptions.
 * Furthermore, we need to avoid unrolling any FIX more than once.
 *)
(* DBM: this is wrong. Because datatypes are generative, we should
 * not use unrolling when testing type equivalence of data types. All
 * we need to do is test equivalence of generators, params, and also
 * check equality of sizes and indexes. *)

(* testing if two recursive datatypes are equivalent *)
fun eq_fix (t1, t2) =
    (case (tc_outX t1, tc_outX t2)
      of (TC_FIX{family={size=n1,gen=gen1,params=par1,...},index=i1},
          TC_FIX{family={size=n2,gen=gen2,params=par2,...},index=i2}) =>
          (n1 = n2 andalso i1 = i2 andalso
           tc_eqv (gen1, gen2) andalso
           eqlist tc_eqv (par1, par2))
       | _ => bug "unexpected types in eq_fix")


(* tc_eqv_generator, invariant: t1 and t2 are in the wh-normal form
 *     eqop1 is the default equality to be used for tycs
 *     eqop2 is used for body of FN, arguments in APP,
 *     eqop3 is used for ABS and BOX.
 *     eqop4 is used for arrow arguments and results
 * Each of these first takes the set of hypotheses.
 *)
and tc_eqv_gen (t1, t2) =
    case (tc_outX t1, tc_outX t2)
     of (TC_FIX _, TC_FIX _) =>
        eq_fix (t1, t2)
      | (TC_FN(ks1, b1), TC_FN(ks2, b2)) =>
        eqlist tk_eqv (ks1, ks2) andalso tc_eqv (b1, b2)
      | (TC_APP(a1, b1), TC_APP(a2, b2)) =>
        tc_eqv (a1, a2) andalso eqlist tc_eqv (b1, b2)
      | (TC_SEQ ts1, TC_SEQ ts2) =>
        eqlist tc_eqv (ts1, ts2)
      | (TC_SUM ts1, TC_SUM ts2) =>
        eqlist tc_eqv (ts1, ts2)
      | (TC_TUPLE (_, ts1), TC_TUPLE (_, ts2)) =>
        eqlist tc_eqv (ts1, ts2)
      | (TC_ABS a, TC_ABS b) =>
        tc_eqv (a, b)
      | (TC_BOX a, TC_BOX b) =>
        tc_eqv (a, b)
      | (TC_TOKEN(k1,t1), TC_TOKEN(k2,t2)) =>
        token_eq(k1,k2) andalso tc_eqv (t1,t2)
      | (TC_PROJ(a1, i1), TC_PROJ(a2, i2)) =>
        i1 = i2 andalso tc_eqv (a1, a2)
      | (TC_ARROW(r1, a1, b1), TC_ARROW(r2, a2, b2)) =>
        r1 = r2 andalso eqlist tc_eqv (a1, a2)
                andalso eqlist tc_eqv (b1, b2)
      | (TC_PARROW(a1, b1), TC_PARROW(a2, b2)) =>
        tc_eqv (a1, a2) andalso tc_eqv (b1, b2)
      | (TC_CONT ts1, TC_CONT ts2) =>
        eqlist tc_eqv (ts1, ts2)
      | (TC_PRIM ptyc1, TC_PRIM ptyc2) =>
        PT.pt_eq(ptyc1,ptyc2)
      | _ => (if !debugging
	      then with_pp(fn s =>
			      (PU.pps s "%%%tc_eqv_gen DEFAULT";
			       PP.newline s))
	      else ();
              false)

(** general equality for tycs *)
and tc_eqv (x, y) =
    let val t1 = tc_whnm x
        val t2 = tc_whnm y
        val res = if tcp_norm t1 andalso tcp_norm t2 then tc_eq (t1, t2)
                  else tc_eqv_gen (t1, t2)
    in res orelse
       (dgPrint ("TC_EQV",
         (fn s => fn (t1,t2) =>
             (PU.pps s "t1:"; PP.newline s; PPLty.ppTyc 10 s t1; PP.newline s;
              PU.pps s "t2:"; PP.newline s; PPLty.ppTyc 10 s t2; PP.newline s;
              PU.pps s "***************************************************";
              PP.newline s)), (t1,t2));
        false)
    end (* tc_eqv *)

in

val tc_eqv = tc_eqv

end (* tyc equivalence utilities *)

(* temporary placeholder implementation of a tyc match function, where
 * t1 is a scheme with variables of sig ks, and t2 is a "ground" tyc
 * being matched against t1 *)
fun tc_match(ks: tkind list, ltys: lty list, argtyc: tyc) = true

(** lt_eqv_generator, invariant: t1 and t2 are in the wh-normal form *)
(** The LT_TCY/LT_POLY case is asymmetric, reflecting the fact that
 ** a polytype in a functor argument (t2) can match a monotype in the
 ** functor parameter signature (t1) **)
fun lt_eqv_gen (t1 : lty, t2: lty) =
    (case (lt_outX t1, lt_outX t2)
      of (LT_POLY(ks1, b1), LT_POLY(ks2, b2)) =>
         (eqlist tk_eqv (ks1, ks2)) andalso (eqlist lt_eqv (b1, b2))
       | (LT_TYC a, LT_TYC b) => tc_eqv(a, b)
       | (LT_TYC a, LT_POLY(ks, b)) => tc_match(ks, b, a)
       | (LT_FCT(as1, bs1), LT_FCT(as2, bs2)) =>
         (eqlist lt_eqv (as1, as2)) andalso (eqlist lt_eqv (bs1, bs2))
       | (LT_STR s1, LT_STR s2) => eqlist lt_eqv (s1, s2)
       | (LT_CONT s1, LT_CONT s2) => eqlist lt_eqv (s1, s2)
       | _ => (dgPrint("LT_CONT",
		       (fn s => fn () => PU.pps s "%%%lt_eqv_gen DEFAULT\n"),
		       ());
	       false))
    (* function lt_eqv_gen *)

and lt_eqv(x : lty, y: lty) : bool =
    let val t1 = lt_whnm x
        val t2 = lt_whnm y
        val res = if (ltp_norm t1) andalso (ltp_norm t2) then lt_eq(t1, t2)
                  else lt_eqv_gen(t1, t2)
    in res (* orelse
       (dgPrint ("LT_EQV",
         (fn s => fn (t1,t2) =>
             (PU.pps s "t1:"; PP.newline s; PPLty.ppLty 10 s t1; PP.newline s;
              PU.pps s "t2:"; PP.newline s; PPLty.ppLty 10 s t2; PP.newline s;
              PU.pps s"***************************************************";
              PP.newline s)), (t1,t2));
        false) *)
    end (* function lt_eqv *)

(*
fun lt_eqv_x(x : lty, y) =
  let val seq = lt_eqv_gen (lt_eqv_x, tc_eqv_x)
   in if ((ltp_norm x) andalso (ltp_norm y)) then
           (lt_eq(x, y)) orelse (seq(x, y))
      else (let val t1 = lt_whnm x
                val t2 = lt_whnm y
             in if (ltp_norm t1) andalso (ltp_norm t2) then
                  (lt_eq(t1, t2)) orelse (seq(t1, t2))
                else seq(t1, t2)
            end)
  end (* function lt_eqv_x *)
*)

(** testing equivalence of fflags and rflags *)
fun ff_eqv (FF_VAR (b1, b2), FF_VAR (b1', b2')) = b1 = b1' andalso b2 = b2'
  | ff_eqv (FF_FIXED, FF_FIXED) = true
  | ff_eqv ((FF_FIXED, FF_VAR _) | (FF_VAR _, FF_FIXED)) = false

fun rf_eqv (RF_TMP, RF_TMP) = true

(***************************************************************************
 *  UTILITY FUNCTIONS ON FINDING OUT THE DEPTH OF THE FREE TYC VARIABLES   *
 ***************************************************************************)
(** finding out the innermost binding depth for a tyc's free variables *)
fun tc_depth (x, d) =
      (* unfortunately we have to reduce everything to normal form
       * before we can talk about its list of free type variables. *)
    (case tc_vs (tc_norm x)
       of NONE => bug "unexpected case in tc_depth"
        | SOME [] => DI.top
        | SOME (a::_) => d + 1 - (#1(tvDecode a)))

fun tcs_depth ([], d) = DI.top
  | tcs_depth (x::r, d) = Int.max(tc_depth(x, d), tcs_depth(r, d))

(* these return the list of free NAMED tyvars, after nomalization *)
fun tc_nvars (tyc:tyc) =
    Lty.tc_nvars(tc_norm tyc)

fun lt_nvars (lty:lty) =
    Lty.lt_nvars(lt_norm lty)

end (* abstraction LtyKernel *)
