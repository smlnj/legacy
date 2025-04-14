(* Elaborator/modules/evalent.sml *)
(* Copyright 1996 by AT&T Bell Laboratories *)
(* Copyright 2025 by The Fellowship of SML/NJ (www.smlnj.org) *)

signature EVALENTITY =
sig

  val evalApp : Modules.fctEntity * Modules.strEntity
                * DebIndex.depth * EntPathContext.context
                * InvPath.path
                -> Modules.strEntity

  val debugging : bool ref

end (* signature EVALENTITY *)

structure EvalEntity : EVALENTITY =
struct

local (* imports *)

  structure SL = SourceLoc
  structure SM = SourceMap
  structure EM = ErrorMsg

  structure SP = Stamp
  structure SS = SpecialSymbols
  structure EP = EntPath
  structure IP = InvPath
 
  structure LV = LambdaVar

  structure T = Types
  structure TU = TypesUtil
 
  structure EE = EntityEnv
  structure EPC = EntPathContext
  structure EU = ElabUtil
  structure ED = ElabDebug

  structure M = Modules
  structure MI = ModuleId
  structure MU = ModuleUtil
  structure I = Instantiate

in

(* usual debugging stuff *)
val say = Control_Print.say
val debugging = ElabDataControl.eedebugging
fun debugmsg (msg: string) =
    if !debugging then (say msg; say "\n") else ()

val debugPrint = (fn x => ED.debugPrint debugging x)  (* Value Restriction *)
fun bug msg = EM.impossible ("EvalEntity: " ^ msg);

(* DBM: should the following three "special symbols" be added to SpecialSymbols? *)
val anonFctSym = Symbol.fctSymbol "AnonFct"
val paramSym = Symbol.strSymbol "<FsigParamInst>"
val anonStrSym = Symbol.strSymbol "<AnonStr>"

val resultId = SS.resultId
val returnId = SS.returnId

fun evalTyc (entv, tycExp, entEnv, epc, rpath) =
      case tycExp
       of M.CONSTtyc tycon => tycon
        | M.FORMtyc (T.GENtyc { kind, arity, eq, path, ... }) =>
	  (case kind of
	       T.DATATYPE{index=0, stamps, freetycs, family, root=NONE, stripped} =>
               let val viztyc = MU.transTycon entEnv
                   val nstamps = Vector.map (fn _ => SP.fresh ()) stamps
                   val nst = Vector.sub(nstamps,0)
                   val nfreetycs = map viztyc freetycs
                   val _ = EPC.bindTycPath (epc, nst, entv)
               in
		   T.GENtyc{stamp=nst, arity=arity, eq=eq,
                            kind=T.DATATYPE{index=0, stamps=nstamps,
					    root=NONE,
					    freetycs=nfreetycs,
					    family=family,
					    stripped=stripped},
                            path=IP.append(rpath,path), stub=NONE}
               end
             | T.DATATYPE{index=i, root=SOME rtev, stripped, ...} =>
               let val (nstamps, nfreetycs, nfamily, nstripped) =
                       case EE.lookTycEnt(entEnv, rtev)
			of T.GENtyc { kind = T.DATATYPE{stamps,freetycs,family,stripped,...}, ... } =>
			   (stamps, freetycs, family, stripped)
			 | _ => bug "unexpected case in evalTyc-FMGENtyc (2)"
                   val nst = Vector.sub(nstamps,i)
                   val _ = EPC.bindTycPath (epc, nst, entv)
               in
		   T.GENtyc{stamp=nst, arity=arity,
                            kind=T.DATATYPE{index=i, stamps=nstamps,
					    root=NONE,
					    freetycs=nfreetycs,
					    family=nfamily,
					    stripped=nstripped},
                            path=IP.append(rpath,path),
			    eq=eq, stub=NONE}
               end
	     | _ => bug "unexpected GENtyc in evalTyc")
        | M.FORMtyc (T.DEFtyc{stamp,tyfun=T.TYFUN{arity, body},strict,path}) =>
          let val newstamp = SP.fresh()
	      (* tycId=stamp (this should perhaps be more abstract some day) *)
	      val _ = EPC.bindTycPath (epc, newstamp, entv)
	      val newbody = MU.transType entEnv body
	      val newstrict = TU.calcStrictness(arity, newbody)
	  in
	      T.DEFtyc{stamp = newstamp,
		       tyfun=T.TYFUN{arity=arity, body=newbody},
		       strict=newstrict, path=IP.append(rpath,path)}
          end
        | M.VARtyc entPath =>
	    (debugmsg (">>evalTyc[VARtyc]: "^EP.entPathToString entPath);
	     EE.lookTycEP(entEnv,entPath))
        | _ => bug "unexpected tycExp in evalTyc"

and evalStr(strExp, depth, epc, entsv, entEnv, rpath) =
     (debugmsg ("[Inside EvalStr ......");
      case strExp
       of M.VARstr entPath =>
	    (debugmsg (">>evalStr[VARstr]: "^EP.entPathToString entPath);
	     (EE.lookStrEP(entEnv,entPath), entEnv))

        | M.CONSTstr strEnt => (strEnt, entEnv)

        | M.STRUCTURE {stamp, entDec} =>
            let val epc = EPC.enterOpen(epc, entsv)
                val stp = evalStp(stamp, depth, epc, entEnv)
                val env = evalDec(entDec, depth, epc, entEnv, rpath)
	     in ({stamp = stp, entities=env,
		  properties = PropList.newHolder (),
		  (*lambdaty=ref NONE,*)
		  rpath = rpath, stub = NONE},
		 entEnv)
            end

        | M.APPLY (fctExp, strExp) =>
	    let val (fctRlzn, entEnv1) =
                    evalFct(fctExp, depth, epc, entEnv)
	        val (argRlzn, entEnv2) =
                    evalStr (strExp, depth, epc, entsv, entEnv1, IP.empty)
                val epc = EPC.enterOpen (epc, entsv)
             in (evalApp (fctRlzn, argRlzn, depth, epc, rpath),
                 entEnv2)
            end

        | M.LETstr (entDec, strExp) =>
            let val entEnv1 = evalDec(entDec, depth, epc, entEnv, rpath)
                val (strEnt, entEnv2) =
                  evalStr(strExp, depth, epc, entsv, entEnv1, rpath)
 	     in (strEnt, entEnv2)
            end

        | M.ABSstr (sign, strExp) =>
	    let val (srcRlzn, entEnv1) =
                  evalStr(strExp, depth, epc, entsv, entEnv, rpath)
                val {rlzn=rlzn, abstycs=abstycs, tyceps=tyceps} =
                  I.instAbstr{sign=sign, entEnv=entEnv, srcRlzn=srcRlzn,
                              rpath=rpath, region=SL.NULLregion}

                (* because the abstraction creates a bunch of new stamps,
                   we have to bind them to the epcontext.
                 *)
                val epc = EPC.enterOpen(epc, entsv)
                fun h (T.GENtyc gt, ep) =
		    EPC.bindTycLongPath (epc, MI.tycId gt, ep)
                  | h _ = ()
                val _ = ListPair.app h (abstycs, tyceps)
	     in (rlzn, entEnv1)
	    end

        | M.CONSTRAINstr {boundvar,raw,coercion} =>
            (* propagage the context rpath into the raw uncoerced structure *)
            let val (rawEnt, entEnv1) =
                    evalStr (raw, depth, epc, SOME boundvar, entEnv, rpath)
                val entEnv2 = EE.bind(boundvar, M.STRent rawEnt, entEnv1)
                val (strEnt, entEnv3) =
 	            evalStr(coercion, depth, epc, entsv, entEnv2, IP.empty)
             in (strEnt, entEnv3)
            end

        | M.FORMstr _ => bug "unexpected FORMstr in evalStr")


and evalFct (fctExp, depth, epc, entEnv) =
      case fctExp
       of M.VARfct entPath =>
	    (debugmsg (">>evalFct[VARfct]: "^EP.entPathToString entPath);
	     (EE.lookFctEP(entEnv,entPath), entEnv))

        | M.CONSTfct fctEntity => (fctEntity, entEnv)

        | M.LAMBDA {param, body} =>
            let val clos = M.CLOSURE{param=param, body=body, env=entEnv}
	     in ({stamp = SP.fresh (),
		  closure=clos,
		  properties = PropList.newHolder (),
		  (*lambdaty=ref NONE,*)
  		  tycpath=NONE,
		  rpath=IP.IPATH[anonFctSym],
		  stub=NONE},
		 entEnv)
            end

        | M.LAMBDA_TP{param, body, sign as M.FSIG {paramsig, bodysig, ...}} =>
            let val clos = M.CLOSURE{param=param, body=body, env=entEnv}
                val tps =
                  let val rpath' = IP.IPATH [paramSym]
                      val {rlzn=paramEnt, tycpaths=paramTps} =
                        I.instParam{sign=paramsig, entEnv=entEnv,
                                    rpath=rpath', tdepth=depth,
                                    region = SL.NULLregion}
                      val entEnv' = EE.mark (SP.fresh, EE.bind(param, M.STRent paramEnt,
                                                 entEnv))
                      val (bodyRlzn,_) =
                          evalStr(body, DebIndex.next depth, epc, NONE, entEnv', IP.empty)
                      val bodyTps =
                          I.getTycPaths{sign=bodysig, rlzn=bodyRlzn, entEnv=entEnv'}
                   in T.TP_FCT(paramTps, bodyTps)
                  end

             in ({stamp = SP.fresh (),
		  closure=clos,
		  properties = PropList.newHolder (),
		  (* lambdaty=ref NONE, *)
		  tycpath=SOME tps, rpath=IP.IPATH[anonFctSym],
		  stub = NONE},
		 entEnv)
            end

        | M.LETfct (entDec, fctExp) =>
            let val entEnv1 = evalDec (entDec, depth, epc, entEnv, IP.empty)
                val (fctEnt, entEnv2) = evalFct (fctExp, depth, epc, entEnv1)
             in (fctEnt, entEnv2)
            end

        | _ => bug "unexpected cases in evalFct"

and evalApp (fctRlzn : Modules.fctEntity, argRlzn, depth, epc, rpath) =
      let val {closure = M.CLOSURE{param, body, env}, tycpath, ...} = fctRlzn
	  val nenv = EE.mark (SP.fresh, EE.bind(param, M.STRent argRlzn, env))
          val  _ = debugmsg ("[Inside EvalAPP] ......")
       in case (body, tycpath)
           of (M.FORMstr (M.FSIG{paramsig, bodysig, ...}), SOME tp) =>
               let val argTps = I.getTycPaths{sign=paramsig, rlzn=argRlzn, entEnv=env}
                   val resTp = T.TP_APP(tp, argTps)

                   (** failing to add the stamps into the epcontext is
                       a potential bug here. Will fix this in the
                       future.  ZHONG **)

                   val {rlzn=rlzn, abstycs=abstycs, tyceps=tyceps} =
                     I.instFmBody {sign=bodysig, entEnv=nenv, tycpath=resTp,
                                   rpath=rpath, region=SL.NULLregion}

                   fun h (T.GENtyc gt, ep) =
                       EPC.bindTycLongPath (epc, MI.tycId gt, ep)
                     | h _ = ()
                   val _ = ListPair.app h (abstycs, tyceps)
                in rlzn
               end
            | _ =>
               let val (strEnt, deltaEE) =
                       evalStr (body, depth, epc, NONE, nenv, rpath)
                   (* invariant: deltaEE should always be same as nenv
                      if the body of an functor is always a BaseStr. Notice
                      functor body is constructed either in the source
                      programs (ml.grm) or in the elabmod.sml when dealing
                      with curried functor applications.
                    *)
                in strEnt
               end
      end

and evalDec(dec, depth, epc, entEnv, rpath) =
     (debugmsg ("[Inside EvalDec ......");
      case dec
       of M.TYCdec (entVar, tycExp) =>
            let val tycEnt =
                  evalTyc(entVar, tycExp, entEnv, epc, rpath)
	     in EE.bind(entVar, M.TYCent tycEnt, entEnv)
            end
        | M.STRdec (entVar, strExp, sym) =>
            let val rpath' =
		    if Symbol.eq(sym, returnId)
		       orelse Symbol.eq(sym, resultId)
		    then rpath
		    else IP.extend(rpath,sym)
		val (strEnt, entEnv1) =
                  evalStr(strExp, depth, epc, SOME entVar, entEnv, rpath')
             in EE.bind(entVar, M.STRent strEnt, entEnv1)
            end

        | M.FCTdec (entVar, fctExp) =>
            let val (fctEnt, entEnv1) =
                    evalFct(fctExp, depth, epc, entEnv)
             in EE.bind(entVar, M.FCTent fctEnt, entEnv1)
            end
        | M.SEQdec decs =>
            let fun h (dec, entEnv0) =
                    evalDec(dec, depth, epc, entEnv0, rpath)
             in EE.mark (SP.fresh, foldl h entEnv decs)
            end
        (*
         * The following may be wrong, but since ASSERTION! the bound symbols
         * are all distinct,it would not appear to cause any harm.
         *)
        | M.LOCALdec (localDec, bodyDec) =>
            let val entEnv1 = evalDec (localDec, depth, epc, entEnv, IP.empty)
             in evalDec(bodyDec, depth, epc, entEnv1, rpath)
            end

        | _  => entEnv)

and evalStp (stpExp, depth, epc, entEnv) =
      case stpExp
        of M.NEW             => SP.fresh ()
         | M.GETSTAMP strExp => #stamp (#1 (evalStr(strExp, depth, epc, NONE,
						 entEnv, IP.empty)))

(* -- make evalApp a phase 
val evalApp = Stats.doPhase(Stats.makePhase "Compiler 044 x-evalApp") evalApp
*)

end (* toplevel local - imports *)
end (* structure EvalEntity *)
