(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* lookup.sml *)

structure Lookup : LOOKUP =
struct

local structure SP = SymPath
      structure CVP = ConvertPaths
      structure M = Modules
      structure MU = ModuleUtil
      structure T = Types
      structure TU = TypesUtil
      structure A = Access
      structure V = VarCon
      structure B = Bindings
      structure SE = StaticEnv
      structure EM = ErrorMsg
      structure S = Symbol
in

fun bug s = EM.impossible ("Lookup: "^s)

fun spmsg spath = 
  if SP.length spath > 1 then " in path "^(SP.toString spath) else ""

fun unboundError(badsym, sp, err) =
      err EM.COMPLAIN ("unbound " ^
	               S.nameSpaceToString(S.nameSpace badsym) ^ 
                       ": " ^ S.name badsym ^ sp) EM.nullErrorBody

fun otherError(s, err) = err EM.COMPLAIN s EM.nullErrorBody

(* error values for undefined structure and functor variables *)
val bogusSTR = M.ERRORstr
val bogusFCT = M.ERRORfct
val bogusVAL = V.VAL V.ERRORvar

(*** look for a fixity binding ***)
fun lookFix (env,id) : Fixity.fixity =
  let val b = case SE.look(env,id)
	       of B.FIXbind fixity => fixity
	        | _ => bug "lookFIX"
   in b
  end handle SE.Unbound => Fixity.NONfix

(*** look for a signature ***)
fun lookSig (env,id,err) : M.Signature = 
  let val b = case SE.look(env,id) 
               of B.SIGbind sign => sign
                | _ => bug "lookSIG"
   in b
  end handle SE.Unbound => (unboundError(id,"",err); M.ERRORsig)

(*** look for a functor signature ***)
fun lookFsig (env,id,err) : M.fctSig = 
  let val b = case SE.look(env,id) 
               of B.FSGbind fs => fs
                | _ => bug "lookFSIG"
   in b
  end handle SE.Unbound => (unboundError(id,"",err); M.ERRORfsig)

(*** look for a variable or a constructor bound to a symbol ***)
fun lookValSym (env,sym,err) : V.value = 
  let val b = case SE.look(env,sym)
               of B.VALbind v => V.VAL v
                | B.CONbind c => V.CON c
                | _ => bug "lookValSym"
   in b
  end handle SE.Unbound => (unboundError(sym,"",err); bogusVAL)


(*** lookup path ****)

(* 
 * lookGen: generic lookup function for identifiers which may occur in:
 *   1. environments
 *   2. actual structure environments
 *   3. signature parsing environments 
 *)
fun lookGen(env,spath,outBind,getPath,errorVal,err) =
    case spath of
	SP.SPATH [id] =>
        (outBind(SE.look(env,id))
	 handle SE.Unbound => (unboundError(id,spmsg spath,err); errorVal))
      | SP.SPATH(first::rest) =>
	((case SE.look(env,first)
	   of B.STRbind str =>
	      (getPath(str,SP.SPATH rest,spath)
	       handle MU.Unbound sym =>
		      (unboundError(sym,spmsg spath,err); errorVal))
	    | _ =>  bug "lookGen1")
	 handle SE.Unbound => (unboundError(first,spmsg spath,err); 
                               errorVal))
      | SP.SPATH [] => bug "lookGen:SP.SPATH[]"

(*** look for a variable or a constructor (complete path) ***)
fun lookVal (env,path,err) : V.value = 
  let fun outVal(B.VALbind v) = V.VAL v
        | outVal(B.CONbind c) = V.CON c
        | outVal _ = bug "outVal"
   in lookGen(env,path,outVal,MU.getValPath,bogusVAL,err)
  end

(*** look for a structure ***)
fun lookStr (env,path,err) : M.Structure =
  let fun outStr(B.STRbind str) = str
        | outStr _ = bug "lookStr"
   in lookGen(env,path,outStr,MU.getStrPath,bogusSTR,err)
  end

(*** look for a strDef; used in elabsig.sml ***)
fun lookStrDef (env,path,err) : M.strDef = 
  let fun outSD(B.STRbind s) =
	  (case s of
	       M.STRSIG{sign,entPath} => M.VARstrDef(sign,entPath)
             | sv => M.CONSTstrDef sv)
        | outSD _ = bug "lookStrDef"
   in lookGen(env,path,outSD,MU.getStrDef,M.CONSTstrDef bogusSTR,err)
  end

(*** look for a functor ***)
fun lookFct (env,path,err) : M.Functor = 
  let fun outFct(B.FCTbind fct) = fct
        | outFct _ = bug "lookFct"
   in lookGen(env,path,outFct,MU.getFctPath,bogusFCT,err)
  end

(*** look for a type constructor ***)
fun lookTyc (env,path,err) : T.tycon = 
  let fun outTyc(B.TYCbind tycon) = tycon
        | outTyc _ = bug "lookTyc"
   in lookGen(env,path,outTyc,MU.getTycPath,T.ERRORtyc,err)
  end

(*** tycon lookup with arity checking ***)
fun lookArTyc (env, path, arity, err) =
      (case lookTyc(env,path,err)
        of T.ERRORtyc => T.ERRORtyc
         | tycon =>
	     if TU.tyconArity(tycon) <> arity
 	     then (otherError("type constructor " ^
		      (SP.toString(CVP.invertIPath(TU.tycPath(tycon)))) ^
		      " given " ^ (Int.toString arity) ^ " arguments, wants "
		      ^ (Int.toString (TU.tyconArity tycon)), err);
		   T.ERRORtyc)
	     else tycon)

(*** looking for an exception ***)
fun lookExn (env,path,err) : V.datacon =
      (case lookVal (env,path,err)
        of V.CON(c as T.DATACON{rep=(A.EXN _), ...}) => c
         | V.CON _ => 
             (otherError("found data constructor instead of exception", err);
              V.bogusEXN)
         | V.VAL _ => 
             (otherError("found variable instead of exception", err);
              V.bogusEXN))

end (* local *)
end (* structure Lookup *)

