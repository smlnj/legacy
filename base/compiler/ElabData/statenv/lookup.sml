(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* lookup.sml *)

(* [DBM, 2025.03.07] Change in interface design: The lookup functions return an option,
rather than calling an error function argument. The caller of a lookup
function will be responsible for generating an error message if the lookup fails
(returns NONE). The reason is that virtually everywhere we call the same error
function (ErrorMsg.error). *)

structure Lookup : LOOKUP =
struct

local (* imports *)

  structure SP = SymPath
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

  (* move these error-related functions to ElabUtil? 

  [spmsg --> SP.pathToString?]
  fun spmsg spath = 
    if SP.length spath > 1 then " in path "^(SP.toString spath) else ""
  *)

  (* lookFix : StaticEnv.staticEnv * Symbol.symbol -> Fixity.fixity option
   * Lookup a fixity binding; the id should be in the fixity namespace.
   * If the id doesn't have a fixity binding in env, then we default to NONfix,
   * instead of returning NONE. *)
  fun lookFix (env, id) : Fixity.fixity =
    let val b = case SE.look(env,id)
		 of B.FIXbind fixity => fixity
		  | _ => bug "lookFIX"
     in b
    end handle SE.Unbound => Fixity.NONfix

  (* lookSig : StaticEnv.staticEnv * Symbol.symbol -> Modules.Signature option *)
  (* look for a signature *)
  fun lookSig (env, id) : M.Signature option = 
    let val b = case SE.look(env,id) 
		  of B.SIGbind sign => sign
		   | _ => bug "lookSIG" (* id in wrong namespace *)
     in SOME b
    end handle SE.Unbound => NONE (* => ERRORsig *)

  (*** look for a functor signature ***)
  fun lookFsig (env, sym) : M.fctSig option= 
    let val b = case SE.look (env, sym) 
		  of B.FSGbind fs => fs
		   | _ => bug "lookFSIG"
     in SOME b
    end handle SE.Unbound => NONE (* => ERRORfsig *)

  (* lookValSym : SE.staticEnv * S.symbol -> V.value option *)
  (*** look for a variable or a constructor bound to a symbol ***)
  fun lookValSym (env, sym) : V.value option = 
    let val b = case SE.look (env, sym)
		  of B.VALbind v => V.VAL v
		   | B.CONbind c => V.CON c
		   | _ => bug "lookValSym"
     in SOME b
    end handle SE.Unbound => NONE (* => V.VAL V.ERRORvar *)


(*** path lookups ***)

  type 'a getpath = M.Structure * SP.path * SP.path -> 'a

  (* 
   * lookGen: SE.staticEnv * SP.path * (B.binding -> 'a) * [getPath:]'a getpath -> 'a option
   * Generic lookup function for symbolic paths in environments.
   *)
  fun 'a lookGen (env: SE.staticEnv, spath: SP.path, outBind : B.binding -> 'a,
		  getPath: 'a getpath) =
      case spath
	of SP.SPATH [id] =>
	     (SOME (outBind (SE.look (env, id)))
	      handle SE.Unbound => NONE) (* failure on head of path; => errorVal *)
	 | SP.SPATH (first :: rest) =>
	     ((case SE.look (env, first)
		 of B.STRbind str =>
		      (SOME (getPath (str, SP.SPATH rest, spath))
		       handle MU.Unbound sym => NONE)
		  | _ =>  bug "lookGen1") (* bad namespace for first *)
	      handle SE.Unbound => NONE) (* errorVal *)
	 | SP.SPATH [] => bug "lookGen:SP.SPATH []"

  (* lookVal : staticEnv * SP.path -> V.value option *)
  (* look for a variable or a constructor (a "value") *)
  fun lookVal (env, path) : V.value  option = 
      let fun outVal (B.VALbind v) = V.VAL v
	    | outVal (B.CONbind c) = V.CON c
	    | outVal _ = bug "outVal"
       in lookGen (env, path, outVal, MU.getValPath)
          (* V.Val (V.ERRORval) *)
      end

  (* lookStr : staticEnv * SP.path -> M.structure option *)
  (* look for a structure *)
  fun lookStr (env, path) : M.Structure option =
      let fun outStr (B.STRbind str) = str
	    | outStr _ = bug "lookStr"
       in lookGen (env, path, outStr, MU.getStrPath)
	  (* NONE => M.ERRORstr *)
      end

  (* lookStr : staticEnv * SP.path -> M.strDef option *)
  (* look for a strDef; used in elabsig.sml *)
  fun lookStrDef (env, path) : M.strDef option = 
      let fun outSD (B.STRbind s) =
	      (case s
		 of M.STRSIG {sign,entPath} => M.VARstrDef(sign,entPath)
		  | sv => M.CONSTstrDef sv)
	    | outSD _ = bug "lookStrDef"
       in lookGen (env, path, outSD, MU.getStrDefPath)
	  (* NONE => M.CONSTstrDef M.ERRORstr *)
      end

  (* lookFct : staticEnv * SP.path -> M.Functor option *)
  (* look for a functor *)
  fun lookFct (env, path) : M.Functor option = 
      let fun outFct (B.FCTbind fct) = fct
	    | outFct _ = bug "lookFct"
       in lookGen (env, path, outFct, MU.getFctPath)
          (* NONE => M.ERRORfct *)
      end

  (* lookTyc : staticEnv * SP.path -> T.tycon option *)
  (* look for a type constructor *)
  fun lookTyc (env, path) : T.tycon option = 
      let fun outTyc (B.TYCbind tycon) = tycon
	    | outTyc _ = bug "lookTyc"
       in lookGen (env, path, outTyc, MU.getTycPath)
          (* NONE => T.ERRORtyc *)
      end
 
end (* local *)
end (* structure Lookup *)

