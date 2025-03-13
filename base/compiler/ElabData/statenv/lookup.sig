(* COPYRIGHT 1996 Bell Laboratories *)
(* lookup.sig *)

signature LOOKUP =
sig
  val lookFix : StaticEnv.staticEnv * Symbol.symbol -> Fixity.fixity

  val lookSig : StaticEnv.staticEnv * Symbol.symbol -> Modules.Signature option

  val lookFsig : StaticEnv.staticEnv * Symbol.symbol -> Modules.fctSig option

  val lookStr : StaticEnv.staticEnv * SymPath.path -> Modules.Structure option

  val lookStrDef : StaticEnv.staticEnv * SymPath.path -> Modules.strDef option

  val lookFct : StaticEnv.staticEnv * SymPath.path -> Modules.Functor option

  val lookTyc : StaticEnv.staticEnv * SymPath.path -> Types.tycon option

  (* lookValSym and lookSym return value or constructor bindings *)
  val lookValSym : StaticEnv.staticEnv * Symbol.symbol -> VarCon.value option

  val lookVal : StaticEnv.staticEnv * SymPath.path -> VarCon.value option

end (* signature LOOKUP *)

