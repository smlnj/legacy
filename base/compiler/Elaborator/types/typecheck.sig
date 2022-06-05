
signature TYPECHECK = 
sig

  val decType : StaticEnv.staticEnv * Absyn.dec * bool
		* ErrorMsg.errorFn * SourceMap.region -> Absyn.dec
  val debugging : bool ref

end (* signature TYPECHECK *)