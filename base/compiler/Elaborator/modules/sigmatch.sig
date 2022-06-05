
signature SIGMATCH =
sig

  structure EvalEntity : EVALENTITY

  (*** these four functions are only called inside elabmod.sml ***)
  val matchStr : 
       {sign     : Modules.Signature, 
        str      : Modules.Structure,
        strExp   : Modules.strExp,
        evOp     : EntPath.entVar option,
        depth    : DebIndex.depth,
        entEnv   : Modules.entityEnv,
        rpath    : InvPath.path,
        statenv  : StaticEnv.staticEnv,
        region   : SourceMap.region,
        compInfo : ElabUtil.compInfo} -> {resDec : Absyn.dec,
                                          resStr : Modules.Structure,
                                          resExp : Modules.strExp}

  val matchFct : 
       {sign     : Modules.fctSig,
        fct      : Modules.Functor,
        fctExp   : Modules.fctExp,
        depth    : DebIndex.depth,
        entEnv   : Modules.entityEnv,
        rpath    : InvPath.path,
        statenv  : StaticEnv.staticEnv,
        region   : SourceMap.region,
        compInfo : ElabUtil.compInfo} -> {resDec : Absyn.dec,
                                          resFct : Modules.Functor,
                                          resExp : Modules.fctExp}

  val packStr : 
       {sign     : Modules.Signature, 
        str      : Modules.Structure,
        strExp   : Modules.strExp,
        depth    : DebIndex.depth,
        entEnv   : Modules.entityEnv,
        rpath    : InvPath.path,
        statenv  : StaticEnv.staticEnv,
        region   : SourceMap.region,
        compInfo : ElabUtil.compInfo} -> {resDec : Absyn.dec,
                                          resStr : Modules.Structure,
                                          resExp : Modules.strExp}

  val applyFct : 
       {fct      : Modules.Functor,
        fctExp   : Modules.fctExp,
        argStr   : Modules.Structure, 
        argExp   : Modules.strExp,
        evOp     : EntPath.entVar option,
        depth    : DebIndex.depth,
        epc      : EntPathContext.context,                                
        statenv  : StaticEnv.staticEnv,
	rpath    : InvPath.path,
        region   : SourceMap.region,
        compInfo : ElabUtil.compInfo} -> {resDec : Absyn.dec,
                                          resStr : Modules.Structure,
                                          resExp : Modules.strExp}


  val debugging : bool ref
  val showsigs : bool ref

end (* signature SIGMATCH *)
