(* COPYRIGHT (c) 1998 Bell Laboratories *)
(* elabtype.sig *)

signature ELABTYPE =
sig

  val elabType :
        Ast.ty * StaticEnv.staticEnv * SourceMap.region
        -> Types.ty * TyvarSet.tyvarset

  val elabTyvarList : 
        Ast.tyvar list * SourceMap.region 
        -> Types.tyvar list

  val elabTYPEdec :
        Ast.tb list * StaticEnv.staticEnv * InvPath.path * SourceMap.region
        -> Absyn.dec * StaticEnv.staticEnv

  val elabDATATYPEdec :
        {datatycs: Ast.db list, withtycs: Ast.tb list} * StaticEnv.staticEnv 
        * ExpandTycon.sigContext * EntityEnv.entityEnv 
        * (Types.tycon -> bool) * InvPath.path 
        * SourceMap.region
        -> Types.tycon list * Types.tycon list * VarCon.datacon list 
           * StaticEnv.staticEnv

  val debugging : bool ref

end (* signature ELABTYPE *)
