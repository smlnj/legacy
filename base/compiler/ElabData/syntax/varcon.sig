(* varcon.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature VARCON =
  sig

    datatype var
      = VALvar of	                (* ordinary variables *)
	  {path : SymPath.path,
	   typ : Types.ty ref,
	   btvs : Types.tyvar list ref,
	   access : Access.access,
	   prim   : PrimopId.prim_id}
      | OVLDvar of       	        (* overloaded identifier *)
	{name : Symbol.symbol,          (* name of the overloaded operator *)
	 variants : var list}           (* variant variables (VALvars) *)
      | ERRORvar

    type datacon = Types.datacon

    datatype value
      = VAL of var
      | CON of datacon

    val mkVALvar : Symbol.symbol * Access.access ->  var

    val varName : var -> Symbol.symbol

    val varToLvar : var -> LambdaVar.lvar
    val varToType : var -> Types.ty

    val bogusCON : datacon
    val bogusEXN : datacon

  end (* signature VARCON *)
