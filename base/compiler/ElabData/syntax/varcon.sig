(* varcon.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature VARCON =
  sig

    datatype var
      = VALvar of	                (* ordinary variables *)
	  {path : SymPath.path,         (* for "local" variables, path will be a singleton *)
	   typ : Types.ty ref,
	   btvs : Types.tyvar list ref,
	   access : Access.access,
	   prim   : PrimopId.prim_id}
      | OVLDvar of       	        (* overloaded identifier *)
	{name : Symbol.symbol,          (* name of the overloaded operator *)
	 variants : var list}           (* variant variables (always VALvars) *)
      | ERRORvar                        (* supporting error recovery *)

    (* to move to Absyn: VarCon.value -> Absyn.value *)
    datatype value
      = VAL of var      (* should the dataconstructor be "VAR" instead? *)
      | CON of Types.datacon

    val mkVALvar : Symbol.symbol * Access.access ->  var  (* rename to "mkVar"? Why the "VAL" part? *)
    val varName : var -> Symbol.symbol

    val varToLvar : var -> LambdaVar.lvar
    val varToType : var -> Types.ty

    (* to move to TypesUtil *)
    val bogusCON : Types.datacon
    val bogusEXN : Types.datacon

  end (* signature VARCON *)
