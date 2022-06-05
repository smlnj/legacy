(* varcon.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure VarCon : VARCON =
  struct

    structure A  = Access
    structure T  = Types
    structure S  = Symbol
    structure SP = SymPath

    fun bug msg = ErrorMsg.impossible ("VarCon: " ^ msg)

    datatype var
      = VALvar of			(* ordinary variables *)
	  {path : SP.path,
	   typ : T.ty ref,
	   btvs : T.tyvar list ref,
	   access : A.access,
	   prim : PrimopId.prim_id}
      | OVLDvar of       	      	(* overloaded identifier *)
	  {name : S.symbol,             (* name of the overloaded operator *)
	   variants : var list}         (* variant variables (VALvars) *)
      | ERRORvar			(* error variables *)

    type datacon = T.datacon

    datatype value
      = VAL of var
      | CON of datacon

    fun varName (VALvar{path,...}) = SymPath.last path
      | varName _ = bug "varName"

    fun mkVALvar (id, acc) =
	  VALvar{path = SP.SPATH [id],
		 typ = ref T.UNDEFty,
		 access = acc,
		 btvs = ref [],
		 prim = PrimopId.NonPrim}

    (* varToLvar : var -> LV.lvar *)
    fun varToLvar (VALvar{access=A.LVAR lvar, ...}) = lvar
      | varToLvar _ = bug "varToLvar - bad variable"

    (* varToType : var -> T.ty *)
    fun varToType (VALvar{typ, ...}) = !typ
      | varToType _ = bug "varToType - bad variable"

    val bogusCON = T.DATACON{
	    name=S.varSymbol "bogus",
	    typ=T.WILDCARDty,
	    rep=A.CONSTANT 0,
	    const=true,
	    lazyp=false,
	    sign=A.CSIG(0,1)
	  }

    val bogusEXN = T.DATACON{
	    name=S.varSymbol "bogus",
	    typ=BasicTypes.exnTy,
	    rep=A.CONSTANT 0,
	    const=true,
	    lazyp=false,
	    sign=A.CNIL
	  }

  end (* structure VarCon *)

(* rename Var : VAR, with value datatype moved to Absyn(?), and
 * bogusCON and bogusEXN moved to TypesUtil.
 *)
