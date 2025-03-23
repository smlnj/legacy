(* varcon.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* [DBM, 2025.03.20]
 * Should move value datatype to Absyn, and bogusCON and bogusEXN to TypesUtil.
 * Then VarCon structure should be (has been in smlnj/newpp branch) renamed "Variable: VARIABLE",
 * since it then deals only with variables (type "var" or "variable").  See smlnj/newpp branch.
 *)

structure VarCon : VARCON =   (* => Variable: VARIABLE (newpp branch) *)
struct

(* imports *)
    structure A  = Access
    structure T  = Types
    structure S  = Symbol
    structure SP = SymPath

    fun bug msg = ErrorMsg.impossible ("VarCon: " ^ msg)

    datatype var
      = VALvar of			(* ordinary variables *)
	  {path : SP.path,              (* for "exp-local" vars, path should be of length 1 *)
	   typ : T.ty ref,
	   btvs : T.tyvar list ref,     (* "bound" tyvars *)
	   access : A.access,
	   prim : PrimopId.prim_id}     (* indicates whether it denotes a primitive operator, default NonPrim *)

      | OVLDvar of       	      	(* overloaded identifier *)
	  {name : S.symbol,             (* simple name of the overloaded operator *)
	   variants : var list}         (* variant variables, expected to be VALvars *)

      | ERRORvar			(* error variables *)

    datatype value
      = VAL of var
      | CON of T.datacon

    fun varName (VALvar{path,...}) = SymPath.last path
      | varName _ = bug "varName"  (* does not work on OVLDvar and ERRORvar *)

    (* mkVALvar : S.symbol * A.access -> var *)
    fun mkVALvar (id, acc) =
	  VALvar {path = SP.SPATH [id],
		  typ = ref T.UNDEFty,
		  access = acc,
		  btvs = ref [],
		  prim = PrimopId.NonPrim}

    (* varToLvar : var -> LV.lvar *)
    (* partial function that works on VALvars that have LVAR access, otherwise reporting BUG *)
    fun varToLvar (VALvar{access=A.LVAR lvar, ...}) = lvar
      | varToLvar _ = bug "varToLvar - bad variable"

    (* varToType : var -> T.ty *)
    fun varToType (VALvar{typ, ...}) = !typ
      | varToType _ = bug "varToType - bad variable"


    (* bogusCON and bogusEXN to move to TypesUtil. *)

    val bogusCON = T.DATACON
		     {name = S.varSymbol "bogus",
		      typ = T.WILDCARDty,
		      rep = A.CONSTANT 0,
		      const = true,
		      lazyp = false,
		      sign = A.CSIG(0,1)}

    val bogusEXN = T.DATACON
		     {name = S.varSymbol "bogus",
		      typ = BasicTypes.exnTy,
		      rep = A.CONSTANT 0,
		      const = true,
		      lazyp = false,
		      sign = A.CNIL}

end (* structure VarCon *)

