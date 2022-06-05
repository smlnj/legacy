(* absyn.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ABSYN =
  sig

    type region = Ast.region  (* = int * int *)

    datatype numberedLabel = LABEL of {name: Symbol.symbol, number: int}

    datatype exp
      = VARexp of VarCon.var ref * Types.tyvar list (* instance type *)
      | CONexp of VarCon.datacon * Types.tyvar list (* instance type *)
      | NUMexp of string * num_lit	(* string is source text of literal *)
      | REALexp of string * real_lit	(* string is source text of literal *)
      | STRINGexp of string
      | CHARexp of string
      | RECORDexp of (numberedLabel * exp) list
      | SELECTexp of numberedLabel * exp
      | VECTORexp of exp list * Types.ty
      | APPexp of exp * exp
      | HANDLEexp of exp * fnrules
      | RAISEexp of exp * Types.ty
      | CASEexp of exp * rule list * bool
      | IFexp of { test: exp, thenCase: exp, elseCase: exp }
      | ANDALSOexp of exp * exp
      | ORELSEexp of exp * exp
      | WHILEexp of { test: exp, expr: exp }
      | FNexp of fnrules
      | LETexp of dec * exp
      | SEQexp of exp list
      | CONSTRAINTexp of exp * Types.ty
      | MARKexp of exp * region

    and rule = RULE of pat * exp

    and pat
      = WILDpat
      | VARpat of VarCon.var
      | NUMpat of string * num_lit	(* string is source text of literal *)
      | STRINGpat of string
      | CHARpat of string
      | CONpat of VarCon.datacon * Types.tyvar list (* instance type *)
      | RECORDpat of {fields : (Types.label * pat) list,
		      flex : bool, typ : Types.ty ref}
      | APPpat of VarCon.datacon * Types.tyvar list * pat
      | CONSTRAINTpat of pat * Types.ty
      | LAYEREDpat of pat * pat
      | ORpat of pat * pat
      | VECTORpat of pat list * Types.ty
      | MARKpat of pat * region
      | NOpat

    and dec
      = VALdec of vb list
      | VALRECdec of rvb list
      | DOdec of exp
      | TYPEdec of Types.tycon list
      | DATATYPEdec of {datatycs: Types.tycon list, withtycs: Types.tycon list}
      | ABSTYPEdec of {abstycs: Types.tycon list,
		       withtycs: Types.tycon list, body: dec}
      | EXCEPTIONdec of eb list
      | STRdec of strb list
      | FCTdec of fctb list
      | SIGdec of Modules.Signature list
      | FSIGdec of Modules.fctSig list
      | OPENdec of (SymPath.path * Modules.Structure) list
      | LOCALdec of dec * dec
      | SEQdec of dec list
      | OVLDdec of VarCon.var
      | FIXdec of {fixity: Fixity.fixity, ops: Symbol.symbol list}
      | MARKdec of dec * region

    and strexp
      = VARstr of Modules.Structure
      | STRstr of Bindings.binding list
      | APPstr of {oper: Modules.Functor, arg: Modules.Structure,
		   argtycs: Types.tycpath list}
      | LETstr of dec * strexp
      | MARKstr of strexp * region

    and fctexp
      = VARfct of Modules.Functor
      | FCTfct of {param: Modules.Structure, argtycs: Types.tycpath list,
		   def: strexp}
      | LETfct of dec * fctexp
      | MARKfct of fctexp * region

    and vb = VB of {pat: pat, exp: exp, boundtvs: Types.tyvar list,
		    tyvars: Types.tyvar list ref}

    and rvb = RVB of {var: VarCon.var, exp: exp, boundtvs: Types.tyvar list,
		      resultty: Types.ty option, tyvars: Types.tyvar list ref}

    and eb = EBgen of {exn: VarCon.datacon, etype: Types.ty option, ident: exp}
	   | EBdef of {exn: VarCon.datacon, edef: VarCon.datacon}

    and strb = STRB of {name: Symbol.symbol, str: Modules.Structure, def: strexp}
    and fctb = FCTB of {name: Symbol.symbol, fct: Modules.Functor, def: fctexp}

    withtype fnrules = rule list * Types.ty
         and num_lit = Types.ty IntConst.t
         and real_lit = Types.ty RealConst.t

  end (* signature ABSYN *)
