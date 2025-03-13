(* absyn.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Absyn : ABSYN =
  struct

    structure S = Symbol
    structure F = Fixity
    structure SP = SymPath
    structure B = Bindings
    structure Ty = Types

    type region = Ast.region  (* = int * int *)

    datatype numberedLabel = LABEL of {name: S.symbol, number: int}

    datatype exp
      = VARexp of VarCon.var ref * Ty.tyvar list
	(* the 2nd arg is a type univar list used to capture the instantiation
	   parameters for this occurence of VarCon.var when its type is polymorphic.
	   FLINT will use these to provide explicit type parameters for VarCon.var
           if VarCon.var is bound to a primop, which will be used to specialize
	   the primop. *)
      | CONexp of VarCon.datacon * Ty.tyvar list (* ditto *)
      | NUMexp of string * num_lit	(* string is source text of literal *)
      | REALexp of string * real_lit	(* string is source text of literal *)
      | STRINGexp of string
      | CHARexp of string
      | RECORDexp of (numberedLabel * exp) list
      | SELECTexp of numberedLabel * exp	(* record selections *)
      | VECTORexp of exp list * Ty.ty
      | APPexp of exp * exp
      | HANDLEexp of exp * fnrules
      | RAISEexp of exp * Ty.ty
      | CASEexp of exp * rule list * bool	(* true: match; false: bind *)
      | IFexp of { test: exp, thenCase: exp, elseCase: exp }
      | ANDALSOexp of exp * exp
      | ORELSEexp of exp * exp
      | WHILEexp of { test: exp, expr: exp }
      | FNexp of fnrules
      | LETexp of dec * exp
      | SEQexp of exp list
      | CONSTRAINTexp of exp * Ty.ty
      | MARKexp of exp * region

    and rule = RULE of pat * exp

    and pat
      = WILDpat
      | VARpat of VarCon.var
      | NUMpat of string * num_lit	(* string is source text of literal *)
      | STRINGpat of string
      | CHARpat of string
      | CONpat of VarCon.datacon * Ty.tyvar list (* See comment for VARexp *)
      | RECORDpat of {fields: (Ty.label * pat) list, flex: bool, typ: Ty.ty ref}
      | APPpat of VarCon.datacon * Ty.tyvar list * pat
      | CONSTRAINTpat of pat * Ty.ty
      | LAYEREDpat of pat * pat
      | ORpat of pat * pat
      | VECTORpat of pat list * Ty.ty
      | MARKpat of pat * region
      | NOpat

    and dec
      = VALdec of vb list  (* always a single element list (FLINT normalization) *)
      | VALRECdec of rvb list
      | DOdec of exp
      | TYPEdec of Ty.tycon list
      | DATATYPEdec of {datatycs: Ty.tycon list, withtycs: Ty.tycon list}
      | ABSTYPEdec of {abstycs: Ty.tycon list, withtycs: Ty.tycon list, body: dec}
      | EXCEPTIONdec of eb list
      | STRdec of strb list
      | FCTdec of fctb list
      | SIGdec of Modules.Signature list
      | FSIGdec of Modules.fctSig list
      | OPENdec of (SP.path * Modules.Structure) list
      | LOCALdec of dec * dec
      | SEQdec of dec list
      | OVLDdec of VarCon.var
      | FIXdec of {fixity: F.fixity, ops: S.symbol list}
      | MARKdec of dec * region

    (*
     * [FLINT] The "argtycs" field in APPstr is used to record the list of instantiated
     * hotycs passed to functor during the functor application.
     *)
    and strexp
      = VARstr of Modules.Structure
      | STRstr of B.binding list
      | APPstr of {oper: Modules.Functor,
		   arg: Modules.Structure,
		   argtycs: Ty.tycpath list}
      | LETstr of dec * strexp
      | MARKstr of strexp * region

    (*
     * [FLINT] For typing purpose, a functor is viewed as a high-order type constructor
     * (hotyc) that takes a list of hotycs returns another list of hotycs. The
     * "argtycs" field in FCTfct records the list of formal hotyc paramaters.
     *)
    and fctexp
      = VARfct of Modules.Functor
      | FCTfct of {param: Modules.Structure, argtycs: Ty.tycpath list, def: strexp}
      | LETfct of dec * fctexp
      | MARKfct of fctexp * region

    (*
     * Each value binding vb only binds one variable identifier [FLINT "normalization"].
     * That is, pat is always a simple VARpat (with type constraints) or it simply
     * does not contain any variable patterns; boundtvs gives the list of
     * type variables that are being generalized at this binding, as determined
     * (and recorded via updating the ref?) during type checking.
     *)
    and vb = VB of {pat: pat, exp: exp, boundtvs: Ty.tyvar list,
		    tyvars: Ty.tyvar list ref}

    (*
     * Like value binding vb, boundtvs gives a list of type variables
     * being generalized at this binding. However, the mutually recursive
     * list of RVBs could share type variables, that is, the boundtvs sets
     * used in these RVBs could contain overlapping set of type variables.
     *)
    and rvb = RVB of {var: VarCon.var, exp: exp, boundtvs: Ty.tyvar list,
		      resultty: Ty.ty option, tyvars: Ty.tyvar list ref}

    and eb = EBgen of {exn: VarCon.datacon, etype: Ty.ty option, ident: exp}
	   | EBdef of {exn: VarCon.datacon, edef: VarCon.datacon}

    and strb = STRB of {name: S.symbol, str: Modules.Structure, def: strexp}
    and fctb = FCTB of {name: S.symbol, fct: Modules.Functor, def: fctexp}

    withtype fnrules = rule list * Ty.ty
         and num_lit = Types.ty IntConst.t
         and real_lit = Types.ty RealConst.t

  end
