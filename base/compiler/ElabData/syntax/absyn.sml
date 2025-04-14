(* absyn.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Absyn : ABSYN =
struct

local (* imports *)

  structure SL = SourceLoc
  structure SM = SourceMap
  structure S = Symbol
  structure F = Fixity
  structure SP = SymPath
  structure B = Bindings
  structure T = Types
  structure TS = TyvarSet		       
  structure V = VarCon  (* ==> Variable *)
    
in

    datatype numberedLabel = LABEL of {name: S.symbol, number: int}

    datatype exp
      = VARexp of VarCon.var ref * T.tyvar list
	(* the 2nd arg is a type univar list used to capture the instantiation
	   parameters for this occurence of VarCon.var when its type is polymorphic.
	   FLINT will use these to provide explicit type parameters for VarCon.var
           if VarCon.var is bound to a primop, which will be used to specialize
	   the primop. *)
      | CONexp of T.datacon * T.tyvar list (* ditto *)
      | NUMexp of string * num_lit	(* string is source text of literal *)
      | REALexp of string * real_lit	(* string is source text of literal *)
      | STRINGexp of string
      | CHARexp of string
      | RECORDexp of (numberedLabel * exp) list
      | SELECTexp of numberedLabel * exp	(* record selections *)
      | VECTORexp of exp list * T.ty
      | APPexp of exp * exp
      | HANDLEexp of exp * fnrules
      | RAISEexp of exp * T.ty
      | CASEexp of exp * rule list * bool	(* true: match; false: bind; should be two dcons *)
      | IFexp of { test: exp, thenCase: exp, elseCase: exp }
      | ANDALSOexp of exp * exp
      | ORELSEexp of exp * exp
      | WHILEexp of { test: exp, expr: exp }
      | FNexp of fnrules
      | LETexp of dec * exp
      | SEQexp of exp list
      | CONSTRAINTexp of exp * T.ty
      | MARKexp of exp * SL.region

    and rule = RULE of pat * exp

    and pat
      = WILDpat
      | VARpat of VarCon.var
      | NUMpat of string * num_lit	(* string is source text of literal *)
      | STRINGpat of string
      | CHARpat of string
      | CONpat of T.datacon * T.tyvar list (* See comment for VARexp *)
      | RECORDpat of {fields: (T.label * pat) list, flex: bool, typ: T.ty ref}
      | APPpat of T.datacon * T.tyvar list * pat
      | CONSTRAINTpat of pat * T.ty
      | LAYEREDpat of pat * pat
      | ORpat of pat * pat
      | VECTORpat of pat list * T.ty
      | MARKpat of pat * SL.region
      | NOpat

    and dec
      = VALdec of vb list  (* always a single element list (FLINT normalization) *)
      | VALRECdec of rvb list
      | DOdec of exp
      | TYPEdec of T.tycon list
      | DATATYPEdec of {datatycs: T.tycon list, withtycs: T.tycon list}
      | ABSTYPEdec of {abstycs: T.tycon list, withtycs: T.tycon list, body: dec}
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
      | MARKdec of dec * SL.region

    (*
     * [FLINT] The "argtycs" field in APPstr is used to record the list of instantiated
     * hotycs passed to the functor during the functor application.
     *)
    and strexp
      = VARstr of Modules.Structure
      | STRstr of B.binding list
      | APPstr of {oper: Modules.Functor,
		   arg: Modules.Structure,
		   argtycs: T.tycpath list}
      | LETstr of dec * strexp
      | MARKstr of strexp * SL.region

    (*
     * [FLINT] For typing purpose, a functor is viewed as a high-order type constructor
     * (hotyc) that takes a list of hotycs returns another list of hotycs. The
     * "argtycs" field in FCTfct records the list of formal hotyc paramaters.
     *)
    and fctexp
      = VARfct of Modules.Functor
      | FCTfct of {param: Modules.Structure, argtycs: T.tycpath list, def: strexp}
      | LETfct of dec * fctexp
      | MARKfct of fctexp * SL.region

    (*
     * Each value binding vb only binds one variable identifier [a FLINT "normalization"].
     * That is, pat is always a simple VARpat (with type constraints) or it simply
     * does not contain any variable patterns; boundtvs gives the list of
     * type variables that are being generalized at this binding, as determined
     * (and recorded via updating the ref?) during type checking.
     *)
    and vb = VB of {pat: pat, exp: exp, boundtvs: T.tyvar list,
		    tyvars: TS.tyvarset ref}

    (*
     * Like value binding vb, boundtvs gives a list of type variables
     * being generalized at this binding. However, the mutually recursive
     * list of RVBs could share type variables, that is, the boundtvs sets
     * used in these RVBs could overlap (have nonempty intersections).
     *)
    and rvb = RVB of {var: VarCon.var, exp: exp, boundtvs: T.tyvar list,
		      resultty: T.ty option, tyvars: TS.tyvarset ref}

    and eb = EBgen of {exn: T.datacon, etype: T.ty option, ident: exp}
	   | EBdef of {exn: T.datacon, edef: T.datacon}

    and strb = STRB of {name: S.symbol, str: Modules.Structure, def: strexp}
    and fctb = FCTB of {name: S.symbol, fct: Modules.Functor, def: fctexp}

    withtype fnrules = rule list * T.ty
         and num_lit = Types.ty IntConst.t
         and real_lit = Types.ty RealConst.t

end (* local - imports *)
end (* structure Absyn *)
