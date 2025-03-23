(* Copyright 1992 by AT&T Bell Laboratories *)

(* Utility functions to build absyn from ast *)

signature ELABUTIL =
sig

  datatype context
    = TOP    (* at top level -- not inside any module, rigid *)
    | INSTR  (* inside a rigid structure, i.e. not inside any functor body *)
    | INFCT of {flex: Stamps.stamp -> bool,  depth: DebIndex.depth}
             (* predicate recognizing flexible stamps *)
    | INSIG  (* within a signature body *)

  val debugging : bool ref

  val sort3 : (Symbol.symbol * 'a * 'b) list -> (Symbol.symbol * 'a * 'b) list

  val EQUALsym : Symbol.symbol
  val bogusID : Symbol.symbol
  val bogusExnID : Symbol.symbol
  val anonParamName : Symbol.symbol

  val CONSexp : Absyn.exp
  val CONSpat : Absyn.pat -> Absyn.pat
  val FALSEexp : Absyn.exp
  val FALSEpat : Absyn.pat
  val NILexp : Absyn.exp
  val NILpat : Absyn.pat
  val TRUEexp : Absyn.exp
  val TRUEpat : Absyn.pat
  val TUPLEexp : Absyn.exp list -> Absyn.exp
  val TPSELexp : Absyn.exp * int -> Absyn.exp
  val TUPLEpat : Absyn.pat list -> Absyn.pat
  val unitExp : Absyn.exp
  val unitPat : Absyn.pat
  val bogusExp: Absyn.exp

  val bindVARp : Absyn.pat list -> StaticEnv.staticEnv

  val checkUniq : Symbol.symbol list -> bool
  val checkForbiddenCons : Symbol.symbol -> bool

  val clean_pat : Absyn.pat -> Absyn.pat

(*
  val getCoreExn : (StaticEnv.staticEnv * string) -> VarCon.datacon
  val getCoreVar : (StaticEnv.staticEnv * string) -> VarCon.var
*)
  val completeMatch : (StaticEnv.staticEnv * string)
		      -> Absyn.rule list -> Absyn.rule list
  val completeMatch' : Absyn.rule -> Absyn.rule list -> Absyn.rule list

  val makeAPPpat : Absyn.pat * Absyn.pat -> Absyn.pat
  val makeHANDLEexp : Absyn.exp * Absyn.rule list -> Absyn.exp
  val makeLAYEREDpat : Absyn.pat * Absyn.pat -> Absyn.pat
  val makeRECORDexp : (Symbol.symbol * Absyn.exp) list -> Absyn.exp
  val makeRECORDpat : (Symbol.symbol * Absyn.pat) list * bool -> Absyn.pat

  val checkBoundTyvars : TyvarSet.tyvarset * Types.tyvar list -> unit

  val pat_id : SymPath.path * StaticEnv.staticEnv -> Absyn.pat

  val sortRecordFields : (Symbol.symbol * 'a) list -> (Symbol.symbol * 'a) list

  val FUNdec :
       (Absyn.rule list -> Absyn.rule list)
       * {var : VarCon.var,
          clauses: {pats: Absyn.pat list,
                    resultty: Types.ty option,
                    exp: Absyn.exp} list,
          tyvars: TyvarSet.tyvarset ref,
	  region: SourceMap.region } list
       -> (Absyn.dec * StaticEnv.staticEnv)

  val wrapRECdec : Absyn.rvb list -> (Absyn.dec * StaticEnv.staticEnv)

  val labsym : Absyn.numberedLabel -> Symbol.symbol

  val recDecs : Absyn.rvb list -> Absyn.dec

  val hasModules : Ast.dec -> bool

end (* signature ELABUTIL *)
