(* plambda.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PLambda : PLAMBDA =
  struct

    structure A  = Access
    structure LK = PLambdaType
    structure LV = LambdaVar
    structure PO = Primop
    structure S  = Symbol

    type tkind = LK.tkind
    type tyc = LK.tyc
    type lty = LK.lty

    type lvar = LV.lvar

  (*
   * dataconstr records the name of the constructor, the corresponding conrep,
   * and the lambda type lty; value carrying data constructors would have
   * arrow type.
   *)
    type dataconstr = S.symbol * A.conrep * lty

  (*
   * con: used to specify all possible switching statements. Efficient switch
   * generation can be applied to DATAcon and INTcon. Otherwise, it is just a
   * shorthand for binary branch trees. In the future, we probably should make
   * it more general, including constants of any numerical types.
   *)
    datatype con
      = DATAcon of dataconstr * tyc list * lvar
      | INTcon of int IntConst.t	(* sz = 0 for IntInf.int *)
      | WORDcon of int IntConst.t
      | STRINGcon of string
      | VLENcon of int

  (*
   * lexp: the universal typed intermediate language. TFN, TAPP is abstraction
   * and application on type constructors. Structure abstractions and functor
   * abstractions are represented as normal structure and functor definitions
   * with its component properly PACKed. FN defines normal function, FIX defines
   * a set of recursive functions, LET(v,e1,e2) is a syntactic sugar for exprs
   * of forms like APP(FN(v,_,e2), e1); the type of v will be that of e1.
   * APP is the function application. RECORD, VECTOR, and SRECORD are record,
   * vector, and structure construction, SELECT is record, vector, and structure
   * selection. ETAG, RAISE, and HANDLE are for exceptions.
   *)
    datatype lexp
      = VAR of lvar
      | INT of int IntConst.t	(* sz = 0 for IntInf.int *)
      | WORD of int IntConst.t
      | REAL of int RealConst.t
      | STRING of string
      | PRIM of PO.primop * lty * tyc list
      | GENOP of dict * PO.primop * lty * tyc list

      | FN of lvar * lty * lexp
      | FIX of lvar list * lty list * lexp list * lexp
      | APP of lexp * lexp
      | LET of lvar * lexp * lexp

      | TFN of tkind list * lexp
      | TAPP of lexp * tyc list

      | RAISE of lexp * lty
      | HANDLE of lexp * lexp
      | ETAG of lexp * lty

      | CON of dataconstr * tyc list * lexp
      | SWITCH of lexp * A.consig * (con * lexp) list * lexp option

      | VECTOR of lexp list * tyc
      | RECORD of lexp list
      | SRECORD of lexp list
      | SELECT of int * lexp

      | PACK of lty * tyc list * tyc list * lexp
      | WRAP of tyc * bool * lexp
      | UNWRAP of tyc * bool * lexp

    withtype dict = {default: lexp, table: (tyc list * lexp) list}

  end (* structure PLambda *)
