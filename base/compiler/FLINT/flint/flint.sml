(* flint.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure FLINT : FLINT =
  struct

    structure A  = Access   (* should go away soon *)
    structure LD = LtyDef
    structure LV = LambdaVar

    type tkind = LD.tkind
    type tyc = LD.tyc
    type lty = LD.lty

    type tvar = LD.tvar
    type lvar = LV.lvar

    type fflag = LD.fflag
    type rflag = LD.rflag

  (* what kind of inlining behavior is desired for the function *)
    datatype ilhint
      = IH_SAFE				(* only if trivially size-safe *)
      | IH_ALWAYS			(* inline whenever possible *)
      | IH_UNROLL			(* only inline once within itself *)
    (* call-site dependent inlining:
     *     #1 < sum (map2 (fn (a,w) => (known a) * w) (actuals, #2)
     *)
      | IH_MAYBE of int * int list

    (* what kind of recursive function (aka loop) is this *)
    datatype loopkind
      = LK_UNKNOWN			(* something else *)
      | LK_LOOP				(* loop wrapped in a preheader *)
      | LK_TAIL				(* properly tail-recursive *)

  (* calling convention *)
    datatype cconv
      = CC_FCT				(* it's a functor *)
      | CC_FUN of fflag			(* it's a function *)

  (** classifying various kinds of functions *)
    type fkind
     = {inline: ilhint,			(* when should it be inlined *)
	known : bool,			(* are all the call sites known *)
	cconv : cconv,			(* calling convention *)
	isrec : (lty list * loopkind) option} (* is it recursive *)

  (* additional attributes for type abstractions *)
    type tfkind
     = {inline: ilhint}

  (** classifying various kinds of records *)
    datatype rkind
      = RK_VECTOR of tyc           (* vector: all elements have same type *)
      | RK_STRUCT                  (* module: elements may be polymorphic *)
      | RK_TUPLE of rflag          (* tuple: all elements are monomorphic *)

  (*
   * dcon records the name of the constructor (for debugging), the
   * corresponding conrep, and the flint type lty (which must be an
   * arrow type).
   *)
    type dcon = Symbol.symbol * A.conrep * lty

  (*
   * con: used to specify all possible switching statements. Efficient switch
   * generation can be applied to DATAcon and INTcon. Otherwise, the switch is
   * just a short-hand of the binary branch trees. Some of these instances
   * such as VLENcon will go away soon.
   *)
    datatype con
      = DATAcon of dcon * tyc list * lvar
      | INTcon of int IntConst.t	(* sz = 0 for IntInf.int *)
      | WORDcon of int IntConst.t
      | STRINGcon of string
      | VLENcon of int

  (** simple values, including variables and static constants. *)
    datatype value
      = VAR of lvar
      | INT of int IntConst.t	(* sz = 0 for IntInf.int *)
      | WORD of int IntConst.t
      | REAL of int RealConst.t
      | STRING of string

  (** the definitions of the lambda expressions *)
    datatype lexp
      = RET of value list
      | LET of lvar list * lexp * lexp

      | FIX of fundec list * lexp
      | APP of value * value list

      | TFN of tfundec * lexp
      | TAPP of value * tyc list

      | SWITCH of value * A.consig * (con * lexp) list * lexp option
      | CON of dcon * tyc list * value * lvar * lexp

      | RECORD of rkind * value list * lvar * lexp
      | SELECT of value * int * lvar * lexp          (* add rkind ? *)

      | RAISE of value * lty list
      | HANDLE of lexp * value

      | BRANCH of primop * value list * lexp * lexp
      | PRIMOP of primop * value list * lvar * lexp

    withtype fundec = fkind * lvar * (lvar * lty) list * lexp
    and tfundec = tfkind * lvar * (tvar * tkind) list * lexp
    and dict = {default: lvar, table: (tyc list * lvar) list}
    and primop = dict option * Primop.primop * lty * tyc list
	    (* Invariant: primop's lty is always fully closed *)

    type prog = fundec  (* was "lvar * lty * lexp" *)

  end (* structure FLINT *)
