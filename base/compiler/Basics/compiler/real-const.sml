(* real-const.sml
 *
 * A common representation of typed real literals to use throughout the
 * different intermediate representations (from Absyn to CPS).
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure RealConst : sig

    type 'ty t = {
	rval : RealLit.t,	(* the value *)
	ty : 'ty		(* the "type" of the value *)
      }

    (* some standard constants *)
    val zero : bool * 'ty -> 'ty t      (* +/-0.0, where zero true is -0.0 *)
    val one : 'ty -> 'ty t              (* 1.0 *)
    val m_one : 'ty -> 'ty t            (* -1.0 *)
    val nan : 'ty -> 'ty t              (* some quiet NaN *)
    val posInf : 'ty -> 'ty t           (* positive infinity *)
    val negInf : 'ty -> 'ty t           (* negative infinity *)

    val toString : 'ty t -> string

    val fmt : ('ty -> string) -> 'ty t -> string

  (* do two constants have equal values? Note that this test ignores the types! *)
    val same : 'ty t * 'ty t -> bool

  end = struct

    type 'ty t = {rval : RealLit.t, ty : 'ty}

    fun zero (sgn, ty) = {rval = RealLit.zero sgn, ty = ty}
    fun one ty = {rval = RealLit.one, ty = ty}
    fun m_one ty = {rval = RealLit.m_one, ty = ty}
    fun nan ty = {rval = RealLit.nan, ty = ty}
    fun posInf ty = {rval = RealLit.posInf, ty = ty}
    fun negInf ty = {rval = RealLit.negInf, ty = ty}

    fun toString {rval, ty} = RealLit.toString rval

    fun fmt tyToString {rval, ty} = concat[RealLit.toString rval, ":", tyToString ty]

    fun same (a : 'ty t, b : 'ty t) = RealLit.same(#rval a, #rval b)

  end


