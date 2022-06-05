(* smlnj-gctype.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature SMLGCTYPE =
  sig

    structure CPS : CPS
    type ty = int

    datatype gctype
      = CONST of IntInf.int		(* integer constant *)
      | NONREF of CPS.cty ref		(* non-reference value *)
      | REF of CPS.cty ref		(* a reference, pointer to a gc object *)
      | PLUS of ty * gctype * gctype	(* address arithmetic + *)
      | MINUS of ty * gctype * gctype	(* address arithmetic - *)
      | ALLOCPTR			(* SML/NJ allocation pointer *)
      | LIMITPTR			(* SML/NJ limit pointer *)
      | BOT
      | TOP

    val ==       : gctype * gctype -> bool

    val join     : gctype * gctype -> gctype
    val meet     : gctype * gctype -> gctype

    val toString : gctype -> string

    (*
     * Primitive types
     *)
    val TAGGED_INT : gctype  (* default tagged integer *)
    val INT        : gctype  (* machine integers *)

    val REAL64   : gctype  (* unboxed real *)
    val REAL32   : gctype  (* unused *)
    val PTR      : gctype  (* ML objects *)

    val ADD      : ty * gctype * gctype -> gctype
    val SUB      : ty * gctype * gctype -> gctype

    val isRecoverable : gctype -> bool

    exception GCTYPE of gctype
    val GC_TYPE : gctype Annotations.property

  end
