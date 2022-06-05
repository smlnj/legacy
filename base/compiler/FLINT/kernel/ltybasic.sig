(* ltybasic.sig
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(*
 * This file contains all the standard operations defined in LtyDef plus
 * the set of common functions used to manipulate kinds, tycs, and ltys.
 * The rule of thumb about what should be included in this file rather
 * than the ltyextern.sml: well, all primitive lambda tkinds, tycs and
 * ltys should be here, all common utility functions on tkinds, tycs,
 * and ltys should be here. Functions that are of specific use should
 * go to the ltyextern.sml. Still, the module LtyExtern will include
 * all functions defined here, so all clients should use functions via
 * the LtyExtern structure.
 *)

signature LTYBASIC =
sig

(*
 * The abstract definitions of tkind, tyc, and lty are in separate files,
 * i.e., ltydef.sig and ltydef.sml. The internal implementation of tkind,
 * tyc, and lty are in the ltykernel.sig and ltykernel.sml files. In general,
 * the clients of the lambda types should never need to understand what is
 * going on inside the LtyKernel.
 *)

(** the definitions of tkind, tyc, and lty *)
include LTYDEF        (* see ltydef.sig for details *)

(** new a type variable, currently not used *)
val mkTvar : unit -> tvar

(** primitives and utility functions for fflags and rflags *)
val ffc_plambda: fflag
val ffc_rrflint: fflag
val ffc_fspec  : fflag * (bool * bool) -> fflag
val ffd_fspec  : fflag -> bool * bool

(** primitive lambda tycs *)
val tcc_int    : tyc		(* default tagged int type *)
val tcc_num    : int -> tyc	(* int type of given size *)
val tcc_real   : tyc
val tcc_string : tyc
val tcc_exn    : tyc
val tcc_void   : tyc
val tcc_unit   : tyc
val tcc_bool   : tyc

val tcc_tv     : int -> tyc
val tcc_ref    : tyc -> tyc
val tcc_array  : tyc -> tyc
val tcc_vector : tyc -> tyc
val tcc_etag   : tyc -> tyc

(** primitive lambda ltys *)
val ltc_num    : int -> lty
val ltc_int    : lty	(* = ltc_num Target.defaultIntSz *)
val ltc_real   : lty	(* REAL32: need ltc_real32/ltc_real64 *)
val ltc_string : lty
val ltc_exn    : lty
val ltc_void   : lty
val ltc_unit   : lty
val ltc_bool   : lty

val ltc_tv     : int -> lty
val ltc_ref    : lty -> lty
val ltc_array  : lty -> lty
val ltc_vector : lty -> lty
val ltc_etag   : lty -> lty

val ltc_top    : lty    (* used in a dirty hack in prim.sml *)

(** testing equivalence of tkinds, tycs, ltys, fflags, and rflags *)
val tk_eqv     : tkind * tkind -> bool
val tc_eqv     : tyc * tyc -> bool
val lt_eqv     : lty * lty -> bool
val ff_eqv     : fflag * fflag -> bool
val rf_eqv     : rflag * rflag -> bool

(** pretty printing of tkinds, tycs, and ltys *)
val tk_print   : tkind -> string
val tc_print   : tyc -> string
val lt_print   : lty -> string

(** adjusting an lty or tyc from one depth to another *)
val lt_adj     : lty * depth * depth -> lty
val tc_adj     : tyc * depth * depth -> tyc

val lt_adj_k   : lty * depth * depth * int -> lty
val tc_adj_k   : tyc * depth * depth * int -> tyc

(** finding out the depth for a tyc's innermost-bound free variables *)
val tc_depth : tyc * depth -> depth
val tcs_depth: tyc list * depth -> depth

(** automatically flattening the argument or the result type *)
val lt_autoflat : lty -> bool * lty list * bool

(** testing if a tyc is a unknown constructor *)
val tc_unknown : tyc -> bool

(** utility functions on tkindEnv *)
type tkindEnv
exception tkUnbound
val initTkEnv: tkindEnv
val tkLookup : tkindEnv * int * int -> tkind
val tkInsert : tkindEnv * tkind list -> tkindEnv

(** utility functions on tycEnv *)
type tycEnv = Lty.tycEnv
datatype teBinder = datatype Lty.teBinder
val teEmpty : tycEnv
val teCons : teBinder * tycEnv -> tycEnv

(** the ltyEnv maps from lvar to its lty; notice lty is depth-dependent *)
type ltyEnv
exception ltUnbound
val initLtyEnv : ltyEnv
val ltLookup : ltyEnv * LambdaVar.lvar * depth -> lty
val ltInsert : ltyEnv * LambdaVar.lvar * lty * depth -> ltyEnv

end (* signature LTYBASIC *)


