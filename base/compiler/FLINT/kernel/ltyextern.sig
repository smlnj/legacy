(* ltyextern.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This interface hides the implementation details of FLINT tkind, tyc, and
 * lty defined inside Lty. For each entity, we provide a series of
 * constructor funtions, deconstructor functions, predicate functions,
 * and other utility functions. We divide these functions into three files:
 * LtyDef contains the set of abstract constructor, deconstructor, and
 * predicate functions for tkind, tyc, and lty; LtyBasic includes all
 * functions in LtyDef plus all commonly used primitive tycs and ltys, and
 * utility functions; finally, the current LtyExtern structure includes all
 * functions in LtyBasic plus a set of rather specialized utility functions.
 *
 * We design this hierarchy in a way so that LtyDef as a stable interface,
 * so one can refer to types such as tkind, tyc, lty as LtyDef.tkind, etc;
 * LtyBasic is a medium-stable interface, only commonly used functions
 * should be left here; LtyExtern is a least-stable interface, any new
 * utility function that manipulates types should go here.
 *
 * The conventions are (1) types should be referenced as "LtyDef.foo"
 * (2) functions should all be accessed as "LtyExtern.foo". The client
 * in general should never need to access LtyKernel.
 *
 * This interface should only refer to structures such as DebIndex, Lty,
 * PrimTyc, Symbol, and LtyBasic (indirectly LtyDef).
 *)

signature LTYEXTERN =
sig

(*
 * We left the definitions of tkind, tyc, and lty in a separate file, i.e.,
 * ltydef.sig and ltydef.sml. The idea is that those two files should change
 * very rarely while the interface for LtyExtern may change often. The client
 * should refer to LtyDef for the use of type names, and to LtyExtern for the
 * use of utility functions.
 *)
include LTYBASIC        (* see ltydef.sig and ltybasic.sig for details *)


(** tkind constructors *)
val tkc_mono   : tkind
val tkc_box    : tkind
val tkc_seq    : tkind list -> tkind
val tkc_fun    : tkind list * tkind -> tkind

(** tkind deconstructors *)
val tkd_mono   : tkind -> unit
val tkd_box    : tkind -> unit
val tkd_seq    : tkind -> tkind list
val tkd_fun    : tkind -> tkind list * tkind

(** tkind predicates *)
val tkp_mono   : tkind -> bool
val tkp_box    : tkind -> bool
val tkp_seq    : tkind -> bool
val tkp_fun    : tkind -> bool

(** tkind one-arm switch *)
val tkw_mono   : tkind * (unit -> 'a) * (tkind -> 'a) -> 'a
val tkw_box    : tkind * (unit -> 'a) * (tkind -> 'a) -> 'a
val tkw_seq    : tkind * (tkind list -> 'a) * (tkind -> 'a) -> 'a
val tkw_fun    : tkind * (tkind list * tkind -> 'a) * (tkind -> 'a) -> 'a

(** instantiating a polymorphic type or an higher-order constructor *)
val lt_inst     : lty * tyc list -> lty list
val lt_pinst    : lty * tyc list -> lty

val tkc_int : int -> tkind
val tkc_arg : int -> tkind list

exception KindChk of string (* kind checker exception *)
exception LtyAppChk

exception TeUnbound
exception TCENV

(* kind checking functions (re-exported here from Lty) *)
val tcKindCheckGen : unit -> (tkindEnv -> tyc -> tkind)
val tcKindVerifyGen : unit -> (tkindEnv -> (tkind * tyc) -> unit)
val ltKindCheckGen : unit -> (tkindEnv -> lty -> tkind)

(* perform polytype instantiation with kind checking *)
val lt_inst_chk_gen : unit -> lty * tyc list * tkindEnv -> lty list

(* substitution of named type variables *)
(*** CLEAN THIS UP ***)
val tc_nvar_elim_gen : unit -> (tvar * DebIndex.depth -> tyc option)
                            -> DebIndex.depth -> tyc -> tyc
val lt_nvar_elim_gen : unit -> (tvar * DebIndex.depth -> tyc option)
                            -> DebIndex.depth -> lty -> lty

(* !! BEWARE !!
 * The `subst' argument is assumed to be sorted with increasing tvars *)
val tc_nvar_subst_gen : unit -> (tvar * tyc) list -> tyc -> tyc
val lt_nvar_subst_gen : unit -> (tvar * tyc) list -> lty -> lty

val tc_nvar_cvt_gen : unit -> (tvar * int) list
                           -> DebIndex.depth -> tyc -> tyc
val lt_nvar_cvt_gen : unit -> (tvar * int) list
                           -> DebIndex.depth -> lty -> lty
(* The equivalent to ltc_poly for the nvar case *)
val lt_nvpoly : (tvar * tkind) list * lty list -> lty

(* special adjustment functions used during type specializations *)
val lt_sp_adj : tkind list * lty * tyc list * int * int -> lty
val tc_sp_adj : tkind list * tyc * tyc list * int * int -> tyc
val lt_sp_sink: tkind list * lty * depth * depth -> lty
val tc_sp_sink: tkind list * tyc * depth * depth -> tyc

(** utility functions used in CPS only, should go away soon ! *)
val lt_iscont   : lty -> bool
val ltw_iscont  : lty * (lty list -> 'a) * (tyc list -> 'a) * (lty -> 'a) -> 'a

(** other utility functions --- requires clean up!*)
val lt_select : lty * int -> lty
val lt_swap : lty -> lty

(** functions that manipulate the FLINT function and record types *)
val ltc_fkfun   : FLINT.fkind * lty list * lty list -> lty
val ltd_fkfun   : lty -> lty list * lty list (* fkind omitted *)

val ltc_rkind   : FLINT.rkind * lty list -> lty
val ltd_rkind   : lty * int -> lty

(** given a tyc, select the appropriate update primop *)
val tc_upd_prim : tyc -> Primop.primop

(** translating the tkind into the corresponding type *)
val tk_lty      : tkind -> lty

(** twrap type translation generator, used by Wrapping.wrapping *)
val twrap_gen   : bool -> ((tyc -> tyc) * (lty -> lty) *
                           (tyc -> tyc) * (lty -> lty) * (unit -> unit))

(** tnarrow type translation generator, used by Reify.reify *)
val tnarrow_gen : unit -> ((tyc -> tyc) * (lty -> lty) * (unit -> unit))

end (* signature LTYEXTERN *)
