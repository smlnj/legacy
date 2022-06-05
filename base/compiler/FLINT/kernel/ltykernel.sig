(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* ltykernel.sig *)

signature LTYKERNEL =
sig 

type tkind = Lty.tkind
type fflag = Lty.fflag
type rflag = Lty.rflag
type tvar = Lty.tvar
type tyc = Lty.tyc
type lty = Lty.lty
type token = Lty.token
type tycEnv = Lty.tycEnv

exception TCENV
exception teUnbound2 

(** injections and projections on tkind, tyc, and lty *)
val tk_inj   : Lty.tkindI -> tkind 
val tc_inj   : Lty.tycI -> tyc
val lt_inj   : Lty.ltyI -> lty

val tk_out   : tkind -> Lty.tkindI
val tc_out   : tyc -> Lty.tycI
val lt_out   : lty -> Lty.ltyI

(** testing equivalence of tkinds, tycs, ltys, fflags, and rflags *)
val tk_eqv   : tkind * tkind -> bool
val tc_eqv   : tyc * tyc -> bool
val lt_eqv   : lty * lty -> bool
val ff_eqv   : fflag * fflag -> bool
val rf_eqv   : rflag * rflag -> bool

(** finding out the depth for a tyc's innermost-bound free variables *)
val tc_depth : tyc * DebIndex.depth -> DebIndex.depth
val tcs_depth: tyc list * DebIndex.depth -> DebIndex.depth
val tc_nvars : tyc -> tvar list
val lt_nvars : lty -> tvar list

(** utility functions for TC_ENV and LT_ENV types *)
val tcc_env  : tyc * int * int * tycEnv -> tyc
val ltc_env  : lty * int * int * tycEnv -> lty

(** reducing a tyc or lty into the weak-head normal form *)
val tc_whnm : tyc -> tyc
val lt_whnm : lty -> lty

(** reducing a tyc or lty into the true normal form *)
val tc_norm : tyc -> tyc
val lt_norm : lty -> lty

(** automatically flattening the argument or the result type *)
val lt_autoflat : lty -> bool * lty list * bool

(** testing if a tyc is a unknown constructor *)
val tc_unknown : tyc -> bool 

(** automatically tupling up the multiple argument/result into a single one *)
val tc_autotuple : tyc list -> tyc

(** tcc_arw does automatic argument and result flattening, so go away *)
val tcc_arw : fflag * tyc list * tyc list -> tyc

(** token-related functions *)
val token_name    : token -> string 
val token_abbrev  : token -> string            (* used by tc_print *)
val token_isvalid : token -> bool   
val token_eq      : token * token -> bool      
val token_int     : token -> int               (* for pickling *)
val token_key     : int -> token

(** primitive TC_WRAP constructor, built through the token facility *)
val wrap_token    : token

end (* signature LTYKERNEL *)
