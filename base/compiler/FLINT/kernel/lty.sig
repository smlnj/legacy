(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* lty.sig *)

signature LTY = sig 

(* definitions of kind and kind-environment *)
type tkind

datatype tkindI
  = TK_MONO                                   (* ground mono tycon *)
  | TK_BOX				      (* boxed/tagged tycon *)
  | TK_SEQ of tkind list                      (* sequence of tycons *)
  | TK_FUN of tkind list * tkind              (* tycon function *)

type enc_tvar
val tvEncode : int * int -> enc_tvar
val tvDecode : enc_tvar -> int * int

(* definitions of named tyc variables *)
type tvar = LambdaVar.lvar                    (* temporary, not used *)
val mkTvar : unit -> tvar                     (* temporary, not used *)

(* internal information in hash-consed tycs *)
datatype aux_info
  = AX_REG of bool                      (* normalization flag *)
            * enc_tvar list             (* free debruijn-indexed type vars *)
            * tvar list                 (* free named type vars *)
  | AX_NO                               (* no aux_info available *)

(* definitions of tyc and tyc-environment *)
type tyc

(* tycEnv is a sequence of tycEnvElems representing a substitution or
 * type environment produced by lazy beta-reductions or pushing through
 * a lambda abstraction. It is encoded as a tyc so that it can be hash-consed
 * using the tyc hash consing mechanism. 
 *)
type tycEnv 

(* tycEnvs are represented by an encoding as tycs. The abstract representation
 * of tycEnvs would be given by:
 *
 *   datatype teBinder
 *     = Beta of int * tyc list * tkind list
 *     | Lamb of int * tkind list
 *
 *   type tycEnv = teBinder list
 *
 * Invariant: a tycEnv cannot terminate with a Lamb, i.e. the last binder
 *   in a tycEnv must be a Beta. tycEnvs are created when a closure is created
 *   when reducing a beta-redex (rule r1), and they are always initially of
 *   of the form Beta(0,args,ks)::nil.
 *)
             
datatype teBinder
  = Beta of int * tyc list * tkind list
      (* Beta(j,args,ks):
         created when reducing a beta redex (r1);
         j: the embedding level of the original redex -- 0 if the redex was
            created by r1, or the nesting level of the new closure if by r12;
         args: the tycs bound by the n-ary beta reduction, i.e. the arguments;
         ks: the operator domain kinds *)
  | Lamb of int * tkind list
      (* Lamb(j,ks):
         created when pushing a closure (Env) through a lambda (r10);
         j: the nesting level of the closure just before r10 is applied,
            i.e. the nesteing level of the abstraction relative to the
            point where the closure was originally created;
         ks: the kinds of the abstraction parameters *)

val teToBinders : tycEnv -> teBinder list

(* token: a hook to add new tyc *)
type token                                    
type token_info  
  = {name      : string, 
     abbrev    : string,
     reduce_one: token * tyc -> tyc,
     is_whnm   : tyc -> bool,
     is_known  : token * tyc -> bool}

datatype fflag                                (* calling conventions *)
  = FF_VAR of bool * bool                     (* is it fixed ? *)
  | FF_FIXED                                  (* used after rep. analysis *)

datatype rflag = RF_TMP                       (* tuple kind: a template *)

datatype tycI
  = TC_VAR of DebIndex.index * int            (* tyc variable *)
  | TC_NVAR of tvar                           (* named tyc variable *)
  | TC_PRIM of PrimTyc.primtyc                (* primitive tyc *)
  | TC_FN of tkind list * tyc                 (* tyc abstraction *)
  | TC_APP of tyc * tyc list                  (* tyc application *)
  | TC_SEQ of tyc list                        (* tyc sequence *)
  | TC_PROJ of tyc * int                      (* tyc projection *)

  | TC_SUM of tyc list                        (* sum tyc *)
  | TC_FIX of {family: {size: int,            (* recursive tyc *) 
                        names: string vector,
                        gen : tyc,
                        params : tyc list},
               index: int}

  | TC_TUPLE of rflag * tyc list              (* std record tyc *)
  | TC_ARROW of fflag * tyc list * tyc list   (* std function tyc *)
  | TC_PARROW of tyc * tyc                    (* special fun tyc, not used *)

  | TC_BOX of tyc                             (* boxed tyc *)
  | TC_ABS of tyc                             (* abstract tyc *)
  | TC_TOKEN of token * tyc                   (* extensible token tyc *)
  | TC_CONT of tyc list                       (* std continuation tyc *)
  | TC_IND of tyc * tycI                      (* indirect tyc thunk *)
  | TC_ENV of tyc * int * int * tycEnv        (* tyc closure *)

(* definition of lty *)
type lty
datatype ltyI          
  = LT_TYC of tyc                             (* monomorphic type *)  
  | LT_STR of lty list                        (* structure type *)
  | LT_FCT of lty list * lty list             (* functor type *)
  | LT_POLY of tkind list * lty list          (* polymorphic type *)
    
  | LT_CONT of lty list                       (* internal cont type *)
  | LT_IND of lty * ltyI                      (* indirect type thunk *)
  | LT_ENV of lty * int * int * tycEnv        (* type closure *)

(** injections and projections on tkind, tyc, and lty *)
val tk_injX   : tkindI -> tkind 
val tc_injX   : tycI -> tyc
val lt_injX   : ltyI -> lty

val tk_outX   : tkind -> tkindI
val tc_outX   : tyc -> tycI
val lt_outX   : lty -> ltyI

(** key comparison for tkind, tyc, and lty; used in pickling *)
val tk_cmp   : tkind * tkind -> order
val tc_cmp   : tyc * tyc -> order
val lt_cmp   : lty * lty -> order

(** get the hash key of each lty, used by reps/coerce.sml; a hack! *)
val lt_key   : lty -> int

(** utility functions on tycEnv *)

val teEmpty : tycEnv
val teCons : teBinder * tycEnv -> tycEnv
val teDest : tycEnv -> (teBinder * tycEnv) option
val teLookup : tycEnv * int -> teBinder option
val teLength : tycEnv -> int

(** utility functions on tkindEnv *)
type tkindEnv 
exception tkUnbound
val initTkEnv        : tkindEnv
val tkLookup         : tkindEnv * int * int -> tkind
val tkInsert         : tkindEnv * tkind list -> tkindEnv

(* token functions *)
val register_token : token_info -> token
val token_name    : token -> string
val token_abbrev  : token -> string
val token_whnm    : token -> tyc -> bool
val token_reduce  : token * tyc -> tyc
val token_isKnown : token * tyc -> bool
val token_isvalid : token -> bool
val token_eq      : token * token -> bool
val token_int     : token -> int
val token_key     : int -> token

(* simple equality operations *)
val tk_eq : tkind * tkind -> bool
val tc_eq : tyc * tyc -> bool
val lt_eq : lty * lty -> bool

(** updating tycs and ltys *)
val tyc_upd : tyc * tyc -> unit
val lty_upd : lty * lty -> unit

(** testing if a tyc or lty is in normal form *)
val tcp_norm : tyc -> bool
val ltp_norm : lty -> bool

(** accessing free deBruijn tyvars *)
val tc_vs : tyc -> enc_tvar list option
val lt_vs : lty -> enc_tvar list option

(** accessing free named tyvars *)
val tc_nvars : tyc -> tvar list
val lt_nvars : lty -> tvar list

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

(** subkind relation **)
val tksSubkind : tkind list * tkind list -> bool
val tkSubkind : tkind * tkind -> bool

(** utility functions for constructing tkinds *)
val tkc_fn1 : tkind
val tkc_fn2 : tkind
val tkc_fn3 : tkind

val tkc_int : int -> tkind
val tkc_arg : int -> tkind list

(* is a kind monomorphic? *)
val tkIsMono : tkind -> bool

end (* signature LTY *)
