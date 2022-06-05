(* Copyright (c) 1998 YALE FLINT PROJECT *)
(* ltydef.sig *)

(*
 * This interface hides the implementation details of FLINT tkind, tyc, and 
 * lty defined inside Lty. For each entity, we provide a series of 
 * constructor funtions, deconstructor functions, predicate functions, and
 * functions that test equivalence and do pretty-printing. This interface 
 * should only refer to DebIndex, Lty, PrimTyc, and Symbol. 
 *)

signature LTYDEF = 
sig

(** basic entities *)
type index = DebIndex.index
type depth = DebIndex.depth
type primtyc = PrimTyc.primtyc
type tvar = Lty.tvar

type fflag = Lty.fflag 
type rflag = Lty.rflag

type tkind = Lty.tkind
type tyc = Lty.tyc
type lty = Lty.lty

(* 
 * FLINT tkind is roughly equivalent to the following ML datatype 
 *
 *    datatype tkind 
 *      = TK_MONO 
 *      | TK_BOX
 *      | TK_SEQ of tkind list
 *      | TK_FUN of tkind * tkind
 *
 * We treat tkind as an abstract type so we can no longer use 
 * pattern matching. 
 *
 *)

(* 
 * FLINT fflag and rflag are used to classify different kinds of monomorphic 
 * functions and records. As of now, they are roughly equivalent to:
 *
 *    datatype fflag
 *      = FF_VAR of bool * bool
 *      | FF_FIXED
 *
 *    datatype rflag = RF_TMP
 *
 * We treat both as abstract types so pattern matching no longer applies.
 * NOTE: FF_VAR flags are used by FLINTs before we perform representation
 * analysis while FF_FIXED is used by FLINTs after we perform representation
 * analysis. 
 *)

(** fflag and rflag constructors *)
val ffc_var    : bool * bool -> fflag
val ffc_fixed  : fflag
val rfc_tmp    : rflag

(** fflag and rflag deconstructors *)
val ffd_var    : fflag -> bool * bool
val ffd_fixed  : fflag -> unit
val rfd_tmp    : rflag -> unit

(** fflag and rflag predicates *)
val ffp_var    : fflag -> bool
val ffp_fixed  : fflag -> bool
val rfp_tmp    : rflag -> bool

(** fflag and rflag one-arm switch *)
val ffw_var    : fflag * (bool * bool -> 'a) * (fflag -> 'a) -> 'a
val ffw_fixed  : fflag * (unit -> 'a) * (fflag -> 'a) -> 'a
val rfw_tmp    : rflag * (unit -> 'a) * (rflag -> 'a) -> 'a


(* 
 * FLINT tyc is roughly equivalent to the following ML datatype 
 *
 *    datatype tyc
 *      = TC_VAR of index * int
 *      | TC_NVAR of tvar
 *      | TC_PRIM of primtyc
 *      | TC_FN of tkind list * tyc
 *      | TC_APP of tyc * tyc list
 *      | TC_SEQ of tyc list
 *      | TC_PROJ of tyc * int
 *      | TC_SUM of tyc list
 *      | TC_FIX of tyc * int
 *      | TC_WRAP of tyc                   (* used after rep. analysis only *)
 *      | TC_ABS of tyc                    (* NOT USED *)
 *      | TC_BOX of tyc                    (* NOT USED *)
 *      | TC_TUPLE of tyc list             (* rflag hidden *)
 *      | TC_ARROW of fflag * tyc list * tyc list 
 *
 * We treat tyc as an abstract type so we can no longer use 
 * pattern matching. Type applications (TC_APP) and projections 
 * (TC_PROJ) are automatically reduced as needed, that is, the
 * client does not need to worry about whether a tyc is in the
 * normal form or not, all functions defined here automatically 
 * take care of this.
 *)

(** tyc constructors *)
val tcc_var    : index * int -> tyc
val tcc_nvar   : tvar -> tyc
val tcc_prim   : primtyc -> tyc
val tcc_fn     : tkind list * tyc -> tyc
val tcc_app    : tyc * tyc list -> tyc
val tcc_seq    : tyc list -> tyc
val tcc_proj   : tyc * int -> tyc
val tcc_sum    : tyc list -> tyc
val tcc_fix    : (int * string vector * tyc * tyc list) * int -> tyc 
val tcc_wrap   : tyc -> tyc
val tcc_abs    : tyc -> tyc
val tcc_box    : tyc -> tyc
val tcc_tuple  : tyc list -> tyc
val tcc_arrow  : fflag * tyc list * tyc list -> tyc

(** tyc deconstructors *)
val tcd_var    : tyc -> index * int 
val tcd_nvar   : tyc -> tvar
val tcd_prim   : tyc -> primtyc 
val tcd_fn     : tyc -> tkind list * tyc 
val tcd_app    : tyc -> tyc * tyc list 
val tcd_seq    : tyc -> tyc list 
val tcd_proj   : tyc -> tyc * int 
val tcd_sum    : tyc -> tyc list 
val tcd_fix    : tyc -> (int * tyc * tyc list) * int 
val tcd_wrap   : tyc -> tyc
val tcd_abs    : tyc -> tyc 
val tcd_box    : tyc -> tyc 
val tcd_tuple  : tyc -> tyc list 
val tcd_arrow  : tyc -> fflag * tyc list * tyc list 

(** tyc predicates *)
val tcp_var    : tyc -> bool
val tcp_nvar   : tyc -> bool
val tcp_prim   : tyc -> bool
val tcp_fn     : tyc -> bool
val tcp_app    : tyc -> bool
val tcp_seq    : tyc -> bool
val tcp_proj   : tyc -> bool
val tcp_sum    : tyc -> bool
val tcp_fix    : tyc -> bool
val tcp_wrap   : tyc -> bool
val tcp_abs    : tyc -> bool
val tcp_box    : tyc -> bool
val tcp_tuple  : tyc -> bool
val tcp_arrow  : tyc -> bool

(** tyc one-arm switch *)
val tcw_var    : tyc * (index * int -> 'a) * (tyc -> 'a) -> 'a
val tcw_nvar   : tyc * (tvar -> 'a) * (tyc -> 'a) -> 'a
val tcw_prim   : tyc * (primtyc -> 'a) * (tyc -> 'a) -> 'a
val tcw_fn     : tyc * (tkind list * tyc -> 'a) * (tyc -> 'a) -> 'a
val tcw_app    : tyc * (tyc * tyc list -> 'a) * (tyc -> 'a) -> 'a
val tcw_seq    : tyc * (tyc list -> 'a) * (tyc -> 'a) -> 'a
val tcw_proj   : tyc * (tyc * int -> 'a) * (tyc -> 'a) -> 'a
val tcw_sum    : tyc * (tyc list -> 'a) * (tyc -> 'a) -> 'a
val tcw_fix    : tyc * ((int * tyc * tyc list) * int -> 'a) * (tyc -> 'a) -> 'a
val tcw_wrap   : tyc * (tyc -> 'a) * (tyc -> 'a) -> 'a
val tcw_abs    : tyc * (tyc -> 'a) * (tyc -> 'a) -> 'a
val tcw_box    : tyc * (tyc -> 'a) * (tyc -> 'a) -> 'a
val tcw_tuple  : tyc * (tyc list -> 'a) * (tyc -> 'a) -> 'a
val tcw_arrow  : tyc * (fflag * tyc list * tyc list -> 'a) 
                     * (tyc -> 'a) -> 'a
                                      

(* 
 * FLINT lty is roughly equivalent to the following ML datatype 
 *
 *    datatype lty
 *      = LT_TYC of tyc
 *      | LT_STR of lty list
 *      | LT_FCT of lty list * lty list
 *      | LT_POLY of tkind list * lty list
 *
 * We treat lty as an abstract type so we can no longer use pattern
 * matching. The client does not need to worry about whether an lty
 * is in normal form or not. 
 *)

(** lty constructors *)
val ltc_tyc    : tyc -> lty
val ltc_str    : lty list -> lty
val ltc_fct    : lty list * lty list -> lty
val ltc_poly   : tkind list * lty list -> lty    

exception DeconExn

(** lty deconstructors *)
val ltd_tyc    : lty -> tyc
val ltd_str    : lty -> lty list
val ltd_fct    : lty -> lty list * lty list
val ltd_poly   : lty -> tkind list * lty list

(** lty predicates *)
val ltp_tyc    : lty -> bool
val ltp_str    : lty -> bool
val ltp_fct    : lty -> bool
val ltp_poly   : lty -> bool

(** lty one arm switches *)
val ltw_tyc    : lty * (tyc -> 'a) * (lty -> 'a) -> 'a
val ltw_str    : lty * (lty list -> 'a) * (lty -> 'a) -> 'a
val ltw_fct    : lty * (lty list * lty list -> 'a) * (lty -> 'a) -> 'a
val ltw_poly   : lty * (tkind list * lty list -> 'a) * (lty -> 'a) -> 'a
                                        

(* 
 * Because FLINT tyc is embedded inside FLINT lty, we supply the
 * the following utility functions on building ltys that are based
 * on simple mono tycs.
 *)

(** tyc-lty constructors *)
val ltc_var    : index * int -> lty
val ltc_prim   : primtyc -> lty
val ltc_tuple  : lty list -> lty
val ltc_arrow  : fflag * lty list * lty list -> lty

(** tyc-lty deconstructors *)
val ltd_var    : lty -> index * int
val ltd_prim   : lty -> primtyc
val ltd_tuple  : lty -> lty list
val ltd_arrow  : lty -> fflag * lty list * lty list

(** tyc-lty predicates *)
val ltp_var    : lty -> bool
val ltp_prim   : lty -> bool
val ltp_tuple  : lty -> bool
val ltp_arrow  : lty -> bool

(** tyc-lty one-arm switches *)
val ltw_var    : lty * (index * int -> 'a) * (lty -> 'a) -> 'a
val ltw_prim   : lty * (primtyc -> 'a) * (lty -> 'a) -> 'a
val ltw_tuple  : lty * (tyc list -> 'a) * (lty -> 'a) -> 'a
val ltw_arrow  : lty * (fflag * tyc list * tyc list -> 'a) 
                     * (lty -> 'a) -> 'a


(* 
 * The following functions are written for CPS only. If you are writing
 * code for FLINT, you should not use any of these functions. 
 * The continuation referred here is the internal continuation introduced
 * via CPS conversion; it is different from the source-level continuation 
 * ('a cont) or control continuation ('a control-cont) where are represented 
 * as PT.ptc_cont and PT.ptc_ccont respectively. 
 *
 *)

(** cont-tyc-lty constructors *)
val tcc_cont   : tyc list -> tyc      
val ltc_cont   : lty list -> lty                

(** cont-tyc-lty deconstructors *)
val tcd_cont   : tyc -> tyc list      
val ltd_cont   : lty -> lty list        

(** cont-tyc-lty predicates *)
val tcp_cont   : tyc -> bool          
val ltp_cont   : lty -> bool            

(** cont-tyc-lty one-arm switches *)
val tcw_cont   : tyc * (tyc list -> 'a) * (tyc -> 'a) -> 'a
val ltw_cont   : lty * (lty list -> 'a) * (lty -> 'a) -> 'a


(* 
 * The following functions are written for PLambdaType only. If you
 * are writing code for FLINT only, don't use any of these functions. 
 * The idea is that in PLambda, all (value or type) functions have single
 * argument and single return-result. Ideally, we should define 
 * another sets of datatypes for tycs and ltys. But we want to avoid
 * the translation from PLambdaType to FLINT types, so we let them
 * share the same representations as much as possible. 
 *
 * Ultimately, LtyDef should be separated into two files: one for 
 * FLINT, another for PLambda, but we will see if this is necessary.
 *
 *)

(** plambda tyc-lty constructors *)
val tcc_parrow : tyc * tyc -> tyc     
val ltc_parrow : lty * lty -> lty
val ltc_ppoly  : tkind list * lty -> lty  
val ltc_pfct   : lty * lty -> lty         

(** plambda tyc-lty deconstructors *)
val tcd_parrow : tyc -> tyc * tyc
val ltd_parrow : lty -> lty * lty    
val ltd_ppoly  : lty -> tkind list * lty
val ltd_pfct   : lty -> lty * lty       

(** plambda tyc-lty predicates *)
val tcp_parrow : tyc -> bool          
val ltp_parrow : lty -> bool
val ltp_ppoly  : lty -> bool
val ltp_pfct   : lty -> bool            

(** plambda tyc-lty one-arm switches *)
val tcw_parrow : tyc * (tyc * tyc -> 'a) * (tyc -> 'a) -> 'a
val ltw_parrow : lty * (tyc * tyc -> 'a) * (lty -> 'a) -> 'a
val ltw_ppoly  : lty * (tkind list * lty -> 'a) * (lty -> 'a) -> 'a
val ltw_pfct   : lty * (lty * lty -> 'a) * (lty -> 'a) -> 'a

end (* signature LTYDEF *)
