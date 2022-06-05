(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* pflatten.sml *)

signature PFLATTEN =
sig
    type llty  = PLambda.lty
    type ltyc  = PLambda.tyc
    type flty  = FLINT.lty
    type ftyc  = FLINT.tyc
    type lexp  = FLINT.lexp
    type value = FLINT.value
    type lvar  = FLINT.lvar

    (* the following functions are used in flintnm.sml *)
    val ltc_raw      : llty -> flty
    val tcc_raw      : ltyc -> ftyc

    val t_pflatten   : llty -> bool * llty list * bool

    val v_punflatten : llty -> ((bool * llty list * bool) *
                                  ((lvar * lexp) -> (lvar list * lexp)))

    val v_pflatten   : llty -> ((bool * llty list * bool) *
                                  (value -> (value list * (lexp -> lexp))))

    (* the following functions are used during type specialization in FLINT *)
    val t_flatten    : (flty list * bool) -> bool * flty list * bool

    val v_unflatten  : (flty list * bool) -> 
                           ((bool * flty list * bool) *
                            ((lvar list * lexp) -> (lvar list * lexp)))

    val v_flatten    : (flty list * bool) -> 
                           ((bool * flty list * bool) *
                            (value list -> (value list * (lexp -> lexp))))

    (* the following function is used by representation analysis in FLINT *)
    val v_coerce     : bool * ftyc list * ftyc list -> 
                         (ftyc list * 
                          (value list -> (value list * (lexp -> lexp))) option)

end (* signature PFLATTEN *)




