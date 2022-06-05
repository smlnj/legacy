(* frag.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * code and data fragments that need to be compiled.
 *)

signature FRAG = sig

    structure T : MLTREE

    datatype generated
      = UNGEN of CPS.lvar * CPS.lvar list * CPS.cty list * CPS.cexp
      | GEN of T.mlrisc list

    datatype frag
      = STANDARD of {func: CPS.function option ref,
		     fmlTyps: CPS.cty list}
      | KNOWNFUN of generated ref
      | KNOWNCHK of generated ref

    val makeFrag : CPS.function * Label.label -> frag
    val next : unit -> (Label.label * frag) option
    val add : (Label.label * frag) -> unit

  end (* FRAG *)
