(* smlnj-pseudoOps.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * signature to expose the pseudo-op constructors
 *)

signature SMLNJ_PSEUDO_OPS =
  sig

    datatype smlnj_pseudo_op
      = JUMPTABLE of {base:Label.label, targets:Label.label list}
      | FILENAME of string

    include CLIENT_PSEUDO_OPS where type pseudo_op = smlnj_pseudo_op
  end
