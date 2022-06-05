(* ord-key-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Abstract linearly ordered keys.
 *
 *)

signature ORD_KEY =
  sig
    type ord_key

    val cmpKey : ord_key * ord_key -> LibBase.relation
      (* cmpKey (v,v') = Equal   if v = v'
       *               = Less    if v < v'
       *               = Greater if v > v'
       *)

  end (* ORD_KEY *)
