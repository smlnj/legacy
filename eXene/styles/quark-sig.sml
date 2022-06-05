(* quark-sig.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Strings with fast inequality operations.  This should probably be replaced
 * with "names," but there are problems with creating names that are statically
 * initialized in CML.  Once there is a "CML shell," we can replace this by
 * the CML_Name structure.
 *)

signature QUARK =
  sig

    type quark

    val quark : string -> quark

    val stringOf : quark -> string

    val same : (quark * quark) -> bool

    val hash : quark -> word

    val cmp : (quark * quark) -> order

  end;
