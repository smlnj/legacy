(* ElabData/basics/tyconvar.sig
 *  -- replacement for ElabData/basics/lambdavar.sig
 *  -- optionally named and "prefixed" lambda variables
 * COPYRIGHT (c) 2020, 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

signature TYCON_VAR =
sig

  (* type tycvar will be abstract,
   * represented by {name : string option, prefix: string option, index: int} *)
  type tycvar  

  val mkTycvar : string option (* name or prefix *) -> tycvar
  (* create a fresh tycvar, with an optional name/prefix *)

  (* reset the index generator, internal indexCount : word ref.
   * This should be called (using CompInfo.reset) for each compilation unit. *)
  val reset : unit -> unit

  val index : tycvar -> word

  val name : tycvar -> string option

  val isNamed : tycvar -> bool

  val toString: tycvar-> string

  val mkTycvar : string option -> tycvar

 (* comparison, equality, and ordering of tycvars *)
  val compare : tycvar * tycvar -> order  (* comparison based on the index fields *)

  (* equality ("same"), <, and > are derived from compare, and so are redundant *)
  val same : tycvar * tycvar -> bool
  val < : tycvar * tycvar -> bool
  val > : tycvar * tycvar -> bool

  structure Set : ORD_SET where type Key.ord_key = tycvar
  (* tycvar sets; replaces the redundant SortedList substructure *)

  structure Map : ORD_MAP where type Key.ord_key = tycvar
  (* tycvar finite maps -- used extensively in FLINT optimization phases *)

  structure Tbl : MONO_HASH_TABLE where type Key.hash_key = lvar
  (* lvar hash tables -- used extensively in FLINT optimization phases. *)

end (* signature TYCON_VAR *)
