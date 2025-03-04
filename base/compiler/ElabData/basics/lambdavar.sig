(* ElabData/basics/new-lambdavar.sig
 *  -- replacement for ElabData/basics/lambdavar.sig
 *  -- optionally named and "prefixed" lambda variables
 * COPYRIGHT (c) 2020, 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

signature LAMBDA_VAR =
sig

  (* type lvar will be abstract, but its representation is the following record. *)
  type lvar  (* = {name : string option, index: word} *)

  val mkLvar : string option (* name of prefix string *) -> lvar
  (* create a fresh lvar, with an optional name or prefix appended to the string for the index *)

  val index : lvar -> word

  val name : lvar -> string option
  val isNamed : lvar -> bool

  val toString: lvar-> string

  val mkLvar : string option -> string option -> lvar

  val dupLvar : lvar -> lvar
      (* creates a fresh lvar (new index) but uses the name from the argument, if it exists *)

  val sameName : lvar * lvar -> unit
    (* This is a dummy version of the sameName function in the old LambdaVar
     * structure that worked by side-effecting the internal name hash table. This dummy
     * version does nothing, and so it matches the semantics of the old sameName function
     * when the control flag "saveLvarNames" is false, which it is by default.

  (* reset the index generator, lvarCount
   * this should be called (CompInfo.reset) for each compilation unit *)
  val reset : unit -> unit

  (* following included by request of JHR because they are used in pickling *)
  val toID : lvar -> int
  val fromID : int -> lvar
      (* works even for negative ints, though probably it should not *)

 (* comparison, equality, ordering of lvars *)
  val compare : lvar * lvar -> order (* comparison based on index fields *)
  val same : lvar * lvar -> bool  (* equality based in index fields *)
  val < : lvar * lvar -> bool
  val > : lvar * lvar -> bool

  structure Set : ORD_SET where type Key.ord_key = lvar
  (* lvar sets; replaces the redundant SortedList substructure *)

  structure Map : ORD_MAP where type Key.ord_key = lvar
  (* lvar finite maps -- used extensively in FLINT optimization phases *)

  structure Tbl : MONO_HASH_TABLE where type Key.hash_key = lvar
  (* lvar hash tables -- used extensively in FLINT optimization phases. *)

end (* signature LAMBDA_VAR *)
