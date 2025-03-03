(* ElabData/basics/new-lambdavar.sig
 *  -- replacement for ElabData/basics/lambdavar.sig
 *  -- optionally named and "prefixed" lambda variables
 * COPYRIGHT (c) 2020, 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

signature LAMBDA_VAR =
  sig

    (* type lvar will be abstract,
     * represented by {name : string option, prefix: string option, index: int} *)
    type lvar  

    val mkLvar : string option (* name *) -> string option (* prefix *) -> lvar
    (* create a fresh lvar, with an optional name or prefix, but not both! *)

    val index : lvar -> word

    val name : lvar -> string option
    val isNamed : lvar -> bool

    val toString: lvar-> string

    val mkLvar : string option -> string option -> lvar

(* following should be removed after any their uses are fixed
    val dupLvar : lvar -> lvar
    val namedLvar : Symbol.symbol -> lvar
    val lvarSym : lvar -> Symbol.symbol option
    val lvarName : lvar -> string   (* replaced by toString *)
*)
						       
    (* reset the index generator, lvarCount
     * this should be called (CompInfo.reset) for each compilation unit *)
    val reset : unit -> unit

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
