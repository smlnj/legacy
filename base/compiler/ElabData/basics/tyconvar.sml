(* ElabData/basics/tyconvar.sml
 *  -- a new implementation of generalized _tycon_ variables, called tycvars
 *  -- tycon variables (type tycvar) can optionally be named or prefixed
 * COPYRIGHT (c) 2020, 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure TyconVar :> TYCON_VAR =
struct

  type tycvar = {name: string option, index: word}

  val indexCount: word ref = ref 0w0

  (* reset : unit -> unit -- resets lvar generating count to 0 *)
  fun reset () = (indexCount := 0w0)

  (* newIndex: unit -> word *)
  fun newIndex (): word =
      let val new = !indexCount + 0w1
       in indexCount := new; new
      end	 

  (* mkTycvar : string option -> tycvar *)
  fun mkTycvar (nameOp: string option) : tycvar =
      {name = nameOp, index = newIndex()}

  (* index : tycvar -> word *)
  fun index ({index,...}: tycvar) = index

  (* name : tycvar -> string option *)
  fun name ({name, ...}: tycvar) = name

  (* isNamed : tycvar -> bool *)
  fun isNamed ({name,...}: tycvar) = 
      (case name of NONE => false | _ => true)

  (* toString: tycvar -> string *)
  fun toString ({name, index}: tycvar) =
      (case name of NONE => "TV" | SOME s => s) ^ Word.toString index)


  (* comparison of tycvars is done by comparing values of their index fields *)

  (* compare : tycvar * tycvar -> order *)
  fun compare ({index = i1, ...}: tycvar, {index=i2, ...} : tycvar) = Word.compare (i1, i2)

  (* the following are redundant, since they are derived from the compare funtion *)

  (* same : tycvar * tycvar -> bool -- the name "same" should be changed to "equal" *)
  fun same (lv1, lv2) =
      (case compare (lv1, lv2)
         of EQUAL => true
          | _ => false)

  (* < : tycvar * tycvar -> bool *)
  val op < : tycvar * tycvar -> bool =
      (fn (lv1, lv2) =>
	  (case compare (lv1, lv2)
             of LESS => true
              | _ => false))

  (* > : tycvar * tycvar -> bool *)
  val op < : tycvar * tycvar -> bool =
      (fn (lv1, lv2) =>
	  (case compare (lv1, lv2)
             of GREATER => true
              | _ => false))


(* tycvar sets, finite maps, and hash tables *)

  structure OrdKey : ORD_KEY =
  struct
    type ord_key = tycvar
    val compare = compare
  end (* structure TycvarKey *)

  structure HashKey : ORD_KEY =
  struct
    type hash_key = tycvar
    val hashVal = index
    val sameKey = same
  end (* structure TycvarKey *)

  (* sets of tycvars *)
  structure Set : ORD_SET where type Key.ord_key = tycvar =
    RebBlackSetFn (OrdKey)

  (* finite maps over tycvars *)
  structure Map : ORD_MAP where type Key.ord_key = tycvar =
    RebBlackSetFn (OrdKey)

  (* hash tables over tycvars *)
  structure Tbl : MONO_HASH_TABLE where Key.hash_key = tycvar =
    HashTableFn (HashKey)  

end (* structure TyconVar *)
