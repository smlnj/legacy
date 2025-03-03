(* ElabData/basics/new-lambdavar.sml
 *  -- replacement for ElabData/basics/lambdavar.sml
 *  -- optionally named and "prefixed" lambda variables
 * COPYRIGHT (c) 2020, 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure LambdaVar :> LAMBDA_VAR =
struct

  structure S = Symbol
  structure SetFn = RedBlackSetFn

  type lvar = {name: string option, prefix: string option, index: word}

  val lvarCount: word ref = ref 0w0

  (* reset : unit -> unit -- resets lvar generating count to 0 *)
  fun reset () = (lvarCount := 0w0)

  (* newIndex: unit -> int *)
  fun newIndex () =
      let val new = !lvarCount + 0w1
       in lvarCount := new; new
      end	 

  (* mkLvar : string option -> string option -> lvar
     At least one of the string options shoule be NONE *)
  fun mkLvar (nameOp: string option, prefixOp: string option) : lvar =
      {name = nameOp, prefix = prefixOp, index = newIndex()}

  (* index : lvar -> word *)
  fun index ({index,...}: lvar)	= index

  (* name : lvar -> string option *)
  fun name ({name, ...}: lvar) = name

  (* isNamed : lvar -> bool *)
  fun isNamed ({name,...}: lvar) = 
      case name
        of NONE => false
	 |  _ => true

  (* toString: lvar -> string *)
  fun toString ({name,prefix,index}: lvar) =
      (case name
	 of NONE =>
	    (case prefix
	       of NONE => "V" ^ Int.toString index
		| SOME p => p ^ Int.toString index)
	  | SOME s => s)


  (* comparison of lvars is done by comparing values of their index fields *)

  (* compare : lvar * lvar -> order *)
  fun compare ({index = i1, ...}: lvar, {index=i2, ...} : lvar) = Word.compare (i1, i2)

  (* same : lvar * lvar -> bool -- the name "same" should be changed to "equal" *)
  fun same (lv1, lv2) =
      (case compare (lv1, lv2)
         of EQUAL => true
          | _ => false)

  (* < : lvar * lvar -> bool *)
  val op < : lvar * lvar -> bool =
      (fn (lv1, lv2) =>
	  (case compare (lv1, lv2)
             of LESS => true
              | _ => false))

  (* > : lvar * lvar -> bool *)
  val op < : lvar * lvar -> bool =
      (fn (lv1, lv2) =>
	  (case compare (lv1, lv2)
             of GREATER => true
              | _ => false))

(* Deleted functions; redundant or not needed -- find and fix all occurrences where these are called
  sameName : lvar * lvar -> unit
  dupLvar : lvar -> lvar 
  namedLvar : S.symbol -> lvar
  lvarSym : lvar -> S.symbol option
  lvarName : lvar -> string --  duplicates the toString function above 
  val toId = Fn.id
  val fromId = Fn.id
*)

(* lvar sets, finite maps, and hash tables *)

  structure OrdKey : ORD_KEY =
  struct
    type ord_key = lvar
    val compare = compare
  end (* structure LvarKey *)

  structure HashKey : ORD_KEY =
  struct
    type hash_key = lvar
    val hashVal = index
    val sameKey = same
  end (* structure LvarKey *)

  structure Set : ORD_SET where type Key.ord_key = lvar =
    RebBlackSetFn (OrdKey)

  (* LambdaVar.Map is used in several FLINT files *)
  structure Map : ORD_MAP where type Key.ord_key = lvar =
    RebBlackSetFn (OrdKey)

  (* LambdaVar.Tbl is used in several FLINT files *)
  structure Tbl : MONO_HASH_TABLE where Key.hash_key = lvar =
    HashTableFn (HashKey)  

end (* structure LambdaVar *)
