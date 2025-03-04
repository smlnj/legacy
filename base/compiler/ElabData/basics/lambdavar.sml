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

  val indexCount: word ref = ref 0w0

  (* reset : unit -> unit -- resets lvar generating count to 0 *)
  fun reset () = (lvarCount := 0w0)

  (* newIndex: unit -> word *)
  fun newIndex () =
      let val new = !lindexCount + 0w1
       in indexCount := new; new
      end	 

  (* mkLvar : string option -> lvar
     The string option is used to define the name of the lvar and can be viewed either
     as the _name_ of the lvar or as a prefix string.  In either case, it is prepended to
     the string representing the value of the index field. *)
  fun mkLvar (nameOp: string option) : lvar =
      {name = nameOp, index = newIndex()}

  (* dupLvar : lvar -> lvar
   * Create a new lvar, with a new index, but sharing the same name as the argument lvar.
   * If the name field of the argument is NONE (i.e., if the argument lvar is unnamed), 
   * then this produces the same result as mkLvar NONE. *)
  fun dupLvar ({name,...}: lvar) = mkLvar name

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
  fun toString ({name,index}: lvar) =
      (case name of NONE => "V" | SOME s => s) ^ Int.toString index)


  (* deviations from the old LambdaVar structure *)

  (* sameName' : lvar * lvar -> lvar * lvar
   * Here is a functional version of the old LambdaVar.sameName. This version is purely
   * functional and thus has a different type and behavior from the old LambdaVar.sameName,
   * so this won't work as a replacement for the old LambdaVar.sameName, which works by
   * side-effecting the hidden name hash table!
   * The old version was used in CPS/convert and CPS/opt. But I conjecture that the
   * dummy version of sameName given below, which does nothing, will work as a replacement
   * for the old sameName, which never seemed to be "activated" by setting the control
   * flag "saveLvarNames" to true.  The "saveLvarNames" flag has been deleted. *)
  fun sameName' (lv1 as {name=nameOp1, index=index1}: lvar,
		lv2 as {name=nameOp2, index=index2}: lvar) : bool =
      (case nameOp2
         of SOME _ => ({name=nameOp2, index=index1}, lv2)
	  | NONE => (case nameOp1
		       of SOME _ => (lv1, {name=nameOp1, index=index2})
		        | NONE => (lv1, lv2)

  (* sameName : lvar * lvar -> unit
     A dummy version of sameName that does nothing.  This is equivalent to the old
     sameName function when the control flag "saveLvarNames" is false, which it is 
     by default (and is not set to true in any of the compiler code?) *)
  fun sameName (x: lvar * lvar) = ()
  
  (* the following two function toID and fromID are preserved because of their use in pickling *)

  (* toId: lvar -> int *)
  fun toId (lvar: lvar) = Word.toInt (index lvar)

  (* fromId: int -> lvar *)
  fun fromId (n: int) : lvar = {name = NONE, index = Word.fromInt n}


  (* comparison of lvars is done by comparing values of their index fields *)

  (* compare : lvar * lvar -> order *)
  fun compare ({index = w1, ...}: lvar, {index=w2, ...} : lvar) = Word.compare (w1, w2)

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

(* lvar sets, finite maps, and hash tables (added by JRH at some point long ago).
 * The sets and maps use the RedBlack functors from the smlnj-lib/Util library.  *)

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
