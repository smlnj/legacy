(* Elaborator/elaborate/tyvarset.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (www.smlnj.org)
 *)

 (* Managing sets of UBOUND type variables to infer their scopes and virtual bindings.
  * [DBM, 2025.03.05] This structure has been revised to not have the functions 
  * union and diff generate error messages directly through the mem
  * function, but to instead signal errors (due to incompatible tyvars with the same
  * name) by returning option valueso.
 *)

signature TYVARSET = 
sig 

  type tyvarset
  val empty : tyvarset
  val singleton : Types.tyvar -> tyvarset
  val mkTyvarset : Types.tyvar list -> tyvarset
  val add : tyvarset * Types.tyvar -> tyvarset option
  val union : tyvarset * tyvarset -> tyvarset option
  val diff : tyvarset * tyvarset -> tyvarset option
  val diffPure : tyvarset * tyvarset -> tyvarset
  val elements: tyvarset -> Types.tyvar list

end (* signature TYVARSET *)

(* DBM: add, union, and diff can fail, returning NONE, because of incompatible tyvars
 * in the tyvarsets that share names.
 * 
 * INVARIANTS: It is assumed that:
 * (1) all tyvars in a tyvars are UBOUND, and 
 * (2) names of tyvars are unique in a given tyvarset.
 * 
 * Two tyvars sharing the same name are _incompatible_ if they have different
 * equality properties or different (deBruijn) depths.
 *
 * The type TyvarSet.tvarset is abstract, and the only operations for constructing
 * tyvarsets are singleton, union, diff, and diffPure
 *
 *)

structure TyvarSet :> TYVARSET =
struct

local (* structure imports *)

  structure EM = ErrorMsg
  structure T = Types

in

fun bug msg = ErrorMsg.impossible("TyvarSet: "^ msg)

type tyvarset = T.tyvar list
(* INVARIANT 1: all tyvars in a tyvar set should be of kind UBOUND
 * INVARIANT 2: (Uniqueness of names) different tyvars in a tyvar set should have different names. *)

val empty = nil
(* singleton : T.tyvar -> tyvarset *)
fun singleton (t: T.tyvar) : tyvarset = [t]

(* mkTyvarset : T.tyvar list -> tyvarset *)
fun mkTyvarset (tyvars : T.tyvar list) : tyvarset = tyvars (* no checking for incompatibilities *)

(* elements : tyvarset -> T.tyvar list *)
fun elements (tyvarset: tyvarset) : T.tyvar list = tyvarset

(* compatible : T.tyvar * T.tyvar -> bool *)
(* ASSUME: (1) the two tyvar arguments are UBOUND, and
 *         (2) they have the same name.
 * Check that their eqprop and depth properties are the same, and if so, instantiate a
 * to be b, thus identifying a and b, and return true. Otherwise, return false, indicating
 * that the two type variables with the same name are not compatible, and thus cannot be
 * considered "equal". 
 * Compatibility is not relevant unless the two tyvars have the same name.
 * This is the only situation in which a UBOUND tyvar can be instantiated. *)
fun compatible ((a as ref (T.UBOUND {name=name_a,eq=eq_a,depth=depth_a}) : T.tyvar), 
		(b as ref (T.UBOUND {name=name_b,eq=eq_b,depth=depth_b}) : T.tyvar)): bool =
      (* REQUIRE: name_a = name_b  -- not verified here*)
      if eq_a = eq_b (* a and b have the same eqprop *)
	 andalso depth_a = depth_b (* they have same deBruijn depth (?) *)
      then (a := T.INSTANTIATED (T.VARty b); true)
	   (* a is "compatible" with b and a has been instantiated to b, that is
	    * a has become b by instantiation. *)
      else false (* a and b have same name but are "incompatible" *)
  | compatible _ = bug "compatible"  (* at least one of the args is not UBOUND *)

(* add : tyvarset * tyvar -> tyvarset option
 * returns NONE if the tuvar argument is name-equal to but incompatible with a tyvar
 * in the tyvarset argument. *)
fun add (tyvarset as (b as ref (T.UBOUND {name=name_b,eq=eq_b,depth=depth_b})) :: rest,
         a as ref (T.UBOUND {name=name_a,eq=eq_a,depth=depth_a})) =
	if a = b  (* same tyvars, pointer equality of refs *)
	then SOME tyvarset (* success; a is already a member of tyvarset,
			      a does not need to be added to the tyvarset or instantiated *)
	else (if Symbol.eq (name_a, name_b)
	      then (if compatible (a, b) (* a and b are not object-equal tyvars but have
					 * the same name *)
		    then SOME tyvarset   (* a and b are name-equal and compatible.
					 * The compatible function has instantiated a to b,
					 * so a does not actually need to be added. *)
		    else NONE) (* a and b were incompatible -- fail *)
	      else (* a and b have different names and are not name-equal. *)
	        (case add (rest, a)
	           of SOME rest' => SOME (b :: rest')
	            | NONE => NONE))  (* incompatibility failure *)
  | add (nil, a) = SOME (singleton a)
  | add _ = bug "add"

(* mem : tvar * tvarset -> bool option *)
(* set membership based on object equality of the tyvar refs,
 *   or on name equality with a compatibility check *)
fun mem (a as ref (T.UBOUND {name=name_a,eq=eq_a,depth=depth_a}) : T.tyvar, 
	(b as ref (T.UBOUND {name=name_b,eq=eq_b,depth=depth_b})) :: rest : tyvarset) =
      if a = b
      then SOME true  (* a and b are actually the same tyvar *)
      else (if Symbol.eq (name_a, name_b)
	    then (if compatible (a, b)
		  then SOME true (* a and b have same name and are compatible;
				    a is instantiated to b *)
		  else NONE)     (* a is incompatible with a member of the tyvarset with same name *)
	    else mem (a, rest)) (* a and b have different names, keep looking in rest *)
  | mem (_, nil) = SOME false (* the tyvarset is empty *)
  | mem _ = bug "mem"  (* a or b is not UBOUND, which should not happen by INVARIANT 2 *)

(* memPure : tyvar * tyvarset -> bool *)
(* tyvarset membership without checking for tyvar compatibility,
   i.e., just based on object equality of the refs, or equality of the tyvar names *)
fun memPure ((a as ref (T.UBOUND {name=name_a,...})), 
	     (b as ref (T.UBOUND {name=name_b,...})) :: rest) =
      if a=b then true  (* object equality *)
      else if Symbol.eq (name_a,name_b) then true  (* name-equality without compatibility check *)
      else memPure (a,rest)
  | memPure _ = false

(* union : tvarset * tvarset -> tvarset option *)
(* failure, caused by incompatible tyvars with same name, signalled by returning NONE *)
fun union([], s) = SOME s
  | union(s, []) = SOME s
  | union(a::r, s) =
     (case mem (a, s)
        of SOME true => union (r, s) (* a has been "instantiated" to a compatible tyvar in s *)
	 | SOME false => 
	     (case union (r, s)
	        of SOME s' => SOME (a :: s')
		 | NONE => NONE)
	 | NONE => NONE) (* ERROR: a was incompatible with a tvar in s with the same name *)

(* diff : tyvarset * tyvarset -> tyvarset option *)
(* failure, caused by incompatible tyvars with same name, signalled by returning NONE *)
fun diff (s, []) = SOME s
  | diff ([], _) = SOME []
  | diff (a::r, s) =
     (case mem (a, s)
        of SOME true => diff (r, s)
	     (* an element compatible with a was in s and a has been "instantiated" to that
              * compatible tyvar in s *)
	 | SOME false => (* a not found in s *)
	     (case diff (r, s)
	        of SOME r' => SOME (a :: r')
		 | NONE => NONE)  (* incompatible overlap between r and s *)
	 | NONE => NONE) (* ERROR: a was incompatible with a tvar in s with the same name *)

(* diffPure: tyvarset * tyvarset -> tyvarset
 * Set difference based on object equality or simple name-equality of tyvars *)
fun diffPure (tyvarset, nil) = tyvarset
  | diffPure (nil, _) = nil
  | diffPure (a::rest, tyvarset) =
     if memPure (a, tyvarset)
     then diffPure (rest, tyvarset)
     else a :: diffPure (rest, tyvarset)

end (* local *)
end (* abstraction TyvarSet *)
