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
 * It is assumed that:
 * (1)  all tyvars in a tyvars are UBOUND, and 
 * (2) names of tyvars are unique in a given tyvarset.
 * 
 * Two tyvars sharing the same name are _incompatible_ if they have different
 * equality properties or different (deBruijn) depths.
 *
 * The type TyvarSet.tvarset is abstract, and the only operations for constructing
 * tyvarsets are singleton, union, diff, and diffPure.
 *
 *)

structure TyvarSet :> TYVARSET =
struct

local 
  structure EM = ErrorMsg
  open Types 
  fun bug msg = ErrorMsg.impossible("TyvarSet: "^ msg)
in

type tyvarset = tyvar list
(* INVARIANT 1: all tyvars in a tyvar set should be of kind UBOUND
 * INVARIANT 2: (Uniqueness of names) different tyvars in a tyvar set should have different names. *)

val empty = nil
fun singleton t = [t]
fun mkTyvarset l = l (* no checking for incompatibilities *)
fun elements s = s

(* compatible : tvar * tvar -> bool *)
(* Test whether a and b have the same name.  If they do not, they are compatiable.
 * Otherwise, if they have the same name, check that their eqprop and depth properties are
 * the same, and if so, instantiate a to be b and return true. Otherwise, return false. *)
fun compatible (a as ref(UBOUND{name=name_a,eq=eq_a,depth=depth_a}), 
		b as ref(UBOUND{name=name_b,eq=eq_b,depth=depth_b})) =
    if Symbol.eq(name_a,name_b)
    then (if eq_a = eq_b (* a and b have the same eqprop *)
	     andalso depth_a = depth_b (* they have same deBruijn depth (?) *)
	  then (a := INSTANTIATED(VARty b); true)
		    (* a is "compatible" with b and a has been instantiated to b, that is
                     * a has become b by instantiation. *)
	  else false) (* a and b have same name but are "incompatible" *)
    else true (* a and b have different names, so they are compatible *)


(* add : tyvarset * tyvar -> tyvarset option *)
fun add (tyvarset as (b as ref(UBOUND{name=name_b,eq=eq_b,depth=depth_b}))::rest,
         a as ref(UBOUND{name=name_a,eq=eq_a,depth=depth_a})) =

      if a = b
      then SOME tyvarset (* a is already a member of tyvarset *)
      else if Symbol.eq (name_a, name_b)
	   then (if compatible (a, b) (* a and b have the same name *)
	         then SOME tyvarset  (* a has been instantiated to b by the compatible function *)
                 else NONE) (* a and b were incompatible -- fail *)
      else (* a and b have different names *)
	(case add (rest, a)
  	   of SOME s => SOME (b :: s)
	    | NONE => NONE)
  | add (nil, a) = SOME (singleton a)

(* mem : tvar * tvarset -> bool option *)
fun mem (a as ref(UBOUND{name=name_a,eq=eq_a,depth=depth_a}): tyvar, 
	(b as ref(UBOUND{name=name_b,eq=eq_b,depth=depth_b}))::rest: tyvarset) =
      if a = b
      then SOME true  (* a and b are actually the same tyvar *)
      else (if Symbol.eq (name_a, name_b)
	    then (if compatible (a, b)
		  then SOME true (* a and b have same name and are compatible *)
		  else NONE)
	    else mem (a, rest)) (* a and b have different names, keep looking in rest *)
  | mem _ = SOME false  (* the tyvarset is empty, or the first tyvar in the tyvarset is non-UBOUND,
                         * which should not happen by INVARIANT 2 *)

(* memP : tyvar * tyvarset -> bool *)
(* tyvarset membership without checking for tyvar compatibility, i.e., just based on the tyvar name *)
fun memP (a as ref(UBOUND{name=name_a,...}), 
	 (b as ref(UBOUND{name=name_b,...}))::rest) =
      if a=b then true
      else if Symbol.eq(name_a,name_b) then true
      else memP(a,rest)
  | memP _ = false

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
fun diff(s, []) = SOME s
  | diff([], _) = SOME []
  | diff(a::r, s) =
     (case mem (a, s)
        of SOME true => diff (r, s)
	     (* an element compatible with a was in s and a has been "instantiated" to that
              * compatible tyvar in s *)
	 | SOME false => (* a not found in s *)
	     (case diff (r, s)
	        of SOME r' => SOME (a :: r')
		 | NONE => NONE)  (* incompatible overlap between r and s *)
	 | NONE => NONE) (* ERROR: a was incompatible with a tvar in s with the same name *)

fun diffPure(s,[]) = s
  | diffPure([],_) = []
  | diffPure(a::r,s) =
     if memP(a,s) then diffPure(r,s)
     else a::diffPure(r,s)

end (* local *)
end (* abstraction TyvarSet *)

