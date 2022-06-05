(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* debindex.sml *)

(*
 * This implements the abstraction of de Bruijn indices used
 * by the FLINT type and term language. The notion of depth
 * refers to the type-binding depth relative to the top level
 * of the current compilation unit. I can't make type depth
 * and index abstract because certain clients want to use
 * the values of these types as table indices.
 *)

(* I moved this into the elaborator library.  It may be moved
 * back to FLINT if the elaborator gets "cleaned up", i.e., if
 * it is made to be unaware of such middle-end specifics.
 * (08/2001 Blume) *)

(* Basic PLambda type variables are pairs of
 * indices: (index, count)
 * where index (the deBruijn index) is the normal lambda binding distance
 * from the current type variable to its binder, starting with 1 to reference
 * the innermost binder.  Each binder binds a tuple of variables, and
 * and the count is used to select from this tuple. The count is zero-based.
 *
 * depth is used to represent absolute type abstraction depth, with the top-level
 * being 0. The deBruijn index is calculated as the current abstraction depth
 * minus the abstraction depth of the binder of the type variable in question.
 *
 * At an abstraction, the binder depth is the then current depth (e.g. the outermost
 * binder will have depth=0), and the current depth is incremented when entering the
 * the scope of an abstraction. So the index of any bound type variable occurrence
 * is >= 1.
*)

structure DebIndex : DEB_INDEX =
struct

local structure EM = ErrorMsg
in

fun bug s = EM.impossible ("DebIndex: " ^ s)

(* depth: (current) depth of nested type abstractions *)
type depth = int  (* INVARIANT: 0 <= depth *)
(* index: relative depth with respect to binding occurrence of type variables *)
type index = int  (* INVARIANT: 1 <= index *)

val top = 0

fun next i = i + 1

fun prev i = if (i > 0) then i-1 else bug "negative depth in prev"

fun eq (i:int, j) = (i=j)

fun dp_key (i : depth) = i

fun dp_print i = Int.toString i

fun dp_toint (i : depth) = i
fun dp_fromint (i : int) = i

fun getIndex (cur:depth, def:depth) : index =
    if def > cur then bug "the definition is deeper than the use"
    else (cur - def)

val cmp : depth * depth -> order = Int.compare

fun di_key i = i

fun di_print i = Int.toString i

fun di_toint (i : index) = i
fun di_fromint (i : int) = i

val innermost = 1
val innersnd = 2
fun di_inner i = i+1

end (* local *)
end (* structure DebIndex *)
