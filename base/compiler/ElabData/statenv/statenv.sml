(* ElabData/statenv/statenv.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure StaticEnv : STATICENV =
struct

local (* imports *)

  structure B = Bindings
  structure E = Env
  structure M = Modules

in 

type real_binding = B.binding * M.modtree option

type staticEnv = real_binding E.env

exception Unbound = E.Unbound

(* real_binding -> B.binding *)
fun strip ((b, _): real_binding) = b
(* unstrip : B.binding -> real_binding *)
fun unstrip x = (x, NONE)

val empty = E.empty
fun look (e, s) = strip (E.look (e, s))

(* bind0: symbol * real_binding * staticEnv ->  staticEnv *)
(* bind0 = E.bind with instantiated type *)
fun bind0 (s: Symbol.symbol, b: real_binding, e: staticEnv) =
    E.bind (s, b, e)

fun bind (s, b, e) = E.bind (s, unstrip b, e)
fun special (mkb, mks) = E.special (unstrip o mkb, mks)
val atop = E.atop
val consolidate = E.consolidate
val consolidateLazy = E.consolidateLazy
fun app f e = E.app (fn (s, b) => f (s, strip b)) e
fun map f e = E.map (unstrip o f o strip) e
fun fold f x0 e = E.fold (fn ((s, b), x) => f ((s, strip b), x)) x0 e
val symbols = E.symbols

(* fold but only over the elements in the environment with the keys
   given in the key list (last parameter). This functions allows 
   us to compute folds in arbitrary order over a consolidated list.
   In particular, this function is currently used in extractSig in
   elabmod to keep the inferred signature specs in the same order as
   the original structure decls. 
 *)
fun foldOverElems(f, x0, env, []) = x0
  | foldOverElems(f, x0, env, elem::rest) = 
      foldOverElems(f, f ((elem, look (env,elem)), x0), env, rest)  
(* 
 * sort: sort the bindings in an environment.
 *  
 * This is used for the assignment of dynamic access slots in structure
 * elaborate, for printing, and for other purposes.
 * The bindings are sorted in the following order:
 *
 *   values
 *   constructors
 *   types
 *   signatures
 *   structures
 *   funsigs
 *   functors
 *   fixity declarations
 *
 * It is only correct to sort environments which have no duplicate bindings.
 * All routines which build structure environments maintain this
 * invariant, so it is ok to sort any structure environment using
 * this function.
 *)

fun sort env = ListMergeSort.sort B.binderGt (fold (op ::) nil env)

fun filter (e, l) =
    let fun add (sy, e') = bind (sy, look (e, sy), e') handle Unbound => e'
     in foldl add empty l
    end

end (* local *)
end (* structure StaticEnv *)
