(* Copyright 1996 by AT&T Bell Laboratories *)
(* entpath.sml *)

signature ENT_PATH =
sig

  type entVar = Stamps.stamp
  type entPath = entVar list
  type rEntPath

  val mkEntVar : unit -> entVar
  val eqEntVar : entVar * entVar -> bool

  val epnil : entPath
  val repnil : rEntPath
  val repcons : entVar * rEntPath -> rEntPath

  val ep2rep : entPath * rEntPath -> rEntPath
  val rep2ep : rEntPath -> entPath

  val eqEntPath : entPath * entPath -> bool
      (* true if element-wise equal, same length *)

  val cmpEntVar : entVar * entVar -> order
  val cmpEntPath : entPath * entPath -> order

  val nullEntPath : entPath -> bool
  val entVarToString : entVar -> string
  val entPathToString : entPath -> string

  val bogusEntVar : entVar

  structure EvDict : ORD_MAP where type Key.ord_key = entVar

end  (* signature ENT_PATH *)


structure EntPath :> ENT_PATH =
struct

(* imports *)
local

  structure ST = Stamps

in

type entVar = ST.stamp

type entPath = entVar list
(* entPath has entVars in direct order, outer first *)

(* rEntPath -- entPath in reversed order; abstract *)
type rEntPath = entVar list

val eqEntVar = ST.eq
val mkEntVar = ST.fresh

val epnil = []
val repnil = []
val repcons = op ::

val ep2rep = List.revAppend
val rep2ep = rev


(* eqEntPath : entPath * entPath -> bool
eqEntPath: elementwise equality of entPaths; false if entPaths have unequal lengths *)
val eqEntPath = ListPair.allEq eqEntVar

val cmpEntVar = ST.compare

(* cmpEntPath: entPath * entPath -> order
 * lexicographic comparison of two entPaths, shorter path is less *)
fun cmpEntPath (ep1, ep2) = 
    let fun compare(a::ar, b::br) =
	      (case ST.compare(a,b) of EQUAL => compare(ar,br) | z => z)
	  | compare(a::ar, nil) = GREATER
	  | compare(nil, b::br) = LESS
	  | compare(nil,nil) = EQUAL
     in compare(ep1,ep2)
    end

structure EvDict =
    RedBlackMapFn(struct
		    type ord_key = entVar 
		    val compare = cmpEntVar
		  end)

(* ListPair.all didn't cut it because it doesn't require lists of equal length
    length ep1 = length ep2 andalso
    ListPair.all eqEntVar (ep1, ep2)
*)

fun nullEntPath (ep: entPath) = List.null ep

fun entVarToString (v: entVar) = ST.toShortString v

fun entPathToString ([]: entPath) = "[]"
  | entPathToString (x::xs) =
      let val rest = foldr (fn (y,l) => ","::(ST.toShortString y)::l) ["]"] xs
       in String.concat("[" :: (ST.toShortString x) :: rest)
      end

val bogusEntVar = ST.special "bogusEntVar"

end (* local *)
end (* structure EntPath *)
