(* cc-info.sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * signature for structures that describe C data sizes
 *)

signature CC_INFO = 
    sig
	(* all sizes in bytes *)

	val intSzB : int
	val shortSzB : int
	val longSzB : int

	val charSzB : int

	val floatSzB : int
	val doubleSzB : int

	val ptrSzB : int

	(* alignment for structs/unions *)
	val unionAlign : int
	val structAlign : int
    end (* signature CC_INFO *)
