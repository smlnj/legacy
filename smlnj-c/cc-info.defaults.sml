(* cc-info.defaults.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * default sizes of basic C types
 *)

structure CCInfoDefaults : CC_INFO = 
    struct
	(* all sizes in bytes *)

	val intSzB = 4
	val shortSzB = 2
	val longSzB = 4

	val charSzB = 1

	val floatSzB = 4
	val doubleSzB = 8

	val ptrSzB = 4

	val unionAlign = 4
	val structAlign = 4
    end (* structure CCInfoDefaults *)
