(* cutil.sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Signature for structure containing C functions required for the 
 * C interface.  
 *)

signature C_UTIL = 
    sig
	type caddr

	val ptos : caddr -> string
	val ptoi : caddr -> Word32.word

    end (* signature C_UTIL *)