(* cutil.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * functor producing structures with C functions required for
 * the SML/NJ C interface.
 *)

functor CUtil (structure C: C_CALLS) : C_UTIL =
    struct
	open C

	val ptos' = registerAutoFreeCFn("ptos",[CaddrT],CstringT)
	fun ptos p = let val Cstring s = ptos' [Caddr p] in s end
        val ptoi' = registerAutoFreeCFn("ptoi",[CaddrT],CintT)
        fun ptoi p = let val Cint i = ptoi' [Caddr p] in i end
    end (* functor CUtil *)
