(* coreacc.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Supports access to values defined in the "Core" structure (system/smlnj/init/core.sml).
 *
 * TODO: merge the CoreSym structure into this module (ElabData/basics/coresym.sml).
 *)

structure CoreAccess : sig

    val getVar : StaticEnv.staticEnv -> string list -> VarCon.var

    val getCon : StaticEnv.staticEnv -> string list -> Types.datacon

    val getVar' : (unit -> VarCon.var) ->
		  StaticEnv.staticEnv -> string list -> VarCon.var

    val getCon' : (unit -> Types.datacon) ->
		  StaticEnv.staticEnv -> string list -> Types.datacon

  (* like getCon, but returns a bogus exn instead of failing *)
    val getExn : StaticEnv.staticEnv -> string list -> Types.datacon

end = struct

    fun impossible m = ErrorMsg.impossible ("CoreAccess: " ^ m)

    fun impossibleWithId (m, xs) =
	ErrorMsg.impossible
	  (concat ["CoreAccess: ", m, " '", String.concatWith "." xs, "'"])

    exception NoCore

    fun dummyErr _ _ _ = raise NoCore

    fun mkpath [] = impossible "mkpath"
      | mkpath [x] = [Symbol.varSymbol x]
      | mkpath (x :: xs) = Symbol.strSymbol x :: mkpath xs

    fun path xs = SymPath.SPATH (CoreSym.coreSym :: mkpath xs)

    fun getCore (env, xs) =
        (case Lookup.lookVal (env, path xs)
	   of SOME x => x
	    | NONE => raise NoCore)

    fun getVar' err env xs =
	(case getCore (env, xs)
	   of VarCon.VAL r => r
	    | _ => impossibleWithId("getVar'", xs)
	(* end case *))
	handle NoCore => err ()

    fun getVar env xs = getVar' (fn () => impossibleWithId ("getVar", xs)) env xs

    fun getCon' err env xs =
	(case getCore (env, xs)
	   of VarCon.CON c => c
	    | _ => err ()
	(* end case *))
	handle NoCore => err ()

    fun getCon env xs = getCon' (fn () => impossibleWithId ("getCon'", xs)) env xs

    fun getExn env xs = getCon' (fn () => VarCon.bogusEXN) env xs

  end
