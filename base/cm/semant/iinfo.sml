(*
 * Information kept at the boundary between snodes and bnodes in the
 * dependency graph.  (This is information about a library "interface",
 * hence "iinfo" = "interface information".)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure IInfo = struct
    type info = { statenv: unit -> StaticEnv.staticEnv,
		  symenv: unit -> SymbolicEnv.env,
		  statpid: PersStamps.persstamp,
		  sympid: PersStamps.persstamp,
		  guid: string }
end
