(* Elaborator/elaborate/reparse.sig *)
(* Copyright 2025 by The Fellowship of SML/NJ (www.smlnj.org) *)

(* [DBM, 2025.03.25] Rewritten to clarify and simplify the precedence parsing algorithm. 
 * [DBM, 2025.07.05] The parse function has been replaced by two specialized versions:
 *   reparseFlatAppPat for Ast patterns and reparseFlatAppExp for Ast expressiohns.
 * [DBM, 2025.07.12] Reparse is now a functor with REPARSE as its result signature.
 *)

signature ITEM =
sig

  type item
  val apply: item * item -> item,
  val pair: item * item -> item:
  val fixity : item * StaticEnv.staticEnv ->  (item * string * int * int) option

end  (* signature ITEM *)

signature PRECEDENCE_PARSE = 
sig

  type item
  val precedenceParse: item list * StaticEnv.staticEnv * SourceMap.region -> item

end (* signature PRECEDENCE_PARSE *)

signature REPARSE =
sig

  val reparseFlatAppPat : Ast.pat list * StaticEnv.staticEnv * SourceLoc.region -> Ast.pat
  val reparseFlatAppExp : Ast.exp list * StaticEnv.staticEnv * SourceLoc.region -> Ast.exp

end (* signature REPARSE *)


