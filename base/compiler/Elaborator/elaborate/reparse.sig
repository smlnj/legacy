(* Elaborator/elaborate/reparse.sig *)
(* Copyright 2025 by The Fellowship of SML/NJ (www.smlnj.org) *)

(* [DBM, 2025.03.25] Rewritten to clarify and simplify the precedence parsing algorithm. 
 * [DBM, 2025.07.05] The parse function has been replaced by two specialized versions:
 *   reparseFlatAppPat for Ast patterns and reparseFlatAppExp for Ast expressiohns.
 *)

signature REPARSE =
sig

  val reparseFlatAppPat : Ast.pat Ast.fixitem list * StaticEnv.staticEnv * SourceLocL.region -> Ast.pat
  val reparseFlatAppExp : Ast.exp Ast.fixitem list * StaticEnv.staticEnv * SourceLocL.region -> Ast.exp

  val reparsePat : Ast.pat * StaticEnv.staticEnv * SourceLoc.region -> Ast.pat   (* complete reparse *)
  (* reparsePat fully reparses a pattern, which is possible to do with a fixed staticEnv because
     there is no let form that might introduce new local infix bindings. *)

end (* signature REPARSE *)


