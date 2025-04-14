(* astutil.sig
 *
 * COPYRIGHT (c) 2018, 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ASTUTIL =
sig

  val checkFix : IntInf.int * SourceLoc.region -> int

  (* BUILDS VARIOUS CONSTRUCTIONS *)
  val makeSEQdec : Ast.dec * Ast.dec -> Ast.dec

  val layered : Ast.pat * Ast.pat * SourceLoc.region -> Ast.pat

  (* SYMBOLS *)
  val arrowTycon : Symbol.symbol
  val bogusID : Symbol.symbol
  val exnID : Symbol.symbol
  val symArg : Symbol.symbol
  val itsym : Symbol.symbol list

  val unitExp : Ast.exp
  val unitPat : Ast.pat

  (* QUOTES *)
  val quoteExp : string -> Ast.exp
  val antiquoteExp : Ast.exp -> Ast.exp

end  (* signature ASTUTIL *)


