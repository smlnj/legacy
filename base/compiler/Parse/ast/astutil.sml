(* Parse/ast/astutil.sml
 *
 * COPYRIGHT (c) 2018, 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure AstUtil : ASTUTIL =
struct

local (* imports *)

  structure SL = SourceLoc (* <- region *)
  structure S = Symbol
  structure EM = ErrorMsg

in

    val unitPat = Ast.RecordPat {def=nil,flexibility=false}
    val unitExp = Ast.RecordExp nil
    val trueDcon = [S.valSymbol "true"]
    val falseDcon = [S.valSymbol "false"]
    val quoteDcon = [S.strSymbol "SMLofNJ", S.valSymbol "QUOTE"]
    val antiquoteDcon = [S.strSymbol "SMLofNJ", S.valSymbol "ANTIQUOTE"]
    val arrowTycon = S.tycSymbol "->"
    val exnID = S.tycSymbol "exn"
    val bogusID = S.valSymbol "BOGUS"
    val symArg = S.strSymbol "<Parameter>"
    val itsym = S.valSymbol "it"

    (* checkFix : IntInf.int * SL.region -> int *)
    fun checkFix (i: IntInf.int, region: SL.region) =
	  if (i < 0) orelse (9 < i)
	  then (EM.errorRegion (region, "fixity precedence must be between 0 and 9"); 9)
	  else IntInf.toInt i

    (* sequence of declarations *)
    fun makeSEQdec (Ast.SeqDec a, Ast.SeqDec b) = Ast.SeqDec(a@b)
      | makeSEQdec (Ast.SeqDec a, b) = Ast.SeqDec(a@[b])
      | makeSEQdec (a, Ast.SeqDec b) = Ast.SeqDec(a::b)
      | makeSEQdec (a,b) = Ast.SeqDec[a,b]

(* no longer needed -- delete
    (* We might not need the following two functions. Used in Reparse, and in ElabCore to
     * check whether a symbol is an infix function (variable) symbol in a function header LHS. *)
    (* patToSymbols : pat -> (S.symbol * string) option *)
    (* result symbol are in the VALspace and FIXspace namespaces, respectively *)
    fun patToSymbol (Ast.VarPat [name]) = SOME name
      | patToSymbol _ =  NONE

    (* expToSymbol : exp -> (S.symbol) option *)
    (* result symbol is in the fixity namespace *)
    fun expToSymbol (Ast.VarExp [name]) = SOME name
      | expToSymbol _ = NONE
*)

  (* Quotation *)    

    fun quoteExp s = Ast.AppExp{function = Ast.VarExp quoteDcon, argument = Ast.StringExp s}
    fun antiquoteExp e = Ast.AppExp{function = Ast.VarExp antiquoteDcon, argument = e}

end (* top local (imports) *)
end (* structure AstUtil *)
