(* astutil.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
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
    val trueDcon = [S.varSymbol "true"]
    val falseDcon = [S.varSymbol "false"]
    val quoteDcon = [S.strSymbol "SMLofNJ", S.varSymbol "QUOTE"]
    val antiquoteDcon = [S.strSymbol "SMLofNJ", S.varSymbol "ANTIQUOTE"]
    val arrowTycon = S.tycSymbol "->"
    val exnID = S.tycSymbol "exn"
    val bogusID = S.varSymbol "BOGUS"
    val symArg = S.strSymbol "<Parameter>"
    val itsym = [S.varSymbol "it"]

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


    fun quoteExp s = Ast.AppExp{function=Ast.VarExp quoteDcon,argument=Ast.StringExp s}
    fun antiquoteExp e = Ast.AppExp{function=Ast.VarExp antiquoteDcon,argument= e}

end (* top local (imports) *)
end (* structure AstUtil *)
