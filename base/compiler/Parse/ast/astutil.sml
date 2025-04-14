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

    fun checkFix (i, region) =
	  if (i < 0) orelse (9 < i)
	  then (EM.errorRegion (region, "fixity precedence must be between 0 and 9"); 9)
	  else IntInf.toInt i

    (* layered patterns *)

    (* lay3 : Ast.pat * Ast.pat * SL.region -> Ast.pat *)
    fun lay3 ((x as Ast.VarPat _), y: Ast.pat, region: SL.region) = Ast.LayeredPat {varPat=x,expPat=y}
      | lay3 (Ast.MarkPat(x,_), y, region) = lay3 (x, y, region)
      | lay3 (Ast.ConstraintPat{pattern,constraint}, y, region) =
	     (EM.errorRegion (region, "illegal (multiple?) type constraints in AS pattern"); y)
      | lay3 (Ast.FlatAppPat[x], y, region) =
	     (EM.errorRegion (region, "parentheses illegal around variable in AS pattern"); y)
      | lay3 (x, y, region) =
   	     (EM.errorRegion (region, "pattern to left of AS must be variable"); y)

    (* lay2 : Ast.pat * Ast.pat * SL.region -> Ast.pat *)
    fun lay2 (Ast.ConstraintPat{pattern,constraint}, y: Ast.pat, region: SL.region) =
	     (EM.errorRegion (region, "illegal (multiple?) type constraints in AS pattern");
	      case lay2 (pattern, y, region)
	        of Ast.LayeredPat{varPat,expPat} =>
		     Ast.LayeredPat{varPat=varPat,
				    expPat=Ast.ConstraintPat{pattern=expPat,
							     constraint=constraint}}
		 | pat => pat)
      | lay2 (Ast.MarkPat (x,_), y, region) = lay2 (x, y, region)  (* strip MarkPat *)
      | lay2 (Ast.FlatAppPat [{item,...}], y, region) = lay3 (item, y, region)
      | lay2 args = lay3 args

    (* lay : Ast.pat * Ast.pat * SL.region -> Ast.pat *)
    fun lay (Ast.ConstraintPat{pattern,constraint}, y: Ast.pat, region: SL.region) =
	     (case lay2 (pattern, y, region)
	       of Ast.LayeredPat {varPat,expPat} =>
		    Ast.LayeredPat {varPat=varPat,
				    expPat=Ast.ConstraintPat{pattern=expPat,
							     constraint=constraint}}
		| pat => pat)
      | lay (Ast.MarkPat(x,_), y, region) = lay (x, y, region)
      | lay args = lay2 args

    val layered = lay

    (* sequence of declarations *)
    fun makeSEQdec (Ast.SeqDec a, Ast.SeqDec b) = Ast.SeqDec(a@b)
      | makeSEQdec (Ast.SeqDec a, b) = Ast.SeqDec(a@[b])
      | makeSEQdec (a, Ast.SeqDec b) = Ast.SeqDec(a::b)
      | makeSEQdec (a,b) = Ast.SeqDec[a,b]


    fun quoteExp s = Ast.AppExp{function=Ast.VarExp quoteDcon,argument=Ast.StringExp s}
    fun antiquoteExp e = Ast.AppExp{function=Ast.VarExp antiquoteDcon,argument= e}

end (* top local (imports) *)
end (* structure AstUtil *)
