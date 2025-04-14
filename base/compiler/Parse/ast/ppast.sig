(* Copyright 2003 by University of Chicago *)
(* src/Elaborator/print/ppast.sig *)
(* Jing Cao and Lukasz Ziarek *)

signature PPAST =

sig
	val ppExp	:Source.source option
               		 -> PrettyPrint.stream -> Ast.exp * int -> unit 
	val ppPat	:Source.source option
               		 -> PrettyPrint.stream -> Ast.pat * int -> unit
	val ppStrExp	:Source.source option
               		 -> PrettyPrint.stream -> Ast.strexp * int -> unit
	val ppFctExp	:Source.source option
               		 -> PrettyPrint.stream -> Ast.fctexp * int -> unit
	val ppWhereSpec :Source.source option
               		 -> PrettyPrint.stream -> Ast.wherespec * int -> unit
	val ppSigExp	:Source.source option
               		 -> PrettyPrint.stream -> Ast.sigexp * int -> unit
	val ppFsigExp	:Source.source option
               		 -> PrettyPrint.stream -> Ast.fsigexp * int -> unit
	val ppSpec	:Source.source option
               		 -> PrettyPrint.stream -> Ast.spec * int -> unit 
	val ppDec	:Source.source option
               		 -> PrettyPrint.stream -> Ast.dec * int -> unit
	val ppVb   	: Source.source option 
                         -> PrettyPrint.stream -> Ast.vb * int -> unit
  	val ppRvb  	: Source.source option
               		 -> PrettyPrint.stream -> Ast.rvb * int -> unit
	val ppFb	: Source.source option
               		 -> PrettyPrint.stream -> string -> Ast.fb * int -> unit
	val ppClause	: Source.source option
               		 -> PrettyPrint.stream -> Ast.clause * int -> unit
	val ppTb	: Source.source option
               		 -> PrettyPrint.stream -> Ast.tb * int -> unit
	val ppDb	: Source.source option
               		 -> PrettyPrint.stream -> Ast.db * int -> unit  
	val ppDbrhs	: Source.source option
               		 -> PrettyPrint.stream -> (Symbol.symbol * Ast.ty option) list * int -> unit
	val ppEb	: Source.source option
               		 -> PrettyPrint.stream -> Ast.eb * int -> unit
	val ppStrb	: Source.source option
               		 -> PrettyPrint.stream -> Ast.strb * int -> unit
	val ppFctb	: Source.source option
               		 -> PrettyPrint.stream -> Ast.fctb * int -> unit
	val ppTyvar	: Source.source option
               		 -> PrettyPrint.stream -> Ast.tyvar * int -> unit
	val ppTy	: Source.source option
               		 -> PrettyPrint.stream -> Ast.ty * int -> unit 
end
