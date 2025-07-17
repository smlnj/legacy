(* Parse/ast/ast.sml
 *
 * Syntax trees for bare ML
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(****************************************************************************
 *            PLEASE PROPAGATE ANY MODIFICATIONS TO THIS FILE               *
 *                    INTO PERV.SML AND SYSTEM.SIG                          *
 ****************************************************************************)

structure Ast : AST =
struct

    structure SL = SourceLoc
    structure SM = SourceMap
    structure S = Symbol
    structure F = Fixity

    (* to mark positions in files *)
    type srcpos = SL.charpos  (* character position from beginning of stream (base 1) *)

    (* "raw" symbolic path as symbol list (corresponds to Modules.spath) <> SymPath.path *)
    type path = S.symbol list

  (*  old pat fixitem replaced by pat,  exp fixitem replaced by exp *)

  (* integer/word literal; the string is the literal as it appeared in the source
   * and the int is the value of the literal.
   *)
    type literal = string * IntInf.int

  (* real literal; also paired with source string *)
    type real_lit = string * RealLit.t

    datatype 'a sigConst
      = NoSig
      | Transparent of 'a
      | Opaque of 'a


    (* EXPRESSIONS *)

    datatype exp
      = VarExp of path			(* variable *)
      | FnExp of rule list		(* abstraction *)
      | FlatAppExp of exp list   	(* expressions before fixity parsing *) *)
      | AppExp of {function:exp,argument:exp}
				    	(* application *)
      | CaseExp of{expr:exp,rules:rule list}
				    	(* case expression *)
      | LetExp of {dec:dec,expr:exp}	(* let expression *)
      | SeqExp of exp list		(* sequence of expressions *)
      | IntExp of literal		(* integer *)
      | WordExp of literal		(* word literal *)
      | RealExp of real_lit		(* floating point coded by its string *)
      | StringExp of string		(* string *)
      | CharExp of string		(* char *)
      | RecordExp of (S.symbol * exp) list (* record *)
      | ListExp of exp list	        (*  [list,in,square,brackets] *)
      | TupleExp of exp list		(* tuple (derived form) *)
      | SelectorExp of S.symbol		(* selector of a record field *)
      | ConstraintExp of {expr:exp,constraint:ty}
					(* type constraint *)
      | HandleExp of {expr:exp, rules:rule list}
				 	(* exception handler *)
      | RaiseExp of exp			(* raise an exception *)
      | IfExp of {test:exp, thenCase:exp, elseCase:exp}
					(* if expression (derived form) *)
      | AndalsoExp of exp * exp		(* andalso (derived form) *)
      | OrelseExp of exp * exp		(* orelse (derived form) *)
      | WhileExp of {test:exp,expr:exp} (* while (derived form) -- depricated *)
      | VectorExp of exp list   	(* vector *)

      | MarkExp of exp * SL.region	(* mark an expression *)

    (* RULE for case functions and exception handler *)
    and rule = Rule of {pat:pat,exp:exp}

    (* PATTERN *)
    and pat
      = WildPat					(* empty pattern *)
      | VarPat of path				(* variable pattern *)
      | IntPat of literal			(* integer *)
      | WordPat of literal			(* word literal *)
      | StringPat of string			(* string *)
      | CharPat of string			(* char *)

      | RecordPat of {def:(S.symbol * pat) list, flexibility:bool}
						(* record *)
      | ListPat of pat list			(* [list,in,square,brackets] *)
      | TuplePat of pat list			(* tuple *)
      | FlatAppPat of pat list		        (* patterns before fixity parsing *) *)
      | AppPat of {constr:pat, argument:pat}	(* constructor application *)
      | ConstraintPat of {pattern:pat, constraint:ty}
						(* constraint *)
      | LayeredPat of {variable: S.symbol, main: pat}   (* as patterns *)
      | VectorPat of pat list			(* vector pattern *)
      | OrPat of pat list			(* or-pattern *)

      | MarkPat of pat * SL.region		(* mark a pattern *)

    (* STRUCTURE EXPRESSION *)
    (* we don't need the AppStrI, just the single AppStr will do. *)
    and strexp = VarStr of path				(* variable structure *)
	       | BaseStr of dec				(* basic structure (struct...end) *)
	       | ConstrainedStr of strexp * sigexp sigConst (* signature constrained *)
	       | AppStr of path * (strexp * bool) list	(* application (external) *)
	       | AppStrI of path * (strexp * bool) list	(* application (internal) *)
	       | LetStr of dec * strexp			(* let in structure *)
	       | MarkStr of strexp * SL.region		(* mark *)

    (* FUNCTOR EXPRESSION *)
    and fctexp = VarFct of path * fsigexp sigConst	(* functor variable *)
	       | BaseFct of {				(* definition of a functor *)
		    params	   : (S.symbol option * sigexp) list,
		    body	   : strexp,
		    constraint : sigexp sigConst}
	       | LetFct of dec * fctexp
	       | AppFct of path * (strexp * bool) list * fsigexp sigConst
							(* application *)
	       | MarkFct of fctexp * SL.region     	(* mark *)

    (* WHERE SPEC *)
    and wherespec
	= WhType of S.symbol list * tyvar list * ty
        | WhStruct of S.symbol list * S.symbol list

    (* SIGNATURE EXPRESSION *)
    and sigexp = VarSig of S.symbol			(* signature variable *)
	       | AugSig of sigexp * wherespec list	(* sig augmented with where specs *)
	       | BaseSig of spec list			(* basic signature (sig...end) *)
	       | MarkSig of sigexp * SL.region 		(* mark *)

    (* FUNCTOR SIGNATURE EXPRESSION *)
    and fsigexp = VarFsig of S.symbol		(* funsig variable *)
		| BaseFsig of {param: (S.symbol option * sigexp) list, result:sigexp}
						(* basic funsig *)
		| MarkFsig of fsigexp * SL.region	(* mark *)

    (* SPECIFICATION FOR SIGNATURE DEFINITIONS *)
    and spec = StrSpec of (S.symbol * sigexp * path option) list  (* structure *)
	     | TycSpec of ((S.symbol * tyvar list * ty option) list * bool) (* type *)
	     | FctSpec of (S.symbol * fsigexp) list		(* functor *)
	     | ValSpec of (S.symbol * ty) list	                (* value *)
	     | DataSpec of {datatycs: db list, withtycs: tb list} (* datatype *)
	     | DataReplSpec of S.symbol * path                    (* datatype replication *)
	     | ExceSpec of (S.symbol * ty option) list	        (* exception *)
	     | ShareStrSpec of path list			(* structure sharing *)
	     | ShareTycSpec of path list			(* type sharing *)
	     | IncludeSpec of sigexp			        (* include specif *)
	     | MarkSpec of spec * SL.region		        (* mark a spec *)

    (* DECLARATIONS (let and structure) *)
    and dec = ValDec of (vb list * tyvar list)		(* values *)
	    | ValrecDec of (rvb list * tyvar list)	(* recursive values *)
	    | DoDec of exp				(* 'do' exp *)
	    | FunDec of (fb list * tyvar list)		(* mutually recursive functions *)
	    | TypeDec of tb list			(* type dec *)
	    | DatatypeDec of {datatycs: db list, withtycs: tb list} (* datatype dec *)
	    | DataReplDec of S.symbol * path            (* dt replication *)
	    | AbstypeDec of {abstycs: db list, withtycs: tb list, body: dec} (* abstract type *)
	    | ExceptionDec of eb list			(* exception *)
	    | StrDec of strb list			(* structure *)
	    | FctDec of fctb list			(* functor *)
	    | SigDec of sigb list			(* signature *)
	    | FsigDec of fsigb list			(* funsig *)
	    | LocalDec of dec * dec			(* local dec *)
	    | SeqDec of dec list			(* sequence of dec *)
	    | OpenDec of path list			(* open structures *)
	    | OvldDec of S.symbol * exp list            (* overloading (internal; restricted) *)
	    | FixDec of {fixity: F.fixity, ops: S.symbol list}  (* fixity *)
	    | MarkDec of dec * SL.region		        (* mark a dec *)

    (* VALUE BINDINGS *)
    and vb = Vb of {pat: pat, exp: exp, lazyp: bool}
	   | MarkVb of vb * SL.region

    (* RECURSIVE VALUE BINDINGS *)
    and rvb = Rvb of {var: S.symbol, fixity: (S.symbol * SL.region) option,
		      exp: exp, resultty: ty option, lazyp: bool}
	    | MarkRvb of rvb * SL.region

    (* RECURSIVE FUNCTION BINDING *)
    and fb = Fb of clause list * bool (* bool = true => lazy *)
	   | MarkFb of fb * SL.region

    (* CLAUSE: single a rule (pat = exp) in a function binding *)
    (* LHS is a list of patterns that has to be analyzed to determine the function
     * name and the rule parameter patterns. This is not trivial because of infix
     * symbols used as function names and infix datacons. *)
    and clause = Clause of {pats: pat list, resultty: ty option, exp:exp}

    (* TYPE BINDING *)
    and tb = Tb of {tyc : S.symbol, def : ty, tyvars : tyvar list}
	   | MarkTb of tb * SL.region

    (* DATATYPE BINDING *)
    and db = Db of {tyc : S.symbol, tyvars : tyvar list,
		    rhs : (S.symbol * ty option) list, lazyp : bool}
	   | MarkDb of db * SL.region

    (* EXCEPTION BINDING *)
    and eb = EbGen of {exn: S.symbol, etype: ty option} (* Exception definition *)
	   | EbDef of {exn: S.symbol, edef: path}	  (* defined by equality *)
	   | MarkEb of eb * SL.region

    (* STRUCTURE BINDING *)
    and strb = Strb of {name: S.symbol,def: strexp,constraint: sigexp sigConst}
	     | MarkStrb of strb * SL.region

    (* FUNCTOR BINDING *)
    and fctb = Fctb of {name: S.symbol,def: fctexp}
	     | MarkFctb of fctb * SL.region

    (* SIGNATURE BINDING *)
    and sigb = Sigb of {name: S.symbol,def: sigexp}
	     | MarkSigb of sigb * SL.region

    (* FUNSIG BINDING *)
    and fsigb = Fsigb of {name: S.symbol,def: fsigexp}
	      | MarkFsigb of fsigb * SL.region

    (* TYPE VARIABLE *)
    and tyvar = Tyv of S.symbol
	      | MarkTyv of tyvar * SL.region

    (* TYPES *)
    and ty
      = VarTy of tyvar			  (* type variable *)
      | ConTy of S.symbol list * ty list  (* type constructor application *)
      | RecordTy of (S.symbol * ty) list  (* record *)
      | TupleTy of ty list		  (* tuple *)
      | MarkTy of ty * SL.region          (* region-marked type *)


end (* structure Ast *)
