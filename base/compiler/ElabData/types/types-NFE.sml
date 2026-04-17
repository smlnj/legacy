(* ElabData/types/types.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* NFE edited version of ElabData/types/types.sml
   - distinguish different kinds of surface and internal type variables, particularly ETV and UTV
     type variables
   - eliminate some features that were required to support equality polymorphism
     It is assumed that real type variables (PTV, LTV, ETV) range over all ground types of kind Type
   - retain the prior approach to type constructors, both type functions and datatypes
   - NFE assumes that all tycons belong to structures (except primitives?), some special case
     support for the interactive system with "top-level" tycon declarations will be needed. Some
     internal representation support for CM may also be added.
 *)

structure Types : TYPES =  (* need corresponding changes in the defn of TYPES *)
struct

local (* imports *)
  structure SL = SourceLoc

  structure AT = Atom
  structure S  = Symbol  (* = Atom *)
  structure A  = Access
  structure EP = EntPath
  structure IP = InvPath
  structure ST = Stamp
in

type label = AT.atom  (* a record label is an atom.  All labels are alphanumeric. *)

(* MsML: the role and the definition of eqprop may be changing *)
datatype eqprop = YES | NO | IND | OBJ | DATA | ABS | UNDEF

type varSource = S.symbol * SL.region    (* the "occurrence" of an overloaded identifier *)
type litSource = IntInf.int * SL.region  (* the "occurrence" of an overloaded literal *)

type etv = AT.atom  (* symbolic type variables represented as atoms *)

(* UTVkind determines whether a UTV is ordinary (META) or a record row variable (FLEX) *)
datatype UTVkind
  = GEN (* general - used for generic instantiation of a polytype *)
  | FLEX of (label * ty) list  (* flex record *)

(* tvkind (values that define the _state_ of a UTV, a type unification variable):
   - dropping LBOUND of {depth: int, index: int, eq: bool}  - FLINT stuff 
   - IBOUND moved to ty, where it is renamed "DBI", an acronym for "deBruijn index"
   - and eq stuff
   - keeping machinery (OVLDX constructors) for implementing resolution of overloaded variables or literals *)
and tvKind
  = INSTANTIATED of ty (* instantiation of an OPEN UTV changes its contents to INSTANTIATED *)
  | OPEN of {depth: nat, kind: UTVkind}
     (* depth: lambda binding depth at point of creation; propagated by unification
          = infinity for a generic UTV instantiating polytypes at a polymorphic variable occurrence,
          < infinity ("finite") for a UTV introduced to represent the unknown type of a lambda bound variable *)
  | OVLDV of varSource list (* names and locations of overloaded variables or literals *)
     (* used to instantiate overloaded operator type scheme,
      * representing one of a finite set of possible ground types used as
      * "indicator" types to resolve the overloading *)
  | OVLDI of litSource list  (* overloaded integer literal *)
  | OVLDW of litSource list  (* overloaded word literal *)
		       
and tyckind
  = PRIMITIVE		          (* primitive tycons, e.g., int, string *)
  | DATATYPE of
     {index: nat,                 (* positional order within its datatype family (how is that ordered?) *)
      stamps: ST.stamp vector,    (* the tycon stamps of the datatype family members *)
      root : EP.entVar option,    (* the root field used by type spec only - NFE change expected *)
      freetycs: tycon list,       (* tycons provided by functor params - closure values for FREEtyc tycons
				     that appear in the datacon types *)
      family : dtypeFamily,
      stripped : bool}            (* true if datatype has matched a simple type spec *)
  | ABSTRACT of tycon             (* abstracted through opaque signature matching, or "sealing" in the CMU terminology
				     the tycon is the concrete represenation of the abstract tycon. No longer involved in
				     _abstype_ declarations, which have been eliminated in MsML. *)
  | FORMAL                        (* used only inside signatures *)
  | TEMP                          (* placeholder used only during datatype elaborations *)

and tycon
  = GENtyc of gtrec          (* "generative", including primitive tycons, abstract/opaque, and datatypes *)
  | DEFtyc of                (* defined type functions *)
      {stamp : ST.stamp,     (* for shortcut equality *)
       tyabs : tyabs,
       strict: bool list,    (* which parameters occur in the body of the tyabs *)
       path  : IP.path}      (* location relative to the structure hierarchy *)
  | PATHtyc of               (* relative to structure context; used only inside signatures *)
      {arity : int,
       entPath : EP.entPath,
       path : IP.path}
  | RECORDtyc of label list  (* sorted by atom ordering *)
  | RECtyc of int            (* dt=sibling index into datatype family; used only in domain type of dconDesc *)
  | FREEtyc of int           (* index into free tyc list; used only in domain type of dconDesc *)
  | ERRORtyc                 (* for error recovery, and used as a dummy tycon in ElabMod.extractSig *)

and ty  (* _ground_ type expressions of kind Type *)
  = ETV of etv  (* explicit bound type variable (ETV) *)
  | DBI of nat  (* formerly IBOUND of int *)  (* flat deBruijn index, occurring only in body ty of a type abstraction *)
  | UTV of utv  (* unification "working" variable (UTV) *)
  | APPty of tycon * ty list  (* renamed from CONty *)
  | WILDCARDty  (* unifies with any ty; used for error recovery? *)
  | UNDEFty     (* placeholder for situations where we don't have a type (yet) *)
  | MARKty of ty * SL.region  (* a type annotated with a source region *)

  (* type abstraction or "tyabs" ? -- used for type functions and polytypes *)
withtype tyabs = {arity: int, body: ty}  (* do we care about "strictness", i.e. whether a given DBI occurs in body? *)
and utv = ref tvkind 	      

(* datacon description used in dtmember *)
and dconDesc =
    {name: AT.atom,  (* i.e., symbol *)
     rep: A.conrep,
     domain: ty option}
       (* what about occurrences of locally bound parameter type variables (represented as LTVs or ETVs)? 
          perhaps this should be the complete polytype of the constructor *)

(* member of a family of (potentially) mutually recursive datatypes. dropping support for _lazy_. *)
and dtmember =
    {tycname: S.symbol,
     arity: int,
     eq: eqprop ref,  (* still needed in NFE? *)
     dcons: dconDesc list,
     sign: A.consig}

and dtypeFamily =
  {mkey: ST.stamp,
   members: dtmember vector,     (* how are these ordered? *)
   properties: PropList.holder}  (* dubious about need for this "properties" field -- how used? *)

and stubinfo =  (* related to cutoff recompilation, etc.? *)
    {owner : PersStamps.persstamp,
     lib   : bool}

and gtrec =
    {stamp : ST.stamp,
     arity : int,
     eq    : eqprop ref,
     kind  : tyckind,
     path  : IP.path,
     stub  : stubinfo option}

fun mkUTV (kind: tvKind) : utv = ref kind

fun copyUTV (tv: tyvar) = ref (!tv)

val infinity = 10000000

type polyty = {sign: polysign, body: tyabs}   (* a polytype is not a type *)

datatype datacon (* data constructors, dropping support for _lazy_ constructors *)
  = DATACON of
      {name   : S.symbol,
       typ    : polyty,   (* often a degenerate polytype with no bound type variables *)
       rep    : A.conrep,
       const  : bool,     (* redundant, could be determined from typ *)
       sign   : A.consig} (* redundant, ditto *)

end (* local *)
end (* structure Types *)
