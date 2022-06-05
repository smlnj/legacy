(* typesutil.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TYPESUTIL =
sig

  val eqpropToString : Types.eqprop -> string

  (* operations to build tyvars, VARtys *)
  val mkMETA : int -> Types.tvKind
  val mkFLEX : ((Symbol.symbol * Types.ty) list) * int -> Types.tvKind
  val mkUBOUND : Symbol.symbol -> Types.tvKind
  val mkMETAty : unit -> Types.ty
  val mkMETAtyBounded : int -> Types.ty

  (* primitive operations on tycons *)
  val tycName : Types.tycon -> Symbol.symbol
  val tycStamp : Types.tycon -> Stamps.stamp
  val tycPath : Types.tycon -> InvPath.path

  val tycEntPath : Types.tycon -> EntPath.entPath
  val tyconArity : Types.tycon -> int
  val setTycPath : Types.tycon * InvPath.path -> Types.tycon
  val eqTycon : Types.tycon * Types.tycon -> bool
  val eqDatacon : Types.datacon * Types.datacon -> bool

  val prune : Types.ty -> Types.ty
  val pruneTyvar : Types.tyvar -> Types.ty

  val eqTyvar : Types.tyvar * Types.tyvar -> bool
  val bindTyvars : Types.tyvar list -> unit
  val bindTyvars1 : Types.tyvar list -> Types.polysign

  exception ReduceType
  val mapTypeFull: (Types.tycon -> Types.tycon) -> Types.ty -> Types.ty
  val applyTyfun : Types.tyfun * Types.ty list -> Types.ty
  val applyPoly : Types.ty * Types.ty list -> Types.ty
  val reduceType : Types.ty -> Types.ty
  val headReduceType : Types.ty -> Types.ty
  val equalType  : Types.ty * Types.ty -> bool
  val equalTypeP : Types.ty * Types.ty -> bool
  val equalTycon : Types.tycon * Types.tycon -> bool

(* `calcStrictness (arity, ty)` returns a list of bools of length arity,
 * where the ith element indicates whether DB index `(IBOUND i)` occurs
 * in ty.
 *)
  val calcStrictness : int * Types.ty -> bool list

  (* making a "generic" copy of a type *)
  val typeArgs : int -> Types.ty list
  val mkPolySign : int -> Types.polysign

  val dconName : Types.datacon -> Symbol.symbol
  val dconTyc : Types.datacon -> Types.tycon
  val dconType : Types.tycon * Types.ty option  -> Types.ty

  (* get rid of INSTANTIATED indirections throughout a type *)
  val compressTy : Types.ty -> unit

  type occ
  val Abstr : occ -> occ
  val LetDef: occ -> occ
  val Root : occ
  val lamdepth : occ -> int
  val toplevel : occ -> bool

  val instantiatePoly : Types.ty -> Types.ty * Types.tyvar list

  val compareTypes : Types.ty * Types.ty -> bool

  val indexBoundTyvars : int * Types.tyvar list -> unit

  val matchInstTypes : bool * int * Types.ty * Types.ty ->
                         (Types.tyvar list * Types.tyvar list) option
   (* matchInstTypes probably supercedes compareTypes, and if so,
    * compareTypes should be deleted *)

  val tyvarType : Types.ty -> Types.tyvar

  (*
   * Check if a bound tyvar has occurred in some datatypes, e.g. 'a list.
   * this is useful for representation analysis; but it should be
   * obsolete very soon -- zsh.
   *)
  val getRecTyvarMap : int * Types.ty -> (int -> bool)
  val gtLabel : Symbol.symbol * Symbol.symbol -> bool

  (* return true if there exists a value that the pattern does *not* match *)
  val refutable: Absyn.pat -> bool

  val isValue : Absyn.exp -> bool
  (* checks whether an expression is nonexpansive; used to determine
   * when type generalization is permitted under the value rule *)
  (*
  dbm: where has this moved to? typecheck.sml?
  gk: restoring this function because PrimopId is now self-contained.
  *)
  val isVarTy : Types.ty -> bool

  val sortFields : (Absyn.numberedLabel * 'a) list
        -> (Absyn.numberedLabel * 'a) list
  val projectField : Symbol.symbol * Types.ty -> Types.ty option

  val mapUnZip : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list
  (* this is obviously a generic list utility fn, so should be in
   * a more general utility module *)

  type tycset
  val mkTycSet : unit -> tycset
  val addTycSet : Types.tycon * tycset -> tycset
  val filterSet : Types.ty * tycset -> Types.tycon list

  val dtSibling : int * Types.tycon -> Types.tycon
  val extractDcons: Types.tycon -> Types.datacon list

  val wrapDef : Types.tycon * Stamps.stamp -> Types.tycon
      (* make a tycon into a DEFtyc by "eta-expanding" if necessary *)

  val unWrapDef1 : Types.tycon -> Types.tycon option
  val unWrapDefStar : Types.tycon -> Types.tycon

  val dummyTyGen : unit -> unit -> Types.ty
      (* create dummy type generators used to instantiate ungeneralizable
       * free type variables in Typechecking.generalizeTy *)

  val tyToString : Types.ty -> string

(* return size and signedness information about integer and word types.  We use a width
 * of zero for IntInf.int.
 *)
  val numInfo : Types.ty -> { wid : int, signed : bool }

(* check an integer/word literal value for being in range as defined by its type *)
  val numInRange : IntInf.int * Types.ty -> bool

  val dataconName : Types.datacon -> Symbol.symbol
  (* the "name" of a datacon *)

  val dataconSign : Types.datacon -> Access.consig
  (* the "sign" of a datacon, which is the sign of its datatype *)

  val dataconIsConst : Types.datacon -> bool
  (* returns true if the datacon is a constant *)

end  (* signature TYPESUTIL *)
