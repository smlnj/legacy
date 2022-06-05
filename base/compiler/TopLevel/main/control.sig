(* control.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* match compiler controls *)
signature MCCONTROL =
  sig
    val debugging : bool ref
    val printArgs : bool ref
    val printRet : bool ref
    val bindNoVariableWarn : bool ref
    val bindNonExhaustiveWarn : bool ref
    val bindNonExhaustiveError : bool ref
    val matchNonExhaustiveWarn : bool ref
    val matchNonExhaustiveError : bool ref
    val matchRedundantWarn : bool ref
    val matchRedundantError : bool ref
  end (* signature MCCONTROL *)

(* general code-generation controls *)
signature CGCONTROL =
  sig
    val closureStrategy : int ref
    val cpsopt : string list ref		(* list of cpsopt phases *)
    val rounds : int ref
    val betacontract : bool ref
    val eta : bool ref
    val selectopt : bool ref
    val dropargs : bool ref
    val deadvars : bool ref
    val flattenargs : bool ref
    val extraflatten : bool ref
    val switchopt : bool ref
    val handlerfold : bool ref
    val branchfold : bool ref
    val arithopt : bool ref
    val betaexpand : bool ref
    val unroll : bool ref
    val invariant: bool ref
    val lambdaprop: bool ref
    val boxedconstconreps : bool ref
    val sharepath : bool ref
    val staticprof : bool ref
    val unroll_recur : bool ref
    val debugcps : bool ref
    val bodysize : int ref
    val reducemore : int ref
    val comment : bool ref	(* used in CPS/clos/closure.sml to control debug messages *)
    val knownGen : int ref
    val knownClGen : int ref
    val escapeGen : int ref
    val calleeGen : int ref
    val spillGen : int ref
    val etasplit : bool ref
    val uncurry : bool ref
    val ifidiom : bool ref
    val comparefold : bool ref
    val debugLits : bool ref
    val newLiterals : bool ref
    val debugRep : bool ref
    val deadup : bool ref
    val memDisambiguate : bool ref	(* used by MLRISC *)
    val printit : bool ref
    val printClusters : bool ref
  end (* signature CGCONTROL *)

(* main Control structure *)
signature CONTROL =
  sig
    structure Print : PRINTCONTROL
    structure ElabData : ELABDATA_CONTROL
    structure Elab : ELAB_CONTROL
    structure MC : MCCONTROL
    structure FLINT : FLINTCONTROL
    structure CG : CGCONTROL
    structure MLRISC : MLRISC_CONTROL
    val debugging : bool ref
    val printAst : bool ref
    val printAbsyn : bool ref

    include BASIC_CONTROL
    (* provides: val printWarnings : bool ref
     *)
    include PARSER_CONTROL
    (* provides: val primaryPrompt : string ref
		 val secondaryPrompt : string ref
		 val overloadKW : bool ref
		 val lazysml : bool ref
		 val quotation : bool ref
     *)

    val interp : bool ref
       (* turn on interpreter -- defunct *)

    val progressMsgs : bool ref
       (* turn on printing of progress messages at end of major stages *)

    val saveLambda : bool ref
    val preserveLvarNames : bool ref
    val trackExn : bool ref
    val polyEqWarn : bool ref

    val saveit : bool ref
    val saveAbsyn : bool ref
    val saveConvert : bool ref
    val saveCPSopt : bool ref
    val saveClosure : bool ref

    structure LambdaSplitting : sig
	datatype globalsetting
	  = Off			      (* completely disabled *)
	  | Default of int option       (* default aggressiveness; NONE: off *)
	type localsetting = int option option
	val UseDefault : localsetting
	val Suggest : int option -> localsetting
	val set : globalsetting -> unit
	val get : unit -> int option
	val get' : localsetting -> int option
	val parse : string -> globalsetting option
	val show : globalsetting -> string
    end

    val tdp_instrument : bool ref

  end (* signature CONTROL *)
