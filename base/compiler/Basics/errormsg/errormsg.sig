(* errormsg.sig
 *
 * COPYRIGHT (c) 2020, 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* [DBM,  2025.04.01] Considerably simplified (legacy[dbm2] branch).
 * anyErrors: bool ref is now in CompInfo.
 * This is still using the PP prettyprint library.
 *)

(* imports CompInfo, SourcLoc *)

signature ERRORMSG =
sig

    datatype severity = WARN | COMPLAIN | TERMINAL
    type bodyPrinter = PrettyPrint.stream -> unit
  
    val locationString : SL.region -> string
    (* used in FLINT/trans/translate.sml *)
    (* prints region, concatenating source name to the front; belongs somewhere else? *)					   
    exception Error

    val nullErrorBody : bodyPrinter  (* --> "nullBodyPrinter" ? *)

    val error : SourceLoc.region -> severity -> string -> bodyPrinter -> unit

  (* a couple simpler error functions *)
    val errorRegion: SourceLoc.region * string -> unit
    val warnRegion: SourceLoc.region * string -> unit

    val impossible : string -> 'a
    val impossibleWithBody : string -> bodyPrinter -> 'a

end (* signature ERRORMSG *)
