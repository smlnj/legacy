(* errormsg.sig
 *
 * COPYRIGHT (c) 2020, 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* [DBM,  2025.04.01] Considerably simplified (legacy[dbm2] branch).
 * The anyErrors: bool ref is now an internal error flag in CompInfo.
 * This is still using the PP prettyprint library.
 *)

(* imports CompInfo, SourcLoc *)

signature ERRORMSG =
sig

    datatype severity = WARN | COMPLAIN | TERMINAL
    type bodyPrinter = PrettyPrint.stream -> unit
  
    val locationString : SourceLoc.region -> string
    (* Prints region as a pair of locations, concatenating the source name on the front;
     * Used just in FLINT/trans/translate.sml, so this probably belongs somewhere else? *)					   
    exception Error

    val nullErrorBody : bodyPrinter  (* --> "nullBodyPrinter" ? *)

    val error : SourceLoc.region -> severity -> string -> bodyPrinter -> unit

  (* a couple simpler error functions. These can be used instead of error with nullErrorBody. *)
    val errorRegion: SourceLoc.region * string -> unit
    val warnRegion: SourceLoc.region * string -> unit

    val impossible : string -> 'a

end (* signature ERRORMSG *)
