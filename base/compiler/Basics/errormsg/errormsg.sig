(* errormsg.sig
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ERRORMSG =
sig

    datatype severity = WARN | COMPLAIN | TERMINAL
    type bodyPrinter = PrettyPrint.stream -> unit
    type errorFn = SourceMap.region -> severity -> string -> bodyPrinter -> unit
    type complainer = severity -> string -> bodyPrinter -> unit
    type errors (* = {error: errorFn,
                      errorMatch: region->string,
                      anyErrors : bool ref} *)

    exception Error

    val anyErrors : errors -> bool
    val defaultConsumer : unit -> PrettyPrint.device
    val nullErrorBody : bodyPrinter
    val error : Source.inputSource -> errorFn
    (* with a known location string but without access to the actual source: *)
    val errorNoSource :
	PrettyPrint.device * bool ref -> string -> complainer
    val errorNoFile : PrettyPrint.device * bool ref -> SourceMap.region
                      -> complainer

    val matchErrorString : Source.inputSource -> SourceMap.region -> string
    val errors : Source.inputSource -> errors
    val errorsNoFile : PrettyPrint.device * bool ref -> errors

    val impossible : string -> 'a
    val impossibleWithBody : string -> (PrettyPrint.stream -> unit) -> 'a

end (* signature ERRORMSG *)
