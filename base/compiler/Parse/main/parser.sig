(* Parse/main/parser.sig *)

(* COPYRIGHT (c) 2016, 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature SMLNJ_PARSER =
sig

    val parse : Source.source -> unit -> ParseResult.parseResult

end (* signature SMLNJ_PARSER *)
