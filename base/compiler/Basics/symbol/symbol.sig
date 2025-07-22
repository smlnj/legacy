(* Copyright 1989 by AT&T Bell Laboratories *)
(* Copyright 2025 by The Fellowship of SML/NJ *)

(* [DBM, 2025.07.18; merging Symbol and FastSymbol, making FastSymbol redundant.]
 *)

signature SYMBOL =
sig

    type symbol  (* = word * string, where word is a hash of the string *)

    datatype namespace
      = VALspace | TYCspace | SIGspace | STRspace | FCTspace | FIXspace
      | LABspace | TYVspace | FSIGspace

    val compare: symbol * symbol -> order

    val rawSymbol : string -> symbol
    val mkSymbol : namespace -> string -> symbol

    (* generating namespaced symbols from strings.
     * These are just partial applications of mkString to the namespaces. *)
    val valSymbol: string -> symbol  (* e.g. = mkSymbol VALspace *)
    val tycSymbol: string -> symbol
    val sigSymbol: string -> symbol
    val strSymbol: string -> symbol
    val fctSymbol: string -> symbol
    val fsigSymbol: string -> symbol
    val fixSymbol: string -> symbol
    val labSymbol: string -> symbol
    val tyvSymbol: string -> symbol

    (* conversion of raw symbols into namespaced symbols with the same name.
     * these are not needed if the lexer is returning strings rather than raw symbols *)
    val toVal : symbol -> symbol (* raw to VALspace *)
    val toTyc : symbol -> symbol (* raw to TYCspace *)
    val toStr : symbol -> symbol (* raw to STRspace *)
    val toFct : symbol -> symbol (* raw to FCTspace *)
    val toSig : symbol -> symbol (* raw to SIGspace *)
    val toFsig : symbol -> symbol (* raw to FSIGspace *)

    val name: symbol -> string
    val hash: symbol -> word  (* formerly "number" *)
    val nameSpace : symbol -> namespace
    val nameSpaceToString : namespace -> string
    val describe : symbol -> string
    val symbolToString : symbol -> string
    val compare : symbol * symbol -> order

(* redundant given compare -- so eliminated
    val eq : symbol * symbol -> bool
    val symbolGt : symbol * symbol -> bool
    val symbolCMLt : symbol * symbol -> bool  (* special ordering for CM? *)
*)

   structure HashTable : MONO_HASH_TABLE where type Key.hash_key = symbol

end (* signature SYMBOL *)
