(* Copyright 1989 by AT&T Bell Laboratories *)
(* Copyright 2025 by The Fellowship of SML/NJ *)

signature SYMBOL =
sig

    type symbol

    datatype namespace
      = VALspace | TYCspace | SIGspace | STRspace | FCTspace | FIXspace
      | LABspace | TYVspace | FSIGspace

    val compare: symbol * symbol -> order

    val mkSymbol : string * namespace

    val valSymbol: string -> symbol
    val tycSymbol: string -> symbol
    val sigSymbol: string -> symbol
    val strSymbol: string -> symbol
    val fctSymbol: string -> symbol
    val fsigSymbol: string -> symbol
    val fixSymbol: string -> symbol
    val labSymbol: string -> symbol
    val tyvSymbol: string -> symbol

    val name: symbol -> string
    val hash: symbol -> word  (* formerly "number" *)
    val nameSpace : symbol -> namespace
    val nameSpaceToString : namespace -> string
    val describe : symbol -> string
    val symbolToString : symbol -> string
    val compare : symbol * symbol -> order

(* redundant -- so eliminated
    val symbolGt : symbol * symbol -> bool
    val symbolCMLt : symbol * symbol -> bool
*)

end (* signature SYMBOL *)
