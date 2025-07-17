(* symbol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 * (C) 2025 The Fellowship of SML/NJ (www.smlnj.org)
 *)

structure Symbol :> SYMBOL =
struct

local (* imports *)

  structure EM = ErrorMsg
  structure HS = HashString

in

  (* a symbol is essentially a pair of a name string and a hash number (word) that is 
     computed by hashing the name string and adding a small offset corresponding to a name
     space (nameSpaceOffset). There are 9 different varieties of symbols, distinguished
     by their name spaces. *)

  type rawsymbol = word * string

  type symbol = word * string   (* the hash (word) includes a small namespace offset *)

  (* 9 name spaces! Too many! Name spaces will become separate environment components in NFE.
   * LABspace and FIXspace can be dropped, while the module spaces can be merged,
   * as can TYCspace and TYVspace. This reduces the number of name spaces from 9 to 3. *)

  datatype namespace
     = VALspace | TYCspace | SIGspace | STRspace | FCTspace
     | FIXspace | LABspace | TYVspace | FSIGspace

  (* nameSpaceOffset : namespace -> word *)
  fun nameSpaceOffset (ns : namespace ) =
      case ns
	of VALspace => 0w0
	 | TYCspace => 0w1
	 | TYVspace => 0w2
	 | STRspace => 0w3
	 | FCTspace => 0w4
	 | SIGspace => 0w5
	 | FSIGspace => 0w6
	 | LABspace => 0w7
	 | FIXspace => 0w8
			     
  (* nameSpace : symbol -> namespace *)
  fun nameSpace ((hash,_): symbol) : namespace =
      case (hash - HS.hashString name) (* namespace offset of hash number *)
	of 0w0 => VALspace
	 | 0w1 => TYCspace
	 | 0w2 => TYVspace
	 | 0w3 => STRspace
	 | 0w4 => FCTspace
	 | 0w5 => SIGspace
	 | 0w6 => FSIGspace
	 | 0w7 => LABspace
	 | 0w8 => FIXspace
	 | _ => EM.impossible "Symbol.nameSpace"

  (* mkSymbol : namespace -> string -> symbol *)
  fun mkSymbol (ns : namespace) (name: string) : symbol = 
      (HS.hashStrinig name + nameSpaceOffset ns, name)

  (* value Symbols -- names of value variables and data constructors *)

  (* valSymbol : string -> symbol *)
  val valSymbol = mkSymbol VALspace


  (* type constructors and type variables *)

  (* tycSymbol : string -> symbol *)
  val tycSymbol = mkSymbol TYCspace

  (* tyvSymbol : string -> symbol *)
  val tyvSymbol = mkSymbol TYVspace


  (* modules and module signatures *)

  (* strSymbol : string -> symbol *)
  val strSymbol = mkSymbol STRspace

  (* fctSymbol : string -> symbol *)
  val fctSymbol = mkSymbol FCTspace

  (* sigSymbol : string -> symbol *)
  val sigSymbol = mkSymbol SIGspace

  (* fsigSymbol : string -> symbol *)
  val fsigSymbol = mkSymbol FSIGspace


  (* others -- these an not essential *)

  (* fixSymbol : string -> symbol *)
  val fixSymbol = mkSymbol FIXspace

  (* labSymbol : string -> symbol *)
  val labSymbol = mkSymbol LABspace


  (* compare : symbol * symbol -> order *)
  fun compare ((hash1, name1): symbol, (hash2, name2): symbol) =
      case Word.compare (hash1, hash2)
        of EQUAL => String.compare (name1, name2)
	 | order => order

  (* name: symbol -> string *)
  fun name ((_,name): symbol) = name

  (* hash : symbol -> word (was "number") *)
  fun hash ((hash,_): symbol) = hash

  (* nameSpaceToString : namespace -> string
   * used only once in an error message in elabsig.sml *)
  fun nameSpaceToString (ns : namespace) : string =
      case ns
	of VALspace => "variable or constructor"
	 | TYCspace => "type constructor"
	 | SIGspace => "signature"
	 | STRspace => "structure"
	 | FCTspace => "functor"
	 | FIXspace => "fixity"
	 | LABspace => "label"
	 | TYVspace => "type variable"
	 | FSIGspace => "functor signature"

  (* describe : symbol -> string *)
  fun describe (s: symbol) = concat [nameSpaceToString (nameSpace s), " ", name s]

  (* symbolToString: symbol -> string *)
  (* A "printname" for a symbol, making its namespace explicit. *)
  fun symbolToString (s: symbol) : string =
      case nameSpace s
	of VALspace => "VAL$"^name
	 | SIGspace => "SIG$"^name
	 | STRspace => "STR$"^name
	 | FSIGspace => "FSIG$"^name
	 | FCTspace => "FCT$"^name
	 | TYCspace => "TYC$"^name
	 | LABspace => "LAB$"^name
	 | TYVspace => "TYV$"^name
	 | FIXspace => "FIX$"^name

end (* top local - imports *)
end (* structure Symbol *)
