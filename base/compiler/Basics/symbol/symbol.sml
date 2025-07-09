(* symbol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 * (C) 2025 The Fellowship of SML/NJ (www.smlnj.org)
 *)

structure Symbol (* :> SYMBOL *) =
struct

local (* imports *)

  structure EM = ErrorMsg
  structure HS = HashString

in

  (* a symbol is essentially a pair of a name string and a hash number (word) that is 
     computed by hashing the name string and adding a small offset corresponding to a name
     space (nameSpaceOffset) *)

  datatype symbol = word * string

  (* 9 name spaces! Too many! Name spaces will become separate environment components.
   * LABspace and FIXspace will go away. The module spaces can be merged, as can TYCspace and
   * TYVspace. This reduces the number of name spaces from 9 to 3. *)

  datatype namespace
     = VALspace | TYCspace | SIGspace | STRspace | FCTspace
     | FIXspace | LABspace | TYVspace | FSIGspace

  (* nameSpaceOffset : namespace -> word *)
  fun nameSpaceOffset (ns : namespace ) =
      (case ns
 	 of VALspace => 0w0
	  | TYCspace => 0w1
	  | TYVspace => 0w2
	  | STRspace => 0w3
	  | FCTspace => 0w4
	  | SIGspace => 0w5
	  | FSIGspace => 0w6
	  | LABspace => 0w8
	  | FIXspace => 0w7
			     
  (* nameSpace : symbol -> namespace *)
  fun nameSpace ((hash,_): symbol) : namespace =
      case hash - HS.hashString name (* namespace offset *)
	of 0w0 => VALspace
	 | 0w1 => TYCspace
	 | 0w2 => SIGspace
	 | 0w3 => STRspace
	 | 0w4 => FCTspace
	 | 0w5 => FIXspace
	 | 0w6 => LABspace
	 | 0w7 => TYVspace
	 | 0w8 => FSIGspace
	 | _ => EM.impossible "Symbol.nameSpace"

  (* mkSymbol : string * namespace -> symbol *)
  fun mkSymbol (name: string, ns : namespace) = 
      (HS.hashStrinig name + nameSpaceOffset ns, name)

  (* value Symbols -- names of  value variables and data constructors *)

  (* valSymbol : string -> symbol *)
  fun valSymbol (name: string) = mkSymbol (name, VALspace)

  (* type constructors and type variables *)

  (* tycSymbol : string -> symbol *)
  fun tycSymbol (name: string) = mkSymbol (name, TYCspace)

  (* tyvSymbol : string -> symbol *)
  fun tyvSymbol (name: string) = mkSymbol (name, TYVspace)


  (* modules and module signatures *)

  (* strSymbol : string -> symbol *)
  fun strSymbol (name: string) = mkSymbol (name, STRspace)

  (* fctSymbol : string -> symbol *)
  fun fctSymbol (name: string) = mkSymbol (name, FCTspace)

  (* sigSymbol : string -> symbol *)
  fun sigSymbol (name: string) = mkSymbol (name, SIGspace)

  (* fsigSymbol : string -> symbol *)
  fun fsigSymbol (name: string) = mkSymbol (name, FSIGspace)


  (* fixSymbol : string -> symbol *)
  fun fixSymbol (name: string) = mkSymbol (name, FIXspace)

  (* labSymbol : string -> symbol *)
  fun labSymbol (name: string) = mkSymbol (name, LABspace)

  (* eq : symbol * symbol -> bool *)
  fun eq (SYMBOL(a1,b1),SYMBOL(a2,b2)) = a1=a2 andalso b1=b2

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
