(* symbol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 * (C) 2025 The Fellowship of SML/NJ (www.smlnj.org)
 *)

structure Symbol :> SYMBOL =
struct

local (* imports *)

  structure EM = ErrorMsg  (* out of domain error for nameSpaceOffset *)
  structure HS = HashString

in

  (* a symbol is essentially a pair of a name string and a hash number (word) that is 
     computed by hashing the name string and adding a small offset corresponding to a name
     space (nameSpaceOffset). There are 9 different varieties of symbols, distinguished
     by their name spaces, where the namespace is represented by a small positive offset
     from the string's base hash.
     This is a merger of Symbol.symbol and FastSymbol.raw_symbol, so FastSymbol.raw_symbol
     is now redundant and is never referenced. *)

  type symbol = word * string   (* the hash (word) includes a small namespace offset *)

  (* 9 name spaces are too many! Name spaces will become separate environment components
   * in NFE, and there will be just 3 of these: values, types, modules.
   * LABspace and FIXspace can be dropped, while the 4 module spaces can be merged into 1,
   * and TYCspace and TYVspace can be merged into a type namespace.
   * The namespace of a symbol will then correspond to the environment component in which
   * it is bound (i.e. is a member of the domain). It will thus be possible for a symbol
   * (reprsented by an atom) to simultaneously be in more than one name space (but this
   * should not happen!).
   *)

  (* There is a notion of a "raw" symbol, i.e. a symbol without an assigned name space.
     But if the lexer produces strings for identifiers rather than assigning them symbols,
     we probably never have to generate raw symbols.  There is a question of a few special
     symbols like "*" and "=" (and perhaps "->"	) that are a bit ambiguous with respect to
     what name space they should belong to. The main reason for having raw symbols seems to
     be so that the lexer can return (raw) symbols rather than strings for various identifiers,
     because the lexer doesn't have the context required to assign them to name spaces. *)

  datatype namespace
     = VALspace  (* value variables and data constructors *)

     | TYCspace  (* type constructors *)
     | TYVspace  (* type variables *)

     | STRspace  (* structures *)
     | FCTspace  (* functors *)
     | SIGspace  (* structure signatures *)
     | FSIGspace (* functor signatures *)

     | FIXspace  (* "fixity" bindings of variables and data constructors *)
     | LABspace  (* record labels *)

  (* nameSpaceOffset : namespace -> word *)
  fun nameSpaceOffset (ns : namespace ) =
      case ns
	of VALspace => 0w1
	 | TYCspace => 0w2
	 | TYVspace => 0w3
	 | STRspace => 0w4
	 | FCTspace => 0w5
	 | SIGspace => 0w6
	 | FSIGspace => 0w7
	 | LABspace => 0w8
	 | FIXspace => 0w9
			     

  (* name: symbol -> string *)
  fun name ((_,name): symbol) = name

  (* hash : symbol -> word (was "number") *)
  fun hash ((hash,_): symbol) = hash

  (* nameSpace : symbol -> namespace *)
  fun nameSpace ((hash,_): symbol) : namespace =
      case (hash - HS.hashString name) (* namespace offset of hash number *)
	of 0w1 => VALspace
	 | 0w2 => TYCspace
	 | 0w3 => TYVspace
	 | 0w4 => STRspace
	 | 0w5 => FCTspace
	 | 0w6 => SIGspace
	 | 0w7 => FSIGspace
	 | 0w8 => LABspace
	 | 0w9 => FIXspace
	 | _ => EM.impossible "Symbol.nameSpace"

  (* rawSymbol : string -> symbol *)
  (* a rqw symbol without a namespace offset on its hash value.
  *  Hence a symbol belonging to no namespace. *)
  fun rawSymbol name = (HS.hashStrinig name, name)

  (* mkSymbol : namespace -> string -> symbol *)
  fun mkSymbol (ns : namespace) (name: string) : symbol = 
      let val (hash, name) = rawSymbol name
       in (hash + nameSpaceOffset ns, name)
      end

  (* value Symbols -- names of value variables and data constructors *)

  (* valSymbol : string -> symbol *)
  val valSymbol = mkSymbol VALspace


  (* type constructors and type variables  -- these should be in the same name space *)

  (* tycSymbol : string -> symbol *)
  val tycSymbol = mkSymbol TYCspace

  (* tyvSymbol : string -> symbol *)
  val tyvSymbol = mkSymbol TYVspace


  (* modules and module signatures -- these should all be in the same name space *)

  (* strSymbol : string -> symbol *)
  val strSymbol = mkSymbol STRspace

  (* fctSymbol : string -> symbol *)
  val fctSymbol = mkSymbol FCTspace

  (* sigSymbol : string -> symbol *)
  val sigSymbol = mkSymbol SIGspace

  (* fsigSymbol : string -> symbol *)
  val fsigSymbol = mkSymbol FSIGspace


  (* others -- these an not essential and will be eliminated (as namespaces) *)

  (* fixSymbol : string -> symbol *)
  val fixSymbol = mkSymbol FIXspace

  (* labSymbol : string -> symbol *)
  val labSymbol = mkSymbol LABspace


  (* mapping raw symbols (no hash offset) to symbols in name spaces (used in ml.grm)
     We don't need these if the lexer is returning string values rather than (raw) symbols
     for the various kinds of identifiers. *)

  (* toVal : symbol (* raw *) -> symbol (VALspace) *)
  fun toVal (h,n) = (h + nameSpaceOffset VALspace, n)

  (* toTyc : symbol (* raw *) -> symbol (TYCspace) *)
  fun toTyc (h,n) = (h + nameSpaceOffset TYCspace, n)

  (* toStr : symbol (* raw *) -> symbol (STRspace) *)
  fun toStr (h,n) = (h + nameSpaceOffset STRspace, n)

  (* toFct : symbol (* raw *) -> symbol (FCTspace) *)
  fun toFct (h,n) = (h + nameSpaceOffset FCTspace, n)

  (* toSig : symbol (* raw *) -> symbol (SIGspace) *)
  fun toSig (h,n) = (h + nameSpaceOffset SIGspace, n)

  (* toFsig : symbol (* raw *) -> symbol (FSIGspace) *)
  fun toFsig (h,n) = (h + nameSpaceOffset FSIGspace, n)


  (* compare : symbol * symbol -> order *)
  (* two symbols belonging to different name spaces (including none) cannot be equal *)
  fun compare ((hash1, name1): symbol, (hash2, name2): symbol) =
      case Word.compare (hash1, hash2)
        of EQUAL => String.compare (name1, name2)
	 | order => order

  (* nameSpaceToString : namespace -> string
   * used only once in a single error message in elabsig.sml *)
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
  fun symbolToString (s as (_, name): symbol) : string =
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

  structure HashKey: HASH_KEY =
  struct
    type hash_key = symbol
    val hashVal = hash
    fun sameKey (s1: hash_key, s2: hash_key) =
	case compare (s1, s2)
	  of EQUAL => true
	   | _ => false
  end

  structure HashTable : MONO_HASH_TABLE where type Key.hash_key = symbol =
    HashTableFn (HashKey)

end (* top local - imports *)
end (* structure Symbol *)
