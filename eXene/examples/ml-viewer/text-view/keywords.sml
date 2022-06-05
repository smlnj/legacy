(* keywords.sml
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * This structure implements a hash table for mapping identifiers to
 * unique names (and parser tokens).
 *)

structure Keywords : sig

    val mkToken : {space : int, text : string}
	  -> {space : int, kind : ViewBuffer.token_kind, text : string}

  end = struct

    structure VB = ViewBuffer

  (* the keyword hash table *)
    exception Keyword
    val keywords : VB.token_kind Name.name_tbl = Name.mkNameTbl(64, Keyword)

  (* insert the reserved words into the keyword hash table *)
    val _ = let
	  val insert = Name.insert keywords
	  fun ins (s, item) = insert (Name.mkName s, item)
	  in
	    app ins [
	      (* SML reserved words *)
		("*", VB.Symbol),
		("|", VB.Symbol),
		(":", VB.Symbol),
		("=", VB.Symbol),
		("#", VB.Symbol),
		("and", VB.Keyword),
		("abstype", VB.Keyword),
		("->", VB.Symbol),
		("as", VB.Keyword),
		("case", VB.Keyword),
		("datatype", VB.Keyword),
		("else", VB.Keyword),
		("end", VB.Keyword),
		("exception", VB.Keyword),
		("=>", VB.Symbol),
		("fn", VB.Keyword),
		("fun", VB.Keyword),
		("handle", VB.Keyword),
		("if", VB.Keyword),
		("in", VB.Keyword),
		("infix", VB.Keyword),
		("infixr", VB.Keyword),
		("let", VB.Keyword),
		("local", VB.Keyword),
		("nonfix", VB.Keyword),
		("of", VB.Keyword),
		("op", VB.Keyword),
		("open", VB.Keyword),
		("raise", VB.Keyword),
		("then", VB.Keyword),
		("type", VB.Keyword),
		("val", VB.Keyword),
		("with", VB.Keyword),
		("withtype", VB.Keyword),
		("orelse", VB.Keyword),
		("andalso", VB.Keyword),
		("abstraction", VB.Keyword),
		("do", VB.Keyword),
		("eqtype", VB.Keyword),
		("funsig", VB.Keyword),
		("functor", VB.Keyword),
		("include", VB.Keyword),
		("overload", VB.Keyword),
		("rec", VB.Keyword),
		("ref", VB.Keyword),
		("sharing", VB.Keyword),
		("sig", VB.Keyword),
		("signature", VB.Keyword),
		("struct", VB.Keyword),
		("structure", VB.Keyword),
		("while", VB.Keyword)
	      ]
	  end

    val peek = Name.peek keywords

    fun mkToken {space, text} = let
	  val name = Name.mkName text
	  val kind = (case (peek name) of (SOME k) => k | _ => VB.Ident)
	  in
	    {space = space, kind = kind, text = Name.stringOf name}
	  end

  end (* Keywords *)

