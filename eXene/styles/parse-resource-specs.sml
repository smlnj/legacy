(* parse-resource-specs.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Support for parsing X11 format resource specifications.
 *)

structure ParseResourceSpecs : sig

    type comp_name = Quark.quark
    type attr_name = Quark.quark

    datatype component = Wild | Name of comp_name
	(* a component is either "?" or a component name *)

    datatype binding = TIGHT | LOOSE

    datatype resource_spec
      = NoSpec			(* comment or blank line *)
      | Incl of string		(* "#include" directive *)
      | RsrcSpec of {
	    loose : bool,	    (* true, if the spec has a leading "*" *)
	    path : (component * binding) list,
	    attr : attr_name,	    (* the attribute name *)
	    value : string,	    (* the value *)
	    ext : bool		    (* true, if the value extends onto the *)
				    (* next line *)
	  }

  (* this exception is raised, if the specification is ill-formed.  The
   * integer argument is the character position of the error.
   *)
    exception BadSpec of int

    val parseRsrcSpec : string -> resource_spec
	(* decompose a resource specification string into a list
	 * of (component, binding) pairs, an attribute name, and
	 * an attribute value.
	 *)

    val parseValueExt : string -> (string * bool)
	(* Parse a value extension, returning the extension and a boolean flag
	 * that will be true if there is a further extension of the value.
	 *)

    val parseStyleName : string -> comp_name list
	(* Check and decompose a style name, which has the format:
	 *
	 *   <StyleName> ::= <ComponentName> ("." <ComponentName>)*
	 *)

    val checkCompName : string -> comp_name
	(* Check a component name *)

    val checkAttrName : string -> attr_name
	(* Check an attribute name *)

  end = struct

    structure SS = Substring

    val maxChar = 255

    datatype char_class
      = Comment		(* "!" *)
      | Directive	(* "#" *)
      | TightBind	(* "." *)
      | LooseBind	(* "*" *)
      | WildComp	(* "?" *)
      | Space		(* space or tab *)
      | Colon		(* ":" *)
      | NameChar	(* "A"-"Z", "a"-"z", "0"-"9", "-", "_" *)
      | Eol		(* newline *)
      | Escape		(* "\" *)
      | NonPrt		(* other non-printing characters *)
      | Other		(* other printing characters *)

  (* this table maps character ordinals to character classes *)
    val ccMap = CharMap.mkCharMap {
	    default = NonPrt,
	    bindings = [
		("!",				Comment),
		("#",				Directive),
		(".",				TightBind),
		("*",				LooseBind),
		("?",				WildComp),
		(" \t",				Space),
		(":",				Colon),
		("ABCDEFGHIJKLMNOPQRSTUVWXYZ\
		 \abcdefghijklmnopqrstuvwxyz\
		 \0123456789-_",		NameChar),
		("\n",				Eol),
		("\\",				Escape),
		("\"$%&'()+,/;<=>@[]^`{|}~",	Other)
	      ]
	  }
    val mapChr = CharMap.mapStrChr ccMap

  (* get the class of the i'th character of a string *)
    fun getCC (s, i) =
	  if (i < size s) then mapChr(s, i) else Eol

  (* skip white space *)
    fun skipWS (s, i) = if (getCC(s, i) = Space) then skipWS(s, i+1) else i

    type comp_name = Quark.quark
    type attr_name = Quark.quark

    datatype component = Wild | Name of comp_name

    datatype binding = TIGHT | LOOSE

    datatype resource_spec
      = NoSpec			(* comment or blank line *)
      | Incl of string		(* "#include" directive *)
      | RsrcSpec of {
	    loose : bool,	    (* true, if the spec has a leading "*" *)
	    path : (component * binding) list,
	    attr : attr_name,	    (* the attribute name *)
	    value : string,	    (* the value *)
	    ext : bool		    (* true, if the value extends onto the *)
				    (* next line *)
	  }

  (* this exception is raised, if the specification is ill-formed.  The
   * integer argument is the character position of the error.
   *)
    exception BadSpec of int

  (* scan a component *)
    fun scanComp (s, i) = (case getCC(s, i)
	   of WildComp => (Wild, i+1)
	    | NameChar => let
		fun scan j = (case getCC(s, j)
		       of NameChar => scan(j+1)
		        | _ => j-i
		      (* end case *))
		val len = scan (i+1)
		in
		  (Name(Quark.quark(substring(s, i, len))), i+len)
		end
	    | _ => raise (BadSpec i)
	  (* end case *))

  (* Scan a binding, which is a sequence of one or more "." and "*" characters.
   * If any character in the binding is "*", then it is a loose binding,
   * otherwise it is a TIGHT binding.
   *)
    fun scanBinding (s, i) = let
	  fun scan (s, i, bind) = (case getCC(s, i)
		 of LooseBind => scan (s, i+1, LOOSE)
		  | TightBind => scan (s, i+1, bind)
		  | _ => (bind, i)
		(* end case *))
	  in
	    case getCC(s, i)
	     of LooseBind => scan (s, i+1, LOOSE)
	      | TightBind => scan (s, i+1, TIGHT)
	      | _ => raise (BadSpec i)
	    (* end case *)
	  end

  (* Scan a value, returning it as a string with a boolean extension
   * flag.  This recognizes and converts escape sequences as follows:
   *
   *   \<space>		==> a space character
   *   \<tab>		==> a tab character
   *   \\		==> a backslash character
   *   \n		==> a newline character
   *   \<newline>	==> ignore the newline; if the newline is the last
   *			    character in the string, then the extension flag
   *			    is true.
   *   \ddd		==> convert octal digits to character code.
   *)
    fun scanValue (s, i) = let
	  fun getOctal ss = let
		val scan = Int.scan StringCvt.OCT SS.getc
		fun isOct c = (#"0" <= c) andalso (c < #"8")
		val (oct, rest) = SS.splitAt (ss, 3)
		in
		  if isOct(SS.sub(oct, 0))
		    then (case (scan oct)
		       of SOME(n, r) =>
			    if (SS.isEmpty r)
			      then (String.str(Char.chr n), rest)
			      else raise BadSpec i
			| NONE => raise BadSpec i
		      (* end case *))
		    else raise BadSpec i
		end
		  handle _ => raise BadSpec i
	  fun finish (prefix, chunks) = SS.concat(List.rev(prefix::chunks))
	  fun scan (ss, chunks) = let
		val (prefix, rest) =
		      SS.splitl (fn (#"\\" | #"\n") => false | _ => true) ss
		fun add (c, rest) = scan (rest, (SS.full c)::prefix::chunks)
		in
		  case (SS.getc rest)
		   of NONE => (finish(prefix, chunks), false)
		    | SOME(#"\n", rest) => (finish(prefix, chunks), false)
		    | SOME(_, rest) => (case (SS.getc rest)
			 of NONE => (finish(prefix, chunks), true)
			  | (SOME(#"\t", rest)) => add("\t", rest)
			  | (SOME(#" ", rest)) => add(" ", rest)
			  | (SOME(#"\\", rest)) => add("\\", rest)
			  | (SOME(#"\n", rest)) => (case (SS.getc rest)
			       of (SOME _) => scan(rest, prefix::chunks)
				| NONE => (finish(prefix, chunks), true)
			      (* end case *))
			  | (SOME(#"n", rest)) => add("\n", rest)
			  | (SOME _) => add(getOctal rest)
			(* end case *))
		  (* end case *)
		end
	  in
	    scan (SS.triml i (SS.full s), [])
	  end

  (* decompose a resource specification string into a list
   * of (component, binding) pairs, an attribute name, and
   * an attribute value.
   *)
    fun parseRsrcSpec ln = let
	  val start = skipWS(ln, 0)
	  fun getCompBind (i, path) = let
		val (comp, i) = scanComp (ln, i)
		fun getRest i = (case comp
		       of (Name attr) => (rev path, attr, skipWS(ln, i+1))
			| Wild => raise (BadSpec i)
		      (* end case *))
		in
		  case (getCC (ln, i))
		   of Colon => getRest i
		    | Space => let
			val i = skipWS(ln, i+1)
			in
			  case getCC(ln, i)
			   of Colon => getRest i
			    | _ => raise (BadSpec i)
			  (* end case *)
			end
		    | _ => let
			val (bind, i) = scanBinding (ln, i)
			in
			  getCompBind (i, (comp, bind)::path)
			end
		  (* end case *)
		end
	  in
	    case getCC(ln, start)
	     of (Eol | Comment) => NoSpec
	      | Directive => NoSpec (* fix *)
	      | (WildComp | NameChar) => let
		  val (path, attrName, valStart) = getCompBind(start, [])
		  val (value, ext) = scanValue (ln, valStart)
		  in
		    RsrcSpec{
			loose = false, path = path,
			attr = attrName, value = value,
			ext = ext
		      }
		  end
	      | LooseBind => let
		  val (path, attrName, valStart) = getCompBind(start+1, [])
		  val (value, ext) = scanValue (ln, valStart)
		  in
		    RsrcSpec{
			loose = true, path = path,
			attr = attrName, value = value,
			ext = ext
		      }
		  end
	      | _ => raise (BadSpec start)  
	    (* end case *)
	  end (* parseRsrcSpec *)

  (* Parse a value extension, returning the extension and a boolean flag
   * that will be true if there is a further extension of the value.
   *)
    fun parseValueExt ln = scanValue (ln, 0)

  (* Check and decompose a style name, which has the format:
   *
   *   <StyleName> ::= <ComponentName> ("." <ComponentName>)*
   *)
    fun parseStyleName s = let
	  val len = size s
	  fun scanCompName i = (case scanComp(s, i)
		 of (Name name, j) => (name, j)
		  | _ => raise (BadSpec i)
		(* end case *))
	  fun scan (i, comps) = if (i < len)
		then (case (mapChr(s, i))
		   of TightBind => let
			val (name, i) = scanCompName(i+1)
			in
			  scan(i, name::comps)
			end
		    | _ => raise (BadSpec i)
		  (* end case *))
		else rev comps
	  val (name, i) = scanCompName 0
	  in
	    scan (i, [name])
	  end

  (* Check a component name *)
    fun checkCompName str = (case scanComp(str, 0)
	   of (Name name, _) => name
	    | _ => raise (BadSpec 0)
	  (* end case *))

  (* Check an attribute name *)
    val checkAttrName = checkCompName

  end (* ParseResourceSpecs *)
