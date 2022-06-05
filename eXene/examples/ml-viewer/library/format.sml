(* format.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *
 * TODO
 *   - field widths in scan
 *   - add PREC of (int * fmt_item) constructor to allow dynamic control of
 *     precision.
 *   - precision in %d, %s, ...
 *   - * flag in scan (checks, but doesn't scan input)
 *   - %n specifier in scan
 *)

structure Format : FORMAT =
  struct

    structure SC = StringCvt
    structure M = Makestring

    datatype fmt_item
      = INT of int
      | BOOL of bool
      | STR of string
      | REAL of real
      | LEFT of (int * fmt_item)	(* left justify in field of given width *)
      | RIGHT of (int * fmt_item)	(* right justify in field of given width *)

    exception BadFormat
    exception BadArgList
    exception BadInput of fmt_item list

  (* return the index of the next non-whitespace charater in s *)
    fun eatWS (s, i) = let
	  fun f j = if CType.isSpace(s, j) then f(j+1) else j
	  in
	    (f i) handle _ => String.size s
	  end

    local
      fun pad padChars = let
	    fun mkP (i, l) = if (i <= 0)
		    then l
		  else if (i <= 20)
		    then (substring(padChars, 0, i) :: l)
		    else mkP (i-20, padChars :: l)
	    in
	      mkP
	    end
      val mkPad     = pad "                    "
      val mkZeroPad = pad "00000000000000000000"
    in
    fun padLeft (str, pad) = implode (mkPad (pad - (String.size str), [str]))
    fun padRight (str, pad) = implode (str :: mkPad (pad - (String.size str), []))
    fun zeroLPad (str, pad) = implode (mkZeroPad (pad - (String.size str), [str]))
    fun zeroRPad (str, pad) = implode (str :: mkZeroPad (pad - (String.size str), []))
    end

  (* int to string conversions (for positive integers only) *)
    datatype posint = PosInt of int | MaxInt
    local
      fun mkDigit i : string =
	    System.Unsafe.cast(System.Unsafe.ordof("0123456789abcdef", i))
    in
    fun intToOctal MaxInt = "10000000000"
      | intToOctal (PosInt i) = let
	  fun f (i, l) = if (i < 8)
		then implode((mkDigit i) :: l)
		else f(Bits.rshift(i, 3), mkDigit(Bits.andb(i, 0x7)) :: l)
	  in
	    f (i, [])
	  end
    fun intToStr MaxInt = "1073741824"
      | intToStr (PosInt i) = M.intToStr i
    fun intToHex MaxInt = "40000000"
      | intToHex (PosInt i) = let
	  fun f (i, l) = if (i < 16)
		then implode((mkDigit i) :: l)
		else f(Bits.rshift(i, 4), mkDigit(Bits.andb(i, 0xf)) :: l)
	  in
	    f (i, [])
	  end
    fun intToHeX i = CType.toUpper(intToHex i)
    end

  (* string to int conversions *)
    val octToInt = SC.strToInt SC.Oct
    val decToInt = SC.strToInt SC.Dec
    val hexToInt = SC.strToInt SC.Hex

  (* precompiled format specifiers *)
    datatype sign
      = DfltSign	(* default: put a sign on negative numbers *)
      | AlwaysSign	(* "+"      always has sign (+ or -) *)
      | BlankSign	(* " "      put a blank in the sign field for positive numbers *)
    datatype neg_sign
      = MinusSign	(* default: use "-" for negative numbers *)
      | TildeSign	(* "~"      use "~" for negative numbers *)
    type field_flags = {
	sign : sign,
	neg_char : neg_sign,
	zero_pad : bool,
	base : bool,
	ljust : bool
      }

    datatype field_wid = NoPad | Wid of int

    datatype real_format
      = F_Format		(* "%f" *)
      | E_Format of bool	(* "%e" or "%E" *)
      | G_Format of bool	(* "%g" or "%G" *)

    datatype field_type
      = OctalField
      | IntField
      | HexField
      | CapHexField
      | CharField
      | BoolField
      | StrField
      | RealField of {prec : int, format : real_format}

    datatype fmt_spec
      = Raw of string
      | CharSet of (string * int) -> bool
      | Field of (field_flags * field_wid * field_type)

  (* character sets *)
    abstype charset = CS of ByteArray.bytearray
    with
      fun mkCharSet () = CS(ByteArray.array(256, 0))
      fun addChar (CS ba, c) = ByteArray.update(ba, c, 1)
      fun addRange (CS ba, c1, c2) = let
	    fun add i = if (i <= c2)
		  then (ByteArray.update(ba, i, 1); add(i+1))
		  else ()
	    in
	      if (c1 <= c2) then (add c1) else raise BadFormat
	    end
      fun inSet (CS ba) = fn arg => (ByteArray.sub(ba, ordof arg) = 1)
      fun notInSet (CS ba) = fn arg => (ByteArray.sub(ba, ordof arg) = 0)
    end

  (* scan a field specification.  Assume that fmtStr[i-1] = "%", and
   * that fmtStr[i] <> "%".
   *)
    fun scanFieldSpec (fmtStr, i) = let
	  val (i, flags) = let
		fun doFlags (i, flags) = (case (ordof(fmtStr, i), flags)
		       of ((* " " *) 32, {sign=AlwaysSign, ...}) => raise BadFormat
			| ((* " " *) 32, {neg_char, zero_pad, base, ljust, ...}) =>
			    doFlags (i+1, {
				sign = BlankSign, neg_char = neg_char,
				zero_pad = zero_pad, base = base, ljust = ljust
			      })
			| ((* "+" *) 43, {sign=BlankSign, ...}) => raise BadFormat
			| ((* "+" *) 43, {neg_char, zero_pad, base, ljust, ...}) =>
			    doFlags (i+1, {
				sign = AlwaysSign, neg_char = neg_char,
				zero_pad = zero_pad, base = base, ljust = ljust
			      })
			| ((* "~" *) 126, {sign, zero_pad, base, ljust, ...}) =>
			    doFlags (i+1, {
				sign = sign, neg_char = TildeSign,
				zero_pad = zero_pad, base = base, ljust = ljust
			      })
			| ((* "-" *) 45, {sign, neg_char, zero_pad, base, ...}) => 
			    doFlags (i+1, {
				sign = sign, neg_char = neg_char,
				zero_pad = zero_pad, base = base, ljust = true
			      })
			| ((* "#" *) 35, {sign, neg_char, zero_pad, ljust, ...}) =>
			    doFlags (i+1, {
				sign = sign, neg_char = neg_char,
				zero_pad = zero_pad, base = true, ljust = ljust
			      })
			| ((* "0" *) 48, {sign, neg_char, base, ljust, ...}) =>
			    (i+1, {
				sign = sign, neg_char = neg_char,
				zero_pad = true, base = base, ljust = ljust
			      })
			| _ => (i, flags)
		      (* end case *))
		in
		  doFlags (i, {
		      sign = DfltSign, neg_char = MinusSign,
		      zero_pad = false, base = false, ljust = false
		    })
		end
	  val (wid, i) = if (CType.isDigit(fmtStr, i))
		then let val (n, i) = decToInt(fmtStr, i) in (Wid n, i) end
		else (NoPad, i)
	  val (ty, i) = (case (ordof (fmtStr, i))
		 of (* "d" *) 100 => (IntField, i+1)
		  | (* "X" *) 88 => (CapHexField, i+1)
		  | (* "x" *) 120 => (HexField, i+1)
		  | (* "o" *) 111 => (OctalField, i+1)
		  | (* "c" *) 99 => (CharField, i+1)
		  | (* "s" *) 115 => (StrField, i+1)
		  | (* "b" *) 98 => (BoolField, i+1)
		  | (* "." *) 46 => let
(* NOTE: "." ought to be allowed for d,X,x,o and s formats as it is in ANSI C *)
		      val (n, i) = decToInt(fmtStr, i+1)
		      val format = (case (ordof (fmtStr, i))
			     of (* "E" *) 69 => E_Format true
			      | (* "e" *) 101 => E_Format false
			      | (* "f" *) 102 => F_Format
			      | (* "G" *) 71 => G_Format true
			      | (* "g" *) 103 => G_Format false
			      | _ => raise BadFormat
			    (* end case *))
		      in
			(RealField{prec = n, format = format}, i+1)
		      end
		  | (* "E" *) 69 => (RealField{prec=6, format=E_Format true}, i+1)
		  | (* "e" *) 101 => (RealField{prec=6, format=E_Format false}, i+1)
		  | (* "f" *) 102 => (RealField{prec=6, format=F_Format}, i+1)
		  | (* "G" *) 71 => (RealField{prec=6, format=G_Format true}, i+1)
		  | (* "g" *) 103 => (RealField{prec=6, format=G_Format false}, i+1)
		  | _ => raise BadFormat
		(* end case *))
	  in
	    (Field(flags, wid, ty), i)
	  end (* scanFieldSpec *)

    fun scanField (fmtStr, i) = if (ordof(fmtStr, i) = (* "%" *) 37)
	      then (Raw "%", i+1)
	      else scanFieldSpec(fmtStr, i)

    fun scanCharSet (fmtStr, i) = let
	  val cset = mkCharSet()
	  val (isNot, i) = if (ordof(fmtStr, i) = (* "^" *) 94)
		then (true, i+1)
		else (false, i)
	  fun scan (nextChar, j) = (case (ordof(fmtStr, j))
		 of (* "-" *) 45 => let
		      val c = ordof(fmtStr, j+1)
		      in
			if (c = (* "]" *) 93)
			  then (
			    addChar(cset, nextChar);
			    addChar(cset, (* "-" *) 45);
			    j+2)
			  else (
			    addRange(cset, nextChar, c);
			    scanNext(j+2))
		      end
		  | (* "]" *) 93 => (addChar(cset, nextChar); j+1)
		  | c => (addChar(cset, nextChar); scan(c, j+1))
		(* end case *))
	  and scanNext j = (case (ordof(fmtStr, j))
		 of (* "-" *) 45 => raise BadFormat
		  | (* "]" *) 93 => j+1
		  | c => scan(c, j+1)
		(* end case *))
	  val j = scan (ordof(fmtStr, i), i+1)
	  in
	    if isNot then (j, CharSet(notInSet cset)) else (j, CharSet(inSet cset))
	  end

    fun compileFormat isScan str = (let
	  val len = String.size str
	  fun mkStr (i, j, l) =
		if (i = j) then l else (Raw(substring(str, i, (j - i))) :: l)
	  fun scan (i, j, l) = if (j < len)
		then (case (ordof (str, j))
		   of (* "%" *) 37 => let val (f, j') = scanField(str, j+1)
			in
			  scan (j', j', f :: mkStr(i, j, l))
			end
		    | (* "[" *) 91 => if isScan
			then let val (j', cs) = scanCharSet(str, j+1)
			  in
			    scan (j', j', cs :: mkStr(i, j, l))
			  end
			else scan (i, j+1, l)
		    | c => if ((CType.isSpaceOrd c) andalso isScan)
			then let val j' = eatWS(str, j+1)
			  in
			    scan (j', j', mkStr(i, j, l))
			  end
			else scan (i, j+1, l)
		  (* end case *))
		else rev (mkStr(i, j, l))
	  in
	    scan (0, 0, [])
	  end (* compileFormat *)
	    handle _ => raise BadFormat)

    fun format s = let
	  val fmts = compileFormat false s
	  fun doArgs ([], [], l) = implode(rev l)
	    | doArgs ((Raw s)::rf, args, l) = doArgs(rf, args, s::l)
	    | doArgs (Field(flags, wid, ty)::rf, arg::ra, l) = let
		fun padFn s = (case (#ljust flags, wid)
		       of (_, NoPad) => s
			| (false, Wid i) => padLeft(s, i)
			| (true, Wid i) => padRight(s, i)
		      (* end case *))
		fun zeroPadFn (sign, s) = (case wid
		       of NoPad => raise BadFormat
			| (Wid i) => zeroLPad(s, i - (String.size sign))
		      (* end case *))
		fun negate i = ((PosInt(~i)) handle _ => MaxInt)
		fun doSign i = (case (i < 0, #sign flags, #neg_char flags)
		       of (false, AlwaysSign, _) => ("+", PosInt i)
			| (false, BlankSign, _) => (" ", PosInt i)
			| (false, _, _) => ("", PosInt i)
			| (true, _, TildeSign) => ("~", negate i)
			| (true, _, _) => ("-", negate i)
		      (* end case *))
		fun doRealSign sign = (case (sign, #sign flags, #neg_char flags)
		       of (false, AlwaysSign, _) => "+"
			| (false, BlankSign, _) => " "
			| (false, _, _) => ""
			| (true, _, TildeSign) => "~"
			| (true, _, _) => "-"
		      (* end case *))
		fun doExpSign (exp, isCap) = let
		      val e = if isCap then "E" else "e"
		      fun mkExp e = zeroLPad(M.intToStr e, 2)
		      in
			case (exp < 0, #neg_char flags)
			 of (false, _) => [e, mkExp exp]
			  | (true, TildeSign) => [e, "~", mkExp(~exp)]
			  | (true, _) => [e, "-", mkExp(~exp)]
			(* end case *)
		      end
		val s = (case (ty, arg)
		       of (OctalField, INT i) => let
			    val (sign, i) = doSign i
			    val sign = if (#base flags) then sign^"0" else sign
			    val s = intToOctal i
			    in
			      if (#zero_pad flags)
				then sign ^ zeroPadFn(sign, s)
				else padFn (sign ^ s)
			    end
			| (IntField, INT i) => let
			    val (sign, i) = doSign i
			    in
			      padFn (sign ^ (intToStr i))
			    end
			| (HexField, INT i) => let
			    val (sign, i) = doSign i
			    val sign = if (#base flags) then sign^"0x" else sign
			    val s = intToHex i 
			    in
			      if (#zero_pad flags)
				then sign ^ zeroPadFn(sign, s)
				else padFn (sign ^ s)
			    end
			| (CapHexField, INT i) => let
			    val (sign, i) = doSign i
			    val sign = if (#base flags) then sign^"0X" else sign
			    val s = intToHeX i 
			    in
			      if (#zero_pad flags)
				then sign ^ zeroPadFn(sign, s)
				else padFn (sign ^ s)
			    end
			| (CharField, INT i) => padFn(chr i)
			| (BoolField, BOOL false) => padFn "false"
			| (BoolField, BOOL true) => padFn "true"
			| (StrField, STR s) => padFn s
			| (RealField{prec, format=F_Format}, REAL r) => let
			    val {sign, mantissa} = M.realFFormat(r, prec)
			    val sign = doRealSign sign
			    in
			      if ((prec = 0) andalso (#base flags))
				then padFn(implode[sign, mantissa, "."])
				else padFn(sign ^ mantissa)
			    end
			| (RealField{prec, format=E_Format isCap}, REAL r) => let
			    val {sign, mantissa, exp} = M.realEFormat(r, prec)
			    val sign = doRealSign sign
			    val expStr = doExpSign(exp, isCap)
			    in
			      if ((prec = 0) andalso (#base flags))
				then padFn(implode(sign :: mantissa :: "." :: expStr))
				else padFn(implode(sign :: mantissa :: expStr))
			    end
			| (RealField{prec, format=G_Format isCap}, REAL r) => let
			    val prec = if (prec = 0) then 1 else prec
			    val {sign, whole, frac, exp} = M.realGFormat(r, prec)
			    val sign = doRealSign sign
			    val expStr = (case exp
				   of SOME e => doExpSign(e, isCap)
				    | NONE => [])
			    val num = if (#base flags)
				    then let
				      val diff = prec - ((size whole) + (size frac))
				      in
					if (diff > 0)
					  then zeroRPad(frac, (size frac)+diff)
					  else frac
				      end
				  else if (frac = "")
				    then ""
				    else ("." ^ frac)
			    in
			      padFn(implode(sign::whole::frac::expStr))
			    end
			| (_, LEFT(w, arg)) => let
			    val flags = {
				    sign = (#sign flags), neg_char = (#neg_char flags),
				    zero_pad = (#zero_pad flags), base = (#base flags),
				    ljust = true
				  }
			    in
			      doArgs (Field(flags, Wid w, ty)::rf, arg::ra, l)
			    end
			| (_, RIGHT(w, arg)) =>
			      doArgs (Field(flags, Wid w, ty)::rf, arg::ra, l)
			| _ => raise BadArgList
		      (* end case *))
		in
		  doArgs (rf, ra, s::l)
		end
	    | doArgs _ = raise BadArgList
	  in
	    fn args => doArgs(fmts, args, [])
	  end (* format *)

    fun formatf fmt = let
	  val f = format fmt
	  in
	    fn consumer => fn args => consumer(f args)
	  end

(** NOTE: for the time being, this ignores flags and field width **)
    fun scani fmt = let
	  val fmts = compileFormat true fmt
	  fun scan (_, i, [], items) = (rev items, i)
	    | scan (s, i, (Raw s')::rf, items) = (let
		val len = String.size s'
		fun match (i, j) = if (j < len)
		      then if (ordof(s, i) = ordof(s', j))
			then match(i+1, j+1)
			else raise BadInput(rev items)
		      else i
		in
		  scan (s, match(eatWS(s, i), 0), rf, items)
		end
		  handle _ => raise BadInput(rev items))
	    | scan (s, i, (CharSet pred)::rf, items) = let
		fun scanSet i = if (pred (s, i)) then scanSet(i+1) else i
		in
		  (scan (s, scanSet i, rf, items))
		    handle _ => scan(s, size s, rf, items)
		end
	    | scan (s, i, Field(flags, wid, ty)::rf, items) = (let
		val i = eatWS(s, i)
		fun getInt strToInt = let
		      val (n, indx) = strToInt(s, i)
		      in
			(INT n, indx)
		      end
		val (item, i) = (case ty
		       of OctalField => getInt octToInt
			| IntField => getInt decToInt
			| HexField => getInt hexToInt
			| CapHexField => getInt hexToInt
			| CharField => (INT(ordof(s, i)), i+1)
			| BoolField => let val (b, indx) = SC.strToBool(s, i)
			    in
			      (BOOL b, indx)
			    end
			| StrField => let
			    val l = (case wid
				    of NoPad => String.size s
				     | (Wid n) => min(i+n, String.size s)
				  (* end case *))
			    fun getStr j = if ((j = l) orelse CType.isSpace(s, j))
				  then (STR(substring(s, i, j-i)), j)
				  else getStr (j+1)
			    in
			      getStr i
			    end
			| (RealField _) => let val (r, indx) = SC.strToReal(s, i)
			    in
			      (REAL r, indx)
			    end
		      (* end case *))
		in
		  scan (s, i, rf, item::items)
		end
		  handle _ => raise BadInput(rev items))
	  in
	    fn (s, i) => scan(s, i, fmts, [])
	  end (* scani *)

    fun scan fmt = let
	  val scani = scani fmt
	  in
	    fn s => #1(scani (s, 0))
	  end

  end (* Format *)
