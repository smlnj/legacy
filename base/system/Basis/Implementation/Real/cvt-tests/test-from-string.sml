structure ScanUtil : sig

    type prefix_pat = {
	wOkay : bool,           (* true if 0[wW] prefix is okay; if this is
                                 * true, then signs (+, -, ~) are not okay.
                                 *)
	xOkay : bool,           (* true if 0[xX] prefix is okay *)
	maxDigit : word         (* maximum digit (i.e., 7, 9, or 15) *)
      }

  (* scans prefix for a number:
   *
   *   binPat(true)  {wOkay=true, xOkay=false, ptOkay=false, isBinDigit} =>
   *       (0[wW])?b (b binary digit)
   *   binPat(false) {wOkay=true, xOkay=false, ptOkay=false, isBinDigit} =>
   *       [-~+]?b
   *   octPat(true)  {wOkay=true, xOkay=false, ptOkay=false, isOctDigit} =>
   *       (0[wW])?o (o octal digit)
   *   octPat(false) {wOkay=false, xOkay=false, ptOkay=false, isOctDigit} =>
   *       [-~+]?o
   *   hexPat(true)  {wOkay=true, xOkay=true, ptOkay=false, isHexDigit} =>
   *       (0[wW][xX])?h (h hex digit)
   *   hexPat(false) {wOkay=false, xOkay=true, ptOkay=false, isHexDigit} =>
   *       [-~+]?(0[xX])?h
   *   decPat(true)  {wOkay=true, xOkay=false, ptOkay=false, isDecDigit} =>
   *       (0[wW][xX])?d (d decimal digit)
   *   decPat(false) {wOkay=false, xOkay=false, ptOkay=false, isDecDigit} =>
   *       [-~+]?d
   *
   * Sign characters, initial 0x, 0w, etc are consumed.  The initial
   * digit is returned as the value of next.
   *)
    val scanPrefix : prefix_pat
          -> ('a -> (char * 'a) option)
            -> 'a
              -> {neg: bool, next: word, rest: 'a} option

  (* standard prefix patterns *)
    val binPat : bool -> prefix_pat
    val octPat : bool -> prefix_pat
    val decPat : bool -> prefix_pat
    val hexPat : bool -> prefix_pat

  (* map character to its hex value (e.g., #"3" ==> 0w3, #"e" ==> 0w14, etc). *)
    val code : char -> word

  end = struct

  (* A table for mapping digits to values.  Whitespace characters map to
   * 128, "+" maps to 129, "-","~" map to 130, "." maps to 131, and the
   * characters 0-9,A-Z,a-z map to their base-36 value.  All other
   * characters map to 255.
   *)
    val cvtTable = "\
	  \\255\255\255\255\255\255\255\255\255\128\128\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\128\255\255\255\255\255\255\255\255\255\255\129\255\130\131\255\
	  \\000\001\002\003\004\005\006\007\008\009\255\255\255\255\255\255\
	  \\255\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\
	  \\025\026\027\028\029\030\031\255\033\034\035\255\255\255\255\255\
	  \\255\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\
	  \\025\026\027\028\029\030\031\032\033\034\035\255\255\255\130\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	\"

    fun code (c : char) =
	  Word.fromInt(ord(CharVector.sub(cvtTable, ord c)))
    val wsCode : word = 0w128		(* code for whitespace *)
    val plusCode : word = 0w129		(* code for #"+" *)
    val minusCode : word = 0w130	(* code for #"-" and #"~" *)
    val eCode : word = 0w14		(* code for #"e" and #"E" *)
    val wCode : word = 0w32		(* code for #"w" *)
    val xCode : word = 0w33		(* code for #"X" and #"X" *)

    type prefix_pat = {
	wOkay : bool,		(* true if 0[wW] prefix is okay; if this is
				 * true, then signs (+, -, ~) are not okay.
				 *)
	xOkay : bool,		(* true if 0[xX] prefix is okay *)
	maxDigit : word         (* maximum digit (i.e., 7, 9, or 15) *)
      }

    fun scanPrefix (p : prefix_pat) getc cs = let
	  fun getNext cs = (case (getc cs)
		 of NONE => NONE
		  | (SOME(c, cs)) => SOME(code c, cs)
		(* end case *))
	  fun skipWS cs = (case (getNext cs)
		 of NONE => NONE
		  | (SOME(c, cs')) =>
		      if (c = wsCode) then skipWS cs' else SOME(c, cs')
		(* end case *))
	  fun getOptSign NONE = NONE
	    | getOptSign (next as SOME(c, cs)) =
		if (#wOkay p)
		  then getOpt0 (false, SOME(c, cs)) (* no sign for words *)
		else if (c = minusCode)
		  then getOpt0 (true, getNext cs)
		else if (c = plusCode)
		  then getOpt0 (false, getNext cs)
		  else getOpt0 (false, next)
	  and getOpt0 (neg, NONE) = NONE
	    | getOpt0 (neg, SOME(c, cs)) = (case (c, #wOkay p, #xOkay p)
		 of (0w0, true, true) => getOptWX (neg, (c, cs), getNext cs)
		  | (0w0, true, false) => getOptW (neg, (c, cs), getNext cs)
		  | (0w0, false, true) => getOptX (neg, (c, cs), getNext cs)
		  | _ => finish (neg, (c, cs))
		(* end case *))
	(* consume an optional "0[wW]?[xX]" prefix having seen "0" *)
	  and getOptWX (neg, savedCS, NONE) = finish (neg, savedCS)
	    | getOptWX (neg, savedCS, arg as SOME(c, cs)) =
		if (c = wCode)
		  then (case getNext cs
		     of SOME(c', cs') => if (c' = xCode)
			  then chkDigit (neg, savedCS, getNext cs')
			  else finish (neg, savedCS) (* saw "0[wW]" but no "[xX]" *)
		      | NONE => finish (neg, savedCS)
		    (* end case *))
		  else getOptX (neg, savedCS, arg)
	(* consume an optional "0[wW]" prefix having seen "0" *)
	  and getOptW (neg, savedCS, NONE) = finish (neg, savedCS)
	    | getOptW (neg, savedCS, arg as SOME(c, cs)) =
		if (c = wCode)
		  then chkDigit (neg, savedCS, getNext cs)
		  else finish (neg, savedCS)
	(* consume an optional "0[xX]" prefix having seen "0" *)
	  and getOptX (neg, savedCS, NONE) = finish (neg, savedCS)
	    | getOptX (neg, savedCS, arg as SOME(c, cs)) =
		if (c = xCode)
		  then chkDigit (neg, savedCS, getNext cs)
		  else chkDigit (neg, savedCS, arg)
	(* check if the character following the prefix is a valid digit; if it
	 * is, then we consume the prefix, otherwise we reset to savedCS.
	 *)
	  and chkDigit (neg, savedCS, NONE) = finish (neg, savedCS)
	    | chkDigit (neg, savedCS, SOME(c, cs)) =
		if (#maxDigit p >= c)
		  then SOME{neg=neg, next = c, rest = cs}
		  else finish (neg, savedCS)
	  and finish (neg, (c, cs)) =
		if (#maxDigit p >= c)
		  then SOME{neg=neg, next = c, rest = cs}
		  else NONE
	  in
	    getOptSign (skipWS cs)
	  end


    fun binPat wOkay = {wOkay=wOkay, xOkay=false, maxDigit=0w1} : prefix_pat
    fun octPat wOkay = {wOkay=wOkay, xOkay=false, maxDigit=0w7} : prefix_pat
    fun decPat wOkay = {wOkay=wOkay, xOkay=false, maxDigit=0w9} : prefix_pat
    fun hexPat wOkay = {wOkay=wOkay, xOkay=true,  maxDigit=0w15} : prefix_pat

  end

(* float-rep.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure FloatRep : sig

    (* deconstrcted representation of real numbers (precision independent)
     * We interpret the number as
     *
     *    (-1)^sign * d_1 ... d_n * 10^exp
     *)
    type decimal_rep = {
        sign : bool,        (* sign bit; true for negative numbers *)
        nDigits : int,      (* the length of the digit list (> 0) *)
        digits : int list,  (* the non-empty list of decimal digits; these are to
                             * the left of the decimal point (unlike for the
                             * IEEEReal.decimal_approx type, where they are to
                             * the right).
                             *)
        exp : int           (* the signed exponent *)
      }

    datatype float_rep
      = Inf of bool             (* +/- infinity *)
      | NaN of bool             (* +/- NaN *)
      | Zero of bool            (* +/- 0 *)
      | Normal of decimal_rep
      | Subnormal of decimal_rep

  end = struct

    type decimal_rep = {
        sign : bool,        (* sign bit; true for negative numbers *)
        nDigits : int,      (* the length of the digit list *)
        digits : int list,  (* the list of decimal digits; these are to the left
                             * of the decimal point (unlike IEEEReal.decimal_approx,
                             * where they are to the right).
                             *)
        exp : int           (* the signed exponent *)
      }

    datatype float_rep
      = Inf of bool             (* +/- infinity *)
      | NaN of bool             (* +/- NaN *)
      | Zero of bool            (* +/- 0 *)
      | Normal of decimal_rep
      | Subnormal of decimal_rep

  end

(* string-to-frep.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure StringToFRep : sig

    (* scan a real number from a character stream.  Leading and trailing zeros
     * are removed from the representation, but identification of sub-normals
     * and infinities is left for later, since that depends on the target
     * precision.
     *)
    val scan : (char, 'strm) StringCvt.reader
          -> (FloatRep.float_rep, 'strm) StringCvt.reader

  end = struct

    structure W = Word
    structure U = ScanUtil

    datatype float_rep = datatype FloatRep.float_rep

    (* the largest IEEE floating-point format is 256 bits (octuple-precision),
     * so we limit exponents to valid values for that precision.  Doing so means
     * that we do not have to worry about overflow in down-stream exponent adjustments.
     *)
    val minExp = ~262142
    val maxExp = 262143
    val infExp = maxExp+1       (* bigger than the largest 256-bit exponent *)

    (* codes copied from ScanUtil *)
    val plusCode : word = 0w129		(* code for #"+" *)
    val minusCode : word = 0w130	(* code for #"-" and #"~" *)
    val ptCode : word = 0w131		(* code for #"." *)
    val eCode : word = 0w14		(* code for #"e" and #"E" *)

    (* suffixes for special reals *)
    val nfChrs = [#"n",#"f"]                    (* [i]nf *)
    val inityChrs = [#"i",#"n",#"i",#"t",#"y"]  (* [inf]inity *)
    val anChrs = [#"a", #"n"]                   (* [n]an *)

    fun scan getc = let
          (* trim leading zeros from a list of digits *)
          fun trimLeadingZeros (n, 0::ds) = trimLeadingZeros (n+1, ds)
            | trimLeadingZeros arg = arg
          (* scan the optional sign *)
          fun scanSign cs = (case getc cs
                 of SOME(#"~", cs) => (true, cs)
                  | SOME(#"-", cs) => (true, cs)
                  | SOME(#"+", cs) => (false, cs)
                  | _ => (false, cs)
                (* end case *))
          fun scanWhole (cs, sign, n, digits) = (case getc cs
                 of SOME(c, cs') => let
                      val d = U.code c
                      in
                        if (d <= 0w9)
                          then scanWhole (cs', sign, n+1, W.toIntX d :: digits)
                        else if (d = ptCode)
                          then scanFrac (cs', sign, n, 0, digits)
                        else if (d = eCode)
                          then scanExp (cs, cs', sign, n, 0, digits)
                          else mkNoExp (cs, sign, n, 0, digits)
                      end
                  | NONE => mkNoExp (cs, sign, n, 0, digits)
                (* end case *))
          and scanFrac (cs, mSign, nWhole, nFrac, digits) = (case getc cs
                 of SOME(c, cs') => let
                      val d = U.code c
                      in
                        if (d <= 0w9)
                          then scanFrac (cs', mSign, nWhole, nFrac+1, W.toIntX d :: digits)
                        else if (d = eCode)
                          then scanExp (cs, cs', mSign, nWhole, nFrac, digits)
                          else mkNoExp (cs, mSign, nWhole, nFrac, digits)
                      end
                  | NONE => mkNoExp (cs, mSign, nWhole, nFrac, digits)
                (* end case *))
          and scanExp (eIx, cs, mSign, nWhole, nFrac, digits) = let
                val (eSign, cs) = scanSign cs
                fun scan (cs, eDigits) = (case getc cs
                       of SOME(c, cs') => let
                            val d = U.code c
                            in
                              if (d <= 0w9)
                                then scan (cs', W.toIntX d :: eDigits)
                                else withExponent (cs, eDigits)
                            end
                        | NONE => withExponent (cs, eDigits)
                      (* end case *))
                and withExponent (cs, eDigits) =
                      mkWithExp (cs, mSign, nWhole, nFrac, digits, eSign, eDigits)
                fun noExponent () = (* invalid exponent, so backtrack the stream *)
                      mkNoExp (eIx, mSign, nWhole, nFrac, digits)
                in
                  case getc cs
                   of SOME(c, cs') => let
                        val d = U.code c
                        in
                          if (d <= 0w9)
                            then scan (cs', [W.toIntX d])
                            else noExponent ()
                        end
                    | NONE => noExponent ()
                  (* end case *)
                end
          (* make a number w/o an exponent.  The arguments are:
           *   cs       -- the remaing stream
           *   mSign    -- sign of the mantissa
           *   nWhole   -- number of whole digits (i.e., left of decimal)
           *   nFrac    -- number of fractional digits (i.e., right of decimal)
           *   rDigits   -- the digits in reverse order
           *)
          and mkNoExp (cs, mSign, nWhole, nFrac, rDigits) =
(print(concat["# mkNoExp: nWhole = ", Int.toString nWhole, ", nFrac = ",
Int.toString nFrac, ", rDigits = [",
String.concatWithMap "," Int.toString rDigits, "]\n"]);
                mk (cs, mSign, nWhole + nFrac, rDigits, ~nFrac)
)
          (* make a number with an exponent.  The first five arguments are the same
           * as `mkNoExp`; the additional arguments are:
           *   eSign    -- the sign of the exponent
           *   eDigits  -- the exponent digits in reverse order
           *)
          and mkWithExp (cs, mSign, nWhole, nFrac, rDigits, eSign, eDigits) = let
                val exp = (List.foldr (fn (d, e) => 10*e + d) 0 eDigits
                            handle _ => infExp)
                val exp = if eSign then ~exp else exp
                in
                  mk (cs, mSign, nWhole + nFrac, rDigits, exp-nFrac)
                end
          (* make the result.  The arguments are:
           *   cs       -- the remaing stream
           *   mSign    -- sign of the mantissa
           *   nDigits  -- the total number of digits (including excess zeros)
           *   rDigits  -- the digits in reverse order
           *   exp      -- the adjusted exponent
           *)
          and mk (cs, sign, nDigits, rDigits, exp) = let
                val (nz1, rDigits) = trimLeadingZeros (0, rDigits)
                val (nz2, digits) = trimLeadingZeros (0, List.rev rDigits)
val _ = print(concat[
"## mk: nz1 = ", Int.toString nz1, ", nz2 = ", Int.toString nz2,
", exp = ", Int.toString exp, ", nDigits = ", Int.toString nDigits,
", digits = [", String.concatWithMap "," Int.toString digits, "]\n"])
                val nDigits = nDigits - nz1 - nz2
                val exp = exp + nz1
                val frep = if (nDigits = 0) orelse (exp < minExp)
                        then Zero sign
                      else if (exp > maxExp)
                        then Inf sign
                        else Normal{
                            sign = sign,
                            nDigits = nDigits,
                            digits = digits,
                            exp = exp
                          }
                in
                  SOME(frep, cs)
                end
          (* scan special reals: "inf"|"infinity"|"nan".
           * Note that the names are case insesitive.
           *)
          fun scanSpecial (sign, c, cs) = let
                fun match (cs, []) = SOME cs
                  | match (cs, c::cr) = (case getc cs
                       of SOME(c', cs') => if (c = Char.toLower c')
                            then match (cs', cr)
                            else NONE
                        | NONE => NONE
                      (* end case *))
                fun infinity cs = SOME(Inf sign, cs)
                fun nan cs = SOME(NaN sign, cs)
                in
                  case c
                   of (#"I" | #"i") => (case match (cs, nfChrs)
                         of SOME cs' => (case match (cs', inityChrs)
                               of SOME cs'' => infinity cs''
                                | NONE => infinity cs'
                              (* end case *))
                          | NONE => NONE
                        (* end case *))
                    | (#"N" | #"n") => (case match (cs, anChrs)
                         of SOME cs => nan cs
                          | NONE => NONE
                        (* end case *))
                    | _ => NONE
                  (* end case *)
                end
          in
            fn cs => let
                val cs = StringCvt.skipWS getc cs
                val (sign, cs) = scanSign cs
                in
                  case getc cs
                   of SOME(#".", cs') => (case getc cs'
                         of SOME(c, cs'') => let
                              val d = U.code c
                              in
                                if (d <= 0w9)
                                  then scanFrac (cs'', sign, 0, 1, [W.toIntX d])
                                  else NONE
                              end
                          | NONE => NONE
                        (* end case *))
                    | SOME(c, cs') => let
                        val d = U.code c
                        in
                          if (d <= 0w9)
                            then scanWhole (cs', sign, 1, [W.toIntX d])
                            else scanSpecial (sign, c, cs')
                        end
                    | NONE => NONE
                  (* end case *)
                end
          end

  end;

val fromS = StringCvt.scanString StringToFRep.scan;

