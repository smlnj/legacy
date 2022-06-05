(* makestring.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * Basic value to string conversions.
 *
 * AUTHOR:  Emden Gansner & John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    erg@ulysses.att.com & jhr@research.att.com
 *)

structure Makestring : MAKESTRING =
  struct

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

    fun boolToStr true = "true"
      | boolToStr false = "false"

  (* convert an integer between 0..35 to a single digit *)
    fun mkDigit (i : int) : string =
	  System.Unsafe.cast(System.Unsafe.ordof("0123456789abcdef", i))

    fun intToBin i = (let
	  fun mkBit i = if (Bits.andb(i, 0x1) = 0) then "0" else "1"
	  fun f (0, l) = ("0" :: l)
	    | f (i, l) = if (i = 1)
		then ("1" :: l)
		else f(Bits.rshift(i, 1), (mkBit i) :: l)
	  in
	    if (i < 0)
	      then implode("~" :: f (~i, []))
	      else implode(f (i, []))
	  end
	    handle _ => "~10000000000000000000000000000000" (* MaxNegInt *))
    fun intToOct i = (let
	  fun f (i, l) = if (i < 8)
		then ((mkDigit i) :: l)
		else f(Bits.rshift(i, 3), mkDigit(Bits.andb(i, 0x7)) :: l)
	  in
	    if (i < 0)
	      then implode("~" :: f (~i, []))
	      else implode(f (i, []))
	  end
	    handle _ => "~10000000000" (* MaxNegInt *))
    fun intToStr i = (let
	  fun f (i, l) = if (i < 10)
		then ((mkDigit i) :: l)
		else let val j = i quot 10 in f (j,  mkDigit(i - 10*j) :: l) end
	  in
	    if (i < 0) then implode( "~" :: f(~i, [])) else implode(f(i, []))
	  end
	    handle _ => "~1073741824")
    fun intToHex i = (let
	  fun f (i, l) = if (i < 16)
		then ((mkDigit i) :: l)
		else f(Bits.rshift(i, 4), mkDigit(Bits.andb(i, 0xf)) :: l)
	  in
	    if (i < 0)
	      then implode("~" :: f(~i, []))
	      else implode(f(i, []))
	  end
	    handle _ => "~40000000" (* MaxNegInt *))


    exception BadPrecision
	(* raised by real to string conversions, if the precision is < 0. *)

  (* decompose a non-zero real into a list of at most maxPrec significant digits
   * (the first digit non-zero), and integer exponent. The return value
   *   (a::b::c..., exp)
   * is produced from real argument
   *   a.bc... * (10 ^^ exp)
   * If the list would consist of all 9's, the list consisting of 1 followed by
   * all 0's is returned instead.
   *)
    val maxPrec = 15
    fun decompose (f, e, precisionFn) = let
	  fun scaleUp (x, e) = if (x < 1.0) then scaleUp(10.0*x, e-1) else (x, e)
	  fun scaleDn (x, e) = if (x >= 10.0) then scaleDn(0.1*x, e+1) else (x, e)
	  fun mkdigits (f, 0) = ([], if f < 5.0 then 0 else 1)
	    | mkdigits (f, i) = let 
		val d = floor f
		val (digits, carry) = mkdigits (10.0 * (f - real d), i - 1)
		val (digit, c) = (case (d, carry)
		       of (9, 1) => (0, 1)
			| _ => (d + carry, 0)
		      (* end case *))
		in
		  (digit::digits, c)
		end
	  val (f, e) = if (f < 1.0)
		  then scaleUp (f, e)
		else if (f >= 10.0)
		  then scaleDn (f, e)
		  else (f, e)
	  val (digits, carry) = mkdigits(f, max(0, min(precisionFn e, maxPrec)))
	  in
	    case carry
	     of 0 => (digits, e)
	      | _ => (1::digits, e+1)
          end

    fun realFFormat (r, prec) = let
	  fun pf e = e + prec + 1
	  fun rtoa (digits, e) = let
		fun doFrac (_, 0, l) = implode(rev l)
		  | doFrac ([], p, l) = doFrac([], p-1, "0"::l)
		  | doFrac (hd::tl, p, l) = doFrac(tl, p-1, (mkDigit hd) :: l)
		fun doWhole ([], e, l) = if e >= 0
			then doWhole ([], e-1, "0" :: l)
		      else if prec = 0
			then implode(rev l)
			else doFrac ([], prec, "." :: l)
		  | doWhole (arg as (hd::tl), e, l) = if e >= 0
			then doWhole(tl, e-1, (mkDigit hd) :: l)
		      else if prec = 0
			then implode(rev l)
			else doFrac(arg, prec, "." :: l)
		fun doZeros (n, 0, l) = implode(rev l)
		  | doZeros (1, p, l) = doFrac(digits, p, l)
		  | doZeros (n, p, l) = doZeros(n-1, p-1, "0" :: l)
		in
		  if (e >= 0)
		    then doWhole(digits, e, [])
		  else if (prec = 0)
		    then "0"
		    else doZeros (~e, prec, ["0."])
		end
	  in
	    if (prec < 0) then raise BadPrecision else ();
	    if (r < 0.0)
	      then {sign = true, mantissa = rtoa(decompose(~r, 0, pf))}
	    else if (r > 0.0)
	      then {sign=false, mantissa = rtoa(decompose(r, 0, pf))}
	    else if (prec = 0)
	      then {sign=false, mantissa = "0"}
	      else {sign=false, mantissa = zeroRPad("0.", prec+2)}
	  end (* realFFormat *)

    fun realEFormat (r, prec) = let
	  fun pf _ = prec + 1
	  fun rtoa (sign, (digits, e)) = let
		fun mkRes (m, e) = {sign = sign, mantissa = m, exp = e}
		fun doFrac (_, 0, l)  = implode(rev l)
		  | doFrac ([], n, l) = zeroRPad(implode(rev l), n)
		  | doFrac (hd::tl, n, l) = doFrac (tl, n-1, (mkDigit hd) :: l)
		in
		  if (prec = 0)
		    then mkRes(mkDigit(hd digits), e)
		    else mkRes(doFrac(tl digits, prec, [".", mkDigit(hd digits)]), e)
		end
	  in
	    if (prec < 0) then raise BadPrecision else ();
	    if (r < 0.0)
	      then rtoa (true, decompose(~r, 0, pf))
	    else if (r > 0.0)
	      then rtoa (false, decompose(r, 0, pf))
	    else if (prec = 0)
	      then {sign = false, mantissa = "0", exp = 0}
	      else {sign = false, mantissa = zeroRPad("0.", prec+2), exp=0}
	  end (* realEFormat *)

    fun realGFormat (r, prec) = let
	  fun pf _ = prec
	  fun rtoa (sign, (digits, e)) = let
		fun mkRes (w, f, e) = {sign = sign, whole = w, frac = f, exp = e}
		fun doFrac [] = []
		  | doFrac (0::tl) = (case doFrac tl
		       of [] => []
			| rest => "0" :: rest
		      (* end case *))
		  | doFrac (hd::tl) = (mkDigit hd) :: (doFrac tl)
		fun doWhole ([], e, wh) =
		      if e >= 0
			then doWhole([], e-1, "0"::wh)
			else mkRes(implode(rev wh), "", NONE)
		  | doWhole (arg as (hd::tl), e, wh) =
		      if e >= 0
			then doWhole(tl, e-1, (mkDigit hd)::wh)
			else mkRes(implode(rev wh), implode(doFrac arg), NONE)
		in
		  if (e < ~4) orelse (e >= prec)
		    then mkRes(mkDigit(hd digits), implode(doFrac(tl digits)), SOME e)
		  else if e >= 0
		    then doWhole(digits, e, [])
		    else mkRes("0", zeroLPad(implode(doFrac digits), ~1 - e), NONE)
		end
	  in
	    if (prec < 1) then raise BadPrecision else ();
	    if (r < 0.0)
	      then rtoa(true, decompose(~r, 0, pf))
	    else if (r > 0.0)
	      then rtoa(false, decompose(r, 0, pf))
	      else {sign=false, whole="0", frac="", exp=NONE}
	  end (* realGFormat *)

  (* convert a real number to a string of the form [~]ddd.ddd, where
   * the precision (number of fractional digits) is specified by the
   * second argument.
   *)
    fun realToFloStr arg = let
	  val {sign, mantissa} = realFFormat arg
	  in
	    if sign then "~"^mantissa else mantissa
	  end

  (* convert a real number to a string of the form [~]d.dddE[~]dd, where
   * the precision (number of fractional digits) is specified by the
   * second argument.
   *)
    fun realToSciStr arg = let
	  val {sign, mantissa, exp} = realEFormat arg
	  in
	    if sign
	      then implode["~", mantissa, "E", intToStr exp]
	      else implode[mantissa, "E", intToStr exp]
	  end

  end (* Makestring *)
