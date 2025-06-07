(* test-real64-to-frep.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
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

    (* Normalize a `float_rep` value for 32-bit IEEE floating-point. *)
    val normalize32 : float_rep -> float_rep

    (* Normalize a `float_rep` value for 64-bit IEEE floating-point. *)
    val normalize64 : float_rep -> float_rep

    (* `toDecimalApprox f` converts the normalized float `f` to its equivalent
     * `IEEEReal.decimal_approx` representation.  Note that we assume that
     * the argument is well formed, since it will be for all use cases in the
     * Basis implementation.
     *)
    val toDecimalApprox : float_rep -> IEEEReal.decimal_approx

    (* `fromDecimalApprox approx` converts `approx` to its equivalent
     * `float_rep` representation.  If the input is invalid (i.e., digits
     * that are out of range) then NONE is returned.
     *)
    val fromDecimalApprox : IEEEReal.decimal_approx -> float_rep option

  end = struct

    type decimal_rep = {
        sign : bool,
        nDigits : int,
        digits : int list,
        exp : int
      }

    datatype float_rep
      = Inf of bool             (* +/- infinity *)
      | NaN of bool             (* +/- NaN *)
      | Zero of bool            (* +/- 0 *)
      | Normal of decimal_rep
      | Subnormal of decimal_rep

    fun mkDecimalRep (digits, exp) : decimal_rep =  {
            sign = false,
            nDigits = List.length digits,
            digits = digits,
            exp = exp
          }

    (* the decimal representation of the minimum 32-bit subnormal number *)
    val minSubnormal32 = (* 0000 0001 *)
          mkDecimalRep ([1,4,0,1,2,9,8,4,6,4,3], ~45)

    (* the decimal representation of the minimum 32-bit normal number *)
    val minNormal32 = (* 0080 0000 *)
          mkDecimalRep ([1,1,7,5,4,9,4,3,5,0,8], ~38)

    (* the decimal representation of the maximum 32-bit normal number *)
    val maxNormal32 = (* 7f7f ffff *)
          mkDecimalRep ([3,4,0,2,8,2,3,4,6,6,4], 38)

    (* the decimal representation of the minimum 64-bit subnormal number *)
    val minSubnormal64 = (* 0000 0000 0000 0001 *)
          mkDecimalRep ([4,9,4,0,6,5,6,4,5,8,4,1,2,4,6,5,4], ~324)

    (* the decimal representation of the minimum 64-bit normal number *)
    val minNormal64 = (* 0010 0000 0000 0000 *)
          mkDecimalRep ([2,2,2,5,0,7,3,8,5,8,5,0,7,2,0,1,4], ~324)

    (* the decimal representation of the maximum 64-bit normal number *)
    val maxNormal64 = (* 7fef ffff ffff ffff *)
          mkDecimalRep ([1,7,9,7,6,9,3,1,3,4,8,6,2,3,1,5,7], 292)

    (* unsigned "<" comparison *)
    fun uLT (dr1 : decimal_rep, dr2 : decimal_rep) = let
          (* compare digits for "<" order *)
          fun ltDigits (d1::dr1, d2::dr2) =
                (d1 < d2) orelse ((d1 = d2) andalso ltDigits(dr1, dr2))
            | ltDigits ([], _::_) = true
            | ltDigits (_, []) = false
          (* normalize exponents *)
          val e1 = #exp dr1 + #nDigits dr1
          val e2 = #exp dr2 + #nDigits dr2
          in
            (e1 < e2) orelse (
              (e1 = e2) andalso ltDigits (#digits dr1, #digits dr2))
          end

    fun lessThan (dr1 : decimal_rep, dr2 : decimal_rep) = (
          case (#sign dr1, #sign dr2)
           of (true, false) => true (* negative < positive *)
            | (false, true) => false (* not(positive < negative) *)
            | (true, true) => uLT (dr2, dr1) (* both negative, so swap order *)
            | (false, false) => uLT (dr1, dr2)
          (* end case *))

    (* normalize a `decimal_rep` value for a precision that is specified by
     * the minimum sub-normal, minimum normal, and maximum normal values.
     *)
    fun normalizeDecimalRep (minSubNormal, minNormal, maxNormal) (f : decimal_rep) = let
          fun trimLeadingZeros (n, 0::ds) = trimLeadingZeros (n+1, ds)
            | trimLeadingZeros arg = arg
          (* remove leading zeros from digits *)
          val (nz1, digits) = trimLeadingZeros (0, #digits f)
          (* remove trailing zeros from digits *)
          val (nz2, rDigits) = trimLeadingZeros (0, List.rev digits)
          val f' = {
                  sign = #sign f,
                  nDigits = #nDigits f - nz1 - nz2,
                  digits = List.rev rDigits,
                  exp = #exp f + nz2
                }
          in
            if (#nDigits f' = 0) then Zero(#sign f')
            else if uLT(f', minSubNormal) then Zero(#sign f')
            else if uLT(f', minNormal) then Subnormal f'
            else if uLT(maxNormal, f') then Inf(#sign f')
            else Normal f'
          end

    fun normalize (minSubNormal, minNormal, maxNormal) = let
          val normalizeDRep = normalizeDecimalRep (minSubNormal, minNormal, maxNormal)
          in
            fn (Normal f) => normalizeDRep f
             | (Subnormal f) => normalizeDRep f
             | f => f
          end

    (* Normalize a `float_rep` value for 32-bit IEEE floating-point. *)
    val normalize32 = normalize (minSubnormal32, minNormal32, maxNormal32)

    (* Normalize a `float_rep` value for 64-bit IEEE floating-point. *)
    val normalize64 = normalize (minSubnormal64, minNormal64, maxNormal64)

    fun toDecimalApprox arg = let
          fun toDA (cls, {sign, nDigits, digits, exp}) =
                { class = cls, sign = sign, digits = digits, exp = exp + nDigits }
          in
            case arg
             of Inf sgn => { class = IEEEReal.INF, sign = sgn, digits = [], exp = 0 }
              | NaN sgn => { class = IEEEReal.NAN, sign = sgn, digits = [], exp = 0 }
              | Zero sgn => { class = IEEEReal.ZERO, sign = sgn, digits = [], exp = 0 }
              | Normal dr => toDA (IEEEReal.NORMAL, dr)
              | Subnormal dr => toDA (IEEEReal.SUBNORMAL, dr)
            (* end case *)
          end

    fun fromDecimalApprox {class, sign, digits, exp} = let
          fun fromDA cons = let
                (* remove leading zeros from a list *)
                fun rmZ (0::ds, nlz) = (ds, nlz+1)
                  | rmZ (ds, nlz) = (ds, nlz)
                (* first we remove leading zeros *)
                val (digits, nlz) = rmZ (digits, 0)
                (* adjust exponent for removed leading zeros *)
                val exp = exp - nlz
                (* check for invalid digits while counting the digits and removing
                 * trailing zeros.
                 *)
                fun chkDigits ([], n, ds) = let
                      val (ds', nz) = rmZ (ds, 0)
                      in
                        SOME(n - nz, List.rev ds')
                      end
                  | chkDigits (d::dr, n, ds) = if (0 <= d) andalso (d <= 9)
                      then chkDigits (dr, n+1, d::ds)
                      else NONE
                in
                  case chkDigits (digits, 0, [])
                   of NONE => NONE
                    | SOME(0, _) => SOME(Zero sign)
                    | SOME(nDigits, digits) => SOME(cons{
                          sign = sign,
                          nDigits = nDigits,
                          digits = digits,
                          exp = exp - nDigits
                        })
                  (* end case *)
                end
          in
            case class
             of IEEEReal.INF => SOME(Inf sign)
              | IEEEReal.NAN => SOME(NaN sign)
              | IEEEReal.ZERO => SOME(Zero sign)
              | IEEEReal.NORMAL => fromDA Normal
              | IEEEReal.SUBNORMAL => fromDA Subnormal
            (* end case *)
          end

  end
(* real64-to-frep.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Conversion of 64-bit IEEE floating-point numbers to the `FloatRep.float_rep`
 * representation.  This code is an implementation of the algorithm from Section 2
 * of the paper:
 *
 *      Ryũ: Fast Float-to-String Conversion
 *      by Ulf Adams
 *      PLDI 2018
 *
 * This algorithm uses arbitrary precision arithmetic; the paper also describes the
 * faster "ryũ" algorithm that uses 64-bit fixed-point arithmetic, which we might
 * want to implement at some time in the future.
 *
 * The original port of the C code was done by Skye Soss.
 *)

structure Real64ToFRep : sig

    val cvt : real -> FloatRep.float_rep

  end = struct

    structure W = Word
    structure W64 = Word64

    datatype float_class = datatype IEEEReal.float_class

    val toBits = Unsafe.Real64.castToWord

    (* some useful constants *)
    val nExpBits = 0w11                 (* # of exponent bits in double-precision real *)
    val nFracBits = 0w52                (* # of stored fractional (aka mantissa) bits *)
    val bias = 1023                     (* exponent bias for normalized numbers *)

    (* to unify normal and subnormal numbers, we change the representation to
     *
     *    (-1)^s * frac * 2^exp
     *
     * where "frac" is an integer and "exp" is the adjusted exponent.  The following
     * constants are used to compute the adjusted exponent:
     *)
    val normalExpAdjust = bias + W.toInt nFracBits
    val subnormalExp = 1 - bias - W.toInt nFracBits
    (* value of implicit "1" bit for normalized numbers *)
    val implicitBit = W64.<<(0w1, 0w52)

    (* Given 0≤a≤b≤c, returns (d, e) such that:
     * - a ≤ d ≤ c
     * - a < d*10^e < c
     * - e is maximal
     * - The resulting value for d is closest to b as possible
     * - The result respects the acceptBounds flag
     *)
    fun computeShortest (a, b, c, acceptBounds) = let
          fun iter1 (i, a, b, c, digit, allAZero, allBZero) = let
                val (aq10, ar10) = IntInf.quotRem (a, 10)
                val cq10 = IntInf.quot (c, 10)
                in
                  if (aq10 < cq10)
                    then let
                      val allAZero = allAZero andalso (ar10 = 0)
                      val allBZero = allBZero andalso (digit = 0)
                      val (bq10, br10) = IntInf.quotRem (b, 10)
                      in
                        iter1 (i+1, aq10, bq10, cq10, br10, allAZero, allBZero)
                      end
                    else (i, a, b, c, digit, allAZero, allBZero)
                end
          val (i, a, b, c, digit, allAZero, allBZero) =
                iter1 (0, a, b, if acceptBounds then c - 1 else c, 0, true, true)
          fun iter2 (i, a, b, c, digit, allBZero) = let
                val (aq10, ar10) = IntInf.quotRem (a, 10)
                in
                  if (ar10 = 0)
                    then let
                      val cq10 = IntInf.quot(c, 10)
                      val allBZero = allBZero andalso (digit = 0)
                      val (bq10, br10) = IntInf.quotRem (b, 10)
                      in
                        iter2 (i+1, aq10, bq10, cq10, br10, allBZero)
                      end
                    else (i, a, b, c, digit, allBZero)
                end
          val (i, a, b, c, digit, allBZero) = if (acceptBounds andalso allAZero)
                then iter2 (i, a, b, c, digit, allBZero)
                else (i, a, b, c, digit, allBZero)
          val isTie = (digit = 5) andalso allBZero
          val breakTieDown = (IntInf.andb(b, 1) = 0)
          val wantRoundDown = (digit < 5) orelse (isTie andalso breakTieDown)
          val roundDown = (wantRoundDown andalso (allAZero orelse a <> b))
                orelse (b+1 > c)
          in
            if roundDown then (b, i) else (b+1, i)
          end

    (* convert an IntInf.int to a number and list of digits *)
    fun toDigits n = let
          fun lp (0, nd, ds) = (nd, ds)
            | lp (n, nd, ds) = let
                val (n, d) = IntInf.quotRem (n, 10)
                in
                  lp (n, nd+1, (Int.fromLarge d)::ds)
                end
          in
            lp (n, 0, [])
          end

    val signBitMask = W64.<<(0w1, 0w63)
    val fracMask = W64.<<(0w1, nFracBits) - 0w1
    val expMask = W64.<<(0w1, nExpBits) - 0w1

    fun cvt r = let
          (* decompose the 64-bit float into sign, exponent, and fractional parts *)
          val bits = toBits r
          val sign = W64.andb(bits, signBitMask) <> 0w0
          val expBits = W64.andb(W64.>>(bits, nFracBits), expMask)
          val fracBits = W64.andb(bits, fracMask)
          (* given the float f = (-1)^s * frac * 2^exp, convert to decimal
           * representation.
           *)
          fun toDecimal (sign, frac, exp) = let
                (* Determine the interval of information-preserving outputs *)
                val e2 = exp - 2
                val v = W64.<<(frac, 0w2)
                val u = if (fracBits = 0w0 andalso expBits > 0w1)
                      then v - 0w1
                      else v - 0w2
                val w = v + 0w2
                (* convert (u, v, w)×2^e₂ to a decimal power base:
                 * (a, b, c)×10^e₁₀ == (u, v, w)×2^e₂
                 *)
                val (e10, a, b, c) = if e2 >= 0
                      then let
                        val x = W.fromInt e2
                        fun shift y = IntInf.<<(W64.toLargeInt y, x)
                        in
                          (0, shift u, shift v, shift w)
                        end
                      else let
                        val x = IntInf.pow (5, ~e2)
                        fun scale y = W64.toLargeInt y * x
                        in
                          (e2, scale u, scale v, scale w)
                        end
                (* find the shortest, correctly rounded decimal representation *)
                val acceptBounds = (W64.andb (fracBits, 0w1) = 0w0)
                val (ds, e) = computeShortest (a, b, c, acceptBounds)
                val (nDigits, digits) = toDigits ds
                in {
                  sign = sign,
                  nDigits = nDigits,
                  digits = digits,
                  exp = e + e10
                } end (* toDecimal *)
          in
            case expBits
             of 0w0 => if (fracBits = 0w0)
                  then FloatRep.Zero sign
                  else (* subnormal *)
                    FloatRep.Subnormal(toDecimal (sign, fracBits, subnormalExp))
              | 0wx7ff => if (fracBits = 0w0)
                  then FloatRep.Inf sign
                  else FloatRep.NaN sign
              | _ => let (* normal *)
                  val exp = W.toInt(W.fromLarge expBits) - normalExpAdjust
                  in
                    FloatRep.Normal(toDecimal (sign, fracBits + implicitBit, exp))
                  end
            (* end case *)
          end

  end
(* frep-to-string.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Internal conversions for creating strings from IEEEReal.decimal_approx
 * values.  Note that we assume that the inputs are well formed; i.e.,
 *
 *      - the list of digits only contain values in the range [0..9]
 *
 *      - the list of digits is normalized; i.e., there are no trailing
 *        zeros.
 *
 *      - if the class is ZERO, then the list of digits is empty and the
 *        exponent is 0.  Likewise, if the list of digits is empty, then
 *        the class should be ZERO.
 *
 * The interpretation of a IEEEReal.decimal_approx normal or sub-normal
 * number with sign "s", digits "d_1", ..., "d_n", and exponent "e" is
 *
 *      (-1)^s * 0.d_1 ... d_n * 10^e
 *)

structure FRepToString : sig

    (* for implementing Real.fmt *)
    val fmt : StringCvt.realfmt -> FloatRep.float_rep -> string

    (* for implementing Real.toString *)
    val toString : FloatRep.float_rep -> string

    (* for implementing IEEEReal.toString *)
    val toExactString : FloatRep.float_rep -> string

  end = struct

    structure String = String

(* should be
    (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = InlineT.Int.fast_sub(x, y)
    fun x ++ y = InlineT.Int.fast_add(x, y)

    val upd = InlineT.CharVector.update
*)

    datatype realfmt = datatype StringCvt.realfmt
    datatype float_rep = datatype FloatRep.float_rep

    (* wrapper to handle infinities and NaNs *)
    fun wrapper _ (Inf false) = "inf"
      | wrapper _ (Inf true) = "~inf"
      | wrapper _ (NaN _) = "nan"
      | wrapper toS (Zero sgn) = toS (sgn, 0, [], 0)
      | wrapper toS (Normal{sign, nDigits, digits, exp}) =
          toS (sign, nDigits, digits, exp)
      | wrapper toS (Subnormal{sign, nDigits, digits, exp}) =
          toS (sign, nDigits, digits, exp)

    val digits = #["0","1","2","3","4","5","6","7","8","9"]
    fun getDigit d = Vector.sub(digits, d)

    fun prependDigits ([], suffix) = suffix
      | prependDigits (d::dr, suffix) = getDigit d :: prependDigits(dr, suffix)

    (* produce a string of `n` zeros *)
    fun zeros n = CharVector.tabulate(n, fn _ => #"0")
(* should be
    fun zeros n = let
          val s = Assembly.A.create_s n
          fun fill i =
              if i < n then (upd (s, i, #"0"); fill (i ++ 1))
              else ()
          in
            fill 0; s
          end
 *)

    fun prependZeros (0, frags) = frags
      | prependZeros (n, frags) = zeros n :: frags

    fun addSign (false, frags) = frags
      | addSign (true, frags) = "~" :: frags

    (* `roundAndNormalize (ds, m)` rounds the normalized sequence of digits `ds` to `m`
     * digits and returns the triple `(co, ds', nz)`, where
     *
     *    - `ds` is a normalized sequence of `n` digits
     *    - `m <= n` specifies the desired number of digits in the result
     *    - `co` is `true` if the result of rounding produces a carry out
     *    - `ds'` is the normalized result that has `m - nz` digits
     *    - `nz` is the number of trailing zeros removed from the result of
     *      rounding.
     *)
    fun roundAndNormalize (digits, m) = let
          (* given the list of digits in reverse order (lsd first), trim the zeros.
           * we return the remaining digits in msd first order with the number of
           * trimmed zeros.
           *)
          fun trimTrailingZeros ([], nz) = (false, [], nz)
            | trimTrailingZeros (0::ds, nz) = trimTrailingZeros (ds, nz+1)
            | trimTrailingZeros (ds, nz) = (false, List.rev ds, nz)
          fun trim (0, [], ds) = trimTrailingZeros (ds, 0)
            | trim (_, [], _) = raise Fail "impossible"
            | trim (0, d::dr, ds) = if (d < 5)
                  then trimTrailingZeros (ds, 0)
                else if (d > 5)
                  then roundWithCarry (true, ds, [], 0)
                else if null dr
                  then trimTrailingZeros (ds, 0)  (* round down *)
                  else roundWithCarry (true, ds, [], 0)
            | trim (m, d::dr, ds) = trim (m-1, dr, d::ds)
          and roundWithCarry (carry, [], ds', nz) = (carry, ds', nz)
            | roundWithCarry (true, d::ds, ds', nz) = let
                val d' = d+1
                in
                  if (d' <= 9)
                    then roundWithCarry (false, ds, d'::ds', nz)
                  else if null ds'
                    then roundWithCarry (true, ds, ds', nz+1) (* normalize *)
                    else roundWithCarry (true, ds, 0::ds', nz)
                end
            | roundWithCarry (false, d::ds, ds', nz) =
                (false, List.revAppend(ds, d::ds'), nz)
          in
            trim (m, digits, [])
          end

    (* `round (ds, m)` rounds the normalized sequence of digits `ds` to `m`
     * digits and returns the pair `(co, ds')`, where
     *
     *    - `ds` is a normalized sequence of `n` digits
     *    - `m <= n` specifies the desired number of digits in the result
     *    - `co` is `true` if the result of rounding produces a carry out
     *    - `ds'` is the normalized result that has `m` digits
     *
     * Note that unlike `roundAndNormalize`, the result sequence `ds'` can
     * have trailing zeros.
     *)
    fun round (digits, m) = let
          fun trim (0, [], ds) = (false, List.rev ds)
            | trim (_, [], _) = raise Fail "impossible"
            | trim (0, d::dr, ds) = if (d < 5)
                  then (false, List.rev ds)
                else if (d > 5)
                  then roundWithCarry (ds, [])
                else if null dr
                  then (false, List.rev ds) (* round down *)
                  else roundWithCarry (ds, [])
            | trim (m, d::dr, ds) = trim (m-1, dr, d::ds)
          and roundWithCarry ([], ds') = (true, ds')
            | roundWithCarry (d::ds, ds') = let
                val d' = d+1
                in
                  if (d' <= 9)
                    then (false, List.revAppend(ds, d'::ds'))
                    else roundWithCarry (ds, 0::ds')
                end
          in
            trim (m, digits, [])
          end

    (* convert to scientific notation: "[~]?[0-9].[0-9]+(E[~]?[0-9]+)?", where `prec`
     * specifies the digits to the right of the decimal point.
     *)
    fun toSci prec (sign, nDigits, digits, exp) = let
          (* adjust exponent for "dddd." => "d.ddd" conversion *)
          val nFracDigits = Int.max(0, nDigits - 1)
          val exp = exp + nFracDigits
          in
            case (prec, digits)
             of (0, []) => if sign then "~0E0" else "0E0"
              | (_, []) => if sign
                  then String.concat["~0.", zeros prec, "E0"]
                  else String.concat["0.", zeros prec, "E0"]
              | (0, _) => (case roundAndNormalize (digits, 1)
                   of (false, digits, nz) => let
                        val frags = [Int.toString exp]
                        val frags = if (nz > 0)
                              then "0E" :: frags
                              else prependDigits(digits, "E" :: frags)
                        in
                          if sign
                            then String.concat("~" :: frags)
                            else String.concat frags
                        end
                    | (true, _, _) => if sign
                        then "~1E" ^ Int.toString(exp+1)
                        else "1E" ^ Int.toString(exp+1)
                  (* end case *))
              | (_, d::dr) => let
                (* round if number of digits to the right of the decimal is
                 * greater than `prec`
                 *)
                val (whole, nFracDigits, frac, exp) = if (nFracDigits <= prec)
                      then (d, nFracDigits, dr, exp)
                      else (case round (digits, prec+1)
                         of (false, d::ds) => (d, prec, ds, exp)
                          | (true, ds) => (1, prec, List.take(ds, prec), exp+1)
                          | _ => raise Fail "impossible"
                        (* end case *))
                val frags = ["E", Int.toString exp]
                val frags = if (nFracDigits < prec)
                      then zeros (prec - nFracDigits) :: frags
                      else frags
                val frags = getDigit whole :: "." :: prependDigits(frac, frags)
                in
                  String.concat (addSign (sign, frags))
                end
            (* end case *)
          end

    fun insertDecimal (_, digits, 0, _) = prependDigits (digits, [])
      | insertDecimal (nDigits, digits, nFrac, prec) = let
          val (w, f) = List.splitAt (digits, nDigits - nFrac)
          val frags = if (nFrac < prec)
                then [zeros (prec - nFrac)]
                else []
          in
            prependDigits (w, "." :: prependDigits (f, frags))
          end

    (* convert to fixed-point notation: "[~]?[0-9]+.[0-9]+?", where `prec` specifies
     * the number of digits to appear after the decimal point.
     *)
    fun toFix prec (sign, 0, _, _) = (case (prec, sign)
           of (0, false) => "0"
            | (0, true) => "~0"
            | (_, false) => "0." ^ zeros prec
            | (_, true) => "~0." ^ zeros prec
          (* end case *))
      | toFix prec (sign, nDigits, digits, exp) =
          if (exp >= 0)
            then let
              val frags = if prec > 0 then [".", zeros prec] else []
              val frags = if exp > 0 then zeros exp :: frags else frags
              val frags = prependDigits(digits, frags)
              in
                String.concat (addSign (sign, frags))
              end
            else let (* exp < 0 *)
              (* the number of places to shift to the right, which also gives
               * the position of the rightmost digit relative to the decimal
               * point.
               *)
              val rShift = ~exp
              (* position of the leftmost digit relative to the decimal point.
               * Values >= 0 are the number of zeros between the decimal and
               * the most significant fractional digit.  Values < 0 represent
               * the number of digits to the left of the decimal.
               *)
              val lPos = rShift - nDigits
              (* We know that 0 < nDigits, 0 < rShift, and lPos < rShift.  The
               * possible cases are:
               *
               *   a) lPos < 0 < rShift <= prec -- ddd.ddd no rounding
               *   b) lPos < 0 <= prec < rShift -- ddd.ddd round to
               *                                   `nDigits - (rShift - prec)` digits
               *   c) 0 = lPos = prec < rShift  -- d round to 0 digits; d is carry
               *   d) 0 = lPos < prec < rShift  -- x.dddd round to `prec` digits;
               *                                   x is carry
               *   e) 0 = lPos < prec = rShift  -- 0.dddd
               *   f) 0 = lPos < rShift < prec  -- 0.dd00; `prec-rShift` trailing zeros
               *   g) 0 < lPos < prec < rShift  -- 0.00dd round to `prec - lPos` digits
               *   h) 0 < lPos < rShift <= prec -- 0.00dd with possible trailing zeros
               *   i) 0 < prec = lPos < rShift  -- 0.000d round to 0 digits; d is carry
               *   j) 0 = prec < lPos < rShift  -- 0
               *   k) 0 < prec < lPos < rShift  -- 0.0000 `prec` zeros
               *)
              val frags = if (lPos < 0)
                      then if (rShift <= prec)
                        (* (a) `lPos < 0 < rShift <= prec` *)
                        then insertDecimal(nDigits, digits, rShift, prec)
                        (* (b) `lPos < 0 <= prec < rShift` *)
                        else let
                          val nSigDigits = nDigits - (rShift - prec)
                          val (co, ds) = round (digits, nSigDigits)
                          val (nSigDigits, digits) = if co
                                then (nSigDigits + 1, 1::ds)
                                else (nSigDigits, ds)
                          in
                            insertDecimal(nSigDigits, digits, prec, prec)
                          end
                    else if (lPos = 0)
                      then if (prec < rShift)
                        then let
                          val (co, ds) = round (digits, prec)
                          in
                            if (prec = 0)
                              (* (c) `0 = lPos = prec < rShift` *)
                              then if co then ["1"] else ["0"]
                              (* (d) `0 = lPos < prec <= rShift` *)
                              else let
                                val frags = prependDigits(ds, [])
                                in
                                  if co then "1." :: frags else "0." :: frags
                                end
                          end
                      else if (prec = rShift)
                        (* (e) `0 = lPos < prec = rShift` *)
                        then "0." :: prependDigits(digits, [])
                        (* (f) `0 = lPos < rShift < prec` *)
                        else "0." :: prependDigits(digits, [zeros(prec - rShift)])
                    (* 0 < lPos *)
                    else if (lPos < prec)
                      then if (prec < rShift)
                        (* (g) `0 < lPos < prec < rShift` *)
                        then let
                          val nSigDigits = prec - lPos
                          val (co, ds) = round (digits, nSigDigits)
                          val (digits, lPos) = if co
                                then (1 :: ds, lPos-1)
                                else (ds, lPos)
                          in
                            "0." :: prependZeros(lPos, prependDigits(digits, []))
                          end
                        (* (h) `0 < lPos < rShift <= prec` *)
                        else let
                          val frags = if (rShift < prec)
                                then [zeros(prec - rShift)]
                                else []
                          in
                            "0." :: prependZeros(lPos, prependDigits(digits, frags))
                          end
                    else if (lPos = prec)
                      (* (i) `0 < prec = lPos < rShift` *)
                      then let
                        val (co, _) = round (digits, 0)
                        in
                          if co
                            then "0." :: prependZeros(prec-1, ["1"])
                            else ["0.", zeros prec]
                        end
                    (* prec < lPos < rShift *)
                    else if (prec = 0)
                      (* (j) `0 = prec < lPos < rShift` *)
                      then ["0"]
                      (* (k) `0 < prec < lPos < rShift` *)
                      else ["0.", zeros prec]
              in
                String.concat (addSign (sign, frags))
              end

    (* return the number of characters required to represent the exponent *)
    fun expSize 0 = 0
      | expSize e = let
          (* count the "E" and sign *)
          val (n, e) = if (e < 0) then (2, ~e) else (1, e)
          in
            if (n < 10) then n+1
            else if (n < 100) then n+2
            else if (n < 1000) then n+3
            else if (n < 10000) then n+4
            else n+5 (* covers max exponent for 128-bit floats *)
          end

    fun toGen nSigDigits (sign, nDigits, digits, exp) = (
print(concat["# toGen ", Int.toString nSigDigits, "\n"]);
          case (sign, digits)
           of (false, []) => "0"
            | (true, []) => "~0"
            | _ => let
                (* first we round to the maximum number of significant digits; note
                 * that rounding can produce trailing zeros, which are dropped.
                 *)
                val (nDigits, digits, exp) = if (nSigDigits < nDigits)
                      then let (* round to the number of significant digits and normalize *)
                        (* adjust the exponent for the implicit right shift *)
                        val exp = exp + (nDigits - nSigDigits)
                        in
                          case roundAndNormalize (digits, nSigDigits)
                           of (false, ds, nz) => (nSigDigits-nz, ds, exp+nz)
                            | (true, ds, nz) => let
                                val nSigDigits = nSigDigits - nz (* = length ds *)
                                val exp = exp + nz
                                in
                                  if (nSigDigits > 1)
                                    then (nSigDigits, 1 :: List.take(ds, nSigDigits-1), exp)
                                    else (1, [1], exp)
                                end
                          (* end case*)
                        end
                      else (nDigits, digits, exp)
(*+DEBUG*)
val _ = print(concat["# nDigits = ", Int.toString nDigits,
", digits = [", String.concatWithMap "," Int.toString digits,
"], exp = ", Int.toString exp, "\n"]);
(*-DEBUG*)
                (* compute the size (w/o sign) when formatting in fixed-point notation *)
                val fixSize = if (exp >= 0)
                        (* <whole> <zeros> *)
                        then nDigits + exp
                      else if (~exp >= nDigits)
                        (* "0" "." <zeros> <frac> *)
                        then 2 - exp (* note that exp < 0 *)
                        (* <whole> "." <frac> *)
                        else nDigits + 1
                (* compute the size (w/o sign) when formatting in scientific notation *)
                val sciSize = if (nDigits = 1)
                      (* "<digit> "E" <exp> *)
                      then 1 + expSize exp
                      (* "<digit> "." <frac> "E" <exp> *)
                      else nDigits + 1 + expSize(nDigits - 1)
                in
                  if (fixSize <= sciSize)
                    then let (* fixed-point notation *)
                      val frags = if (exp > 0)
                              then prependDigits(digits, [zeros exp])
                            else if (exp = 0)
                              then prependDigits(digits, [])
                            else if (~exp > nDigits)
                              then "0." :: zeros(~exp - nDigits)
                                :: prependDigits(digits, [])
                            else if (~exp = nDigits)
                              then "0." :: prependDigits(digits, [])
                              else let
                                val (w, f) = List.splitAt (digits, nDigits + exp)
                                in
                                  prependDigits(w, "." :: prependDigits(f, []))
                                end
                      val frags = if sign then "~" :: frags else frags
                      in
                        String.concat frags
                      end
                    else let (* scientific notation *)
                      val exp = exp + (nDigits - 1) (* adjust exponent *)
                      val frags = ["E", Int.toString exp]
                      val frags = (case digits
                             of [d] => getDigit d :: frags
                              | d::frac => getDigit d :: "." :: prependDigits(frac, frags)
                              | [] => raise Fail "impossible"
                            (* end case *))
                      val frags = if sign then "~" :: frags else frags
                      in
                        String.concat frags
                      end
                end
          (* end case *))

    (* convert to an "exact" decimal representation of the form
     * "[~]?0.[0-9]+(E[0-9]+)?"
     *)
    fun toExact (sign, nDigits, digits, exp) = (
          case (sign, digits)
           of (false, []) => "0.0"
            | (true, []) => "~0.0"
            | _ => let
                (* adjust `exp` so that digits are to the right of the decimal *)
                val exp = exp + nDigits
                (* build up list of fragments from right to left *)
                val frags = if (exp <> 0) then ["E", Int.toString exp] else []
                val frags = List.foldr
                      (fn (d, frags) => getDigit d :: frags)
                        frags digits
                val frags = if sign then "~0." :: frags else "0." :: frags
                in
                  String.concat frags
                end
          (* end case *))

    fun fmt mode = (case mode
           of SCI NONE => wrapper(toSci 6)
            | SCI(SOME p) => if (p < 0) then raise Size else wrapper(toSci p)
            | FIX NONE => wrapper(toFix 6)
            | FIX(SOME p) => if (p < 0) then raise Size else wrapper(toFix p)
            | GEN NONE => wrapper(toGen 12)
            | GEN(SOME p) => if (p < 1) then raise Size else wrapper(toGen p)
            | EXACT => wrapper toExact
          (* end case *))

    val toString = wrapper (toGen 12)

    val toExactString = wrapper toExact

  end

(*
(* a test case that produces too many trailing zeros *)
val data = [16.168,15.927,15.938,15.958,15.927];
val x = (List.foldl Real.+ 0.0 data) / real(List.length data);

(* the result of `Real64ToFRep.cvt x` *)
val x' = FloatRep.Normal{
        sign = false,
        nDigits = 17,
        digits = [1,5,9,8,3,6,0,0,0,0,0,0,0,0,0,0,1],
        exp = ~15
      }

val x'' = FRepToString.toString x';
*)
