(* test-frep-to-real.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
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
(* frep-to-real64.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Conversion of the `FloatRep.float_rep` representation to
 * 64-bit IEEE floating-point numbers.
 *
 * This code is a port of the `s2d_n` function in the Ryũ library by Ulf Adams.
 * The original C source code for that library is available at
 *
 *      https://github.com/ulfjack/ryu
 *)

structure FRepToReal64 : sig

    val cvt : FloatRep.float_rep -> real

  end = struct

(*
    structure W = InlineT.Word
    structure W64 = InlineT.Word64
    structure Int = IntImp
*)
    structure W = Word
    structure W64 = Word64

    datatype float_rep = datatype FloatRep.float_rep

(* the following should be in the Unsafe structure *)
    (* bitcast a Word64.word to a Real64.real *)
(*
    fun fromBits (b : Word64.word) : real = let
          val r : real ref = InlineT.cast(ref b)
          in
            !r
          end
*)
    val fromBits = Unsafe.Real64.castFromWord

(* the following should be part of the WORD signature *)
    (* count the leading zeros in a Word64.word value *)
    fun nlz (w : Word64.word) : word = let
          fun step (x, n, k) = let
                val y = W64.>>(x, k)
                in
                  if (y <> 0w0) then (n - k, y) else (n, x)
                end
          val (n, x) = step (w, 0w64, 0w32)
          val (n, x) = step (x, n, 0w16)
          val (n, x) = step (x, n, 0w8)
          val (n, x) = step (x, n, 0w4)
          val (n, x) = step (x, n, 0w2)
          in
            if (W64.>>(x, 0w1) <> 0w0)
              then n - 0w2
              else n - W.fromLarge x
          end

(*+DEBUG*)
    fun w128ToString (hi, lo) = concat[
	    "(", W64.fmt StringCvt.DEC hi, ", ", W64.fmt StringCvt.DEC lo, ")"
	  ]
(*-DEBUG*)

    val kMantissaBits = 52
    val kExpBits = 11
    val kExpBias = 1023

    val kPow5InvBitCount = 125
    val kPow5BitCount = 125

    val pow5InvSplit2Tbl : (Word64.word * Word64.word) vector = #[
            (                    0w1, 0w2305843009213693952 ),
            (  0w5955668970331000884, 0w1784059615882449851 ),
            (  0w8982663654677661702, 0w1380349269358112757 ),
            (  0w7286864317269821294, 0w2135987035920910082 ),
            (  0w7005857020398200553, 0w1652639921975621497 ),
            ( 0w17965325103354776697, 0w1278668206209430417 ),
            (  0w8928596168509315048, 0w1978643211784836272 ),
            ( 0w10075671573058298858, 0w1530901034580419511 ),
            (   0w597001226353042382, 0w1184477304306571148 ),
            (  0w1527430471115325346, 0w1832889850782397517 ),
            ( 0w12533209867169019542, 0w1418129833677084982 ),
            (  0w5577825024675947042, 0w2194449627517475473 ),
            ( 0w11006974540203867551, 0w1697873161311732311 ),
            ( 0w10313493231639821582, 0w1313665730009899186 ),
            ( 0w12701016819766672773, 0w2032799256770390445 )
          ]

    val pow5InvOffsetTbl : Word64.word vector = #[
            0wx54544554, 0wx04055545, 0wx10041000, 0wx00400414, 0wx40010000,
            0wx41155555, 0wx00000454, 0wx00010044, 0wx40000000, 0wx44000041,
            0wx50454450, 0wx55550054, 0wx51655554, 0wx40004000, 0wx01000001,
            0wx00010500, 0wx51515411, 0wx05555554, 0wx00000000
          ]

    val pow5Split2Tbl : (Word64.word * Word64.word) vector = #[
            (                    0w0, 0w1152921504606846976 ),
            (                    0w0, 0w1490116119384765625 ),
            (  0w1032610780636961552, 0w1925929944387235853 ),
            (  0w7910200175544436838, 0w1244603055572228341 ),
            ( 0w16941905809032713930, 0w1608611746708759036 ),
            ( 0w13024893955298202172, 0w2079081953128979843 ),
            (  0w6607496772837067824, 0w1343575221513417750 ),
            ( 0w17332926989895652603, 0w1736530273035216783 ),
            ( 0w13037379183483547984, 0w2244412773384604712 ),
            (  0w1605989338741628675, 0w1450417759929778918 ),
            (  0w9630225068416591280, 0w1874621017369538693 ),
            (   0w665883850346957067, 0w1211445438634777304 ),
            ( 0w14931890668723713708, 0w1565756531257009982 )
          ]

    val pow5OffsetTbl : Word64.word vector = #[
            0wx00000000, 0wx00000000, 0wx00000000, 0wx00000000, 0wx40000000,
            0wx59695995, 0wx55545555, 0wx56555515, 0wx41150504, 0wx40555410,
            0wx44555145, 0wx44504540, 0wx45555550, 0wx40004000, 0wx96440440,
            0wx55565565, 0wx54454045, 0wx40154151, 0wx55559155, 0wx51405555,
            0wx00000105
          ]

    (* powers of 5 from 5^0 to 5^25 *)
    val pow5TblSz = 26
    (* NOTE: on 64-bit machines, this table can be represented as a `word vector` *)
    val pow5Tbl : W64.word vector = let
          fun gen (0, _) = []
            | gen (i, n : W64.word) = n :: gen(i-1, 0w5 * n)
          in
            Vector.fromList(gen(pow5TblSz, 0w1))
          end

    (* assume w <> 0w0 *)
    fun floorLog2 (w : Word64.word) = W.toIntX(0w63 - nlz w)

    (* returns log_2(5^e) for 0 < e <= 3528 and 1 for e = 0. *)
    fun log2Pow5 e = W.toIntX(W.>>(W.fromInt e * 0w1217359, 0w19))

    fun pow5Bits e = W.toIntX(W.>>(W.fromInt e * 0w1217359, 0w19) + 0w1)

    (* returns ceil(log_2(5^e)) for 0 < e <= 3528 and 1 for e = 0. *)
    fun ceilLog2Pow5 e = log2Pow5 e + 1

    fun pow5Factor (value : Word64.word) = let
          (* 5 * m_inv_5 = 1 (mod 2^64) *)
          val m_inv_5 : Word64.word = 0w14757395258967641293
          (* #{ n | n = 0 (mod 2^64) } = 2^64 / 5 *)
          val n_div_5 : Word64.word = 0w3689348814741910323
          fun lp (value, count) = let
                val value = value * m_inv_5
                in
                  if (value > n_div_5)
                    then count
                    else lp (value, count+0w1)
                end
          in
            lp (value, 0w0)
          end

    (* returns true if value is divisible by 5^p *)
    fun multipleOfPowerOf5 (value : Word64.word, p : word) = (pow5Factor value >= p)

    (* returns true if value is divisible by 2^p *)
    fun multipleOfPowerOf2 (value, p) = (W64.andb(value, W64.<<(0w1, p)-0w1) = 0w0)

    (* 64x64 --> 128 bit unsigned multiplication *)
    fun umul128 (a, b) = let
          fun lo32 x = W64.andb(x, 0wxffffffff)
          fun hi32 x = W64.>>(x, 0w32)
          val aLo = lo32 a
          val aHi = hi32 a
          val bLo = lo32 b
          val bHi = hi32 b
          val b00 = aLo * bLo
          val b01 = aLo * bHi
          val b10 = aHi * bLo
          val b11 = aHi * bHi
          val b00Lo = lo32 b00
          val b00Hi = hi32 b00
          val mid1 = b10 + b00Hi
          val mid1Lo = lo32 mid1
          val mid1Hi = hi32 mid1
          val mid2 = b01 + mid1Lo
          val mid2Lo = lo32 mid2
          val mid2Hi = hi32 mid2
          val pHi = b11 + mid1Hi + mid2Hi
          val pLo = W64.orb(W64.<<(mid2Lo, 0w32),  b00Lo)
          in
            (pHi, pLo)
          end

    (* returns the low 64 bits from shifting the 128-bit number (hi, lo) by dist
     * bits.  We assume that 0 < dist < 64.
     *)
    fun shiftRight128 (lo, hi, dist) =
          W64.orb (W64.<<(hi, 0w64 - dist), W64.>>(lo, dist))

    fun mulShift64 (m : Word64.word, mul : (Word64.word * Word64.word), j : word) = let
          val (hi1, _) = umul128 (m, #1 mul)
          val (hi2, lo2) = umul128 (m, #2 mul)
          val sum = hi1 + lo2
          val hi2 = if (sum < hi1) then hi2+0w1 (* overflow into hi2 *) else hi2
          in
            shiftRight128 (sum, hi2, j - 0w64)
          end

    (* computes 5^i as a 128-bit result *)
    fun computePow5 i = let
          val base = i div pow5TblSz
          val base2 = base * pow5TblSz
          val offset = i - base2
          val mul = Vector.sub(pow5Split2Tbl, base)
          in
            if (offset = 0)
              then mul
              else let
                val m = Vector.sub(pow5Tbl, offset)
                val (hi1, lo1) = umul128 (m, #1 mul)
                val (hi2, lo2) = umul128 (m, #2 mul)
                val sum = hi1 + lo2
                val hi2 = if (sum < hi1) then hi2+0w1 else hi2
                val delta = W.fromInt(pow5Bits i - pow5Bits base2)
                val i' = W.fromInt i
                in (
                  shiftRight128(lo1, sum, delta) +
                    W64.andb(
                      W64.>>(
                        Vector.sub(pow5OffsetTbl, W.toIntX(i' div 0w16)),
                        W.<<(i' mod 0w16, 0w1)),
                      0w3),
                  shiftRight128(sum, hi2, delta)
                ) end
          end

    (* computes 5^{-i} as a 128-bit result *)
    fun computeInvPow5 i = let
          val base = (i + pow5TblSz - 1) div pow5TblSz
          val base2 = base * pow5TblSz
          val offset = base2 - i
          val mul = Vector.sub(pow5InvSplit2Tbl, base) (* 1 / 5^{base2} *)
(*+DEBUG*)
val _ = print(concat[
	    "computeInvPow5: base = ", Int.toString base,
	    ", base2 = ", Int.toString base2,
	    ", offset = ", Int.toString offset,
	    "\n"
	  ])
val _ = print(concat["  mul = ", w128ToString mul, "\n"])
          in
            if offset = 0
              then mul
              else let
                val m = Vector.sub(pow5Tbl, offset)
val _ = print(concat["  m = ", W64.fmt StringCvt.DEC m, "\n"])
                val (hi1, lo1) = umul128(m, #1 mul - 0w1)
val _ = print(concat["  (hi1, lo1) = ", w128ToString (hi1, lo1), "\n"])
                val (hi2, lo2) = umul128(m, #2 mul)
val _ = print(concat["  (hi2, lo2) = ", w128ToString (hi2, lo2), "\n"])
                val sum = hi1 + lo2
                val hi2 = if sum < hi1 then hi2+0w1 (* overflow into hi2 *) else hi2
                val delta = W.fromInt(pow5Bits base2 - pow5Bits i)
                val i' = W.fromInt i
                in (
                  shiftRight128(lo1, sum, delta) + 0w1 +
                    W64.andb(
                      W64.>>(
                        Vector.sub(pow5InvOffsetTbl, W.toIntX(i' div 0w16)),
                        W.<<(i' mod 0w16, 0w1)),
                      0w3),
                  shiftRight128(sum, hi2, delta)
                ) end
          end

    fun fromDecimalRep (f : FloatRep.decimal_rep) = let
          fun sumDigits ([], accum) = accum
            | sumDigits (d::ds, accum) = sumDigits (ds, W64.fromInt d + 0w10*accum)
          val m10 = sumDigits (#digits f, 0w0)
          val e10 = #exp f
          (* Convert to binary float m2 * 2^e2, while retaining information
           * about whether the conversion was exact (trailingZeros).
           *)
(*+DEBUG*)
          val _ = print(concat[
                  "m10 * 10^e10 = ", W64.fmt StringCvt.DEC m10,
                  " * 10^", Int.toString e10, "\n"
                ])
(*-DEBUG*)
          val (m2, e2, trailingZeros) = if (e10 >= 0)
                then let
                  (* The length of m * 10^e in bits is:
                   *   log2(m10 * 10^e10) =
                   *       log2(m10) + e10 log2(10) = log2(m10) + e10 + e10 * log2(5)
                   *
                   * We want to compute the kMantissaBits + 1 top-most bits (+1 for
                   * the implicit leading one in IEEE format). We therefore choose
                   * a binary output exponent of
                   *   log2(m10 * 10^e10) - (kMantissaBits + 1).
                   *
                   * We use floor(log2(5^e10)) so that we get at least this many
                   * bits; better to have an additional bit than not have enough.
                   *)
                  val e2 = floorLog2 m10 + e10 + log2Pow5 e10 - (kMantissaBits + 1)
                  (* We now compute [m10 * 10^e10 / 2^e2] = [m10 * 5^e10 / 2^(e2-e10)].
                   * To that end, we use the DOUBLE_POW5_SPLIT table.
                   *)
                  val j = e2 - e10 - ceilLog2Pow5 e10 + kPow5BitCount
                  val m2 = mulShift64 (m10, computePow5 e10, W.fromInt j)
                  (* We also compute if the result is exact; i.e.,
                   *   [m10 * 10^e10 / 2^e2] == m10 * 10^e10 / 2^e2.
                   * This can only be the case if 2^e2 divides m10 * 10^e10, which
                   * in turn requires that the largest power of 2 that divides
                   * m10 + e10 is greater than e2. If e2 is less than e10, then
                   * the result must be exact. Otherwise we use the existing
                   * multipleOfPowerOf2 function.
                   *)
                  val trailingZeros = (e2 < e10)
                        orelse ((e2 - e10 < 64)
                          andalso multipleOfPowerOf2(m10, W.fromInt(e2 - e10)))
                  in
                    (m2, e2, trailingZeros)
                  end
                else let (* e10 < 0 *)
                  val e2 = floorLog2 m10 + e10 - ceilLog2Pow5(~e10) - (kMantissaBits + 1)
                  val j = e2 - e10 + ceilLog2Pow5(~e10) - 1 + kPow5InvBitCount
(*+DEBUG*)
                  val pow5 = computeInvPow5 (~e10)
                  val _ = (
                        print(concat["j = ", Int.toString j, "\n"]);
                        print(concat["pow5 = ", w128ToString pow5, "\n"]))
(*-DEBUG*)
                  val m2 = mulShift64(m10, pow5, W.fromInt j)
                  val trailingZeros = multipleOfPowerOf5(m10, W.fromInt(~e10))
                  in
                    (m2, e2, trailingZeros)
                  end
(*+DEBUG*)
          val _ = print(concat[
                  "m2 * 2^e2 = ", W64.fmt StringCvt.DEC m2,
                  " * 2^", Int.toString e2, "\n"
                ])
(*-DEBUG*)
          (* compute the final IEEE exponent *)
          val ieee_e2 = Int.max(0, e2 + kExpBias + floorLog2 m2)
          (* We need to figure out how much we need to shift m2. The tricky part is
           * that we need to take the final IEEE exponent into account, so we need
           * to reverse the bias and also special-case the value 0.
           *)
          val shift = if (ieee_e2 = 0) then 1 else ieee_e2
          val shift = shift - e2 - kExpBias - kMantissaBits
          (* assert(shift >= 0) *)
(*+DEBUG*)
          val _ = print(concat["shift = ", Int.toString shift, "\n"])
(*-DEBUG*)
          val shift = W.fromInt shift
(*+DEBUG*)
          val _ = (
                print(concat["ieee_e2 = ", Int.toString ieee_e2, "\n"]);
                print(concat["shift = ", W.fmt StringCvt.DEC shift, "\n"]))
(*-DEBUG*)
          (* We need to round up if the exact value is more than 0.5 above the
           * value we computed. That's equivalent to checking if the last removed
           * bit was 1 and either the value was not just trailing zeros or the result
           * would otherwise be odd.
           *
           * We need to update trailingZeros given that we have the exact output
           * exponent ieee_e2 now.
           *)
          val trailingZeros = trailingZeros
                andalso (W64.andb(m2, W64.<<(0w1, shift - 0w1) - 0w1) = 0w0)
          val lastRemovedBit = W64.andb(W64.>>(m2, shift-0w1), 0w1)
          val roundUp = (lastRemovedBit <> 0w0)
                andalso (not trailingZeros
                  orelse (W64.andb(W64.>>(m2, shift), 0w1) <> 0w0));
          val ieee_m2 = W64.>>(m2, shift) + (if roundUp then 0w1 else 0w0);
(*+DEBUG*)
          val _ = (
                print(concat["roundUp = ", Bool.toString roundUp, "\n"]);
                print(concat["ieee_e2 = ", Int.toString ieee_e2, "\n"]))
(*-DEBUG*)
          (* assert(ieee_m2 <= (1 << (kMantissaBits + 1))) *)
          val ieee_m2 = W64.andb(ieee_m2, W64.<<(0w1, W.fromInt kMantissaBits) - 0w1)
          val ieee_e2 = if (ieee_m2 = 0w0) andalso roundUp
                (* Due to how the IEEE represents +/-Infinity, we don't need to
                 * check for overflow here.
                 *)
                then ieee_e2+1
                else ieee_e2
          (* build the bit representation *)
          val bits = if #sign f then W64.<<(0w1, W.fromInt kExpBits) else 0w0
          val bits = W64.orb(bits, W64.fromInt ieee_e2)
          val bits = W64.orb(W64.<<(bits, W.fromInt kMantissaBits), ieee_m2)
          in
(*+DEBUG*)
print(concat["bits = 0x", W64.toString bits, "\n"]);
(*-DEBUG*)
            fromBits bits
          end

    val posInf = fromBits 0wx7FF0000000000000
    val negInf = fromBits 0wx7FF0000000000000
    val posNaN = fromBits 0wx7FF8000000000000
    val negNaN = fromBits 0wxFFF8000000000000
    val posZero = fromBits 0wx0000000000000000
    val negZero = fromBits 0wx8000000000000000

    fun cvt (Inf false) = posInf
      | cvt (Inf true) = negInf
      | cvt (NaN false) = posNaN
      | cvt (NaN true) = negNaN
      | cvt (Zero false) = posZero
      | cvt (Zero true) = negZero
      | cvt (Normal f) = fromDecimalRep f
      | cvt (Subnormal f) = fromDecimalRep f

  end

val one = FloatRep.Normal{sign=false,nDigits=1,digits=[1],exp=0};
val onePtThree = FloatRep.Normal{digits=[1,3],exp= ~1,nDigits=2, sign=false};
