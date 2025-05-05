(* tests.sml
 *
 * Tests for real to string conversions
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Tests =
  struct

    local
      datatype realfmt = datatype StringCvt.realfmt
      datatype float_rep = datatype FloatRep.float_rep

      (* the functions we are testing *)
      val fmt = FRepToString.fmt

      (* test values *)
      val posZ = Zero false
      val negZ = Zero true
      val posInf = Inf false
      val negInf = Inf true
      fun normal (s, ds, e) = Normal{
              sign = s,
              nDigits = List.length ds,
              digits = ds,
              exp = e
            }
      val m_0_001 = normal(false, [1], ~3)
      val m_0_01 = normal(false, [1], ~2)
      val m_0_1 = normal(false, [1], ~1)
      val m_1_0 = normal(false, [1], 0)
      val n_1_0 = normal(true, [1], 0)
      val m_1_4 = normal(false, [1,4], ~1)
      val m_1_5 = normal(false, [1,5], ~1)
      val m_1_51 = normal(false, [1,5,1], ~2)
      val m_1_6 = normal(false, [1,6], ~1)
      val m_10_0 = normal(false, [1], 0)
      val m_14_0 = normal(false, [1,4], 0)
      val m_15_0 = normal(false, [1,5], 0)
      val m_16_0 = normal(false, [1,6], 0)
      val m_99_998 = normal(false, [9,9,9,9,8], ~3)
      val m_19_998 = normal(false, [1,9,9,9,8], ~3)
      val m_1230 = normal(false, [1,2,3], 1)
      val m_0_00099998 = normal(false, [9,9,9,9,8], ~8)
      val m_91827364509182 = normal(false, [9,1,8,2,7,3,6,4,5,0,9,1,8,2], 0) (* issue 194 *)
      val m_1_7976931348623157_e308 =
            normal(false, [1,7,9,7,6,9,3,1,3,4,8,6,2,3,1,5,7], 308 - 16)
      val m_9007199254740991 = (* 2^53 - 1 *)
            normal(false, [9,0,0,7,1,9,9,2,5,4,7,4,0,9,9,1], 0)
      val m_9007199254740992 = (* 2^53 *)
            normal(false, [9,0,0,7,1,9,9,2,5,4,7,4,0,9,9,2], 0)
      fun repToS f = let
            fun toS {sign, nDigits, digits, exp} = String.concat[
                    if sign then "~" else "",
                    if nDigits = 0
                      then "0"
                      else String.concatWithMap "" Int.toString digits,
                    "E",
                    Int.toString exp
                  ]
            in
              case f
               of Inf sign => "Inf " ^ Bool.toString sign
                | NaN sign => "NaN " ^ Bool.toString sign
                | Zero sign => "Zero " ^ Bool.toString sign
                | Normal rep => toS rep
                | Subnormal rep => toS rep
              (* end case *)
            end
      fun modeToS m = let
            fun toS (k, NONE) = concat["(", k, " NONE)"]
              | toS (k, SOME p) = concat["(", k, "(SOME ", Int.toString p, "))"]
            in
              case m
               of SCI optP => toS ("SCI", optP)
                | FIX optP => toS ("FIX", optP)
                | GEN optP => toS ("GEN", optP)
                | EXACT => "EXACT"
              (* end case *)
            end
      fun red s = concat[
              ANSITerm.toString [ANSITerm.FG ANSITerm.Red],
              s,
              ANSITerm.toString [ANSITerm.RESET]
            ]
      fun green s = concat[
              ANSITerm.toString [ANSITerm.FG ANSITerm.Green],
              s,
              ANSITerm.toString [ANSITerm.RESET]
            ]
      val greenOk = green "OK"
      fun test cnt (f, mode, expected) = let
            fun cntFail () = (cnt := !cnt + 1)
            val _ = print (concat["fmt ", modeToS mode, " <", repToS f, "> = "])
            val result = let
                  val f' = fmt mode f
                  in
                    concat [
                        f', " ",
                        if (f' <> expected)
                          then (
                            cntFail ();
                            red(concat["FAIL (expected ", expected, ")"]))
                          else greenOk
                      ]
                  end
                  handle exn => (
                    cntFail ();
                    red(concat["[", exnMessage exn, "] (expected ", expected, ")"]))
            in
              print (result ^ "\n")
            end
    in
      fun run () = let
            val nFails = ref 0
            in
              List.app (test nFails) [
                  (** scientific mode **)
                  (posZ,                SCI(SOME 0), "0E0"),
                  (posZ,                SCI(SOME 1), "0.0E0"),
                  (posZ,                SCI(SOME 3), "0.000E0"),
                  (negZ,                SCI(SOME 0), "~0E0"),
                  (negZ,                SCI(SOME 3), "~0.000E0"),
                  (m_0_001,             SCI(SOME 0), "1E~3"),
                  (m_0_001,             SCI(SOME 1), "1.0E~3"),
                  (m_0_001,             SCI(SOME 3), "1.000E~3"),
                  (m_0_01,              SCI(SOME 0), "1E~2"),
                  (m_0_01,              SCI(SOME 1), "1.0E~2"),
                  (m_0_01,              SCI(SOME 3), "1.000E~2"),
                  (m_0_1,               SCI(SOME 0), "1E~1"),
                  (m_0_1,               SCI(SOME 1), "1.0E~1"),
                  (m_0_1,               SCI(SOME 3), "1.000E~1"),
                  (m_1_0,               SCI(SOME 0), "1E0"),
                  (m_1_0,               SCI(SOME 1), "1.0E0"),
                  (m_1_0,               SCI(SOME 3), "1.000E0"),
                  (n_1_0,               SCI(SOME 0), "~1E0"),
                  (n_1_0,               SCI(SOME 1), "~1.0E0"),
                  (n_1_0,               SCI(SOME 3), "~1.000E0"),
                  (m_1_4,               SCI(SOME 0), "1E0"),
                  (m_1_4,               SCI(SOME 1), "1.4E0"),
                  (m_1_4,               SCI(SOME 3), "1.400E0"),
                  (m_1_5,               SCI(SOME 0), "1E0"),
                  (m_1_5,               SCI(SOME 1), "1.5E0"),
                  (m_1_5,               SCI(SOME 3), "1.500E0"),
                  (m_1_51,              SCI(SOME 0), "2E0"),
                  (m_1_51,              SCI(SOME 1), "1.5E0"),
                  (m_1_51,              SCI(SOME 3), "1.510E0"),
                  (m_1_6,               SCI(SOME 0), "2E0"),
                  (m_1_6,               SCI(SOME 1), "1.6E0"),
                  (m_1_6,               SCI(SOME 3), "1.600E0"),
                  (m_99_998,            SCI(SOME 0), "1E2"),
                  (m_99_998,            SCI(SOME 1), "1.0E2"),
                  (m_99_998,            SCI(SOME 2), "1.00E2"),
                  (m_99_998,            SCI(SOME 3), "1.000E2"),
                  (m_99_998,            SCI(SOME 4), "9.9998E1"),
                  (m_19_998,            SCI(SOME 0), "2E1"),
                  (m_19_998,            SCI(SOME 1), "2.0E1"),
                  (m_19_998,            SCI(SOME 2), "2.00E1"),
                  (m_19_998,            SCI(SOME 3), "2.000E1"),
                  (m_19_998,            SCI(SOME 4), "1.9998E1"),
                  (m_1230,              SCI(SOME 0), "1E3"),
                  (m_1230,              SCI(SOME 1), "1.2E3"),
                  (m_1230,              SCI(SOME 3), "1.230E3"),
                  (m_0_00099998,        SCI(SOME 0), "1E~3"),
                  (m_0_00099998,        SCI(SOME 1), "1.0E~3"),
                  (m_0_00099998,        SCI(SOME 2), "1.00E~3"),
                  (m_0_00099998,        SCI(SOME 3), "1.000E~3"),
                  (m_0_00099998,        SCI(SOME 4), "9.9998E~4"),
                  (** fixed-point mode **)
                  (posZ,                FIX(SOME 0), "0"),
                  (posZ,                FIX(SOME 1), "0.0"),
                  (posZ,                FIX(SOME 3), "0.000"),
                  (negZ,                FIX(SOME 0), "~0"),
                  (m_0_001,             FIX(SOME 0), "0"),
                  (m_0_001,             FIX(SOME 1), "0.0"),
                  (m_0_001,             FIX(SOME 3), "0.001"),
                  (m_0_01,              FIX(SOME 0), "0"),
                  (m_0_01,              FIX(SOME 1), "0.0"),
                  (m_0_01,              FIX(SOME 3), "0.010"),
                  (m_0_1,               FIX(SOME 0), "0"),
                  (m_0_1,               FIX(SOME 1), "0.1"),
                  (m_0_1,               FIX(SOME 3), "0.100"),
                  (m_1_0,               FIX(SOME 0), "1"),
                  (m_1_0,               FIX(SOME 1), "1.0"),
                  (m_1_0,               FIX(SOME 3), "1.000"),
                  (n_1_0,               FIX(SOME 0), "~1"),
                  (n_1_0,               FIX(SOME 1), "~1.0"),
                  (n_1_0,               FIX(SOME 3), "~1.000"),
                  (m_1_4,               FIX(SOME 0), "1"),
                  (m_1_4,               FIX(SOME 1), "1.4"),
                  (m_1_4,               FIX(SOME 3), "1.400"),
                  (m_1_5,               FIX(SOME 0), "1"),
                  (m_1_5,               FIX(SOME 1), "1.5"),
                  (m_1_5,               FIX(SOME 3), "1.500"),
                  (m_1_51,              FIX(SOME 0), "2"),
                  (m_1_51,              FIX(SOME 1), "1.5"),
                  (m_1_51,              FIX(SOME 3), "1.510"),
                  (m_1_6,               FIX(SOME 0), "2"),
                  (m_1_6,               FIX(SOME 1), "1.6"),
                  (m_1_6,               FIX(SOME 3), "1.600"),
                  (m_99_998,            FIX(SOME 0), "100"),
                  (m_99_998,            FIX(SOME 1), "100.0"),
                  (m_99_998,            FIX(SOME 2), "100.00"),
                  (m_99_998,            FIX(SOME 3), "99.998"),
                  (m_99_998,            FIX(SOME 4), "99.9980"),
                  (m_19_998,            FIX(SOME 0), "20"),
                  (m_19_998,            FIX(SOME 1), "20.0"),
                  (m_19_998,            FIX(SOME 2), "20.00"),
                  (m_19_998,            FIX(SOME 3), "19.998"),
                  (m_19_998,            FIX(SOME 4), "19.9980"),
                  (m_1230,              FIX(SOME 0), "1230"),
                  (m_1230,              FIX(SOME 1), "1230.0"),
                  (m_1230,              FIX(SOME 3), "1230.000"),
                  (m_0_00099998,        FIX(SOME 0), "0"),
                  (m_0_00099998,        FIX(SOME 1), "0.0"),
                  (m_0_00099998,        FIX(SOME 2), "0.00"),
                  (m_0_00099998,        FIX(SOME 3), "0.001"),
                  (m_0_00099998,        FIX(SOME 4), "0.0010"),
                  (m_0_00099998,        FIX(SOME 8), "0.00099998"),
                  (m_91827364509182,    FIX(SOME 0), "91827364509182"),
                  (m_91827364509182,    FIX(SOME 1), "91827364509182.0"),
                  (m_91827364509182,    FIX(SOME 3), "91827364509182.000"),
                  (** generic mode **)
                  (posZ,                GEN NONE, "0"),
                  (posZ,                GEN(SOME 1), "0"),
                  (posZ,                GEN(SOME 3), "0"),
                  (negZ,                GEN(SOME 1), "~0"),
                  (m_0_001,             GEN NONE, "1E~3"),
                  (m_0_001,             GEN(SOME 1), "1E~3"),
                  (m_0_001,             GEN(SOME 3), "1E~3"),
                  (m_0_01,              GEN NONE, "0.01"),
                  (m_0_01,              GEN(SOME 1), "0.01"),
                  (m_0_01,              GEN(SOME 3), "0.01"),
                  (m_0_1,               GEN NONE, "0.1"),
                  (m_0_1,               GEN(SOME 1), "0.1"),
                  (m_0_1,               GEN(SOME 3), "0.1"),
                  (m_1_0,               GEN NONE, "1"),
                  (m_1_0,               GEN(SOME 1), "1"),
                  (m_1_0,               GEN(SOME 3), "1"),
                  (n_1_0,               GEN NONE, "~1"),
                  (n_1_0,               GEN(SOME 1), "~1"),
                  (n_1_0,               GEN(SOME 3), "~1"),
                  (m_1_4,               GEN NONE, "1.4"),
                  (m_1_4,               GEN(SOME 1), "1"),
                  (m_1_4,               GEN(SOME 3), "1.4"),
                  (m_1_5,               GEN NONE, "1.5"),
                  (m_1_5,               GEN(SOME 1), "1"),
                  (m_1_5,               GEN(SOME 3), "1.5"),
                  (m_1_51,              GEN NONE, "1.51"),
                  (m_1_51,              GEN(SOME 1), "2"),
                  (m_1_51,              GEN(SOME 3), "1.51"),
                  (m_1_6,               GEN NONE, "1.6"),
                  (m_1_6,               GEN(SOME 1), "2"),
                  (m_1_6,               GEN(SOME 3), "1.6"),
                  (m_99_998,            GEN NONE, "99.998"),
                  (m_99_998,            GEN(SOME 1), "100"),
                  (m_99_998,            GEN(SOME 2), "100"),
                  (m_99_998,            GEN(SOME 3), "100"),
                  (m_99_998,            GEN(SOME 4), "100"),
                  (m_99_998,            GEN(SOME 5), "99.998"),
                  (m_99_998,            GEN(SOME 6), "99.998"),
                  (m_19_998,            GEN NONE, "19.998"),
                  (m_19_998,            GEN(SOME 1), "20"),
                  (m_19_998,            GEN(SOME 2), "20"),
                  (m_19_998,            GEN(SOME 3), "20"),
                  (m_19_998,            GEN(SOME 4), "20"),
                  (m_19_998,            GEN(SOME 5), "19.998"),
                  (m_19_998,            GEN(SOME 6), "19.998"),
                  (m_1230,              GEN NONE, "1230"),
                  (m_1230,              GEN(SOME 1), "1E3"),
                  (m_1230,              GEN(SOME 3), "1230"),
                  (m_91827364509182,    GEN NONE, "91827364509200"), (* 12 significant digits *)
                  (m_91827364509182,    GEN(SOME 1), "9E13"),
                  (m_91827364509182,    GEN(SOME 13), "91827364509180"),
                  (m_91827364509182,    GEN(SOME 14), "91827364509182"),
                  (m_91827364509182,    GEN(SOME 15), "91827364509182"),
                  (m_1_7976931348623157_e308, GEN(SOME 17), "1.7976931348623157E308"),
                  (m_9007199254740991,  GEN(SOME 17), "9007199254740991"),
                  (m_9007199254740992,  GEN(SOME 17), "9007199254740992"),
                  (** exact mode **)
                  (posZ,                EXACT, "0.0"),
                  (negZ,                EXACT, "~0.0"),
                  (m_0_001,             EXACT, "0.1E~2"),
                  (m_0_01,              EXACT, "0.1E~1"),
                  (m_0_1,               EXACT, "0.1"),
                  (m_1_0,               EXACT, "0.1E1"),
                  (m_1_4,               EXACT, "0.14E1"),
                  (m_1_5,               EXACT, "0.15E1"),
                  (m_1_51,              EXACT, "0.151E1"),
                  (m_1_6,               EXACT, "0.16E1"),
                  (m_99_998,            EXACT, "0.99998E2"),
                  (m_1230,              EXACT, "0.123E4"),
                  (m_91827364509182,    EXACT, "0.91827364509182E14"),
                  (m_1_7976931348623157_e308, EXACT, "0.17976931348623157E309"),
                  (m_9007199254740991,  EXACT, "0.9007199254740991E16"),
                  (m_9007199254740992,  EXACT, "0.9007199254740992E16")
                ];
              if (!nFails > 0)
                then print(red(concat[
                    "!! ", Int.toString(!nFails),
                    if (!nFails > 1) then " tests" else " test",
                    " failed !!\n"
                  ]))
                else print(green "!! all tests passed !!\n")
            end
    end (* local *)

  end
