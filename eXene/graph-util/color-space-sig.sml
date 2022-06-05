(* color-space-sig.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories
 *
 * Specification of various color space utilities.
 *
 *)

signature COLOR_SPACE =
  sig
    type hsv

    val rgbToHsv : EXeneBase.rgb -> hsv
    val hsbToRgb : hsv -> EXeneBase.rgb

    val getHSVvalues : hsv -> {hue : real, sat : real, value : real}
  end
