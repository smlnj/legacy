(* color-space.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories
 *
 * Code of various color space utilities.
 *
 *)

structure ColorSpace : COLOR_SPACE =
  struct
    structure EXB = EXeneBase

    val maxrgb = 65535.0

    datatype hsv = HSV of {hue : real, sat : real, value : real}
    datatype yiq = YIQ of {y : real, i : real, q : real}

    fun max (a : real, b) = if a >= b then a else b
    fun min (a : real, b) = if a <= b then a else b
    fun sc r = Word.fromInt(Real.trunc(maxrgb*r))
    fun w2r w = real(Word.toIntX w)

    fun rgbToHsv (EXB.RGB{red,green,blue}) = let
          val realr = (w2r red)/maxrgb
          val realg = (w2r green)/maxrgb
          val realb = (w2r blue)/maxrgb
          val MaxV = max(realr, max(realg,realb))
          val MinV = min(realr, min(realg,realb))
          val delta = MaxV - MinV
          in
            if Real.==(delta, 0.0) then HSV{hue=0.0, sat=0.0, value=MaxV}
            else let
              val sat = delta / MaxV
              val rc = (MaxV - realr)/delta
              val gc = (MaxV - realg)/delta
              val bc = (MaxV - realb)/delta
              val h1 = if Real.==(realr, MaxV) then bc - gc
                       else if Real.==(realg, MaxV) then 2.0 + rc - bc
                       else 4.0 + gc - rc
              val h2 = 60.0 * h1                    (*  convert to degrees *)
              val hue = if h2 < 0.0 then h2 + 360.0 (* make nonnegative *)
                        else h2
              in HSV{hue=hue, sat=sat, value=MaxV} end
          end

    fun getHSVvalues (HSV v) = v

    fun hsbToRgb (HSV{sat,value,hue}) =
	  if (Real.==(sat, 0.0))
	    then let val v = sc value in EXB.RGB{red=v,green=v,blue=v} end
	    else let
	      val h = if Real.==(hue, 360.0) then 0.0 else hue/60.0
	      val i = floor h
	      val ri = real i
	      val f = h - ri
	      val p = value*(1.0 - sat)
	      val q = value*(1.0 - (sat*f))
	      val t = value*(1.0 - (sat*(1.0 - f)))
	      fun mkrgb (r,g,b) = EXB.RGB{red=sc r, green=sc g, blue=sc b}
	      in
		case i
		 of 0 => mkrgb(value, t, p)
		  | 1 => mkrgb(q, value, p)
		  | 2 => mkrgb(p, value, t)
		  | 3 => mkrgb(p, q, value)
		  | 4 => mkrgb(t, p, value)
		  | _ => mkrgb(value, p, q)
		(* end case *)
	      end

    fun get_y (EXB.RGB{red,green,blue}) = 
	  (0.3*(w2r red) + 0.59*(w2r green) + 0.11*(w2r blue))/maxrgb
    fun get_i (EXB.RGB{red,green,blue}) = 
          (0.6*(w2r red) - 0.28*(w2r green) - 0.32*(w2r blue))/maxrgb
    fun get_q (EXB.RGB{red,green,blue}) = 
          (0.21*(w2r red) - 0.52*(w2r green) + 0.31*(w2r blue))/maxrgb

    fun get getf color = let
          val rgb = EXB.rgbOfColor color
          in
	    getf rgb
	  end
    val getY = get get_y
    val getI = get get_i
    val getQ = get get_q

    fun rgbToYiq color = let
          val rgb = EXB.rgbOfColor color
          in
	    YIQ{y=get_y rgb, i=get_i rgb, q=get_q rgb}
	  end

  end
