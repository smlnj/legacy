(* string-cvt.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Basic routines to convert strings to other primitive types.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

structure StringCvt : STRING_CVT =
  struct

  (* A table for mapping digits to values.  Whitespace characters map to
   * 128, "-","~" map to 129, "." maps to 130, and 0-9,A-Z,a-z map to their
   * base-36 value.  All others map to 255.
   *)
    val cvtTable = "\
	    \\255\255\255\255\255\255\255\255\255\128\128\255\255\255\255\255\
	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	    \\128\255\255\255\255\255\255\255\255\255\255\255\255\129\130\255\
	    \\000\001\002\003\004\005\006\007\008\009\255\255\255\255\255\255\
	    \\255\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\
	    \\025\026\027\028\029\030\031\032\033\034\035\255\255\255\255\255\
	    \\255\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\
	    \\025\026\027\028\029\030\031\032\033\034\035\255\255\255\129\255\
	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \"

    fun look (s, i) = (ordof(cvtTable, ordof(s, i))) handle _ => 255

    fun eatWS (s, i) = let
	  fun f j = if (look(s, j) = 128) then f(j+1) else j
	  in
	    f i
	  end

    fun eatNeg (s, indx) = if (look (s, indx) = 129)
	  then (true, indx+1)
	  else (false, indx)

    fun eatDecimalPt (s, indx) = if (look(s, indx) = 130)
	  then (true, indx+1)
	  else (false, indx)

    fun eatE (s, indx) = if (look(s, indx) = 14 (* "e" base-36 *))
	  then (true, indx+1)
	  else (false, indx)

    exception Convert

    datatype radix = Bin | Oct | Dec | Hex

    fun scan2 (s, indx) = let
	  fun scan (accum, i) = let
	        val d = look(s, i)
	        in
		  if (d < 2)
		    then scan(2*accum + d, i+1)
		    else (accum, i)
	        end
        (* skip any leading "0b" or "0B" *)
	  val indx = if (ordof(s, indx) = 48(*"0"*))
		  then let
		    val d = look(s, indx+1)
		    in
		      if (d = 11(* base-36 value of "b" *)) then indx+2 else indx
		    end
		  else indx
	  val (v, indx') = scan (0, indx)
	  in
	    if (indx = indx') then raise Convert else (v, indx')
	  end

    fun scan8 (s, indx) = let
	  fun scan (accum, i) = let
		val d = look(s, i)
		in
		  if (d < 8) then scan(8*accum + d, i+1) else (accum, i)
		end
	  val (v, indx') = scan (0, indx)
	  in
	    if (indx = indx') then raise Convert else (v, indx')
	  end

    fun scan10 (s, indx) = let
	  fun scan (accum, i) = let
		val d = look(s, i)
		in
		  if (d < 10) then scan(10*accum + d, i+1) else (accum, i)
		end
	  val (v, indx') = scan (0, indx)
	  in
	    if (indx = indx') then raise Convert else (v, indx')
	  end

    fun scan16 (s, indx) = let
	  fun scan (accum, i) = let
	        val d = look(s, i)
	        in
		  if (d < 16)
		    then scan(16*accum + d, i+1)
		    else (accum, i)
	        end
        (* skip any leading "0x" or "0X" *)
	  val indx = if (ordof(s, indx) = 48(*"0"*))
		  then let
		    val d = look(s, indx+1)
		    in
		      if (d = 33(* base-36 vlue of "x" *)) then indx+2 else indx
		    end
		  else indx
	  val (v, indx') = scan (0, indx)
	  in
	    if (indx = indx') then raise Convert else (v, indx')
	  end

    fun strToInt radix = let
	  fun cvt f (s, indx) = let
		val indx = eatWS(s, indx)
		val (isNeg, indx) = eatNeg(s, indx)
		val (v, indx) = f(s, indx)
		in
		  if isNeg then (~v, indx) else (v, indx)
		end
		  handle Ord => raise Convert
	  in
	    case radix
	     of Bin => cvt scan2
	      | Oct => cvt scan8
	      | Dec => cvt scan10
	      | Hex => cvt scan16
	  end

    local
      fun cvt radix = let val g = strToInt radix in fn s => #1(g(s, 0)) end
    in
    val batoi = cvt Bin
    val oatoi = cvt Oct
    val atoi = cvt Dec
    val xatoi = cvt Hex
    end

  (* this is like scan10, except that it uses a floating-pt accumulator.
   * It is used when scan10 overflows.
   *)
    fun fscan10 (s, indx) = let
	  fun scan (accum, i) = let
		val d = look(s, i)
		in
		  if (d < 10) then scan(10.0*accum + (real d), i+1) else (accum, i)
		end
	  in
	    let val (v, i) = scan10(s, indx) in (real v, i) end
	      handle _ => scan (0.0, indx)
	  end

    local
      val negTbl = #[
	      1.0E~0, 1.0E~1, 1.0E~2, 1.0E~3, 1.0E~4,
	      1.0E~5, 1.0E~6, 1.0E~7, 1.0E~8, 1.0E~9
	    ]
      val posTbl = #[
	      1.0E0, 1.0E1, 1.0E2, 1.0E3, 1.0E4, 1.0E5, 1.0E6, 1.0E7, 1.0E8, 1.0E9
	    ]
      fun scale (tbl, step10 : real) = let
	    fun f (r, 0) = r
	      | f (r, exp) = if (exp < 10)
		  then (r * Vector.sub(tbl, exp))
		  else f (step10 * r, exp-10)
	    in
	      f
	    end
    in
    val scaleUp = scale (posTbl, 1.0E10)
    val scaleDown = scale (negTbl, 1.0E~10)
    end

    fun strToReal (s, indx) = let
	  val indx = eatWS(s, indx)
	  val (isNeg, wholeIndx) = eatNeg(s, indx)
	  val (whole, indx) = fscan10(s, wholeIndx)
	  val hasWhole = (wholeIndx < indx)
	  val (hasDecimal, fracIndx) = eatDecimalPt(s, indx)
	  val (num, indx) = if hasDecimal
		then let val (frac, j) = fscan10(s, fracIndx)
		  in
		    (scaleDown (frac, j-fracIndx) + whole, j)
		  end
		else (whole, fracIndx)
	  val hasFrac = (fracIndx < indx)
	  val num = if (hasWhole orelse hasFrac)
		then if isNeg then ~num else num
		else raise Convert
	  val (hasExp, indx) = eatE (s, indx)
	  in
	    if hasExp
	      then let
		val (negExp, expIndx) = eatNeg(s, indx)
		val (exp, indx) = scan10(s, expIndx)
		in
		  if (expIndx = indx)
		    then raise Convert
		  else if negExp
		    then (scaleDown(num, exp), indx)
		    else (scaleUp(num, exp), indx)
		end
	      else (num, indx)
	  end

    fun atof s = #1(strToReal (s, 0))

    fun strToBool (s, indx) = let
	  val indx = eatWS (s, indx)
	  fun match (prefix, v, indx) = let
		val len = size prefix
		fun f (i, j) =
		      if (i = len)
			then (v, j)
		      else let
			val target = ordof(prefix, i) and c = ordof(s, j)
			in
			  if (target = c) orelse (target = (c+32(* "a" - "A" *)))
			    then f (i+1, j+1)
			    else raise Convert
			end
		in
		  f (0, indx)
		end
	  in
	    case (ordof (s, indx))
	     of (* "F" *) 70 => match ("alse", false, indx+1)
	      | (* "T" *) 84 => match ("rue", true, indx+1)
	      | (* "f" *) 102 => match ("alse", false, indx+1)
	      | (* "t" *) 116 => match ("rue", true, indx+1)
	      | _ => raise Convert
	  end
	    handle _ => raise Convert
	
    fun atob s = #1(strToBool (s, 0))

  end (* StringCvt *)
