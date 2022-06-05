(* string-util.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.
 *
 * A bunch of string operations.  At some point these should be rationalized.
 *
 * AUTHOR:
 *   John Reppy
 *   Cornell University
 *   Ithaca, NY 14853
 *   jhr@cs.cornell.edu
 *)

structure StringUtil : STRING_UTIL =
  struct

    exception NotFound

    fun badArg (func, msg) =
	  LibBase.badArg{module="StringUtil", func=func, msg=msg}

    fun checkArg name (s, i) = if ((i < 0) orelse (size s <= i))
	    then badArg(name, "subscript")
	    else ();

  (* inStr : string -> (string * int) -> bool
   * returns a predicate for testing whether characters are in a string.
   *)
    fun inStr "" = (fn _ => false)
      | inStr s = if (String.size s = 1)
	  then let val c = ord(s)
	    in
	      fn c' => (c = c')
	    end
	  else CharSet.inSetOrd(CharSet.mkCharSet s)

    fun indexp pred (s, i) = let
	  fun lp i = (if (pred (ordof (s, i))) then i else lp(i+1))
	  in
	    checkArg "index[p]" (s, i);
	    (lp i) handle _ => raise NotFound
	  end
    fun index s = indexp (inStr s)
    fun revindexp pred  (s, i) = let
	  fun lp i = (if (pred (ordof (s, i))) then i else lp(i-1))
	  in
	    checkArg "revindex[p]" (s, i-1);
	    (lp (i-1)) handle _ => raise NotFound
	  end
    fun revindex s = revindexp(inStr s)

    fun spanp pred (s, i) = let
	  fun lp i = (if (pred (ordof (s, i))) then lp(i+1) else i)
	  in
	    checkArg "span[p]" (s, i);
	    (lp i) handle _ => (String.size s)
	  end
    fun span s = spanp (inStr s)

    fun cspanp pred (s, i) = let
	  fun lp i = (if (pred (ordof (s, i))) then i else lp(i+1))
	  in
	    checkArg "cspan[p]" (s, i);
	    (lp i) handle _ => (String.size s)
	  end
    fun cspan s = cspanp (inStr s)

    fun tokenizep pred (s, i) = let
	  val n = size s
	  fun substr (i, j, l) = if (i = j) then l else substring(s, i, j-i)::l
	  fun scanTok (i, j, toks) = if (j < n)
		  then if (pred (ordof (s, j)))
		    then skipSep(j+1, substr(i, j, toks))
		    else scanTok (i, j+1, toks)
		  else substr(i, j, toks)
	  and skipSep (j, toks) = if (j < n)
		  then if (pred (ordof (s, j)))
		    then skipSep(j+1, toks)
		    else scanTok(j, j+1, toks)
		  else toks
	  in
	    checkArg "tokenize[p]" (s, i);
	    rev (scanTok (0, 0, []))
	  end
    fun tokenize seps = tokenizep (inStr seps)

  (* find the leftmost occurance of s2 in s1[i..].
   * Raise the exception NotFound if it doesn't exist.
   *)
    fun findstr (s1, i, "") = (checkArg "findstr" (s1, i);  i)
      | findstr (s1, i, s2) = let
	  val _ = checkArg "findstr" (s1, i)
	  val n2 = size s2
	  val stop1 = size s1 - n2
	  val c1 = ord s2
	  fun match (i1, i2) = (i2 = n2)
		orelse ((ordof(s1, i1) = ordof(s2, i2)) andalso match(i1+1, i2+1))
	  fun scan i1 = if (i1 > stop1)
		  then raise NotFound
		else if ((ordof(s1, i1) = c1) andalso match(i1+1, 1))
		  then i1
		  else scan(i1+1)
	  in
	    scan i
	  end

  (* find the rightmost occurance of s2 in s1[i..].
   * Raise the exception NotFound if it doesn't exist.
   *)
    fun revfindstr (s1, i, "") = (checkArg "findstr" (s1, i);  i)
      | revfindstr (s1, i, s2) = let
	  val _ = checkArg "revfindstr" (s1, i)
	  val n2 = size s2
	  val stop1 = i
	  val c1 = ord s2
	  fun match (i1, i2) = (i2 = n2)
		orelse ((ordof(s1, i1) = ordof(s2, i2)) andalso match(i1+1, i2+1))
	  fun scan i1 = if (i1 < stop1)
		  then raise NotFound
		else if ((ordof(s1, i1) = c1) andalso match(i1+1, 1))
		  then i1
		  else scan(i1-1)
	  in
	    scan (size s1 - n2)
	  end

  (* lexically compare two strings and return their relation *)
    fun strcmp (s1, s2) = (case (size s1, size s2)
	   of (0, 0) => LibBase.Equal
	    | (0, _) => LibBase.Less
            | (_, 0) => LibBase.Greater
            | (n1, n2) => let
		fun loop i = let
		      val c1 = ordof(s1, i) and c2 = ordof(s2, i)
                      in
                         if (c1 = c2)
                           then loop(i+1)
                         else if (c1 < c2)
                           then LibBase.Less
                           else LibBase.Greater
                      end
                in
                  (loop 0) handle _ => (
                     if (n1 = n2)
                       then LibBase.Equal
                     else if (n1 < n2)
                       then LibBase.Less
                       else LibBase.Greater)
		end (* strcmp *)
	    (* end case *))

  (* isPrefix(s1, s2, i) returns true, if s1 is a prefix of s2[i..]. *)
    fun isPrefix ("", _, _) = true
      | isPrefix (s1, s2, i) = let
	  fun loop j = (ordof(s1, j) = ordof(s2, i+j)) andalso loop(j+1)
	  in
	    (((size s1 + i) <= (size s2)) andalso (loop 0)) handle _ => true
	  end

  (* prefixCmp(s1, s2) = (t, t)  if s1 = s2
   *                   = (t, f)  if s1 is a prefix of s2
   *                   = (f, t)  if s2 is a prefix of s1
   *                   = (f, f)  otherwise
   *)
    fun prefixCmp (s1, i1, s2, i2) = let
          val n1 = size s1 and n2 = size s2
          in
            if (n1 = n2)
              then if (s1 = s2)
                then (true, true)
                else (false, false)
            else if (n1 < n2)
              then (isPrefix(s1, s2, 0), false)
              else (false, isPrefix(s2, s1, 0))
          end

  (* compare two strings, returning the indecies of the
   * first characters at which they disagree.
   *)
    fun unequalAt (s1, i1, s2, i2) = let
	  fun f (j1, j2) = if (ordof(s1, j1) = ordof(s2, j2))
		then f(j1+1, j2+1)
		else (j1, j2)
	  in
	    (f (i1, i2))
	      handle _ => let
		val n1 = size s1 and n2 = size s2
		in
		  if ((i1 < 0) orelse (n1 <= i1) orelse (i2 < 0) orelse (n2 <= i2))
		    then badArg("unequalAt", "subscript")
		  else let
		    val k1 = n1-i1 and k2 = n2-i2
		    in
		      if (k1 < k2)
			then (i1+k1, i2+k1)
			else (i1+k2, i1+k2)
		    end
		end
	  end

    fun suffix (s, i) = (substring(s, i, (size s)-i)
	    handle _ => if (i < 0) then badArg("suffix", "subscript") else "")

  (* stringTrans (s1, s2) returns a translation function that maps
   * each character in s1 to the corresponding character in s2.
   *)
    fun stringTrans (s1, s2) = let
	  val ba = ByteArray.array(255,0)
	  fun f i = (ByteArray.update(ba, i, i); f(i+1))
	  val n = size s1
	  fun g i = if (i < n)
		then let
		  val from = ordof(s1, i) and to = ordof(s2, i)
		  in
		    ByteArray.update(ba, from, to);
		    g (i+1)
		  end
		else ()
	  fun trans c = chr(ByteArray.sub(ba, ord c))
	  in
	    (f 0) handle _ => ();
	    (g 0) handle _ => badArg("stringTrans", "string map mismatch");
	    fn s => implode(map trans (explode s))
	  end

  (* stringMap f == (fn s => implode(map (f o ord) (explode s))) *)
    fun stringMap transFn s = implode(map (transFn o ord) (explode s))

  (* compress ML-style escape sequences to single characters. *)
    fun compressStr s = let
	  val n = size s
	  fun substr (i, j, l) = if (i < j)
		then substring(s, i, j-i) :: l
		else l
	  fun cvtEscape i = ((case ordof(s, i)
		 of (* "\"" *) 34 => ("\"", i+1)
		  | (* "\\" *) 92 => ("\\", i+1)
		  | (* "n" *) 110 => ("\n", i+1)
		  | (* "t" *) 116 => ("\t", i+1)
		  | c1 => let (* should be "\ddd" *)
		      val c2 = ordof(s, i+1)
		      val c3 = ordof(s, i+2)
		      in
			if ((CType.isDigitOrd c1)
			andalso (CType.isDigitOrd c2)
			andalso (CType.isDigitOrd c3))
			  then (chr((100*c1+10*c2+c3)-5328), i+3)
			    (* 5328 = 111 * ord "0" *)
			  else badArg("compressStr", "bad escape sequence")
		      end
		(* end case *))
		  handle _ => badArg("compressStr", "bad escape sequence"))
	  fun cvt (i, j, l) = if (j < n)
		  then if (ordof(s, j) <> (* "\\" *)92)
		    then cvt (i, j+1, l)
		    else let
		      val (c, k) = cvtEscape (j+1)
		      in
			cvt (k, k, c :: substr(i, j, l))
		      end
		else if (i = 0)
		  then s
		  else implode (rev (substr(i, j, l)))
	  in
	    cvt (0, 0, [])
	  end (* expandStr *)

  (* expand non-printing characters to their escape sequences. *)
    local
    (* all printing characters except "\\" and "\"" *)
      val notEscaped = CharSet.inSet (CharSet.mkCharSet "\
	    \ !#$%&'()*+,-./0123456789:;<=>?\
            \@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_\
            \`abcdefghijklmnopqrstuvwxyz{|}~\
	    \")
    in
    fun expandStr s = let
	  val n = size s
	  fun substr (i, j, l) = if (i < j)
		then substring(s, i, j-i) :: l
		else l
	  fun cvtChar 9 = "\\t"
	    | cvtChar 10 = "\\n"
	    | cvtChar 34 = "\\\""
	    | cvtChar 92 = "\\\\"
	    | cvtChar c = if (c < 10)
		  then "\\00" ^ (Integer.makestring c)
		else if (c < 100)
		  then "\\0" ^ (Integer.makestring c)
		  else "\\" ^ (Integer.makestring c)
	  fun cvt (i, j, l) = if (j < n)
		  then if (notEscaped (s, j))
		    then cvt (i, j+1, l)
		    else cvt (j+1, j+1, (cvtChar(ordof(s, j))) :: substr(i, j, l))
		else if (i = 0)
		  then s
		  else implode (rev (substr(i, j, l)))
	  in
	    cvt (0, 0, [])
	  end (* expandStr *)
    end

  end (* StringUtil *)
