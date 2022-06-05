(* bitmap-io-func.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This module provides code to read and write depth-1 images
 * stored in X11 bitmap file format (see XReadBitmapFile(3X).
 *
 * This structure is functorized over the IO module, so that it
 * can be used both as part of an eXene application (using CIO),
 * and as a standalone SML application (using IO).
 *
 * NOTE: this should be cleaned up once the new basis is established!!!
 *)

functor BitmapIOFunc (IO : IO) : BITMAP_IO =
  struct

    structure IO = IO

    structure EXB = EXeneBase
    structure G = Geometry

    exception BitmapFileInvalid

    local
      fun scan f s = (f s) handle _ => []
      val scanDefine = scan (Format.scan "#define %s %d")
      val scanUChar = scan (Format.scan "static unsigned char %s = {")
      val scanChar = scan (Format.scan "static char %s = {")
    in
    datatype line = SKIP | DEFINE of (string * int) | BEGIN of string
    fun scanString s = (case (scanDefine s)
	   of [Format.STR s, Format.INT n] => DEFINE(s, n)
	    | _ => (case (scanUChar s)
		 of [Format.STR s] => BEGIN s
		  | _ => (case (scanChar s)
		       of [Format.STR s] => BEGIN s
			| _ => SKIP
		      (* end case *))
		(* end case *))
	  (* end case *))
    end

    val delimChars = CharSet.mkCharSet " \t\n,}"
    val isDelim = CharSet.inSet delimChars

    val xtoi = StringCvt.strToInt StringCvt.Hex

  (* return true if s1 is a suffix of s2 *)
    fun isSuffix (s1, s2) = let
	  val n1 = size s1 and n2 = size s2
	  in
	    (n1 <= n2) andalso StringUtil.isPrefix(s1, s2, n2 - n1)
	  end

    fun readBitmap inStrm = let
	  fun inputLine () = IO.input_line inStrm
	  fun setWid ({wid, ht, x_hot, y_hot}, w) =
		{wid=SOME w, ht=ht, x_hot=x_hot, y_hot=y_hot}
	  fun setHt ({wid, ht, x_hot, y_hot}, h) =
		{wid=wid, ht=SOME h, x_hot=x_hot, y_hot=y_hot}
	  fun setXHot ({wid, ht, x_hot, y_hot}, x) =
		{wid=wid, ht=ht, x_hot=SOME x, y_hot=y_hot}
	  fun setYHot ({wid, ht, x_hot, y_hot}, y) =
		{wid=wid, ht=ht, x_hot=x_hot, y_hot=SOME y}
	  fun scanHdr (arg as {wid, ht, x_hot, y_hot}) = (
		case (scanString (inputLine ()))
		 of SKIP => scanHdr arg
		  | (DEFINE("width", n)) => scanHdr(setWid(arg, n))
		  | (DEFINE("height", n)) => scanHdr(setHt(arg, n))
		  | (DEFINE("x_hot", n)) => scanHdr(setXHot(arg, n))
		  | (DEFINE("y_hot", n)) => scanHdr(setYHot(arg, n))
		  | (DEFINE(s, n)) =>
		      if isSuffix("_width", s)
			then scanHdr(setWid(arg, n))
		      else if isSuffix("_height", s)
			then scanHdr(setHt(arg, n))
		      else if isSuffix("_x_hot", s)
			then scanHdr(setXHot(arg, n))
		      else if isSuffix("_y_hot", s)
			then scanHdr(setYHot(arg, n))
			else scanHdr arg
		  | (BEGIN s) => arg
		(* end case *))
	  fun getNextInt (s, i) = let
		fun skipDelim ("", _) = raise BitmapFileInvalid
		  | skipDelim (s, j) = if (j < size s)
		        then if isDelim(s, j)
			  then skipDelim(s, j+1)
			  else (s, j)
		      else skipDelim (inputLine(), 0)
		val (s, i) = skipDelim(s, i)
		val (n, i) = xtoi (s, i)
		in
		  (n, s, i)
		end
	  val (wid, ht, hot) = (
		case (scanHdr{wid=NONE, ht=NONE, x_hot=NONE, y_hot=NONE})
		 of {wid=NONE, ...} => raise BitmapFileInvalid
		  | {ht=NONE, ...} => raise BitmapFileInvalid
		  | {wid=SOME w, ht=SOME h, x_hot=SOME x, y_hot=SOME y} =>
		      (w, h, SOME(G.PT{x=x, y=y}))
		  | {wid=SOME w, ht=SOME h, ...} => (w, h, NONE)
		(* end case *))
	  val bytesPerLine = (wid+7) div 8
	  fun getScanLine (line, i) = let
		val scanLn = System.Unsafe.create_s bytesPerLine
		fun get (line, i, k) = if (k < bytesPerLine)
		      then let
			val (byte, line, i) = getNextInt(line, i)
			in
			  System.Unsafe.store (scanLn, k, Char.chr byte);
			  get(line, i, k+1)
			end
		      else (scanLn, line, i)
		in
		  get (line, i, 0)
		end (* getScanLine *)
	  fun getData (line, _, 0, l) = [rev l]
	    | getData (line, i, n, l) = let
		val (scanLn, line, i) = getScanLine(line, i)
		in
		  getData(line, i, n-1, scanLn::l)
		end
	  in
	    { image = EXB.IMAGE{
		  sz = G.SIZE{wid=wid, ht=ht},
		  data = getData(inputLine (), 0, ht, [])
		},
	      hot_spot = hot
	    }
	  end

    val formatDefine = Format.format "#define %s%s %d\n"
    val formatUChar = Format.format "static unsigned char %sbits[] = {\n"
    val formatByte = Format.format "%#04x"

    exception NotBitmap
    exception BadImageData = EXB.BadImageData

    fun writeBitmap (outStrm, name, {image, hot_spot}) = let
	  val name = (case name of "" => "" | _ => name ^ "_")
	  val pr = IO.outputc outStrm
	  fun writeDefine (s, n) =
		pr(formatDefine [Format.STR name, Format.STR s, Format.INT n])
	  val (wid, ht, data) = (case image
		 of (EXB.IMAGE{sz=G.SIZE{wid, ht}, data=[data]}) => (wid, ht, data)
		  | _ => raise NotBitmap
		(* end case *))
	  fun prData () = let
		val bytesPerLine = (wid + 7) div 8
		fun nextByte (s, r, i) = if (i < bytesPerLine)
		      then (Char.ord(String.sub(s, i)), (s, r, i+1))
		      else nextLine r
		and nextLine [] = raise BadImageData
		  | nextLine (s::r) = if (size s = bytesPerLine)
		      then nextByte(s, r, 0)
		      else raise BadImageData
		fun prLine (0, _, _) = ()
		  | prLine (n, 12, data) = (pr ",\n"; prLine(n, 0, data))
		  | prLine (n, k, data) = let
		      val (byte, data) = nextByte data
		      in
			if (k = 0) then pr "    " else pr ", ";
			pr(formatByte [Format.INT byte]);
			prLine (n-1, k+1, data)
		      end
		in
		  if (length data = ht)
		    then prLine(ht*bytesPerLine, 0, ("", data, bytesPerLine))
		    else raise BadImageData
		end
	  in
	    writeDefine ("height", ht);
	    writeDefine ("width", wid);
	    case hot_spot
	     of (SOME(G.PT{x, y})) => (
		  writeDefine ("x_hot", x);
		  writeDefine ("y_hot", y))
	      | _ => ()
	    (* end case *);
	    pr(formatUChar [Format.STR name]);
	    prData ();
	    pr "\n};\n";
	    IO.flush_out outStrm
	  end

  end; (* functor BitmapIOFunc *)

