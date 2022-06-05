(* list-format.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 *)

structure ListFormat : LIST_FORMAT =
  struct

  (* given an initial string (init), a separator (sep), a terminating
   * string (final), and an item formating function (fmt), return a list
   * formatting function.  The list ``[a, b, ..., c]'' gets formated as
   * ``init ^ (fmt a) ^ sep ^ (fmt b) ^ sep ^ ... ^ sep ^ (fmt c) ^ final.''
   *)
    fun formatList {init, sep, final, fmt} = let
	  fun format [] = init ^ final
	    | format [x] = implode[init, fmt x, final]
	    | format (x::r) = let
		fun f ([], l) = implode(rev(final::l))
		  | f (x::r, l) = f (r, (fmt x) :: sep :: l)
		in
		  f (r, [fmt x, init])
		end
	  in
	    format
	  end (* formatList *)

    exception ScanList of int

  (* given an expected initial string, a separator, a terminating
   * string, and an item scanning function, return a function that
   * scans a string for a list of items.  Whitespace is ignored.
   * If the input string has the incorrect syntax, then the exception
   * ScanList is raised with the position of the first error.
   *)
    fun scanList {init, sep, final, scan = scanItem} (s, startPos) = let
	  fun skipWS i = if (CType.isSpace(s, i)) then skipWS(i+1) else i
	  fun eat "" = (fn i => (true, skipWS i))
	    | eat s' = let
		val n = size s'
		fun eat' i = let val i = skipWS i
		      in
			if (StringUtil.isPrefix(s', s, i))
			  then (true, i + n)
			  else (false, i)
		      end
		in
		  eat'
		end
	  val isInit = eat init
	  val isSep = eat sep
	  val isFinal = eat final
	  fun scan (i, l) = (case (isSep i)
		 of (true, i) => let val (x, i) = scanItem(s, i)
		      in
			scan (i, x::l)
		      end
		  | (false, i) => (case (isFinal i)
		       of (true, i) => (rev l, i)
			| (false, i) => raise (ScanList i)
		      (* end case *))
		(* end case *))
	  in
	    case (isInit startPos)
	     of (true, i) => (case (isFinal i)
		   of (true, i) => ([], i)
		    | (false, i) => let val (x, i) = scanItem(s, i)
			in
			  scan (i, [x])
			end
		  (* end case *))
	      | (false, i) => raise (ScanList i)
	    (* end case *)
	  end (* scanList *)

  end; (* ListFormat *)
