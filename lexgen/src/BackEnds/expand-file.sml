(* expand-file.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 * (Used with permission)
 *
 * Copy a template file to an output file while expanding placeholders.
 * Placeholders are denoted by @id@ on a line by themselves.
 *)

structure ExpandFile : sig

    type hook = TextIO.outstream -> unit

    val expand : {src : string list, dst : string, hooks : (string * hook) list} -> unit

  end = struct

    structure TIO = TextIO
    structure SS = Substring
(*
    structure RE = RegExpFn (
      structure P = AwkSyntax
      structure E = BackTrackEngine)
    structure M = MatchTree
*)

    type hook = TextIO.outstream -> unit

(*
    val placeholderRE = RE.compileString "[\\t ]*@([a-zA-Z][-a-zA-Z0-9_]* )@[\\t ]*"
    val prefixPlaceholder = RE.prefix placeholderRE SS.getc

    fun findPlaceholder s = (case prefixPlaceholder(SS.full s)
	   of SOME(M.Match(_, [M.Match(SOME{pos, len}, _)]), _) =>
		SOME(SS.string(SS.slice(pos, 0, SOME len)))
	    | _ => NONE
	  (* end case *))
*)

    fun findPlaceholder s = let
          val trim = SS.dropr Char.isSpace (SS.dropl Char.isSpace (SS.full s))
	  val size = SS.size trim
          in if size > 2 andalso
		SS.isPrefix "@" trim andalso
		SS.isSuffix "@" trim then
	       SOME (SS.string (SS.slice (trim, 1, SOME (size - 2))))
	     else
	       NONE
          end

  (* copy from inStrm to outStrm expanding placeholders *)
    fun copy (inStrm, outStrm, hooks) = let
	  fun lp [] = ()
	    | lp (s::ss) = (
	        case findPlaceholder s
		 of NONE => TIO.output (outStrm, s)
		  | (SOME id) => (
		      case (List.find (fn (id', h) => id = id') hooks)
		       of (SOME(_, h)) => h outStrm
			| NONE => raise Fail "bogus placeholder"
		      (* end case *))
	        (* end case *);
		lp(ss))
		
	  in
	    lp(inStrm)
	  end

    exception OpenOut

    fun expand {src, dst, hooks} = (let
	  val dstStrm = TIO.openOut dst
		handle ex => (
		  TIO.output(TIO.stdOut, concat[
		      "Warning: unable to open output file \"",
		      dst, "\"\n"
		    ]);
		  raise OpenOut)
	  fun done () = (TIO.closeOut dstStrm)
	  in
	    copy (src, dstStrm, hooks) handle ex => (done(); raise ex);
	    done()
	  end
	    handle OpenOut => ())

  end
