(* user.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common user decls from the two different lexers.
 *)

structure UserDeclarations =
struct

local (* imports *)

  structure CI = CompInfo (* CI.source *)
  structure SL = SourceLoc
  structure SM = SourceMap
  structure SR = Source
  structure EM = ErrorMsg
		     
in
    type pos = int

    type arg = 
      {comLevel : int ref,
       charlist : string list ref,
       stringtype : bool ref,
       stringstart : int ref, (* start of current string or comment*)
       brack_stack : int ref list ref} (* for frags *)

    (* common code to handle EOF *)
    fun eof ({comLevel, charlist, stringstart, ...} : arg) =
	let val sourcemap = SR.sourcemap (CI.source ())
	    val pos = Int.max(!stringstart+2, SM.lastLinePos sourcemap)
	 in if !comLevel > 0
	    then (EM.errorRegion (SL.REGION (!stringstart, pos), "unclosed comment"); pos)
	    else (if null (!charlist)
	          then ()
	          else EM.errorRegion (SL.REGION (!stringstart, pos),
				      "unclosed string, character, or quotation");
	         pos)
	end

    (* support for string literals *)
    fun addString (charlist,s:string) = charlist := s :: (!charlist)
    fun addChar (charlist, c:char) = addString(charlist, String.str c)
    fun makeString charlist = (concat(rev(!charlist)) before charlist := nil)

end (* top local (imports) *)
end (* structure UserDeclarations *)
