(* source.sml
 *
 * COPYRIGHT (c) 2012, 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* [DBM, 2025.03.26] Simplified inputSource.
 * inputSource is interactive if fileOp = NONE
 * removed anyErrors and errconsumer (could put these in either CompInfo or ErrorMsg) *)

structure Source : SOURCE =
struct

  structure SM = SourceMap

  type inputSource =
     {fileOp: string option,
      sourceMap: SM.sourcemap option,
      sourceStream: TextIO.instream,
      content: string option ref}

  (* newSource : string option -> inputSource *) 
  fun newSource (fileOp: string option) =
      {fileOp = fileNameOp,  (* NONE => interactive source (stdIn) *)
       sourceMap =
	 (case fileOp
	    of NONE => NONE
	    | SOME file => SM.newSourceMap file)
       sourceStream = 
         (case fileOp
	   of NONE => TextIO.stdIn
	    | SOME file => TextIO.openIn file)
       content=ref NONE}  (* remains NONE if interactive *)

  (* closeSource : inputStource -> unit *)
  fun closeSource ({fileOp, sourceStream, ...} : inputSource) =
      (case fileOp
	of NONE => ()
	 | SOME file =>
	    (TextIO.closeIn sourceStream
	     handle IO.Io _ => ()))

  (* filepos: inputSource -> SM.charpos -> SM.sourceloc *)
  fun filepos ({sourceMap,...}: inputSource) pos = SM.filepos sourceMap pos

  (* getContent: inputSource -> string option *)
  fun getContent ({fileOpened,interactive,content,...}: inputSource) : string option =
      case !content
        of NONE =>
           (case fileOp
	      of NONE => NONE (* could we capture history of interactive input? *)
	       | SOME file => 
		   let val s = TextIO.inputAll(TextIO.openIn fileOpened)
		    in content := SOME s;
		       !content
		   end handle IO.Io _ => NONE)

  (* regionContent:
          inputSource * SM.region
       -> (SM.region * string * int) option *)
  fun regionContent (src as {sourceMap,...}: inputSource, region) =
      case getContent src
        of NONE => NONE
         | SOME content =>
           let val wideregion as (lo,hi) = SM.widenToLines sourceMap region
	       val content = substring(content, lo-1, hi-lo)
	       val ({line,...},_)::_ = SM.fileregion sourceMap wideregion
	    in SOME(content, wideregion, line)
	   end

end (* structure Source *)
