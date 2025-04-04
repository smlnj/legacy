(* Basics/source/source.sml
 *
 * COPYRIGHT (c) 2012, 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* [DBM, 2025.03.26] Simplified inputSource, clarifying differences and commonalities
 * of the two kinds of input sources, file and interactive.
 * An inputSource is interactive if its kind = STDIN.
 * Removed anyErrors and errconsumer, moving them to CompInfo.
 *)

structure Source :> SOURCE =
struct

  structure SL = SourceLoc
  structure SM = SourceMap

  datatype sourceKind
    = FILE of 
        {name: string, (* file name or path? Assumed to be a suitalbe
			* argument for TextIO.openIn. *)
	 content: string option ref} (* initially NONE, SOME after inputing whole file *)
    | STDIN  (* input instream is assumed to be TextIO.stdIn *)
        (* We could capture interactive input history in some form (such as a
	 * list of lines?). In fact, we capture a partial history in a sourcemap. *)


 (* both file and interactive sources (kind STDIN) have an instream
  * and a sourcemap The sourcemap for both kinds is constructed by the
  * SourceMap.newline function as it is called during lexing. *)

   type source =
        {instream: TextIO.instream, (* the input stream supplying the source text *)
	 map: SM.sourcemap, (* sourcemap is incrementally created while lexing the instream *)
	 kind: sourceKind} 

  (* newSource : string option -> source *) 
  fun newSource (fileNameOp: string option) : source =
      let val (kind, instream) =
	      case fileNameOp
                of NONE => (STDIN, TextIO.stdIn)  (* interactive source *)
		 | SOME file => (FILE {name = file, content = ref NONE}, TextIO.openIn file)
       in {map = SM.newSourcemap (),
	   instream = instream,
	   kind = kind}
      end

  (* close : source -> unit *)
  fun closeSource ({kind, instream, ...}: source): unit =
      (case kind
	 of STDIN => ()  (* we won't close stdIn *)
	  | FILE _ =>
	      (TextIO.closeIn instream
	       handle IO.Io _ => ()))

  (* sourcemap: source -> SM.sourcemap *)
  fun sourcemap ({map, ...}: source) : SM.sourcemap = map

  (* instream: source -> TextIO.instream *)
  fun instream ({instream=strm, ...}: source) : TextIO.instream = strm
							  
  (* name: source -> string *)
  fun name ({kind, ...}: source) : string =
      (case kind
	 of STDIN => "stdIn"
	  | FILE {name, ...} => name)

  (* interactive: source -> bool *)
  fun interactive ({kind = STDIN, ...}: source) : bool = true
    | interactive _ = false

  (* getContent: inputSource -> string option *)
  fun getContent ({kind, ...}: source) : string option =
      (case kind
         of STDIN => NONE  (* we don't collect the "content" of the stdIn instream *)
	  | FILE{name, content} => 
	      (case !content
                 of NONE =>
		      let val contentOp = SOME (TextIO.inputAll(TextIO.openIn name))
		       in content := contentOp;
			  contentOp
		      end handle IO.Io _ => NONE))
                      (* couldn't read the file: Return NONE, 
			 leave value of !content unchanged,
			 report Error? *)

end (* structure Source *)
