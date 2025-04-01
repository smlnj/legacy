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
      let val kind =
	      case fileNameOp
                of NONE => STDIN  (* interactive source *)
		 | SOME f => 
		     FILE {fileName = f,  (* NONE => interactive source (stdIn) *)
			   content = ref NONE}
       in {sourceMap = SM.newSourcemap (),
	   instream = TextIO.openIn file,
	   kind = kind}
      end

  (* close : source -> unit *)
  fun closeSource ({kind, ...}): source = ()
      (case kind
	 of STDIN => ()
		  | FILE {instream, ...} =>
		      (TextIO.closeIn sourceStream
		       handle IO.Io _ => ()))

  (* sourcemap: source -> SM.sourcemap *)
  fun sourcemap ({map, ...}: source) : SM.sourcemap = map

  (* instream: source -> TextIO.instream *)
  fun instream ({instream, ...}: source) : SM.sourcemap = instream
							  
  (* name: source -> string *)
  fun name ({kind, ...}: source) : string =
      (case kind
	 of STDIN => "stdIn"
	  | FILE {fileName, ...} => fileName)

  (* interactive: source -> bool *)
  fun interactive ({kind, ...}: source) : string =
      (case kind
	 of STDIN => true
	  | FILE {fileName, ...} => false)


  (* getContent: inputSource -> string option *)
  fun getContent ({kind, ...}: source) : string option =
      case kind
        of STDIN => NONE
	| FILE{name, content, ...} => 
	    case !content
              of NONE =>
		   let val cOp = SOME (TextIO.inputAll(TextIO.openIn name))
		    in content := cOp;
		       cOp
		   end handle IO.Io _ => NONE)  (* couldn't read the file. Error? *)

  (* filepos: source -> SL.charpos -> SL.location *)
  (* should not be used: call SM.charposToLocation directly instead. *)
  fun filepos ({map, ...} : source) (pos: SL.charpos) : SL.location =
        SM.charposToLocation (map, pos)

end (* structure Source *)
