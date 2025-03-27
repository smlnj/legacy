(* Basics/source/source.sig
 * COPYRIGHT (c) 1996 Bell Laboratories.
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (www.smlnj.org).
 *)

(* [DBM, 2025.03.26] Simplified. See source.sml.] *)

signature SOURCE =
sig

  type inputSource =
    {file: string option,  (* NONE iff interactive *)
     sourceMap: SourceMap.sourcemap option,  (* NONE iff interactive *)
     sourceStream: TextIO.instream,  (* stdIn iff interactive *)
     content: string option ref} (* !content = NONE if interactive *)

  val newSource : (string option) -> inputSource
  (* string option is an optional file name (path?). If none, the input
   * instream defaults to TextIO.stdInd. *)

  val closeSource: inputSource -> unit

  val filepos: inputSource -> SourceMap.charpos -> SourceMap.sourceloc
  (* Simply calls SourceMap.filepos on the sourceMap component of inputSource,
   * Provided for convenience. *)

  val getContent : inputSource -> string option

  val regionContent : inputSource * SourceMap.region ->
		      (string * SourceMap.region * int) option

end (* signature SOURCE *)

(* [DBM: update this comment!]
The fileOpened field contains the name of the file that was opened to
produce a particular inputSource.  It is used to derive related
file names (for example, see CompileF.codeopt and CompileF.parse
in build/compile.sml.). It is also used when we need to access the content
of the sourcefile for error messages (getContent).  This assumes that the
current directory remains fixed if the file name is a relative path.

newSource takes as argument a file name, the corresponding instream of the
opened file, a boolean flag indicating whether the source is interactive
(i.e. stdIn), and a prettyPrint device. (Note: Formerly newSource also took
an additional int argument representing the initial line number, but this
argument was always 1, so it has been dropped). (DBM: the PrettyPrint.device
argument of newSource should also be dropped. This should be replaced by 
an error outstream stored within the ErrorMsg structure.)

getContent only works if the source is a single file (no #line directives
changing the source file), and it obiously won't work for an interactive source,
i.e., stdIn. The first exception no longer applies, since we do not support #line
directives [DBM, 2025.03.05].

[DBM: 2025.03.05]
What file name (for fileOpened) should be provided if the source is interactive, i.e., the
sourceStream is TextIO.stdIn?  Should it be "stdIn" in that case, or the empty string?

The definition of inputSource should be simplified by dropping anyErrors and errConsumer,
which are not logically part of or depending on the source. The target for outputting
error messages should be determined and maintained elsewhere, say in ErrorMsg. Usually it
would be the outstream for stdOut or stdErr.

The record of the current inputSource should be maintained in either CompInfo or ErrorMsg,
where it would be set before each compilation to the source correspondiing the the
compilation unit.

*)

