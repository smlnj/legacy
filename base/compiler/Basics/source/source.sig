(* Basics/source/source.sig
 * COPYRIGHT (c) 1996 Bell Laboratories.
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (www.smlnj.org).
 *)

(* [DBM, 2025.03.26] Simplified. See source.sml for the implementation.] *)

signature SOURCE =
sig

  type source   (* abstract! *)

  val newSource : (string option) -> source
  (* string option is an optional file name (path?). If none, the input
   * instream defaults to TextIO.stdIn. *)

  val closeSource: source -> unit

  val sourcemap: source -> SourceMap.sourcemap

  val interactive : source -> bool

  val name: source -> string

  val instream : source -> TextIO.instream

  val getContent : source -> string option

end (* signature SOURCE *)

(* [DBM: 2025.03.31]
The former files sourcemap.sml and source.sml have been substantially rewritten into
three files: Basics/source/{sourceloc.sml, sourcemap.sml, and source.sml}
with corresponding signatures (sourceloc.sig not written yet).

The sourceloc.sml file defines the SourceLoc structure that contains the
types charpos, location, and region.  SourceMap (in sourcemap.sml) defines
a limited history structure that is used to translate charpos positions into
line.column locations in the input stream.  The Source structure (source.sml)
defines the input source type, dealing with the differences and common
functionalities of file and interactive input sources.

getContent returns the contents of a source, and only works if the source
is derived from a file.  Currently there is no mechanism for capturing the full
text history for an interactive source.

The former anyErrors and errConsumer parts of a source (formerly inputSource) have
been droped and are now to be found in the CompInfo structure.
*)

