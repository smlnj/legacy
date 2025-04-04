(* compiler/Basics/source/sourcemap.sig
 *
 * COPYRIGHT (c) 2012, 2022, 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* [DBM, 2025.03.28] Source depends on SourceMap, in that a source record contains
 * a sourcemap (ref?), so changed so that Source is not referenced.
 *)

(* Character positions, source locations, source maps

The goal of this interface is to map character positions to locations in source files,
where a location is described in ``line-column'' format. We don't have access to a file
name here, so adding a file name to complete the location must be left to the Source
structure.  The major type exported by this interface is sourcemap, which supports such a
mapping. This way, most of a compiler can work with character positions, but we can use a
more "friendly" notation for source locations in error messages.  For a friendlier
notation for file locations, we have chosen line and column coordinates, since virtually
all editors allow one to move to a given line, where the line address is a positive
integer.

The most fundamental notion of a location in a text file is the _character position_, (or
"charpos"), which is the number of characters in the file coming before the given
position. Thus the beginning of the file corresponds to charpos 0.  A charpos coordinates
identifies a position between two characters, or before the first character, or after the
last character in a file.

The convention chosen for line-column coordinates is that the 1st line of the file is line
1, and the first column of a line is column 1.

A region represents a contiguous span of characters within a file, between two given
character positions (start-region and end-region).  In REGION (start, end), start and end
should be non-negative and should be ordered start < end.  Region (n, n) is empty
(contains no characters), but empty regions should be represented by NULLregion.

A source map is maintained as mutable state consisting of a list of line numbers with
their initial character positions, in reverse order (first line comes last).

Column numbers are obtained by counting characters (base 1) from the beginning of the
line, so the first character on the line is in column 1.  Tabs are given no special
treatment (they count as one character, that is, tabs are not implicitly expanded into
spaces).

Character positions increase as the compiler moves through the source, and the lexer
mutates the source map any time something interesting happens. The only interesting event
is:

* The lexer consumes a newline, which should increment the line number
  in the source file.

Character positions are nonnegative, and they increase in successive lines
added to a sourcemap, and the initial line of the sourcemap starts at charpos 0.

A sourcemap for a file could be precomputed by reading the entire contents of the file
into a string and then analyzing the string to construct the sourcemap. Alternatively,
as is currently the case, the sourcemap is stored in a ref which is updated at each
newline encountered by the lexer.

================================================================================
Change Log:
1. [Ramsey? 1990s?]
  - changed ErrorMsg to use SourceMap to get source locations; only the
    formatting is done internally
  - added SourceMap structure
  - [DBM: support for NoWeb literate programming system: noncontiguous regions
    across multiple files. #line directives, "resynching"]

2. [DBM, 2022.09] Revised, along with ErrorMsg:
  - convert to the new PrettyPrint library
  - remove Ramsey's NoWeb(?) machinery supporting "#line" directives
    and "resynchronization" with multiple input files.
    A source is associated with a single file, or with an interactive
    input stream like StdIn.

3. [DBM, 2025.03.29] Revised to remove dependency on Source, eliminating some
   functions that need the source or one of its components such as file: string,
   the file name or path.
   Now there are three structures with the following dependency graph:

      SourceLoc --> SourceMap --> Source
                --> Source
*)

signature SOURCE_MAP =
sig

  (* types *)
  type sourcemap  (* Abstract, represented by smap ref *)

  val newSourcemap : unit -> sourcemap

  val charposToLocation : SourceLoc.charpos * sourcemap -> SourceLoc.location

  val regionToLocations : sourcemap * SourceLoc.region
			  -> (SourceLoc.location * SourceLoc.location) option
      (* using sourcemaps to translate region to a location pair;
       * returns NONE for NULLregion *)

(* interface functions used in various lexer files *)

  val newlineCount : sourcemap * SourceLoc.region -> int
      (* Number of newlines within the region -- 0 for NULL region.
       * Used in Parse/main/(s)ml-parser.sml files. *)

  val lastLinePos : sourcemap -> SourceLoc.charpos

  val newline : sourcemap -> SourceLoc.charpos (* = int *) -> unit
  (* This adds a new line to the lines list and increments the line count,
   * mutating the sourcemap. *)

  val resynch : sourcemap -> SourceLoc.charpos * SourceLoc.charpos * int * int
                             * string option
		          -> unit
  (* Formerly used in the implementation of the #line directive. Now does nothing.
   * WARNING: there should no longer be any #line directives in SML source code!
   * Is there any remaining ancient NoWeb code?  I think not.
   *)

end (* signature SOURCE_MAP *)
