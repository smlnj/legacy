(* Basics/source/sourcemap.sml 
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* [DBM, 2022.09] Reversion to single-file (or interactive stream) model. Thus no
 * no "#line" directives, and no "resynchronization".
 *
 * [DBM, 2025.03.27] Adopting this simplified sourcemap implementation for
 * branch legacy[dbm2]. But note that Source depends on (imports) SourceMap,
 * not the other way around. In fact, SourceMap imports no modules at all, and
 * hence is a minimal point in the module dependency graph.  In Ramsey's version,
 * a compilation unit could consist of text collected from several different files,
 * so a source map had to also keep track of such files, but this is no longer 
 * the case and there is a fixed file currenly being compiled, so sourceMap does
 * not contain information about the current file (as compilation unit).
 *
 * Note that we could have an analogue of a sourceMap for interactive input from
 * TextIO.stdIn. This would presumably construct a history of lines input during a
 * an interactive "session", and could support some functionality for accessing this
 * history (e.g. to repeat an interactive "command"). But note that interactive
 * "compilation units" can consist of more than one input line, so such functionality
 * would not be entirely independent of the content of the recorded lines.
 * 
 * [DBM, 2025.03.31] Definitions of types charpos, location, region have been
 * moved to a new ScourceLoc structure (in Basics/source/sourceloc.sml). SourceMap
 * and Source now both depend of SourceLoc and Source also depends on SourceMap.
 * SourceMap has been further simplified; some unnecessary functions have been
 * removed from SourceMap and Source. 
 * Now have to update ErrorMsg accordingly.
 *)

(* Note [DBM, 2025.03.31]: Some Edge cases. 
   A compilation unit may consist of a partial line. For instance, consider

     - 3+true; 4*false;<nl>

   This line contains two compilation units (expressions terminated by semicolons.
   When compiling the first compilation unit, "3+true;" only part of the first line
   has been read (Is this true?). A type error is reported based on positions in the
   first part of this line, and the rest of the line appears to be discarded.
   Question: How are lines input by the lexer?  Does it input complete lines, or character
   by character, or as many characters as required to fill a predefined buffer size, like
   4kb?  Is there a buffer containing the current line, or does the input
   buffer contain some number of characters (4k, for example)?  What happens to
   buffered input characters when a front-end error occurs?  How much input is lost?
   Is the buffering internal to in the instream value.

   This issue may differ depending on whether input is from a file or interactive, from
   the stdIn instream.  In the interactive case, characters are probably only available after 
   a newline has been typed, so there is probably only one line of "lookahead" characters
   available. Input will finish, and compilation begin, at the first top-level semicolon.

      - 1+3; 2*4;
      val it = 4 : int
      val it = 8 : int
      - 

   If there is more than one compilation unit on a line, in the absence of errors both
   will be compiled and executed.  The pair of compilation units are not treated as an
   implicit sequence expression (i.e. not equivalent to "(1+3; 2* 4);".
*)

structure SourceMap :> SOURCE_MAP =
struct

local

  structure SL = SourceLoc (* <- ltypes charpos, location, region *)

  (* compiler bug errors; FilePos precedes ErrorMsg, so cannot use ErrorMsg.impossible *)
  exception SOURCEMAP_ERROR of string
  fun bug msg = raise SOURCEMAP_ERROR msg

in 
 
  (* types ------------------------------- *)

  type smap = {lines: SL.charpos list, count: int}
    (* INVARIANT: {lines, count}: smap
         (1) not (null lines)
	 (2) count > 0  (we assume a source in non-empty, so there is at least one line)
	 (3) count = length lines  ((2) is a consequence of (1) and (3))
       lines: a list of the charpos of the first character of each line,
         in reverse of the order that the lines were input (so the first element
         of lines is the initial charpos of the last line input, and the last element
         of lines is 1.);
       count: the number of lines in the smap (number of complete lines input so far.
    *)

  type sourcemap = smap ref

(* A basic source map (type smap), records the initial character position (charpos) for
   each line (the column of the first character of the line) along with the number of
   lines (that is the length of the list of initial positions) to support translation from
   file charpos to the line-column coordinate system. The map is constructed
   incrementally as the lexer consumes a file (it is modified by the newline function
   defined below) so we define a sourcemap to be a ref to an smap so that it can be
   mutated by newline.
    
   The charpos components of successive lines are in decreasing order, i.e. each new
   line is added to the front of the list. The last elementin the smap list is
   always 1, representing the first charpos of the first line in the stream.

   The representation satisfies these invariants:

     * The line list is never empty (initialized to [1], and thereafter elements (lines)
       are only added).

     * Initial positions are strictly decreasing in the line list, even for "empty"
       lines, which have length at least 1 because we count the newline char as
       a character (the last character) in each line (except, perhaps, the last line,
       if it is incomplete).

   NOTE: If there is an empty line, the initial charpos for that line will be the
   charpos of the newline character that terminates that empty line.  Such a charpos
   should never occur in a region.  But regions can contain newline characters, i.e.,
   can span multiple lines.

   NOTE: the source map does not store the _contents_ of the lines, only the starting character
   positions of the lines. Thus it can be viewed as a partial or incomplete history of the
   input lines. If we captured the contents of each line, this could serve as a full history
   for interactive input, for instance. But for file input, it would be simpler to just input
   the full contents of the file. SML source files, even when machine generated, will normally
   be at most a few thousand characters (a few kilobytes, e.g. 4.5kb for ml.grm.sml). For
   human-written source files, the sizes are usually less than 2k and could be made smaller
   by stripping out comments (as is done by the lexer anyway).

   The !sourcemap for a file will grow as the file is "consumed" by the compiler, in particular
   by the lexer. So the sourcemap is not fixed, but is built incrementally. Thus the
   source contains a sourcemap ref.
*)

  (* newSourcemap : unit -> sourcemap *)
  fun newSourcemap () = ref {lines = [1], count = 1}
			     
  (* charposToLocation : [pos:]charpos *  sourcemap -> location
   * REQUIRE: pos >= 1 (and char[pos] not newline?)
   * We strip lines from the !sourcemap lines list until the charpos occurs in the
   * first line of the remainder, while keeping count of how many lines have been
   * stripped to determine the line number the line containing the pos.
   * Does it matter if the pos is the charpos of a newline terminating a line? *)
  fun charposToLocation (pos: SL.charpos, sourcemap: sourcemap) : SL.location =
      let val {lines, count} = !sourcemap
	  fun strip (lines as pos' :: lines', count') : int * int =
              if pos < pos'
	      then strip (lines', count'-1) (* pos comes before this head line *)
              else (count', pos - pos' + 1)  (* pos is in this first line of lines *)
	    | strip _ = bug "uptoPos"
	  val (line', column') = strip (lines, count)
       in {line = line', column = column'}
      end

  (* regionToLocations : sourcemap * SL.region -> (SL.location * SL.location) option
   * this could be made more efficient by sweeping smap while looking for both hi and lo
   * limits of a region *)
  fun regionToLocations (SL.REGION (lo, hi), smap: sourcemap) : (SL.location * SL.location) option =
        SOME (charposToLocation (lo, smap), charposToLocation (hi, smap))
    | regionToLocations (NULLregion, _) = NONE

(* functions called in the lexer files: Parse/lex/ ml.lex.sml, sml.lex.sml, user.sml,
 *   and Parse/main/ (s)ml-parser.sml  *)

  (* regionNewlineCount : sourcemap * SL.region -> int
   * determines the number of newlines occurring within a region,
   * which may be 0 for a region that lies within a single line.
   * Also, by convention, returns 0 for NULLregion *)
  fun newlineCount (smap: sourcemap, region: SL.region) =
      (case regionToLocations (region, smap)
	 of SOME ({line=lo_line, ...}, {line=hi_line,...}) =>
	      hi_line - lo_line  (* hi_line and lo_line may be equal *)
	  | NONE => 0)

  (* lastLinePos : sourcemap -> SL.charpos
   * exported and used in Parse/lex/user.sml and in Parse/main/ (s)ml-parse.sml *)
  fun lastLinePos (sourcemap: sourcemap) : SL.charpos =
      let val {lines, ...} = !sourcemap
       in case lines
            of (pos::_) => pos
             | nil => bug "lastLineStart: empty sourcemap" (* sourcemap invariant (1) violated *)
      end

  (* newline : sourcemap ->SL.charpos -> unit
   * pos should be the position of the newline character (from the lexer's yypos),
   * the next line starts with the succeeding position, pos+1, skipping over the
   * newline char. *)
  fun newline (sm: sourcemap) (pos: SL.charpos) =
      let val {lines, count} = !sm
       in sm := {lines = pos+1::lines, count = count+1}
      end

  (* resynch : sourcemap -> SL.charpos * SL.charpos * int * int * string option -> unit
   * This now does nothing.  It was used to process the obsolete #line directive
   * in the lexer. *)
  fun resynch (sm: sourcemap) (initpos, newpos, line, column, fileNameOp) = ()

end (* top local *)
end (* structure SourceMap *)

(* How much of this interface is actually used? *)
