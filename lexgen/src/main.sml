(* main.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Driver for lexgen
 *)

structure Main = 
  struct

    structure RE = RegExp
    structure Lex = LexGen
    structure LO = LexOutputSpec

    fun debug s = (print s; print "\n")

  (* command-line parameters *)
    datatype options = 
	Opt of {
	    fname : string ref,
	    lexCompat : bool ref,
	    dump : bool ref,
	    dot : bool ref,
	    match : bool ref,
	    beTest : bool ref
          }

  (* count the total number of DFA states *)
    fun numStates (LO.Spec{dfa, ...}) = List.length dfa

    fun mlFlex (Opt {fname, lexCompat, dot, dump, match, beTest}) = let
          val _ = if String.size (!fname) = 0 
		  then (print "No input file specified (usage: lexgen [options] file)\n";
			OS.Process.exit OS.Process.failure)
		  else ()
	  val _ = if (!lexCompat = false) 
		  then (print "--ml-lex-mode switch must be specified\n";
			OS.Process.exit OS.Process.failure)
		  else()
	  val _ = debug "[lexgen: parsing]"
          val inSpec' = MLLexInput.parseFile (!fname)
	  val inSpec = if (!beTest) 
		       then LexSpec.emptyActions inSpec'
		       else inSpec'
	  val _ = debug "[lexgen: DFA gen]"
	  val outSpec = Lex.gen inSpec
	  val _ = (debug (concat [" ", Int.toString (numStates outSpec),
				  " states in full DFA"]))
	  val _ = if !dump then
		    (debug "[lexgen: DFA dump]";
		     DumpOutput.output (outSpec, !fname))
		  else ()
	  val _ = if !dot then
		    (debug "[lexgen: DOT gen]";
		     DotOutput.output (outSpec, !fname))
		  else ()
	  val _ = debug "[lexgen: SML gen]"
	  val _ = SMLFunOutput.output (outSpec, !fname)
	  val _ = if !match then 
		    (debug "-- Interactive matching (blank line to quit) --";
		     Match.output (outSpec, !fname))
		  else ()
	  in
            OS.Process.success
          end
	    handle ex => (
	      TextIO.output(TextIO.stdErr, concat[
		  "uncaught exception ", General.exnName ex,
		  " [", General.exnMessage ex, "]\n"
	        ]);
	      app (fn s => TextIO.output(TextIO.stdErr, concat[
		  "  raised at ", s, "\n"
	        ]))
	        (SMLofNJ.exnHistory ex);
	      OS.Process.exit OS.Process.failure)

    fun procArgs (Opt {fname, lexCompat, dot, dump, match, beTest}) arg = 
	  (case arg
	    of "--dot"    => dot := true
	     | "--dump"	  => dump := true
	     | "--match"  => match := true
	     | "--testbe" => beTest := true
	     | "--ml-lex-mode" => lexCompat := true
	     | file	  => 
	         if String.size (!fname) > 0 
		 then 
		   (print "Only one input file may be specified\n";
		    OS.Process.exit OS.Process.failure)
		 else fname := file
	   (* end case *))

    fun main (_, args) = let
          val opt = Opt {fname = ref "", lexCompat = ref false,
			 dot = ref false, dump = ref false, 
			 match = ref false, beTest = ref false}
	  val _ = List.app (procArgs opt) args
	  in 
	    mlFlex opt
          end

  end
