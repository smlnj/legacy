(* dump-output.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Dump (to stdout) the complete DFA
 *)

structure DumpOutput : OUTPUT = 
  struct

    structure RE = RegExp
    structure LO = LexOutputSpec

    fun nameOf (LO.State{id, ...}) = "Q" ^ Int.toString id

    fun prState (s as LO.State{id, label, final, next, ...}) = let
          val name = (case final
		       of [] => nameOf s
			| id::_ => concat[nameOf s, " (act ", Int.toString id, ")"]
		      (* end case *))
	  fun prEdge (symSet, st) = print(concat[
		  "  -- ", RE.toString (RE.mkSymSet symSet), " --> ", nameOf st, "\n"
		])
	  fun prRE re = print (concat[" ", RE.toString re, "\n"])
          in
            print(concat[name, ": "(*, RE.toString label*), "\n"]);
	    Vector.app prRE label;
	    List.app prEdge (!next);
	    print "\n"
          end

    fun dumpDFA states = 
	  (List.app prState states;
	   print (Int.toString (List.length states));
	   print " states\n\n")


    fun outSS (label, ss) =
	        (print "Start state: ";
		 print label; print " => "; print (nameOf ss);
		 print "\n")

    fun output (spec, _) = let
          val LO.Spec {dfa, startStates, ...} = spec
          in
            dumpDFA dfa;
	    print "\n";
	    List.app outSS startStates
	  end

  end
