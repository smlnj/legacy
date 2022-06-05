(* random-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Random number generator.
 * Recommended by Stephen K. Park and Keith W. Miller, 
 * Random number generators: good ones are hard to find,
 * CACM 31 (1988), 1192-1201
 * Based on Paulson, pp. 170-171.
 *
 *)

signature RANDOM =
  sig

    (* MAXRAND is some maximum real defined in the structure *)

    val random : real -> real
      (* Given seed, return value 0.0 <= v < MAXRAND
       * Iteratively using the value returned by random as the
       * next seed to random will produce a sequence of pseudo-random
       * numbers.
       *)

    val mkRandom : real -> unit -> real
      (* Given seed, return function generating a sequence of
       * random numbers 0.0 <= v < MAXRAND
       *)

    val norm : real -> real
      (* r -> r / MAXRAND *)

    val range : (int * int) -> real -> int 
      (* Map v, 0.0 <= v < MAXRAND to integer range [i,j]
       * Exception -
       *   BadArg if j < i
       *)

  end (* RANDOM *)
