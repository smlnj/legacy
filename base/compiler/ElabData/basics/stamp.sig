(* ElabData/basics/stampsig *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* Copyright 2025 by The Fellowship of SML/NJ *)
(* Re-written by Matthias Blume (3/2000) *)
(* Rewritten by DBM, 2025.02 *)

signature STAMP =
sig
    type stamp
    type ord_key = stamp		(* to match signature ORD_KEY *)
    type pid = PersStamps.persstamp	(* for global stamps *)

    val eq : stamp * stamp -> bool
    val compare : stamp * stamp -> order (* instead of "cmp" (ORD_KEY) *)

    val reset : unit -> unit
    (* reset the internal counter used to generate fresh stamps *)

    val fresh : unit -> stamp
    (* generatre a new fresh stamp *)

    (* Make a new "special" stamp (for things in primEnv). *)
    val special : string -> stamp

    (* Make a "global" stamp (i.e., one that comes from a different
     * compilation unit). Used only by the unpickler. *)
    val global  : { pid: pid, cnt: int } -> stamp

    (* testing for freshness quickly... *)
    val isFresh : stamp -> bool

    (* for debugging: *)
    val toString : stamp -> string
    val toShortString : stamp -> string

    (* Case analysis on the abstract type with built-in alpha-conversion
     * for fresh stamps. Used only by the pickler. *)
    type converter
    val newConverter : unit -> converter
    val Case : converter  -> stamp ->
	       { fresh   : int -> 'a,	(* already alpha-converted *)
		 global  : { pid: pid, cnt: int } -> 'a,
		 special : string -> 'a } -> 'a

end (* signature STAMP *)
