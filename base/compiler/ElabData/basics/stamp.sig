(* ElabData/basics/stamp.sig *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* Copyright 2025 by The Fellowship of SML/NJ *)
(* Re-written by Matthias Blume (3/2000) *)
(* Rewritten by DBM, 2025.02 *)

signature STAMPS =
sig
    type stamp
    type ord_key = stamp		(* to match ORD_KEY *)
    type pid = PersStamps.persstamp	(* for global stamps *)

    val compare : stamp * stamp -> order (* to match ORD_KEY *)
    val eq : stamp * stamp -> bool

    val reset : unit -> unit
    (* reset the internal counter used to generate fresh stamps *)

    val fresh : unit -> stamp
    (* generatre a new fresh stamp *)

    val special : string -> stamp
    (* make a new "special" stamp (for things in primEnv). *)

    val global  : { pid: pid, cnt: int } -> stamp
    (* make a "global" stamp (i.e., one that comes from a different
     * compilation unit). Used only by the unpickler. *)

    (* quick test for for freshness *)
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

end (* signature STAMPS *)
