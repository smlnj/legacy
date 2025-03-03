(* ElabData/basics/stamp.sml *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* Copyright 2025 by The Fellowship of SML/NJ *)
(* Re-written by Matthias Blume (3/2000) *)
(* rewritten by DBM 2025.02 *)

structure Stamp :> STAMP =
struct

  type pid = PersStamps.persstamp	(* for global stamps *)

  datatype stamp
    = Special of string
    | Global of { pid: pid, cnt: int }
    | Fresh of int

  type ord_key = stamp

  fun compare (Fresh i, Fresh i') = Int.compare (i, i')
    | compare (Fresh _, _) = GREATER
    | compare (_, Fresh _) = LESS
    | compare (Special s, Special s') = String.compare (s, s')
    | compare (Special _, _) = GREATER
    | compare (_, Special _) = LESS
    | compare (Global g, Global g') =
      (case Int.compare (#cnt g, #cnt g')
         of EQUAL => PersStamps.compare (#pid g, #pid g')
          | unequal => unequal)

  fun eq (s, s') =
      case compare (s, s')
        of EQUAL => true
	 | _ => false

  (* generator : int ref
   * a counter used to generate the ints identifyiing new fresh stamps *)
  val generator : int ref = ref 0

  (* reset : unit -> unit *)
  (* resets the counter for generating fresh stamps to 0 *)				
  fun reset () = generator := 0

  (* fresh : unit -> stamp *)
  fun fresh () = let val i = !generator in generator := i + 1; Fresh i end

  (* special: string -> stamp *)
  val special = Special

  (* global : {pid: pid, cnt: int} -> stamp *)
  (* convert a global stamp (from a separate compilation unit?) to a stamp.
   * Used only in pickling/unpickling? *)
  val global = Global

  fun isFresh (Fresh _) = true
    | isFresh _ = false


  (* printable names for stamps *)
		      
  fun toString (Fresh i) = concat ["FSTAMP(", Int.toString i, ")"]
    | toString (Global { pid, cnt }) =
      concat ["GSTAMP(", PersStamps.toHex pid, ",", Int.toString cnt, ")"]
    | toString (Special s) = concat ["SSTAMP(", s, ")"]

  fun toShortString (Fresh i) = "#F" ^ Int.toString i
    | toShortString (Special s) = "#S:" ^ s
    | toShortString (Global { pid, cnt }) =
      let fun trim3 s = substring (s, size s - 3, 3)
       in concat ["#G", trim3 (PersStamps.toHex pid), ".", Int.toString cnt]
      end

  (* the stuff below is used only in pickling (& unpickling?) --
   * It should be moved to a pickleutil.sml file or some such. *)

  local
      structure M = IntRedBlackMap
  in
      type converter = int M.map ref * int ref
      fun newConverter () = (ref M.empty, ref 0)
      fun Case _ (Special s) { fresh, global, special } = special s
        | Case _ (Global g) { global, ... } = global g
        | Case (m, n) (Fresh i) { fresh, ... } =
	    (case M.find (!m, i)
	       of SOME i' => fresh i'
		| NONE =>
		    let val i' = !n
		     in n := i' + 1; m := M.insert (!m, i, i');
			fresh i'
		    end)
  end (* local *)

end (* structure Stamp *)
