(* ElabData/basics/stamp.sml *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* Copyright 2025 by The Fellowship of SML/NJ *)
(* Re-written by Matthias Blume (3/2000) *)
(* rewritten by DBM 2025.02 *)

structure Stamps :> STAMPS =
struct

  type pid = PersStamps.persstamp	(* for global stamps *)

  datatype stamp
    = Special of string
    | Global of { pid: pid, cnt: int }
    | Fresh of int  (* [DBM, 2025.03.04] stick with int instead of word for compatibility *)

  type ord_key = stamp

  fun compare (Fresh i, Fresh i') = Int.compare (i, i')
    | compare (Fresh _, _) = GREATER
    | compare (_, Fresh _) = LESS
    | compare (Special s, Special s') = String.compare (s, s')
    | compare (Special _, _) = GREATER
    | compare (_, Special _) = LESS
    | compare (Global {pid=pid1, cnt=cnt1}, Global {pid=pid2, cnt=cnt2}) =
	(case Int.compare (cnt1, cnt2)
	   of EQUAL => PersStamps.compare (pid1, pid2)
	    | unequal => unequal)

  (* eq: stamp * stamp -> bool *)
  fun eq (s, s') =
      case compare (s, s')
        of EQUAL => true
	 | _ => false

  (* generator : word ref
   * a counter used to generate the ints identifyiing new fresh stamps *)
  val generator : int ref = ref 0

  (* reset : unit -> unit *)
  (* resets the counter for generating fresh stamps to 0 *)				
  fun reset () = generator := 0

  (* fresh : unit -> stamp *)
  fun fresh () = let val w = !generator in generator := w + 1; Fresh w end

  (* special: string -> stamp *)
  val special = Special

  (* global : {pid: pid, cnt: int} -> stamp *)
  (* convert a global stamp (from a separate compilation unit?) to a stamp.
   * Used only in pickling/unpickling? *)
  val global = Global

  fun isFresh (Fresh _) = true
    | isFresh _ = false


  (* printable names for stamps *)
		      
  fun toString (Fresh n) = concat ["FSTAMP(", Int.toString n, ")"]
    | toString (Global { pid, cnt }) =
      concat ["GSTAMP(", PersStamps.toHex pid, ",", Int.toString cnt, ")"]
    | toString (Special s) = concat ["SSTAMP(", s, ")"]

  fun toShortString (Fresh n) = "#F" ^ Int.toString n
    | toShortString (Special s) = "#S:" ^ s
    | toShortString (Global { pid, cnt }) =
      let fun trim3 s = substring (s, size s - 3, 3)
       in concat ["#G", trim3 (PersStamps.toHex pid), ".", Int.toString cnt]
      end

(* -------------------------------------------------------------------------------- *)

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

end (* structure Stamps :> STAMPS *)
