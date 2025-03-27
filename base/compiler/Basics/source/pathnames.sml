(* pathnames.sml
 *
 *   This currently doesn't do anything useful.
 *
 * Copyright (c) 2004, 2025 by The Fellowship of SML/NJ
 *)

(* [DBM, 2025.03.26] Not used, so can be delete (anticipated undefined functionality!).] *)

structure Pathnames : sig
  val trim : string -> string
end =
struct
  fun trim path = path  (* the identity! *)
end
