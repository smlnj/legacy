(* Basics/symbol/fixity.sml *)
(* Copyright 1996 by AT&T Bell Laboratories *)
(* Copyright 2025 by The Fellowship of SML/NJ (www.smlnj.org) *)

signature FIXITY =
sig
  datatype fixity = NONfix | INfix of (int*int)
  val infixleft : int -> fixity
  val infixright : int -> fixity
  val fixityToString : fixity -> string

end (* signature FIXITY *)


structure Fixity : FIXITY =
struct

  datatype fixity = NONfix | INfix of (int*int)

  (* building fixities *)
  fun infixleft n = INfix (2*n, 2*n+1)
  fun infixright n = INfix (2*n+1, 2*n)

  fun fixityToString NONfix = "nonfix "
    | fixityToString (INfix (i,_)) =
         (if i mod 2 = 0 then "infix " else "infixr ")^
         (if i div 2 > 0 then Int.toString (i div 2)^" " else "")

end (* structure Fixity *)

