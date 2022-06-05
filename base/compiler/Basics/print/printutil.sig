(* Copyright 1996 by AT&T Bell Laboratories *)
(* printutil.sig *)

signature PRINTUTIL =
sig

(* following functions actually print (to stdout) *)
    
  val newline : unit -> unit
  val tab : int -> unit
  val nlindent : int -> unit

  val printSequence : string -> ('a -> unit) -> 'a list -> unit
  val printClosedSequence : (string*string*string) -> ('a -> unit) ->
					 'a list -> unit
  val printSym : Symbol.symbol -> unit

  val printvseq : int -> string -> ('a -> unit) -> 'a list -> unit

(* following functions translate to strings, and do not print *)

(*  val quoteString : string -> string *)
      (* was "mlstr"? -- puts double quote characters around string *)
  val formatString : string -> string
      (* was "pr_mlstr"? -- quotes and trims string according to
       * Control_Print.stringDepth. *)
  val formatIntInf : IntInf.int -> string
      (* was "pr_intinf". Calls IntInf.toString but trims output according to
       * Contro_Print.intinfDepth. *)

end (* signature PRINTUTIL *)
