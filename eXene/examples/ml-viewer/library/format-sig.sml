(* format-sig.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Formatted conversion to and from strings.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

signature FORMAT =
  sig

    datatype fmt_item
      = INT of int
      | BOOL of bool
      | STR of string
      | REAL of real
      | LEFT of (int * fmt_item)	(* left justify in field of given width *)
      | RIGHT of (int * fmt_item)	(* right justify in field of given width *)

    exception BadFormat			(* bad format string *)
    exception BadArgList		(* raised on specifier/item type mismatch *)
    exception BadInput of fmt_item list	(* bad input *)

    val format : string -> fmt_item list -> string
    val formatf : string -> (string -> unit) -> fmt_item list -> unit

    val scan : string -> string -> fmt_item list
    val scani : string -> (string * int) -> (fmt_item list * int)
(*    val scanf : string -> (int -> string) -> string -> fmt_item list*)

  end (* FORMAT *)
