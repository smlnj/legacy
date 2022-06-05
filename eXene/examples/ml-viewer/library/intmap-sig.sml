(* intmap-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Signature of an applicative-style integer map.
 * Compatible with DICT with Key.ord_key = int 
 *
 *)

signature INTMAP =
  sig

    type 'a intmap

    exception NotFound

    val empty : unit -> 'a intmap
	(* Create a new map.
	 *)

    val insert : 'a intmap * int * 'a -> 'a intmap
	(* Insert an item.  
	 *)

    val find : 'a intmap * int -> 'a
	(* Find an item, raising NotFound if not found
         *)

    val peek : 'a intmap * int -> 'a option
	(* Look for an item, return NONE if the item doesn't exist *)

    val remove : 'a intmap * int -> 'a intmap
	(* Remove an item.
         * Raise NotFound if not found
	 *)

    val numItems : 'a intmap ->  int
	(* Return the number of items in the table *)

    val listItems : 'a intmap -> (int * 'a) list
	(* Return a list of the items (and their keys) in the map.
         *)

    val app : (int * 'a -> 'b) -> 'a intmap -> unit
	(* Apply a function to the entries of the map
         * in increasing order.
         *)

    val revapp : (int * 'a -> 'b) -> 'a intmap -> unit
	(* Apply a function to the entries of the map 
         * in decreasing order.
         *)

    val fold : (int * 'a * 'b -> 'b) -> 'a intmap -> 'b -> 'b
	(* Apply a folding function to the entries of the map
         * in decreasing order.
         *)

    val revfold : (int * 'a * 'b -> 'b) -> 'a intmap -> 'b -> 'b
	(* Apply a folding function to the entries of the intmap
         * in increasing order.
         *)

    val map : (int * 'a -> '2b) -> 'a intmap -> '2b intmap
	(* Create a new table by applying a map function to the
         * name/value pairs in the table.
         *)

    val transform : ('a -> '2b) -> 'a intmap -> '2b intmap
	(* Create a new table by applying a map function to the
         * values in the table.
         *)

  end (* INTMAP *)
