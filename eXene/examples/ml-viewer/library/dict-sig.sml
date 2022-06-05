(* dict-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Abstract signature of an applicative-style map/dictionary structure.
 * The weak type variables in this signature are because some implementations
 * (e.g., splay trees) are self adjusting under lookup.
 *
 *)

signature DICT =
  sig

    structure Key : ORD_KEY

    type 'a dict

    exception NotFound

    val mkDict : unit -> '1a dict
	(* Create a new dict
	 *)

    val insert : '1a dict * Key.ord_key * '1a -> '1a dict
	(* Insert an item.  
	 *)

    val find : 'a dict * Key.ord_key -> 'a
	(* Find an item, raising NotFound if not found
         *)

    val peek : 'a dict * Key.ord_key -> 'a option
	(* Look for an item, return NONE if the item doesn't exist *)

    val remove : '1a dict * Key.ord_key -> '1a dict * '1a
	(* Remove an item, returning new dictionary and value removed.
         * Raise NotFound if not found
	 *)

    val numItems : 'a dict ->  int
	(* Return the number of items in the table *)

    val listItems : 'a dict -> (Key.ord_key * 'a) list
	(* Return a list of the items (and their keys) in the dictionary
         *)

    val app : ((Key.ord_key * 'a) -> 'b) -> 'a dict -> unit
	(* Apply a function to the entries of the dictionary
         * in dictionary order.
         *)

    val revapp : ((Key.ord_key * 'a) -> 'b) -> 'a dict -> unit
	(* Apply a function to the entries of the dictionary 
         * in reverse dictionary order.
         *)

    val fold : (Key.ord_key * 'a * 'b -> 'b) -> 'a dict -> 'b -> 'b
	(* Apply a folding function to the entries of the dictionary
         * in reverse dictionary order.
         *)

    val revfold : (Key.ord_key * 'a * 'b -> 'b) -> 'a dict -> 'b -> 'b
	(* Apply a folding function to the entries of the dictionary
         * in dictionary order.
         *)

    val map : (Key.ord_key * 'a -> '2b) -> 'a dict -> '2b dict
	(* Create a new table by applying a map function to the
         * name/value pairs in the table.
         *)

    val transform : ('a -> '2b) -> 'a dict -> '2b dict
	(* Create a new table by applying a map function to the
         * values in the table.
         *)

  end (* DICT *)
