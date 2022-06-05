(* ordset-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Signature for a set of values with an order relation.
 *)

signature ORD_SET =
  sig

    type item
    type set

    exception NotFound

    val empty : set
	(* Create a new set
	 *)

    val singleton : item -> set
	(* Create a singleton set
	 *)

    val add : set * item -> set
	(* Insert an item.  
	 *)

    val find : set * item -> item
	(* Find an item, raising NotFound if not found
         *)

    val peek : set * item -> item option
	(* Look for an item, return NONE if the item doesn't exist *)

    val member : set * item -> bool
	(* Return true if and only if item is an element in the set *)

    val delete : set * item -> set
	(* Remove an item.
         * Raise NotFound if not found
	 *)

    val numItems : set ->  int
	(* Return the number of items in the table *)

    val union : set * set -> set
        (* Union *)

    val intersection : set * set -> set
        (* Intersection *)

    val difference : set * set -> set
        (* Difference *)

    val listItems : set -> item list
	(* Return a list of the items in the set *)

    val app : (item -> 'b) -> set -> unit
	(* Apply a function to the entries of the set 
         * in decreasing order
         *)

    val revapp : (item -> 'b) -> set -> unit
	(* Apply a function to the entries of the set 
         * in increasing order
         *)

    val fold : (item * 'b -> 'b) -> set -> 'b -> 'b
	(* Apply a folding function to the entries of the set 
         * in decreasing order
         *)

    val revfold : (item * 'b -> 'b) -> set -> 'b -> 'b
	(* Apply a folding function to the entries of the set 
         * in increasing order
         *)

  end (* ORD_SET *)
