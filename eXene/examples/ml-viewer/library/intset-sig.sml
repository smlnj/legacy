(* intset-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Signature for sets of integers.
 *)

signature INTSET =
  sig

    type intset

    exception NotFound

    val empty : intset
	(* Create a new set
	 *)

    val singleton : int -> intset
	(* Create a singleton set
	 *)

    val add : intset * int -> intset
	(* Insert an int.  
	 *)

    val member : intset * int -> bool
	(* Return true if and only if the int is an element in the set *)

    val delete : intset * int -> intset
	(* Remove an int.
         * Raise NotFound if not found
	 *)

    val numItems : intset ->  int
	(* Return the number of ints in the table *)

    val union : intset * intset -> intset
        (* Union *)

    val intersection : intset * intset -> intset
        (* Intersection *)

    val difference : intset * intset -> intset
        (* Difference *)

    val listItems : intset -> int list
	(* Return a list of the ints in the set *)

    val app : (int -> 'b) -> intset -> unit
	(* Apply a function to the entries of the set 
         * in decreasing order
         *)

    val revapp : (int -> 'b) -> intset -> unit
	(* Apply a function to the entries of the set 
         * in increasing order
         *)

    val fold : (int * 'b -> 'b) -> intset -> 'b -> 'b
	(* Apply a folding function to the entries of the set 
         * in decreasing order
         *)

    val revfold : (int * 'b -> 'b) -> intset -> 'b -> 'b
	(* Apply a folding function to the entries of the set 
         * in increasing order
         *)

  end (* INTSET *)
