(* poly-hash-table-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * The signature of the polymorphic hash table structure.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

signature POLY_HASH_TABLE =
  sig

    type ('a, 'b) hash_table
	(* type of hash table mapping 'a to 'b *)

    val mkTable : (('2a -> int) * (('2a * '2a) -> bool)) -> (int * exn)
	  -> ('2a,'2b) hash_table
	(* Given a hashing function and an equality predicate, create a new table;
	 * the int is a size hint and the exception is to be raised by find.
	 *)

    val insert : ('2a, '2b) hash_table -> ('2a * '2b) -> unit
	(* Insert an item.  If the key already has an item associated with it,
	 * then the old item is discarded.
	 *)

    val find : ('a, 'b) hash_table -> 'a -> 'b
	(* Find an item, the table's exception is raised if the item doesn't exist *)

    val peek : ('a, 'b) hash_table -> 'a -> 'b option
	(* Look for an item, return NONE if the item doesn't exist *)

    val remove : ('a, 'b) hash_table -> 'a -> 'b
	(* Remove an item, returning the item.  The table's exception is raised if
	 * the item doesn't exist.
	 *)

    val numItems : ('a, 'b) hash_table ->  int
	(* Return the number of items in the table *)

    val listItems : ('a, 'b) hash_table -> ('a * 'b) list
	(* Return a list of the items (and their keys) in the table *)

    val apply : (('a * 'b) -> 'c) -> ('a, 'b) hash_table -> unit
	(* Apply a function to the entries of the table *)

    val map : (('2a * 'b) -> '2c) -> ('2a, 'b) hash_table -> ('2a, '2c) hash_table
	(* Map a table to a new table that has the same keys *)

    val filter : (('a * 'b) -> bool) -> ('a, 'b) hash_table -> unit
	(* remove any hash table items that do not satisfy the given
	 * predicate.
	 *)

    val transform : ('b -> '2c) -> ('2a, 'b) hash_table -> ('2a, '2c) hash_table
	(* Map a table to a new table that has the same keys *)

    val copy : ('1a, '1b) hash_table -> ('1a, '1b) hash_table
	(* Create a copy of a hash table *)

    val bucketSizes : ('a, 'b) hash_table -> int list
	(* returns a list of the sizes of the various buckets.  This is to
	 * allow users to gauge the quality of their hashing function.
	 *)

  end (* POLY_HASH_TABLE *)
