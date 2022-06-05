(* poly-hash-table.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Polymorphic hash tables.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

structure PolyHashTable : POLY_HASH_TABLE =
  struct

    datatype ('a, 'b) bucket_t
      = NIL
      | B of (int * 'a * 'b * ('a, 'b) bucket_t)

    datatype ('a, 'b) hash_table = HT of {
	hash_fn : 'a -> int,
	eq_pred : ('a * 'a) -> bool,
	not_found : exn,
	table : ('a, 'b) bucket_t array ref,
	n_items : int ref
      }

    fun index (i, sz) = Bits.andb(i, sz-1)

  (* find smallest power of 2 (>= 32) that is >= n *)
    fun roundUp n = let
	  fun f i = if (i >= n) then i else f (Bits.lshift (i, 1))
	  in
	    f 32
	  end

  (* Create a new table; the int is a size hint and the exception
   * is to be raised by find.
   *)
    fun mkTable (hash, eq) (sizeHint, notFound) = HT{
	    hash_fn = hash,
	    eq_pred = eq,
	    not_found = notFound,
	    table = ref (Array.array(roundUp sizeHint, NIL)),
	    n_items = ref 0
	  }

  (* conditionally grow a table *)
    fun growTable (HT{table, n_items, ...}) = let
	    val arr = !table
	    val sz = Array.length arr
	    in
	      if (!n_items >= sz)
		then let
		  val newSz = sz+sz
		  val newArr = Array.array (newSz, NIL)
		  fun copy NIL = ()
		    | copy (B(h, key, v, rest)) = let
			val indx = index (h, newSz)
			in
			  Array.update (newArr, indx,
			    B(h, key, v, Array.sub(newArr, indx)));
			  copy rest
			end
		  fun bucket n = (copy (Array.sub(arr, n)); bucket (n+1))
		  in
		    (bucket 0) handle Array.Subscript => ();
		    table := newArr
		  end
		else ()
	    end (* growTable *)

  (* Insert an item.  If the key already has an item associated with it,
   * then the old item is discarded.
   *)
    fun insert (tbl as HT{hash_fn, eq_pred, table, n_items, ...}) (key, item) = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hash_fn key
	  val indx = index (hash, sz)
	  fun look NIL = (
		Array.update(arr, indx, B(hash, key, item, Array.sub(arr, indx)));
		n_items := !n_items + 1;
		growTable tbl;
		NIL)
	    | look (B(h, k, v, r)) = if ((hash = h) andalso eq_pred(key, k))
		then B(hash, key, item, r)
		else (case (look r)
		   of NIL => NIL
		    | rest => B(h, k, v, rest)
		  (* end case *))
	  in
	    case (look (Array.sub (arr, indx)))
	     of NIL => ()
	      | b => Array.update(arr, indx, b)
	  end

  (* find an item, the table's exception is raised if the item doesn't exist *)
    fun find (HT{hash_fn, eq_pred, table, not_found, ...}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hash_fn key
	  val indx = index (hash, sz)
	  fun look NIL = raise not_found
	    | look (B(h, k, v, r)) = if ((hash = h) andalso eq_pred(key, k))
		then v
		else look r
	  in
	    look (Array.sub (arr, indx))
	  end

  (* look for an item, return NONE if the item doesn't exist *)
    fun peek (HT{hash_fn, eq_pred, table, ...}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hash_fn key
	  val indx = index (hash, sz)
	  fun look NIL = NONE
	    | look (B(h, k, v, r)) = if ((hash = h) andalso eq_pred(key, k))
		then SOME v
		else look r
	  in
	    look (Array.sub (arr, indx))
	  end

  (* Remove an item.  The table's exception is raised if
   * the item doesn't exist.
   *)
    fun remove (HT{hash_fn, eq_pred, not_found, table, n_items}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hash_fn key
	  val indx = index (hash, sz)
	  fun look NIL = raise not_found
	    | look (B(h, k, v, r)) = if ((hash = h) andalso eq_pred(key, k))
		then (v, r)
		else let val (item, r') = look r in (item, B(h, k, v, r')) end
	  val (item, bucket) = look (Array.sub (arr, indx))
	  in
	    Array.update (arr, indx, bucket);
	    n_items := !n_items - 1;
	    item
	  end (* remove *)

  (* Return the number of items in the table *)
   fun numItems (HT{n_items, ...}) = !n_items

  (* return a list of the items in the table *)
    fun listItems (HT{table = ref arr, n_items, ...}) = let
	  fun f (_, l, 0) = l
	    | f (~1, l, _) = l
	    | f (i, l, n) = let
		fun g (NIL, l, n) = f (i-1, l, n)
		  | g (B(_, k, v, r), l, n) = g(r, (k, v)::l, n-1)
		in
		  g (Array.sub(arr, i), l, n)
		end
	  in
	    f ((Array.length arr) - 1, [], !n_items)
	  end (* listItems *)

  (* Apply a function to the entries of the table *)
    fun apply f (HT{table, ...}) = let
	  fun appF NIL = ()
	    | appF (B(_, key, item, rest)) = (
		f (key, item);
		appF rest)
	  val arr = !table
	  fun appToTbl i = (appF (Array.sub (arr, i)); appToTbl(i+1))
	  in
	    (appToTbl 0) handle _ => ()
	  end (* apply *)

  (* Map a table to a new table that has the same keys and exception *)
    fun map f (HT{hash_fn, eq_pred, table, n_items, not_found}) = let
	  fun mapF NIL = NIL
	    | mapF (B(hash, key, item, rest)) =
		B(hash, key, f (key, item), mapF rest)
	  val arr = !table
	  val sz = Array.length arr
	  val newArr = Array.array (sz, NIL)
	  fun mapTbl i = (
		Array.update(newArr, i, mapF (Array.sub(arr, i)));
		mapTbl (i+1))
	  in
	    (mapTbl 0) handle _ => ();
	    HT{
		hash_fn = hash_fn, eq_pred = eq_pred,
		table = ref newArr, n_items = ref(!n_items),
		not_found = not_found
	      }
	  end (* transform *)

  (* remove any hash table items that do not satisfy the given
   * predicate.
   *)
    fun filter pred (HT{table, ...}) = let
	  fun filterP NIL = NIL
	    | filterP (B(hash, key, item, rest)) = if (pred(key, item))
		then B(hash, key, item, filterP rest)
		else rest
	  val arr = !table
	  fun filterTbl i = (filterP (Array.sub (arr, i)); filterTbl(i+1))
	  in
	    (filterTbl 0) handle _ => ()
	  end (* filter *)

  (* Map a table to a new table that has the same keys and exception *)
    fun transform f (HT{hash_fn, eq_pred, table, n_items, not_found}) = let
	  fun mapF NIL = NIL
	    | mapF (B(hash, key, item, rest)) = B(hash, key, f item, mapF rest)
	  val arr = !table
	  val sz = Array.length arr
	  val newArr = Array.array (sz, NIL)
	  fun mapTbl i = (
		Array.update(newArr, i, mapF (Array.sub(arr, i)));
		mapTbl (i+1))
	  in
	    (mapTbl 0) handle _ => ();
	    HT{
		hash_fn = hash_fn, eq_pred = eq_pred,
		table = ref newArr, n_items = ref(!n_items),
		not_found = not_found
	      }
	  end (* transform *)

  (* Create a copy of a hash table *)
    fun copy (HT{hash_fn, eq_pred, table, n_items, not_found}) = let
	  val arr = !table
	  val sz = Array.length arr
	  val newArr = Array.array (sz, NIL)
	  fun mapTbl i = (
		Array.update (newArr, i, Array.sub(arr, i));
		mapTbl (i+1))
	  in
	    (mapTbl 0) handle _ => ();
	    HT{
		hash_fn = hash_fn, eq_pred = eq_pred,
		table = ref newArr, n_items = ref(!n_items),
		not_found = not_found
	      }
	  end (* copy *)

  (* returns a list of the sizes of the various buckets.  This is to
   * allow users to gauge the quality of their hashing function.
   *)
    fun bucketSizes (HT{table = ref arr, ...}) = let
	  fun len (NIL, n) = n
	    | len (B(_, _, _, r), n) = len(r, n+1)
	  fun f (~1, l) = l
	    | f (i, l) = f (i-1, len (Array.sub (arr, i), 0) :: l)
	  in
	    f ((Array.length arr)-1, [])
	  end

  end (* HashTable *)
