(* hash-util.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 * COPYRIGHT 1989 by AT&T Bell Laboratories
 *
 * These are generic routines for supporting hash-tables of various
 * kinds in the CX system.  Since the hash table use mutable state, they
 * should be isolated inside server threads.  Some of this code was
 * lifted from SML/NJ.
 *)

signature HASH_UTIL =
  sig

    exception NotFound

  (* a generic hash table mapping unique integer keys to values *)
    type 'a int_map_t
  (* create a new table *)
    val newIntMap : unit -> '1a int_map_t
  (* insert an item *)
    val insertInt : '2a int_map_t -> (int * '2a) -> unit
  (* find an item, the exception NotFound is raised if the item doesn't exist *)
    val findInt : 'a int_map_t -> int -> 'a
  (* remove an item, returning the item *)
    val removeInt : 'a int_map_t -> int -> 'a
  (* return a list of the items in the table *)
    val listInts : 'a int_map_t -> 'a list

  (* a generic hash table mapping string keys to values *)
    type 'a name_map_t
  (* create a new table *)
    val newNameMap : unit -> '1a name_map_t
  (* insert an item *)
    val insertName : '2a name_map_t -> (string * '2a) -> unit
  (* find an item, the exception NotFound is raised if the item doesn't exist *)
    val findName : 'a name_map_t -> string -> 'a
  (* remove an item *)
    val removeName : 'a name_map_t -> string -> 'a
  (* return a list of the items in the table *)
    val listNames : 'a name_map_t -> 'a list

  end (* signature HASH_UTIL *)

structure HashUtil : HASH_UTIL =
  struct

    exception NotFound

    local
    (* a string hash function *)
      fun hashString str = let
	    val prime = 8388593 (* largest prime less than 2^23 *)
	    val base = 128
	    val l = String.size str
	    in
	      case l
	       of 0 => 0
		| 1 => ord str
		| 2 => ordof(str,0) + base * ordof(str,1)
		| 3 => ordof(str,0) + base * (ordof(str,1) + base * ordof(str,2))
		| _ => let
		    fun loop (0,n) = n
		      | loop (i,n) = let
			  val i = i-1
			  val n' = (base * n + ordof(str,i)) 
			  in
			    loop (i, (n' - prime * (n' quot prime)))
			  end
		    in
		      loop (l,0)
		    end
	    end (* hashString *)

      datatype ('a, 'b) bucket_t
        = NIL
        | B of ('b * 'a * ('a, 'b) bucket_t)

      type ('a, 'b) map_t = {
	  table : ('a, 'b) bucket_t array ref,
	  elems : int ref
        }

    (* generic routines on maps *)
      fun newMap () = {table = ref (Array.array(32, NIL)), elems = ref 0}
      fun hash (i, sz) = Bits.andb(i, sz-1)

    (* conditionally grow a map *)
      fun mapGrow (keyIndx, {table, elems}) = let
	    val arr = !table
	    val sz = Array.length arr
	    val newSz = sz+sz
	    val newArr = Array.array (newSz,NIL)
	    fun copy NIL = ()
	      | copy (B(key, v, rest)) = let
		  val indx = hash(keyIndx key, newSz)
		  in
		    Array.update(newArr, indx, B(key, v, Array.sub(newArr, indx)));
		    copy rest
		  end
	    fun bucket n = (copy(Array.sub(arr, n)); bucket(n+1))
	    in
	      if (!elems >= sz)
		then (
		  (bucket 0) handle Array.Subscript => ();
		  table := newArr)
		else ()
	    end

    (* insert a (key, value) pair into a map (assuming size is okay) *)
      fun mapInsert (mkKey, keyIndx, keyEq) = let
	    fun insert ({table, elems}, realKey, v) = let
		  val key = mkKey realKey
		  val arr = !table
		  val sz = Array.length arr
		  val indx = hash(keyIndx key, sz)
		  fun look NIL = (
			Array.update(arr, indx, B(key, v, Array.sub(arr, indx)));
			elems := !elems + 1)
		    | look (B(k, _, r)) = if keyEq(key, k) then () else look r
		  in
		    look (Array.sub (arr, indx))
		  end
	    in
	      insert
	    end

      fun mapRemove (mkKey, keyIndx, keyEq) = let
	    fun remove {table, elems} key = let
		  val key = mkKey key
		  fun look NIL = raise NotFound
		    | look (B(k, v, r)) = if keyEq(key, k)
			  then (v, r)
			  else let val (removedVal, rest) = look r
			    in
			      (removedVal, B(k, v, rest))
			    end
		  val arr = !table
		  val indx = hash (keyIndx key, Array.length arr)
		  val (removedVal, rest) = look (Array.sub(arr, indx))
		  in
		    Array.update (arr, indx, rest);
		    elems := !elems - 1;
		    removedVal
		  end
	    in
	      remove
	    end (* mapRemove *)

      fun mapList {table = ref tbl, elems} = let
	    fun f (_, l, 0) = l
	      | f (~1, l, _) = l
	      | f (i, l, n) = let
		  fun g (NIL, l, n) = f (i-1, l, n)
		    | g (B(_, x, r), l, n) = g(r, x::l, n-1)
		  in
		    g (Array.sub(tbl, i), l, n)
		  end
	    in
	      f ((Array.length tbl) - 1, [], !elems)
	    end (* list *)

      fun f o g = (fn x => f(g x))  (* for inlining *)

      fun intTblIndx i = i
      val intTblEq = ((op =) : (int * int) -> bool)
      val mkIntKey = intTblIndx

      fun nameTblIndx (i, _) = i
      fun nameTblEq ((i1:int, s1:string), (i2, s2)) = ((i1 = i2) andalso (s1 = s2))
      fun mkNameKey s = (hashString s, s)
    in

    datatype 'a int_map_t = INT_MAP of ('a, int) map_t
    datatype 'a name_map_t = NAME_MAP of ('a, (int * string)) map_t

    fun projIntMap (INT_MAP tbl) = tbl
    fun projNameMap (NAME_MAP tbl) = tbl

    fun newIntMap () = INT_MAP(newMap())
    fun newNameMap () = NAME_MAP(newMap())

    val insertInt = let
	  val insert = mapInsert (mkIntKey, intTblIndx, intTblEq)
	  fun doit (INT_MAP tbl) (key, v) = (
		mapGrow(intTblIndx, tbl); insert (tbl, key, v))
	  in
	    doit
	  end
    val insertName = let
	  val insert = mapInsert (mkNameKey, nameTblIndx, nameTblEq)
	  fun doit (NAME_MAP tbl) (key, v) = (
		mapGrow(nameTblIndx, tbl); insert (tbl, key, v))
	  in
	    doit
	  end

    val removeInt = (mapRemove (mkIntKey, intTblIndx, intTblEq)) o projIntMap
    val removeName = (mapRemove (mkNameKey, nameTblIndx, nameTblEq)) o projNameMap

    fun listInts (INT_MAP m) = mapList m
    fun listNames (NAME_MAP m) = mapList m

   (** The find functions could also be generic, but they are the most used, so it
    ** is good to make them fast (instead of trusting in the optimizer).
    **)
    fun findInt (INT_MAP{table, elems}) key = let
	  fun look NIL = raise NotFound
	    | look (B(i, v, rest)) = if (key <> i) then (look rest) else v
	  val arr = !table
	  in
	    look (Array.sub (arr, hash (key, Array.length arr)))
	  end (* find *)

    fun findName (NAME_MAP{table, elems}) key = let
	  val h = hashString key
	  fun look NIL = raise NotFound
	    | look (B((i, s), v, rest)) =
		if ((h <> i) orelse (key <> s)) then (look rest) else v
	  val arr = !table
	  in
	    look (Array.sub (arr, hash (h, Array.length arr)))
	  end (* find *)

    end (* local *)
  end (* structure HashUtil *)
