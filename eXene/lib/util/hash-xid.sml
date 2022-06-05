(* hash-xid.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A hash table package for hashing on xids (which are, by definition, unique).
 *)

structure HashXId : sig

    type 'a xid_map

    exception XIdNotFound

  (* create a new table *)
    val newMap : unit -> '1a xid_map
  (* insert an item *)
    val insert : '2a xid_map -> (XProtTypes.xid * '2a) -> unit
  (* find an item, the exception XIdNotFound is raised if the item doesn't exist *)
    val lookup : 'a xid_map -> XProtTypes.xid -> 'a
  (* remove an item *)
    val remove : 'a xid_map -> XProtTypes.xid -> 'a
  (* return a list of the items in the table *)
    val list : 'a xid_map -> (XProtTypes.xid * 'a) list

  end = struct

    structure Tbl = HashTableFn (struct
        type hash_key = XProtTypes.xid
        fun sameKey (XProtTypes.XID x1, XProtTypes.XID x2) = (x1 = x2)
	fun hashVal (XProtTypes.XID x) = x
      end)

    type 'a xid_map = 'a Tbl.hash_table

    exception XIdNotFound

    fun newMap () = Tbl.mkTable(32, XIdNotFound);
    val lookup = Tbl.lookup
    val insert = Tbl.insert
    val remove = Tbl.remove
    val list = Tbl.listItemsi

  end (* HashXId *)
