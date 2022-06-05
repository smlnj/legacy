(* hash-window.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A hash table package for hashing on windows.
 *)

structure HashWindow : sig

    type 'a window_map

    exception WindowNotFound

  (* create a new table *)
    val newMap : unit -> 'a window_map
  (* insert an item *)
    val insert : 'a window_map -> (DrawTypes.window * 'a) -> unit
  (* find an item, the exception WindowNotFound is raised if the
   * item doesn't exist.
   *)
    val lookup : 'a window_map -> DrawTypes.window -> 'a
  (* find an item (using an id as the key), the exception WindowNotFound
   * is raised if the item doesn't exist
   *)
    val lookupWinId : 'a window_map -> XProtTypes.win_id -> 'a
  (* remove an item *)
    val remove : 'a window_map -> DrawTypes.window -> 'a
  (* return a list of the items in the table *)
    val list : 'a window_map -> 'a list

  end = struct

    type 'a window_map = 'a HashXId.xid_map

    exception WindowNotFound = HashXId.XIdNotFound

    val newMap = HashXId.newMap
    fun lookup m (DrawTypes.WIN{id, ...}) = HashXId.lookup m id
    val lookupWinId = HashXId.lookup
    fun insert m (DrawTypes.WIN{id, ...}, a) = HashXId.insert m (id, a)
    fun remove m (DrawTypes.WIN{id, ...}) = HashXId.remove m id
    fun list tbl = map #2 (HashXId.list tbl)

  end (* HashXId *)
