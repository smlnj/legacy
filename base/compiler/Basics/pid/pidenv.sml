(* pidenv.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * Environments that bind persistent IDs.
 * (Instantiated to dynamic and symbolic environments by the compiler.)
 *)
functor PidEnvFn (type binding) : PIDENV where type binding = binding =
struct
    type binding = binding
    type env = binding PersMap.map

    val empty = PersMap.empty
    fun look e p = PersMap.find (e, p)
    fun bind (p, l, e) = PersMap.insert (e, p, l)
    fun atop (e1, e2) = PersMap.unionWith #1 (e1, e2)
    fun remove (pl, e) = let
	fun rmv (key, map) = let
	    val (newMap, _) = PersMap.remove(map, key) 
	in
	    newMap
	end handle e => map
    in
        foldr rmv e pl
    end
    fun consolidate e = e
    fun singleton (p, l) = bind (p, l, empty)
    fun listItemsi e = PersMap.listItemsi e
    fun fromListi il = foldl PersMap.insert' empty il
    fun mk (NONE, _) = empty
      | mk (_, NONE) = empty
      | mk (SOME p, SOME l) = singleton (p, l)
end
