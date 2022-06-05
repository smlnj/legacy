(* xatom-tbl.sml
 *
 * Hash tables of XAtoms.
 *)

structure XAtomTbl = HashTableFn (struct
    type hash_key = XProtTypes.atom
    fun hashVal (XProtTypes.XAtom n) = n
    fun sameKey (XProtTypes.XAtom a1, XProtTypes.XAtom a2) = (a1 = a2)
  end);

