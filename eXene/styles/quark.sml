(* quark.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Strings with fast inequality operations.  This should probably be replaced
 * with "names," but there are problems with creating names that are statically
 * initialized in CML.  Once there is a "CML shell," we can replace this by
 * the CML_Name structure.
 *)

structure Quark :> QUARK =
  struct

    datatype quark = Q of {str : string, hash : word}

    fun quark s = Q{str = s, hash = HashString.hashString s}

    fun stringOf (Q{str, ...}) = str

    fun same (Q{str=s1, hash=h1}, Q{str=s2, hash=h2}) =
	  (h1 = h2) andalso (s1 = s2)

    fun hash (Q{hash, ...}) = hash

    fun cmp (Q{str=s1, hash=h1}, Q{str=s2, hash=h2}) =
	  if (h1 < h2)
	    then LESS
	  else if (h2 < h1)
	    then GREATER
	    else String.compare(s1, s2)

  end;
