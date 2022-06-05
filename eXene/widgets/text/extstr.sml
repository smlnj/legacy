(* extstr.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.
 *
 * Extensible string data type.
 *)

signature EXTSTR = 
  sig

    type ext_str

    exception BadIndex of int

    val mkExtStr : string -> ext_str
    val es_len : ext_str -> int
    val es_gets : ext_str -> string
    val es_subs : (ext_str * int * int) -> string
    val es_ins : (ext_str * int * char) -> ext_str
    val es_del : (ext_str * int) -> ext_str

  end (* EXTSTR *)

structure ExtStr : EXTSTR = struct

  datatype ext_str = ExtStr of {
    suffix : string,
    listc : char list,
    listl : int
  }

  exception BadIndex of int

  fun mkExtStr s = ExtStr{suffix="", listc = rev(explode s),listl=size s}

  fun es_len (ExtStr{suffix,listl,...}) = size suffix + listl

  fun es_gets (ExtStr{suffix,listc,...}) = (implode(rev listc)) ^ suffix

  fun es_subs (str, i, len) = let
    val s = es_gets str
  in
    (substring (s, i, Int.min((size s)-i, len)))
      handle Substring => raise BadIndex i
  end

  fun es_split (str, i) = let
    val s = es_gets str
  in
    (substring(s,0,i),substring (s, i, (size s)-i))
      handle Substring => raise BadIndex i
  end

  fun es_ins (s as ExtStr{suffix,listc,listl},i,c) =
    if i < 0 then raise BadIndex i
    else if i = listl then
      ExtStr{suffix=suffix,listc=c::listc,listl=listl+1}
    else let
      val (pref,suff) = es_split (s, i)
    in
      ExtStr{suffix=suff,listc=c::(rev(explode pref)),listl=i+1}
    end

  fun es_del (s as ExtStr{suffix,listc,listl},i) =
    if i <= 0 then raise BadIndex i
    else if i = listl then
      ExtStr{suffix=suffix,listc=tl listc, listl = listl-1}
    else let
      val (pref,suff) = es_split (s, i)
    in
      ExtStr{suffix=suff,listc=tl(rev(explode pref)),listl=i-1}
    end

end (* ExtStr *)
