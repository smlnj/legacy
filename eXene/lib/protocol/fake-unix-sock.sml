(*
 * A dummy implementation of structure UnixSock to satisfy dependencies
 * on systems that do not have such a structure.
 *
 * (C) 2003 The Fellowship of SML/NJ
 *
 * author: Matthias Blume (blume@tti-c.org)
 *)
structure UnixSock = struct
    local
	fun notsupported _ = raise Fail "unix domain sockets not supported"
    in
        structure Strm = struct
            val socket = notsupported
	end
	val toAddr = notsupported
    end
end
