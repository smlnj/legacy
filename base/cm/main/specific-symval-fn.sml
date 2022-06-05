(* specific-symval-fn.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Building a host/OS-specific environments for CM "preprocessor" variables.
 *
 * Author: Matthias Blume
 *)

functor SpecificSymValFn (val arch: string
			  val os: SMLofNJ.SysInfo.os_kind
			  val abi_variant: string option) =
    struct
	local
	    val (arch, big, size) = (case arch
		   of "amd64" => ("AMD64", false, 64)
		    | "ppc" => ("PPC", true, 32)
		    | "sparc" => ("SPARC", true, 32)
		    | "x86" => ("X86", false, 32)
		    | arch => ErrorMsg.impossible ("unknown architecture: " ^ arch)
		  (* end case *))
	    val extra_syms = (case abi_variant
		   of NONE => []
		    | SOME s => ["ABI_" ^ s]
		  (* end case *))
	    val env0 = SymVal.default {
		    arch = arch, big = big, size = size, os = os,
		    version = #version_id SMLNJVersion.version,
		    extra_syms = extra_syms
		  }
	    val er = ref env0
	in
	    fun symval s = let
		  fun get () = SymVal.look (!er) s
		  fun set v = er := SymVal.define (!er, s, v)
		  in
		    { get = get, set = set }
		  end
	end (* local *)
    end
