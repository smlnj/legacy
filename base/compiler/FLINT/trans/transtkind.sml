(* transtkind.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure TransTKind =
  struct

(* translation from the front-end simplified version of tkind (TKind.tkind) to
 * PLambdaType.tkind *)

    local
      structure TK = TKind
      structure PLT = PLambdaType
    in

  (* trans : TKind.tkind -> PlambdaType.tkind *)
    fun trans (TK.TKCint i) = PLT.tkc_int i
      | trans (TK.TKCfun (args,res)) = PLT.tkc_fun (map trans args, trans res)
      | trans (TK.TKCseq tks) = PLT.tkc_seq(map trans tks)

    end

end (* structure TransTKind *)
