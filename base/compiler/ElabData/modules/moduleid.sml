(* Elaborator/modules/moduleid.sml *)
(* Copyright 1996 by AT&T Bell Laboratories *)
(* Re-written by M.Blume (3/2000) *)
(* Copyright 2025 by the Fellowship of SML/NJ *)
(* Edited by DBM, 2025.02 *)


signature MODULE_ID = sig

    type tycId  (* = Stamp.stamp *)
    type sigId  (* = Stamp.stamp *)
    type strId
    type fctId
    type envId  (* = Stamp.stamp *)

    val tycId : Types.gtrec -> tycId
    val sigId : Modules.sigrec -> sigId  (* why not Signature -> sigId option ? *)
    val strId : Modules.strrec -> strId  (* why not Structure -> strId option ? *)
    val fctId : Modules.fctrec -> fctId  (* why not Functor -> fctId option ? *)
    val envId : Modules.envrec -> envId  (* why not entityEnv -> envId option ? *)

    val strId2 : Modules.sigrec * Modules.strEntity -> strId
    val fctId2 : Modules.fctSig * Modules.fctEntity -> fctId

    val sameTyc : tycId * tycId -> bool
    val sameSig : sigId * sigId -> bool
    val sameStr : strId * strId -> bool
    val sameFct : fctId * fctId -> bool
    val sameEnv : envId * envId -> bool

    val freshTyc : tycId -> bool
    val freshSig : sigId -> bool
    val freshStr : strId -> bool
    val freshFct : fctId -> bool
    val freshEnv : envId -> bool

    (* a tmap is a record of 5 partial maps on module IDs, from the various kinds to module ID to
     * corresponding kinds of static components (tycons, signatures, structures, functors,
     * entityEnvs). *)
    type tmap

    val emptyTmap : tmap

    val lookTyc : tmap * tycId -> Types.gtrec option        (* why not map to tycon option ? *)
    val lookSig : tmap * sigId -> Modules.sigrec option     (* why not map to Signature option ? *)
    val lookStr : tmap * strId -> Modules.strEntity option  (* why not map to Structure option ? *)
    val lookFct : tmap * fctId -> Modules.fctEntity option  (* why not map to Functor option ? *)
    val lookEnv : tmap * envId -> Modules.envrec option     (* why not map to entityEnv option ? *)

    val insertTyc : tmap * tycId * Types.gtrec -> tmap
    val insertSig : tmap * sigId * Modules.sigrec -> tmap
    val insertStr : tmap * strId * Modules.strEntity -> tmap
    val insertFct : tmap * fctId * Modules.fctEntity -> tmap
    val insertEnv : tmap * envId * Modules.envrec -> tmap

    val tycId' : Types.tycon -> tycId

    type 'a umap

    val emptyUmap : 'a umap

    val uLookTyc : 'a umap * tycId -> 'a option
    val uLookSig : 'a umap * sigId -> 'a option
    val uLookStr : 'a umap * strId -> 'a option
    val uLookFct : 'a umap * fctId -> 'a option
    val uLookEnv : 'a umap * envId -> 'a option

    val uInsertTyc : 'a umap * tycId * 'a -> 'a umap
    val uInsertSig : 'a umap * sigId * 'a -> 'a umap
    val uInsertStr : 'a umap * strId * 'a -> 'a umap
    val uInsertFct : 'a umap * fctId * 'a -> 'a umap
    val uInsertEnv : 'a umap * envId * 'a -> 'a umap

end (* signature MODULE_ID *)

structure ModuleId : MODULE_ID = struct

    structure M = Modules
    structure T = Types
    structure A = Access
    structure ST = Stamp

    fun bug m = ErrorMsg.impossible ("ModuleId: " ^ m)

    type stamp = ST.stamp (* == entVar *)


    (* the 5 kinds of module Id (actually module static component Id *)

    type tycId = stamp
    type sigId = stamp
    type strId = { sign: stamp, rlzn: stamp }  (* 2 stamps, for signature and realization *)
    type fctId = { paramsig: stamp, bodysig: stamp, rlzn: stamp }  (* 3 stamps, for param, body sig, and rlzn *)
    type envId = stamp

    (* "freshness" of tycId, strId, fctId (which are just stamps) *)
    val freshTyc = ST.isFresh
    val freshSig = ST.isFresh
    val freshEnv = ST.isFresh

    fun freshStr { sign, rlzn } = ST.isFresh sign orelse ST.isFresh rlzn

    fun freshFct { paramsig, bodysig, rlzn } =
	ST.isFresh paramsig orelse ST.isFresh bodysig orelse ST.isFresh rlzn


    fun tycId (r: Types.gtrec) = #stamp r

    fun sigId (s: M.sigrec) = #stamp s

    fun strId2 (sign: M.sigrec, rlzn: M.strEntity) =
	{ sign = #stamp sign, rlzn = #stamp rlzn }

    fun strId ({ sign = M.SIG s, rlzn, ... }: M.strrec) =
	{ sign = #stamp s, rlzn = #stamp rlzn }
      | strId _ = bug "strId: bad signature"

    fun fctId2 (M.FSIG { paramsig = M.SIG psg, bodysig = M.SIG bsg, ... },
		rlzn: M.fctEntity) =
	{ paramsig = #stamp psg, bodysig = #stamp bsg, rlzn = #stamp rlzn }
      | fctId2 _ = bug "fctId2/fctId2: bad funsig"

    fun fctId ({ sign, rlzn, ... }: M.fctrec) = fctId2 (sign, rlzn)

    fun envId (e: M.envrec) = #stamp e

    (* ordering of strIds (a kind of module Id) *)
    structure StrKey =
    struct
       type ord_key = strId
       fun compare (i1: strId, i2: strId) =
	   case ST.compare (#sign i1, #sign i2) of
	       EQUAL => ST.compare (#rlzn i1, #rlzn i2)
	     | unequal => unequal
    end (* structure StrKey *)

    (* ordering of FctIds (a kind of module Id) *)
    structure FctKey =
    struct
       type ord_key = fctId
       fun compare ({paramsig=p1, bodysig=b1, rlzn=r1} : fctId,
		    {paramsig=p2, bodysig=b2, rlzn=r2} : fctId) =
	   (case ST.compare (p1, p2)
	      of EQUAL => (case ST.compare (b1, b2)
			     of EQUAL => ST.compare (r1, r2)
			      | unequal => unequal)
	       | unequal => unequal)
    end (* structure FctKey *)

    structure StampM = RedBlackMapFn (ST) (* finite maps over stamps/entVars *)
    structure StrM = RedBlackMapFn (StrKey) (* finite maps over strIds *)
    structure FctM = RedBlackMapFn (FctKey) (* finite maps over fctIds *)

    val sameTyc = ST.eq
    val sameSig = ST.eq
    fun sameStr (x, y) = StrKey.compare (x, y) = EQUAL
    fun sameFct (x, y) = FctKey.compare (x, y) = EQUAL
    val sameEnv = ST.eq

    type tmap = { m_tyc: T.gtrec StampM.map,
		  m_sig: M.sigrec StampM.map,
		  m_str: M.strEntity StrM.map,
		  m_fct: M.fctEntity FctM.map,
		  m_env: M.envrec StampM.map }

    val emptyTmap = { m_tyc = StampM.empty,
		      m_sig = StampM.empty,
		      m_str = StrM.empty,
		      m_fct = FctM.empty,
		      m_env = StampM.empty }

    (* lookTyc : tmap * tycId -> T.gtrec option *)
    (* lookSig : tmap * sigId -> M.sigrec option *)
    (* lookStr : tmap * strId -> M.strEntity option *)
    (* lookFct : tmap * tycId -> T.fctEnitity option *)
    (* lookEnv : tmap * envId -> M.envrec option *)

    fun lookTyc ({m_tyc, ...}: tmap, id: tycId) : T.gtrec option =
	StampM.find(m_tyc, id)

    fun lookSig ({m_sig, ...}: tmap, id: sigId) : M.sigrec option =
	StampM.find(m_sig, id)

    fun lookEnv ({m_env, ...}: tmap, id: envId) : M.envrec option =
	StampM.find (m_env, id)

    fun lookStr ({m_str, ...}: tmap, id: strId) : M.strEntity option =
	StrM.find(m_str, id)

    fun lookFct ({m_fct, ...}: tmap, id: fctId) : M.fctEntity option =
	FctM.find(m_fct, id)


    (* RECORD UPDATE! *)
    fun insertTyc ({ m_tyc, m_sig, m_str, m_fct, m_env }: tmap, k, t) =
	{ m_tyc = StampM.insert (m_tyc, k, t),
	  m_sig = m_sig, m_str = m_str, m_fct = m_fct, m_env = m_env }
	  
    fun insertSig ({ m_tyc, m_sig, m_str, m_fct, m_env }: tmap, k, t) =
	{ m_sig = StampM.insert (m_sig, k, t),
	  m_tyc = m_tyc, m_str = m_str, m_fct = m_fct, m_env = m_env }
	  
    fun insertStr ({ m_tyc, m_sig, m_str, m_fct, m_env }: tmap, k, t) =
	{ m_str = StrM.insert (m_str, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_fct = m_fct, m_env = m_env }
	  
    fun insertFct ({ m_tyc, m_sig, m_str, m_fct, m_env }: tmap, k, t) =
	{ m_fct = FctM.insert (m_fct, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_str = m_str, m_env = m_env }
	  
    fun insertEnv ({ m_tyc, m_sig, m_str, m_fct, m_env }: tmap, k, t) =
	{ m_env = StampM.insert (m_env, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_str = m_str, m_fct = m_fct }

    fun tycId' (T.GENtyc r) = tycId r
      | tycId' (T.DEFtyc { stamp, ... }) = stamp
      | tycId' _ = bug "tycId': neither GENtyc nor DEFtyc"


    (* umap : TYPE => TYPE
     * Uniformely typed maps; collections of maps over module Ids to a common target type 'a
     *)

    type 'a umap = { m_tyc: 'a StampM.map,
		     m_sig: 'a StampM.map,
		     m_str: 'a StrM.map,
		     m_fct: 'a FctM.map,
		     m_env: 'a StampM.map }
    local
	fun look (sel, find) (m as { m_tyc, m_sig, m_str, m_fct, m_env }, k) =
	    find (sel m, k)
    in
        fun lookTyc x = look (#m_tyc, StampM.find) x
	fun lookSig x = look (#m_sig, StampM.find) x
	fun lookStr x = look (#m_str, StrM.find) x
	fun lookFct x = look (#m_fct, FctM.find) x
	fun lookEnv x = look (#m_env, StampM.find) x
    end

    val emptyUmap = emptyTmap

    val uLookTyc = lookTyc
    val uLookSig = lookSig
    val uLookStr = lookStr
    val uLookFct = lookFct
    val uLookEnv = lookEnv

   fun uInsertTyc ({ m_tyc, m_sig, m_str, m_fct, m_env }: 'a umap, k, t) =
	{ m_tyc = StampM.insert (m_tyc, k, t),
	  m_sig = m_sig, m_str = m_str, m_fct = m_fct, m_env = m_env }
	  
    fun uInsertSig ({ m_tyc, m_sig, m_str, m_fct, m_env }: 'a umap, k, t) =
	{ m_sig = StampM.insert (m_sig, k, t),
	  m_tyc = m_tyc, m_str = m_str, m_fct = m_fct, m_env = m_env }
	  
    fun uInsertStr ({ m_tyc, m_sig, m_str, m_fct, m_env }: 'a umap, k, t) =
	{ m_str = StrM.insert (m_str, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_fct = m_fct, m_env = m_env }
	  
    fun uInsertFct ({ m_tyc, m_sig, m_str, m_fct, m_env }: 'a umap, k, t) =
	{ m_fct = FctM.insert (m_fct, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_str = m_str, m_env = m_env }
	  
    fun uInsertEnv ({ m_tyc, m_sig, m_str, m_fct, m_env }: 'a umap, k, t) =
	{ m_env = StampM.insert (m_env, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_str = m_str, m_fct = m_fct }

(*
    val uInsertTyc = insertTyc
    val uInsertSig = insertSig
    val uInsertStr = insertStr
    val uInsertFct = insertFct
    val uInsertEnv = insertEnv
*)
end (* structure ModuleId *)
