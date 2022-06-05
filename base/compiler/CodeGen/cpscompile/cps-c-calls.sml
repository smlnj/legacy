(* cps-c-calls.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module now contains all the code which handles C-Calls.
 * I've moved Matthias' c-call code from MLRiscGen into here and added
 * my own hacks for handling reeentrant C calls.
 *
 * On the implementation of reentrant C calls, or why it is a hack
 * ---------------------------------------------------------------
 *
 *   For reentrant C call, we need a way of flushing/restore the ML state
 * to/from the msp_state data structure and preserving all live values.
 * Determining the set of live values is a bit tricky and I handle it
 * by doing a liveness analysis.  Ideally, the cps phases should be able
 * to do the liveness part for us, but after spending weeks
 * looking at the source and asking questions with no one answering,
 * I've decided that I've had enough: I need this working NOW
 * so I going to do it the stupid way.  At least this way it is
 * completely self-contained and doesn't involve any cps hacking.
 * If in the future someone gets the right info it should be redone in the
 * right way.
 *
 *  The code for saving/restore live values is quite similar to what
 * the InvokeGC stuff is doing, but I'm deathly afraid of merging it into the
 * InvokeGC code, because the GC handling code had taken me a long time to
 * get right.  It is an angry slumbering power which will visit its
 * horrible wraths on all who dares to disturb it.
 *
 * On saving/restoring ML state
 * ----------------------------
 *
 * The ml state must be threaded into a reentrant C call because the C call
 * may invoke ML code internally before it returns.   Saving the state means
 * two things:
 *   1. Making sure all the live values are properly saved and restored
 *      (and properly tagged so that the gc can find them)
 *   2. Making sure dedicated register such as ml_allocPtr are properly
 *      single threaded through the calls.
 *
 * The ml state is defined in the runtime struct ml_state.
 * For our purposes, the relevant fields are these:
 *
 *  ml_val_t    *ml_allocPtr;
 *  ml_val_t    *ml_limitPtr;
 *  ml_val_t    ml_arg;
 *  ml_val_t    ml_cont;
 *  ml_val_t    ml_closure;
 *  ml_val_t    ml_linkReg;
 *  ml_val_t    ml_pc;
 *  ml_val_t    ml_exnCont;
 *  ml_val_t    ml_varReg;
 *  ml_val_t    ml_calleeSave[CALLEESAVE];
 *  ml_val_t    ml_storePtr;
 *  ml_val_t    ml_faultExn;
 *  Word_t      ml_faultPC;
 *  ml_val_t    *ml_realLimit;
 *  bool_t      ml_pollPending;
 *  bool_t      ml_inPollHandler;
 *
 * To make a c-call reentrant we flush the following registers back into
 * the ml_state record:
 *
 *     ml_allocPtr  --
 *     ml_limitPtr  --
 *     ml_storePtr  --
 *     ml_varReg    --
 *     ml_exnCont   --
 *
 * All all untagged values are packed into a single record
 *     ml_arg       --
 *     ml_cont      --
 *
 *
 * --- Allen
 *)
functor CPSCCalls (
    structure MS         : MACH_SPEC
    structure C          : CPSREGS where T.Region=CPSRegions
    structure Cells      : CELLS
    structure MLTreeComp : MLTREECOMP where TS.T = C.T
    structure CCalls     : C_CALLS where T = C.T
   ) : sig

    val c_call : {
	  stream   : MLTreeComp.mltreeStream, (* mltree stream *)
	  regbind  : CPS.value -> C.T.rexp,   (* lookup integer lvar *)
	  fregbind : CPS.value -> C.T.fexp,   (* lookup fp lvar *)
	  typmap   : CPS.lvar -> CPS.cty,     (* lvar -> cty *)
	  vfp      : bool,                    (* virtual frame pointer *)
	  hp       : int                      (* heap pointer *)
	} ->
	   (* arguments to RCC *)
	 bool * string * CTypes.c_proto * CPS.value list *
	 (CPS.lvar * CPS.cty) list * CPS.cexp ->
	   (* return *)
	 { result : C.T.mlrisc list,  (* result(s) *)
	   hp     : int                 (* heap pointer *)
	 }

  end = struct

   structure TS  = MLTreeComp.TS  (* Streams *)
   structure M   = TS.T           (* MLRISC trees *)
   structure CPS = CPS            (* CPS expressions *)
   structure R   = M.Region       (* Aliasing info *)
   structure Set = LambdaVar.Set  (* typed set for liveness *)
   structure D   = MS.ObjDesc     (* ML Object Descriptors *)
   structure CB  = CellsBasis

   fun error msg = MLRiscErrorMsg.error("CPSCalls", msg)

   val ity = MS.wordBitWidth		(* size of ml integer width *)
   val pty = MS.wordBitWidth		(* size of ml pointer *)
   val addrTy = MS.addressBitWidth	(* size of C pointer *)

   (*
    * Utilities
    *)
   (*
    * A CPS register may be implemented as a physical
    * register or a memory location.  The function assign moves a
    * value v into a register or a memory location.
    *)
   fun assign(M.REG(ty,r), v) = M.MV(ty, r, v)
     | assign(M.LOAD(ty, ea, mem), v) = M.STORE(ty, ea, v, mem)
     | assign _ = error "assign"

   fun LI i = M.LI (M.I.fromInt(ity, i))
   fun LW w = M.LI (M.I.fromWord32(ity, w))

   (*
    * convert object descriptor to int
    *)
   val dtoi = LargeWord.toInt


   fun ea(r, 0) = r
     | ea(r, n) = M.ADD(addrTy, r, LI n)

   fun sameRegAs x y = CB.sameCell (x, y)


   (*
    * Set abbreviations
    *)
   infix 6 \/
   infix 7 /\
   infix 5 --
   val O         = Set.empty
   val op\/      = Set.union
   fun unions Ss = foldr op\/ O Ss
   fun def(w, S) = Set.delete(S,w) handle _ => S

   (*
    * Liveness analysis.
    * Given a cps expression e, return the set of lvars that are live.
    *)
   fun liveness e =
   let fun use(CPS.VAR v,S) = Set.add(S,v)
         | use(_, S) = S
       fun uses([], S) = S
         | uses(v::vs,S) = uses(vs, use(v, S))
   in  case e of
          CPS.APP(v,args)          => uses(v::args,O)
        | CPS.SWITCH(v,c,l)        => use(v,unions(map liveness l))
        | CPS.SELECT(_,v,w,t,e)    => use(v,def(w,liveness e))
        | CPS.RECORD(_,l,w,e)      => uses((map #1 l),def(w,liveness e))
        | CPS.OFFSET(_,v,w,e)      => use(v,def(w,liveness e))
        | CPS.SETTER(_,vl,e)       => uses(vl,liveness e)
        | CPS.LOOKER(_,vl,w,t,e)   => uses(vl,def(w,liveness e))
        | CPS.ARITH(_,vl,w,t,e)    => uses(vl,def(w,liveness e))
        | CPS.PURE(_,vl,w,t,e)     => uses(vl,def(w,liveness e))
        | CPS.RCC(_,_,_,vl,wtl,e)  => uses(vl,foldl (fn ((w, _), s) => def (w, s)) (liveness e) wtl)
        | CPS.BRANCH(_,vl,c,e1,e2) => uses(vl,liveness e1 \/ liveness e2)
        | CPS.FIX _ => error "FIX in CPSCCalls.liveness"
   end

   (*
    * Pack live values into records.
    *
    * 1. Untagged stuff like INT32t or FLTt are packed into an unboxed record
    *    with record tag tag_raw.  Small stuff goes first so that there
    *    will be at most one hole in the record due to alignment.
    * 2. Tagged stuff goes into a normal record with tag_record.
    *
    * NOTE: live values include only the lvars, not dedicated registers
    *       like the heap pointer, base pointer, current exception pointer,
    *       etc.
    *)
   fun save_live_lvars {emit,typmap,regbind,fregbind} (w, exp, hp) =
   let val L = liveness exp    (* compute liveness *)
       val L = def(w, L)       (* remove the lvar that the RCC defines *)
       val L = Set.listItems L (* in list form *)

       (* Store a record item *)
       fun store (v,sz,false) offset =
            M.STORE(sz,ea(C.allocptr,offset), regbind v, R.memory)
         | store (v,sz,true) offset =
            M.FSTORE(sz,ea(C.allocptr,offset), fregbind v, R.memory)

       (* Reload a record item *)
       fun reload (sz,false) (v,record,offset) =
            M.MV(sz, v, M.LOAD(sz, ea(record,offset), R.memory))
         | reload (sz,true) (v,record,offset) =
            M.FMV(sz, v, M.FLOAD(sz, ea(record,offset), R.memory))

       (* Partition the live values into tagged and untagged *)
       fun partition([], tagged, untagged) = (tagged, untagged)
         | partition(v::vl, tagged, untagged) =
           let val t       = typmap v
               val sz      = CPSUtil.sizeOf t
               val tag     = CPSUtil.isTagged t
               val isFloat = CPSUtil.isFloat t
               val store   = store(v,sz,isFloat)
               val load    = reload(sz,isFloat)
           in  if tag then partition(vl, (store,load,sz)::tagged, untagged)
               else partition(vl, tagged, (store,load,sz)::untagged)
           end
       val (tagged, untagged) = partition(L, [], [])

       (* Sort by non-decreasing size *)
       val sortBySize = ListMergeSort.sort(fn ((_,_,x),(_,_,y)) => x>y)

       (* Determine offset *)
       fun assignOffset ([], ls, hp) = (rev ls, hp)
         | assignOffset ((v as (_,_,sz))::vl, ls, hp) = (case sz
	     of 32 => assignOffset(vl, (v,hp)::ls, hp+4)
	      | 64 => let
(* 64BIT: alignment adjustment is only required on 32-bit targets *)
		  val hp = if hp mod 8 = 4 then hp+4 else hp
		  in
		    assignOffset(vl, (v,hp)::ls, hp+8)
		  end
              | _  => error "assignOffset"
	    (* end case *))

       val tagged   = sortBySize tagged
       val untagged = sortBySize untagged

   in  ()
   end

   (*
    * This function generates code to save the ml state.
    *)
   fun save_restore_ml_state () = ()

   (*
    * This is the main entry point for C calls.
    * It takes the following things as arguments.
    *   1. An mltree stream.
    *   2. regbind  : lvar -> rexp
    *   3. fregbind : lvar -> fexp
    *   4. typmap   : lvar -> cty
    *   5. vfp      : using virtual frame pointer?
    *   6. hp       : heap pointer
    *   7. arguments to RCC
    * The function emits the call code and returns:
    *   1. result --- return value of call
    *   2. hp     --- the heap pointer
    *
    *)
   fun c_call {stream=TS.S.STREAM{emit, ...},
               regbind,
               fregbind,
               typmap,
               vfp,
               hp
              }
              (reentrant, linkage, p, vl, wtl, e) =
   let

       val { retTy, paramTys, ... } = p : CTypes.c_proto

       fun build_args vl = let
           open CTypes
           fun m (C_double, v :: vl) = ([CCalls.FARG (fregbind v)], vl)
             | m (C_float, v :: vl) =
                 ([CCalls.FARG (M.CVTF2F (32, 64, fregbind v))], vl)
             | m ((C_unsigned (I_char | I_short | I_int | I_long) |
                   C_signed (I_char | I_short | I_int | I_long) |
                   C_PTR),
                  v :: vl) = ([CCalls.ARG (regbind v)], vl)
             | m ((C_STRUCT _ | C_UNION _), v :: vl) =
                 (* pass struct using the pointer to its beginning *)
                 ([CCalls.ARG (regbind v)], vl)
	     | m ((C_signed I_long_long | C_unsigned I_long_long), v :: vl) =
	       let fun field off =
		       M.LOAD (ity, M.LOAD (pty, ea (regbind v, off), R.memory),
			       R.memory)
	       in ([CCalls.ARG (field 4), CCalls.ARG (field 0)], vl)
	       end
	     | m (C_long_double, _) =
	         error "RCC: unexpected long double argument"
	     | m (C_ARRAY _, _) = error "RCC: unexpected array argument"
	     | m (C_void, _) = error "RCC: unexpected void argument"
             | m (_, []) = error "RCC: not enough ML args"

           and ml (tl, vl) = let
                  fun one (t, (ral, vl)) = let val (a, vl') = m (t, vl)
                                           in (a @ ral, vl') end
                  val (ral, vl') = foldl one ([], vl) tl
               in (rev ral, vl')
               end
       in  case ml (paramTys, vl) of
             (al, []) => al
           | _ => error "RCC: too many ML args"
       end (* build_args *)

       val (f, sr, a) =
           case (retTy, vl) of
               ((CTypes.C_STRUCT _ | CTypes.C_UNION _), fv :: srv :: avl) =>
               let val s = regbind srv
               in (regbind fv, fn _ => s, build_args avl)
               end
             | (_, fv :: avl) =>
               (regbind fv,
                fn _ => error "RCC: unexpected struct return",
                build_args avl)
             | _ => error "RCC: prototype/arglist mismatch"

       fun srd defs = let
           fun loop ([], s, r) = { save = s, restore = r }
             | loop (M.GPR (M.REG (ty, g)) :: l, s, r) =
               if List.exists (sameRegAs g) C.ccallCallerSaveR then
                   let val t = Cells.newReg ()
                   in
                       loop (l, M.COPY (ty, [t], [g]) :: s,
                                M.COPY (ty, [g], [t]) :: r)
                   end
               else loop (l, s, r)
             | loop (M.FPR (M.FREG (ty, f)) :: l, s, r) =
               if List.exists (sameRegAs f) C.ccallCallerSaveF then
                   let val t = Cells.newFreg ()
                   in
                       loop (l, M.FCOPY (ty, [t], [f]) :: s,
                                M.FCOPY (ty, [f], [t]) :: r)
                   end
               else loop (l, s, r)
             | loop _ = error "saveRestoreDedicated: unexpected def"
       in
           loop (defs, [], [])
       end (* srd *)

       val paramAlloc =
	   case MS.ccall_prealloc_argspace of
	       NONE => (fn { szb, align } => false)
	     | SOME s => (fn { szb, align } =>
			     if szb > s then
				 error "argument list in C-call too big"
			     else true)

       val { callseq, result } =
           CCalls.genCall
               { name = f, proto = p, structRet = sr,
                 saveRestoreDedicated = srd,
                 paramAlloc = paramAlloc,
                 callComment =
                 SOME ("C prototype is: " ^ CTypes.protoToString p),
                 args = a }

       fun withVSP f = let
           val frameptr = C.frameptr vfp

           val msp =
               M.LOAD (addrTy, ea (frameptr, MS.ML_STATE_OFFSET),
                       R.stack)
           val vsp =
               M.LOAD (addrTy, ea (msp, MS.VProcOffMSP), R.memory)

           val vsp' = M.REG (addrTy, Cells.newReg ())
           val inML = M.LOAD (ity, ea (vsp', MS.InMLOffVSP),
                              R.memory)
           val LimitPtrMask =
               M.LOAD (32, ea (vsp', MS.LimitPtrMaskOffVSP),
                       R.memory)
       in
           (* move vsp to its register *)
           emit (assign (vsp', vsp));
           f { inML = inML, LimitPtrMask = LimitPtrMask }
       end (* withVSP *)

       (* prepare for leaving ML *)
       val () =
       withVSP (fn { inML, LimitPtrMask } =>
                   ((* set vp_limitPtrMask to ~1 *)
                    emit (assign (LimitPtrMask, LW 0wxffffffff));
                    (* set vp_inML to 0 *)
                    emit (assign (inML, LW 0w0))));

       (* now do the actual call! *)
       val () =
       app emit callseq;

       (* come back to ML, restore proper limit pointer *)
       val () =
       withVSP (fn { inML, LimitPtrMask } =>
                   ((* set vp_inML back to 1 *)
                    emit (assign (inML, LW 0w1));
                    (* limitPtr := limitPtr & vp_limitPtrMask *)
                    emit (assign (C.limitptr(vfp),
                                  M.ANDB (pty, LimitPtrMask,
                                               C.limitptr(vfp))))));
       (* Find result *)
       val result =
       case (result, retTy) of
           (([] | [_]), (CTypes.C_void | CTypes.C_STRUCT _ | CTypes.C_UNION _)) => []
         | ([], _) => error "RCC: unexpectedly few results"
         | ([M.FPR x], CTypes.C_float) => [M.FPR(M.CVTF2F (64, 32, x))]
         | ([r as M.FPR x], CTypes.C_double) => [r]
         | ([M.FPR _], _) => error "RCC: unexpected FP result"
	 | ([r1 as M.GPR _, r2 as M.GPR _],
	    (CTypes.C_signed CTypes.I_long_long |
	     CTypes.C_unsigned CTypes.I_long_long)) =>
	    [r1, r2]
         | ([r as M.GPR x], _) => [r] (* more sanity checking here ? *)
         | _ => error "RCC: unexpectedly many results"
   in  { result = result,
         hp     = hp
       }
   end (* c_call *)

end (* functor CPSCCalls *)
