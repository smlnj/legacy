signature LSPLIT_INLINE = sig

    type flint = FLINT.prog
    type pid = PersStamps.persstamp
    type importTree = ImportTree.importTree
    type import = pid * importTree
    type symenv = SymbolicEnv.env

    val inline: flint * import list * symenv -> flint * import list
end

structure LSplitInline :> LSPLIT_INLINE = struct

    type flint = FLINT.prog
    type pid = PersStamps.persstamp
    datatype importTree = datatype ImportTree.importTree
    type import = pid * importTree
    type symenv = SymbolicEnv.env

    structure LT = Lty
    structure LK = LtyKernel
    structure LV = LambdaVar
    structure F  = FLINT
    structure FU = FlintUtil

    fun bug s = ErrorMsg.impossible ("LSplitInline: " ^ s)

    fun inline0 ((mainFkind, mainLvar, [(mainArgLvar, mainArgLty)], mainBody),
		 imports, symenv) =
	let
	    val importTypes =
		case LK.lt_out mainArgLty of
		    LT.LT_STR it => it
		  | _ => bug "non-structure arg to comp-unit"
	    val newArgLvar = LV.mkLvar ()
	    val symLook = SymbolicEnv.look symenv
	    fun cnt (ITNODE []) = 1
	      | cnt (ITNODE l) = foldl (fn ((_, t), n) => cnt t + n) 0 l
	    fun selHdr (v, t, rvl) = let
		fun oneNode (v, ITNODE [], h, r) = (h, v :: r)
		  | oneNode (v, ITNODE l, h, r) = let
			fun oneBranch ((s, t), (h, r)) = let
			    val v' = LV.mkLvar ()
			    val (h, r) = oneNode (v', t, h, r)
			in
			    (fn e => F.SELECT(F.VAR v, s, v', h e), r)
			end
		    in
			foldl oneBranch (h, r) l
		    end
	    in
		oneNode (v, t, fn e => e, rvl)
	    end
	    (*
	     * build: imports * types * offset * vars -> types * imports * lexp
	     *)
	    fun build ([], [], _, rvl) =
		([], [],
		 F.RECORD (F.RK_STRUCT, rev (map F.VAR rvl),
			   mainArgLvar, mainBody))
	      | build ([], _, _, _) = bug "build mismatch: too many types"
	      | build ((imp as (pid, tr)) :: rest, tyl, i, rvl) = let
		    val lc = cnt tr
		in
		    case Option.map FU.copyfdec (symLook pid) of
			NONE => let
			    fun h (0, tyl, i, rvl) = build (rest, tyl, i, rvl)
			      | h (n, ty :: tyl, i, rvl) = let
				    val rv = LV.mkLvar ()
				    val (tyl, imps, body) =
					h (n - 1, tyl, i + 1, rv :: rvl)
				in
				    (ty :: tyl, imps,
				     F.SELECT (F.VAR newArgLvar, i, rv, body))
				end
			      | h _ = bug "build mismatch: too few types"
			    val (tyl, imps, body) = h (lc, tyl, i, rvl)
			in
			    (tyl, imp :: imps, body)
			end
		      | SOME (f as (fk, fv, [(av, at)], b)) => let
			    (* val _ = Control_Print.say "hello\n" *)
			    val inlv = LV.mkLvar ()
			    val (wrapSel, rvl) = selHdr (inlv, tr, rvl)
			    val (tyl, imps, body) =
				build (rest, List.drop (tyl, lc), i + 1, rvl)
			in
			    (at :: tyl, (pid, ITNODE []) :: imps,
			     F.SELECT (F.VAR newArgLvar, i, av,
				       F.LET ([inlv], b, wrapSel body)))
			end
		      | _ => bug "bad cross-inlining argument list"
		end

	    val (tyl, imps, newBody) = build (imports, importTypes, 0, [])
	    val newArgLty = LK.lt_inj (LT.LT_STR tyl)
	in
	    ((mainFkind, mainLvar, [(newArgLvar, newArgLty)], newBody), imps)
	end
      | inline0 _ = bug "bad comp-unit argument list"

    fun inline args = let
	val (e, i) = inline0 args
    in
	((* LContract.lcontract *) e, i)
    end
end
