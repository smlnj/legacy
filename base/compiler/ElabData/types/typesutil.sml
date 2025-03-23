(* typesutil.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure TypesUtil : TYPESUTIL =
struct

local (* imports *)
    structure EM = ErrorMsg
    structure SS = Substring
    structure EP = EntPath
    structure BT = BasicTypes
    structure SP = SymPath
    structure IP = InvPath
    structure S = Symbol
    structure ST = Stamps
    structure A = Access
    structure T = Types
    structure V = VarCon		      
    structure AS = Absyn
    structure SSS = SpecialSymbols

    val array = Array.array
    val sub = Array.sub
    val update = Array.update
    infix 9 sub

in (* top local *)

    val --> = BT.-->
    infix -->

    val say = Control_Print.say
    val debugging = ElabDataControl.tudebugging
    fun debugmsg msg = if !debugging then say ("TypesUtil: " ^ msg ^ "\n") else ()
    fun bug msg = EM.impossible("TypesUtil: "^msg)

    fun eqpropToString p =
        (case p
	   of T.NO => "NO"
	    | T.YES => "YES"
	    | T.IND => "IND"
	    | T.OBJ => "OBJ"
	    | T.DATA => "DATA"
	    | T.UNDEF => "UNDEF"
	    | T.ABS => "ABS"
	  (* end case *))

  (*************** operations to build tyvars, VARtys ***************)

    fun mkMETA depth = T.OPEN{kind=T.META, depth=depth, eq=false}

    fun mkFLEX (fields, depth) = T.OPEN{kind=T.FLEX fields, depth=depth, eq=false}

    fun extract_varname_info name =
	let val name = SS.triml 1 (SS.full name)  (* remove leading "'" *)
	    val (name, eq) =
		  if SS.sub(name, 0) = #"'"
  		  then (SS.triml 1 name, true) (* initial "'" signifies equality *)
		  else (name, false)
	 in (SS.string name, eq)
	end

    fun mkUBOUND (id : Symbol.symbol) : T.tvKind =
	let val (name, eq) = extract_varname_info (Symbol.name id)
	 in T.UBOUND {name=Symbol.tyvSymbol name, depth=T.infinity, eq=eq}
	end

(* mkLITERALty moved to ElabCore because of use of OverloadLit *)

  (*
   * mkMETAty:
   *
   *   This function returns a type that represents a new meta variable
   * which does NOT appear in the "context" anywhere.  To do the same
   * thing for a meta variable which will appear in the context (because,
   * for example, we are going to assign the resulting type to a program
   * variable), use mkMETAtyBounded with the appropriate depth.
   *)

    fun mkMETAtyBounded (depth: int) : T.ty = T.VARty(T.mkTyvar (mkMETA depth))

    fun mkMETAty() = mkMETAtyBounded T.infinity


  (*************** primitive operations on tycons ***************)
    fun bugTyc (s: string, tyc) = (case tyc
	   of T.GENtyc { path, ... } => bug (s ^ " GENtyc " ^ S.name (IP.last (path, SSS.errorTycId)))
	    | T.DEFtyc {path,...} => bug (s ^ " T.DEFtyc " ^ S.name(IP.last (path, SSS.errorTycId)))
	    | T.RECORDtyc _ => bug (s ^ " RECORDtyc")
	    | T.PATHtyc{path,...} => bug (s ^ " PATHtyc " ^ S.name(IP.last (path, SSS.errorTycId)))
	    | T.RECtyc _ => bug (s ^ " RECtyc")
	    | T.FREEtyc _ => bug (s ^ " FREEtyc")
	    | T.ERRORtyc => bug (s ^ " ERRORtyc")
	  (* end case *))

  (* short (single symbol) name of tycon *)
    fun tycName (T.GENtyc { path, ... } | T.DEFtyc{path,...} | T.PATHtyc{path,...}) =
	  IP.last (path, SSS.errorTycId)
      | tycName (T.RECORDtyc _) = S.tycSymbol "<RECORDtyc>"
      | tycName (T.RECtyc _) = S.tycSymbol "<RECtyc>"
      | tycName (T.FREEtyc _) = S.tycSymbol "<FREEtyc>"
      | tycName T.ERRORtyc = S.tycSymbol "<ERRORtyc>"

  (* get the stamp of a tycon *)
    fun tycStamp (T.GENtyc { stamp, ... } | T.DEFtyc { stamp, ... }) = stamp
      | tycStamp tycon = bugTyc("tycStamp",tycon)

  (* full path name of tycon, an InvPath.path *)
    fun tycPath (T.GENtyc{path,...} | T.DEFtyc{path,...} | T.PATHtyc{path,...}) = path
      | tycPath T.ERRORtyc = IP.IPATH[S.tycSymbol "error"]
      | tycPath tycon  = bugTyc("tycPath",tycon)

    fun tycEntPath(T.PATHtyc{entPath,...}) = entPath
      | tycEntPath tycon = bugTyc("tycEntPath",tycon)

    fun tyconArity(T.GENtyc { arity, ... } | T.PATHtyc{arity,...}) = arity
      | tyconArity(T.DEFtyc{tyfun=T.TYFUN{arity,...},...}) = arity
      | tyconArity(T.RECORDtyc l) = length l
      | tyconArity(T.ERRORtyc) = 0
      | tyconArity tycon = bugTyc("tyconArity",tycon)

    fun setTycPath(tycon,path) = (case tycon
	   of T.GENtyc { stamp, arity, eq, kind, path = _, stub = _ } =>
	      T.GENtyc { stamp = stamp, arity = arity, eq = eq, kind = kind,
		       path = path, stub = NONE }
	    | T.DEFtyc{tyfun,strict,stamp,path=_} =>
		T.DEFtyc{tyfun=tyfun,path=path,strict=strict,stamp=stamp}
	    | _ => bugTyc("setTycName",tycon)
	  (* end case *))

    fun eqRecordLabels(nil,nil) = true
      | eqRecordLabels(x::xs,y::ys) = Symbol.eq(x,y) andalso eqRecordLabels(xs,ys)
      | eqRecordLabels _ = false

    fun eqTycon (T.GENtyc g, T.GENtyc g') = Stamps.eq (#stamp g, #stamp g')
      | eqTycon (T.ERRORtyc,_) = true
      | eqTycon (_,T.ERRORtyc) = true
      (* this rule for PATHtycs is conservatively correct, but is only an
	 approximation *)
      | eqTycon(T.PATHtyc{entPath=ep,...},T.PATHtyc{entPath=ep',...}) =
	  EP.eqEntPath(ep,ep')
      | eqTycon(T.RECORDtyc l1, T.RECORDtyc l2) = eqRecordLabels(l1,l2)
      (*
       * This next case used for comparing DEFtyc's, where we can be
       * sure they are equal if they share the same creation stamp,
       * but otherwise we'll assume they may be different.
       * Also used in PPBasics to check data constructors of
       * a datatype.  Used elsewhere?
       *)
      | eqTycon(T.DEFtyc{stamp=s1,...},T.DEFtyc{stamp=s2,...}) =
	  Stamps.eq(s1,s2)
      | eqTycon _ = false

    (* eqDatacon : T.datacon * T.datacon -> bool
     * equality of datacons belonging to the same datatype, based on
     * the fact that no two datacons of a datatype have the same rep value,
     * and conrep is an equality type. *)
    fun eqDatacon(T.DATACON{rep=a1,...}: T.datacon, T.DATACON{rep=a2,...}: T.datacon) =
        (a1 = a2)

    (* prune: ty -> ty; eliminates outermost T.INSTANTIATED indirections *)
    fun prune (T.MARKty(ty, _)) = prune ty
      | prune (T.VARty(tv as ref(T.INSTANTIATED ty))) = let
	  val pruned = prune ty
	  in
	    tv := T.INSTANTIATED pruned; pruned
	  end
      | prune ty = ty

    fun pruneTyvar (tv as ref(T.INSTANTIATED ty)) : T.ty =
	let val pruned = prune ty
	 in tv := T.INSTANTIATED pruned; pruned
	end
      | pruneTyvar _ = bug "pruneTyvar: not an instantiated tyvar"

    fun eqTyvar (tv1: T.tyvar, tv2: T.tyvar) = (tv1 = tv2)

    fun bindTyvars (tyvars: T.tyvar list) : unit =
	let fun loop([],_) = ()
	      | loop(tv::rest,n) =
		  (tv := T.INSTANTIATED (T.IBOUND n);
		   loop(rest,n+1))
	 in loop(tyvars,0)
	end

    fun bindTyvars1 (tyvars: T.tyvar list) : Types.polysign =
	let fun loop([],_) = []
	      | loop((tv as ref (T.UBOUND {eq,...}))::rest,n) =
		   (tv := T.INSTANTIATED (T.IBOUND n);
		    eq :: loop(rest,n+1))
	      | loop _ = bug "bindTyvars1:T.UBOUND"
	 in loop(tyvars,0)
	end

    exception SHARE

  (* assume that f fails on identity, i.e. f x raises SHARE instead of
     returning x *)
    fun shareMap f nil = raise SHARE
      | shareMap f (x::l) =
	  (f x) :: ((shareMap f l) handle SHARE => l)
	  handle SHARE => x :: (shareMap f l)

  (* applyTyfun is more general than instantiatePoly and has
     many uses beyond applyPoly *)
    fun applyTyfun(T.TYFUN{arity,body}, args: T.ty list) =
      let fun subst(T.IBOUND n) = List.nth(args,n)
	    | subst(T.CONty(tyc,args)) = T.CONty(tyc, shareMap subst args)
	    | subst(T.VARty(ref(T.INSTANTIATED ty))) = subst ty
	    | subst(T.MARKty(ty,_)) = subst ty
	    | subst _ = raise SHARE
       in if arity <> length args
	    then (say ("$$$ applyTyfun: arity = "^(Int.toString arity)^
		       ", |args| = "^(Int.toString(length args))^"\n");
		  bug "applyTyfun: arity mismatch")
	  else if arity > 0
	    then subst body
		 handle SHARE => body
		      | Subscript => bug "applyTyfun - not enough arguments"
	  else body
      end

    fun applyPoly(T.POLYty{tyfun,...}, args) =
	  applyTyfun(tyfun, args)
      | applyPoly _ = bug "TypesUtil.applyPoly"

    fun mapTypeFull f =
	let fun mapTy ty =
		case ty
		  of T.CONty (tc, tl) =>
		      T.CONty(f tc, map mapTy tl)
		   | T.POLYty {sign, tyfun=T.TYFUN{arity, body}} =>
		      T.POLYty{sign=sign, tyfun=T.TYFUN{arity=arity,body=mapTy body}}
		   | T.VARty(ref(T.INSTANTIATED ty)) => mapTy ty
		   | T.MARKty(ty, region) => mapTy ty
		   | _ => ty
	 in mapTy
	end

    fun appTypeFull f =
	let fun appTy ty =
		case ty
		  of T.CONty (tc, tl) => (f tc;  app appTy tl)
		   | T.POLYty {sign, tyfun=T.TYFUN{arity, body}} => appTy body
		   | T.VARty(ref(T.INSTANTIATED ty)) => appTy ty
		   | T.MARKty(ty, region) => appTy ty
		   | _ => ()
	 in appTy
	end


    exception ReduceType

    fun reduceType(T.CONty(T.DEFtyc{tyfun,...}, args)) = applyTyfun(tyfun,args)
      | reduceType(T.POLYty{sign=[],tyfun=T.TYFUN{arity=0,body}}) = body
      | reduceType(T.VARty(ref(T.INSTANTIATED ty))) = ty
      | reduceType(T.MARKty(ty, region)) = ty
      | reduceType _ = raise ReduceType

    fun headReduceType ty = headReduceType(reduceType ty) handle ReduceType => ty

    fun equalType(ty: T.ty, ty': T.ty) : bool =
	let fun eq(T.IBOUND i1, T.IBOUND i2) = i1 = i2
	      | eq(ty1 as T.VARty(tv1), ty2 as T.VARty(tv2)) =
		eqTyvar(tv1,tv2) orelse
		(case (tv1,tv2)
		  of (ref(T.INSTANTIATED ty1'), ref(T.INSTANTIATED ty2')) =>
		      equalType(ty1', ty2')
		   | (ref(T.INSTANTIATED ty1'), _) =>
		      equalType(ty1',ty2)
		   | (_, ref(T.INSTANTIATED ty2')) =>
		      equalType(ty1,ty2')
		   | _ => false)
	      | eq(ty as T.CONty(tycon, args), ty' as T.CONty(tycon', args')) =
		  if eqTycon(tycon, tycon') then
		     (case tycon
			of T.DEFtyc{strict,...} =>
			   (* since tycons are equal, both are DEFtycs with
			    * the same arity and strict field values *)
			   let fun eqargs([],[],[]) = true
				 | eqargs(true::ss,ty1::rest1,ty2::rest2) =
				     equalType(ty1,ty2) andalso eqargs(ss,rest1,rest2)
				 | eqargs(false::ss,ty1::rest1,ty2::rest2) =
				     eqargs(ss,rest1,rest2)
				 | eqargs _ = bug "eqargs in equalType [TypesUtil]"
			    in eqargs(strict,args,args')
			   end
			 | _ => ListPair.all equalType(args,args'))
		  else (eq(reduceType ty, ty')
			handle ReduceType =>
			  (eq(ty,reduceType ty')
			    handle ReduceType => false))
	      | eq(ty1 as (T.VARty _ | T.IBOUND _), ty2 as T.CONty _) =
		  (eq(prune ty1,reduceType ty2)
		   handle ReduceType => false)
	      | eq(ty1 as T.CONty _, ty2 as (T.VARty _ | T.IBOUND _)) =
		  (eq(reduceType ty1, prune ty2)
		   handle ReduceType => false)
	      | eq(T.WILDCARDty,_) = true
	      | eq(_,T.WILDCARDty) = true
	      | eq(ty1, T.MARKty(ty, region)) = eq(ty1, ty)
	      | eq(T.MARKty(ty, region), ty2) = eq(ty, ty2)
	      | eq _ = false
	 in eq(prune ty, prune ty')
	end

    fun equalTypeP(T.POLYty{sign=s1,tyfun=T.TYFUN{body=b1,...}},
		   T.POLYty{sign=s2,tyfun=T.TYFUN{body=b2,...}}) =
	if s1 = s2 then equalType(b1,b2) else false
      | equalTypeP(T.POLYty _, t2) = false
      | equalTypeP(t1, T.POLYty _) = false
      | equalTypeP(t1,t2) = equalType(t1,t2)

    local
      (* making dummy argument lists to be used in equalTycon *)
	fun makeDummyType() =
	    T.CONty(T.GENtyc{stamp = CompInfo.mkStamp (),
			 path = IP.IPATH[Symbol.tycSymbol "dummy"],
			 arity = 0, eq = ref T.YES, stub = NONE,
			 kind = T.PRIMITIVE},[])
	     (*
	      * Making dummy type is a temporary hack ! pt_void is not used
	      * anywhere in the source language ... Requires major clean up
	      * in the future. (ZHONG)
	      * DBM: shouldn't cause any problem here.  Only thing relevant
	      * property of the dummy types is that they have different stamps
	      * and their stamps should not agree with those of any "real" tycons.
	      *)
	(* precomputing dummy argument lists
	 * -- perhaps a bit of over-optimization here. [dbm] *)
	fun makeargs (0,args) = args
	  | makeargs (i,args) = makeargs(i-1, makeDummyType()::args)
	val args10 = makeargs(10,[])  (* 10 dummys *)
	val args1 = [hd args10]
	val args2 = List.take (args10,2)
	val args3 = List.take (args10,3)  (* rarely need more than 3 args *)
    in
      fun dummyargs 0 = []
	| dummyargs 1 = args1
	| dummyargs 2 = args2
	| dummyargs 3 = args3
	| dummyargs n =
	    if n <= 10 then List.take (args10,n) (* should be plenty *)
	    else makeargs(n-10,args10)  (* but make new dummys if needed *)
    end (* local *)

  (* equalTycon.  This definition deals only partially with types that
     contain PATHtycs.  There is no interpretation of the PATHtycs, but
     PATHtycs with the same entPath will be seen as equal because of the
     definition on eqTycon. *)
    fun equalTycon(T.ERRORtyc,_) = true
      | equalTycon(_,T.ERRORtyc) = true
      | equalTycon(t1,t2) =
	 let val a1 = tyconArity t1 and a2 = tyconArity t2
	  in a1=a2 andalso
	     (let val args = dummyargs a1
	      in equalType(T.CONty(t1,args),T.CONty(t2,args))
	      end)
	 end

    (* calcStrictness : int * Types.ty -> bool list *)
    (* Returns a list of bools of length arity, where the ith element indicates
     * whether DB index (IBOUND i) occurs in the type "body". *)
    fun calcStrictness (arity, body) =
	let val argument_found = Array.array(arity,false)
	    fun search (T.VARty(ref(tvkind))) =
		  (case tvkind
		     of T.INSTANTIATED ty => search ty
		      | _ => ())
	      | search (T.IBOUND n) = Array.update(argument_found,n,true)
	      | search (ty as T.CONty(tycon, args)) =
		  (case tycon
		     of T.DEFtyc _ => search(headReduceType ty)
		      | _ => app search args)
	      | search (T.MARKty(ty,_)) = search ty
	      | search (T.POLYty _) = bug "calcStrictness: POLYty"
	      | search T.WILDCARDty = bug "calcStrictness: WILDCARDty"
	      | search T.UNDEFty = bug "calcStrictness: UNDEFty"
	 in search body;
	    Array.foldr (op ::) nil argument_found
	end

  (* instantiating polytypes *)

    fun typeArgs n =
	if n>0
	then mkMETAty() :: typeArgs(n-1)
	else []

    val default_tvprop = false

    fun mkPolySign 0 = []
      | mkPolySign n = default_tvprop :: mkPolySign(n-1)

    fun dconName(T.DATACON{name,...}) = name

    fun dconTyc(T.DATACON{typ,const,name,...}) =
	let (* val _ = say "*** the screwed datacon ***"
	       val _ = say (S.name(name))
	       val _ = say " \n" *)
	    fun f (T.POLYty{tyfun=T.TYFUN{body,...},...},b) = f (body,b)
	      | f (T.CONty(tyc,_),true) = tyc
	      | f (T.CONty(_,[_,T.CONty(tyc,_)]),false) = tyc
	      | f (T.MARKty(ty, region), b) = f(ty, b)
	      | f _ = bug "dconTyc"
	 in f (typ,const)
	end

    fun boundargs n =
	let fun loop(i) =
	    if i>=n then nil
	    else T.IBOUND i :: loop(i+1)
	 in loop 0
	end

    fun dconType (tyc,domain) =
	(case tyconArity tyc
	  of 0 => (case domain
		     of NONE => T.CONty(tyc,[])
		      | SOME dom => dom --> T.CONty(tyc,[]))
	   | arity =>
	     T.POLYty{sign=mkPolySign arity,
		    tyfun=T.TYFUN{arity=arity,
				body = case domain
					 of NONE => T.CONty(tyc,boundargs(arity))
					  | SOME dom =>
					    dom --> T.CONty(tyc,boundargs(arity))}})

    (* compressTy : ty -> unit
     * uses of compressTy can probably be replaced by calls of mapTy (fn x => x)
     *)
    fun compressTy (t as T.VARty(x as ref(T.INSTANTIATED(T.VARty(ref v))))) =
	    (x := v; compressTy t)
      | compressTy (T.VARty(ref(T.OPEN{kind=T.FLEX fields,...}))) =
	    app (compressTy o #2) fields
      | compressTy (T.CONty(tyc,tyl)) = app compressTy tyl
      | compressTy (T.POLYty{tyfun=T.TYFUN{body,...},...}) = compressTy body
      | compressTy _ = ()

  (*
   * 8/18/92: cleaned up occ "state machine" some and fixed bug #612.
   * Known behaviour of the attributes about the context that are kept:
   * lamd = # of Abstr's seen so far.  Starts at 0 with Root.
   * top = true iff haven't seen a LetDef yet.
   *)

    abstype occ = OCC of {lamd: int, top: bool}
    with

     val Root = OCC{lamd=0, top=true}

     fun LetDef(OCC{lamd,...}) = OCC{lamd=lamd, top=false}

     fun Abstr(OCC{lamd,top})  = OCC{lamd=lamd+1, top=top}

     fun lamdepth (OCC{lamd,...}) = lamd

     fun toplevel (OCC{top,...})  = top

    end (* abstype occ *)

  (* instantiatePoly: ty -> ty * tyvar list
     if argument is a POLYty, instantiates body of POLYty with new META typa
     variables, returning the instantiatied body and the list of META tyvars.
     if argument is not a POLYty, does nothing, returning argument type *)
    fun instantiatePoly(T.POLYty{sign,tyfun}) : T.ty * T.tyvar list =
	  let val args =  (* fresh T.OPEN metavariables *)
		  map (fn eq =>
			  ref(T.OPEN{kind = T.META, depth = T.infinity, eq = eq}))
		      sign
	   in (applyTyfun(tyfun, map T.VARty args), args)
	  end
      | instantiatePoly ty = (ty,[])

    local
      exception CHECKEQ
    in
    fun checkEqTySig(ty, sign: T.polysign) =
	let fun eqty(T.VARty(ref(T.INSTANTIATED ty))) = eqty ty
	      | eqty(T.CONty(T.DEFtyc{tyfun,...}, args)) =
		  eqty(applyTyfun(tyfun,args))
	      | eqty(T.CONty(T.GENtyc { eq, ... }, args)) =
		 (case !eq
		    of T.OBJ => ()
		     | T.YES => app eqty args
		     | (T.NO | T.ABS | T.IND) => raise CHECKEQ
		     | p => bug ("checkEqTySig: "^eqpropToString p))
	      | eqty(T.CONty(T.RECORDtyc _, args)) = app eqty args
	      | eqty(T.IBOUND n) = if List.nth(sign,n) then () else raise CHECKEQ
	      | eqty _ = ()
	 in eqty ty;
	    true
	end
	handle CHECKEQ => false

    fun checkEqTyInst(ty) =
	let fun eqty(T.VARty(ref(T.INSTANTIATED ty))) = eqty ty
	      | eqty(T.VARty(ref(T.OPEN{eq,...}))) = if eq then () else raise CHECKEQ
	      | eqty(T.VARty(ref(T.LBOUND{eq,...}))) = if eq then () else raise CHECKEQ
	      | eqty(T.CONty(T.DEFtyc{tyfun,...}, args)) =
		  eqty(applyTyfun(tyfun,args))
	      | eqty(T.CONty(T.GENtyc { eq, ... }, args)) =
		 (case !eq
		    of T.OBJ => ()
		     | T.YES => app eqty args
		     | (T.NO | T.ABS | T.IND) => raise CHECKEQ
		     | p => bug ("checkEqTyInst: "^eqpropToString p))
	      | eqty(T.CONty(T.RECORDtyc _, args)) = app eqty args
	      | eqty(T.IBOUND n) = bug "checkEqTyInst: IBOUND in instantiated polytype"
	      | eqty _ = () (* what other cases? dbm *)
	 in eqty ty;
	    true
	end
	handle CHECKEQ => false
    end

  (* compType, compareTypes used to compare specification type with type of
   * corresponding actual element.  Check that spec type is an instance of
   * the actual type *)
    exception CompareTypes
    fun compType(specty, specsign: T.polysign, actty,
		 actsign: T.polysign, actarity): unit =
	let val env = array(actarity,T.UNDEFty) (* instantiations of IBOUNDs in actual body *)
	    fun comp'(T.WILDCARDty, _) = ()
	      | comp'(_, T.WILDCARDty) = ()
	      | comp'(ty1, T.IBOUND i) =
		 (case env sub i
		    of T.UNDEFty =>
			(let val eq = List.nth(actsign,i)
			  in if eq andalso not(checkEqTySig(ty1,specsign))
			     then raise CompareTypes
			     else update(env,i,ty1)
			 end handle Subscript => ())
		     | ty => if equalType(ty1,ty)
			     then ()
			     else raise CompareTypes)
	      | comp'(T.CONty(tycon1, args1), T.CONty(tycon2, args2)) =
		  if eqTycon(tycon1,tycon2)
		  then ListPair.app comp (args1,args2)
		  else raise CompareTypes
	      | comp' _ = raise CompareTypes
	    and comp(ty1,ty2) = comp'(headReduceType ty1, headReduceType ty2)
	 in comp(specty,actty)
	end

  (* returns true if actual type > spec type, i.e. if spec is an instance of actual *)
    fun compareTypes (spec : T.ty, actual: T.ty): bool =
	let val actual = prune actual
	 in case spec
	      of T.POLYty{sign,tyfun=T.TYFUN{body,...}} =>
		  (case actual
		     of T.POLYty{sign=sign',tyfun=T.TYFUN{arity,body=body'}} =>
			  (compType(body,sign,body',sign',arity); true)
		      | T.WILDCARDty => true
		      | _ => false) (* if spec is poly, then actual must be poly *)
	       | T.WILDCARDty => true
	       | _ => (* spec is a monotype *)
		  (case actual
		     of T.POLYty{sign,tyfun=T.TYFUN{arity,body}} =>
			  (compType(spec,[],body,sign,arity); true)
		      | T.WILDCARDty => true
		      | _ => equalType(spec,actual))
	end handle CompareTypes => false

    exception WILDCARDmatch

    fun indexBoundTyvars (tdepth : int, []: T.tyvar list) : unit = ()
      | indexBoundTyvars (tdepth, lboundtvs) =
	let fun setbtvs (i, []) = ()
	      | setbtvs (i, (tv as ref (T.OPEN{eq,...}))::rest) =
		 (tv := T.LBOUND{depth=tdepth,eq=eq,index=i};
		  setbtvs (i+1, rest))
	      | setbtvs (i, (tv as ref (T.LBOUND _))::res) =
		 bug ("unexpected tyvar LBOUND in indexBoundTyvars")
	      | setbtvs _ = bug "unexpected tyvar T.INSTANTIATED in mkPE"
	 in setbtvs(0, lboundtvs)
	end

  (* matchInstTypes: bool * ty * ty -> (tyvar list * tyvar list) option
   * The first argument tells matchInstTypes to ignore the abstract property
   * of abstract types, i.e., this call is being used in FLINT where
   * we can look into abstract types.
   * The third argument is a spec type (e.g. from a signature spec),
   * while the fourth is a potentially more general actual type. The
   * two types are instantiated (if they are polymorphic), and a one-way
   * match is performed on their generic instantiations.
   * [Note that the match cannot succeed if spec is polymorphic while
   * actualTy is monomorphic.]
   * This function is also used more generally to obtain instantiation
   * parameters for a polytype (actualTy) to match one of its instantiations
   * (specTy). This usage occurs in translate.sml where we match an occurrence
   * type of a primop variable with the intrinsic type of the primop to obtain
   * the instantiation parameters for the primop relative to its intrinsic type.
   *)
    fun matchInstTypes(doExpandAbstract,tdepth,specTy,actualTy) =
	let	fun debugmsg' msg = debugmsg ("matchInstTypes: " ^ msg)
	    fun expandAbstract(T.GENtyc {kind=T.ABSTRACT tyc', ...}) =
		expandAbstract tyc'
	      | expandAbstract(tyc) = tyc
	    fun match'(T.WILDCARDty, _) = () (* possible? how?
					     [GK 4/20/07] See bug1179
					     We do matches against WILDCARDs
					     when signature matching fails to
					     match a type spec and yet we have
					     to match valspec and vals mentioning
					     that missing type. *)
	      | match'(_, T.WILDCARDty) = ()
	      | match'(T.MARKty (t, _), t') = match'(t, t')
	      | match'(t, T.MARKty (t', _)) = match'(t, t')
	      | match'(ty1, ty2 as T.VARty(tv as ref(T.OPEN{kind=T.META,eq,...}))) =
		  (* If we're told to ignore abstract, then we can't
		     check for equality types because the original GENtyc
		     was lost by setting the type to abstract imperatively.
		     Thus, if doExpandAbstract, we skip the equality type
		     check. At this point, the elaborator already checked
		     for equality types (before they were side-effected),
		     hence it is guaranteed that if one is an equality type
		     so is the other. The regression test suite coresml
		     d005a-ac.sml tests this. [GK 4/11/07]
		    *)
		  if not(doExpandAbstract) andalso
		     (eq andalso not(checkEqTyInst(ty1)))
		  then (debugmsg' "VARty META\n"; raise CompareTypes)
		  else if equalType(ty1, ty2)
		  then ()
		  else tv := T.INSTANTIATED ty1
	      | match'(ty1, T.VARty(tv as ref(T.INSTANTIATED ty2))) =
		  if equalType(ty1,ty2) then ()
		  else (debugmsg' "T.INSTANTIATED"; raise CompareTypes)
	      (* GK: Does this make sense? matchInstTypes should not apply
		     as is if all the metavariables have been translated
		     into LBOUNDs *)
	      | match'(ty1, ty2 as T.VARty(tv' as (ref(T.LBOUND _)))) =
		  if equalType(ty1,ty2) then ()
		  else (debugmsg' "matchInstTypes: matching and LBOUND tyvar";
			raise CompareTypes)
	      | match'(T.CONty(tycon1, args1), T.CONty(tycon2, args2)) =
		  if eqTycon(tycon1,tycon2)
		  then ListPair.app match (args1,args2)
		  else
		      (* Example:
		       *)
		      if doExpandAbstract (* Expand GENtyc ABSTRACT for translate *)
		      then
			  let val tyc1 = expandAbstract tycon1
			      val tyc2 = expandAbstract tycon2
			  in if not (eqTycon(tyc1,tycon1)
				     andalso eqTycon(tyc2,tycon2))
			     then match(T.CONty(tyc1, args1),
					T.CONty(tyc2, args2))
			     else raise CompareTypes
			  end
		  else (debugmsg' "CONty"; raise CompareTypes)
	      | match'(_, T.UNDEFty) = (debugmsg' "UNDEFty"; raise CompareTypes)
	      | match'(_, T.IBOUND _) = (debugmsg' "IBOUND"; raise CompareTypes)
	      | match'(_, T.POLYty _) = (debugmsg' "POLYty"; raise CompareTypes)
	      | match'(_, T.CONty _) = (debugmsg' "unmatched CONty"; raise CompareTypes)
	      | match'(t1, T.VARty vk) = (debugmsg' "VARty other";
					raise CompareTypes)
	    and match(ty1,ty2) = match'(headReduceType ty1, headReduceType ty2)
	    val (actinst, actParamTvs) = instantiatePoly actualTy
	    val (specinst, specGenericTvs) = instantiatePoly specTy
	    val _ = indexBoundTyvars(tdepth,specGenericTvs)
	    val _ = debugmsg' "Instantiated both\n"
	in match(specinst, actinst);
	   debugmsg' "matched\n";
	   SOME(specGenericTvs, actParamTvs)
	end handle CompareTypes => NONE

  (* given a single-type-variable type, extract out the tyvar *)
    fun tyvarType (T.VARty (tv as ref(T.OPEN _))) = tv
      | tyvarType (T.VARty (tv as ref(T.INSTANTIATED t))) = tyvarType t
      | tyvarType T.WILDCARDty = ref(mkMETA T.infinity)  (* fake a tyvar *)
      | tyvarType (T.IBOUND i) = bug "tyvarType: IBOUND"
      | tyvarType (T.CONty(_,_)) = bug "tyvarType: CONty"
      | tyvarType (T.POLYty _) = bug "tyvarType: POLYty"
      | tyvarType T.UNDEFty = bug "tyvarType: UNDEFty"
      | tyvarType _ = bug "tyvarType - unexpected argument"

  (*
   * getRecTyvarMap : int * ty -> (int -> bool)
   * see if a bound tyvar has occurred in some datatypes, e.g. 'a list.
   * this is useful for representation analysis. This function probably
   * will soon be obsolete (dbm: Why?).
   *)
    fun getRecTyvarMap (n,ty) =
	let val s = Array.array(n,false)
	    fun notArrow tyc = not (eqTycon (tyc, BT.arrowTycon))
			      (* orelse eqTycon(tyc,contTycon) *)
	    fun special (tyc as T.GENtyc { arity, ... }) =
		arity <> 0 andalso notArrow tyc
	      | special(T.RECORDtyc _) = false
	      | special tyc = notArrow tyc

	    fun scan(b,(T.IBOUND n)) = if b then (update(s,n,true)) else ()
	      | scan(b,T.CONty(tyc,args)) =
		 let val nb = (special tyc) orelse b
		  in app (fn t => scan(nb,t)) args
		 end
	      | scan(b,T.VARty(ref(T.INSTANTIATED ty))) = scan(b,ty)
	      | scan _ = ()

	    val _ = scan(false,ty)

	 in fn i => (Array.sub(s,i) handle General.Subscript =>
		       bug "Strange things in TypesUtil.getRecTyvarMap")
	end

    fun gtLabel(a,b) =
	let val a' = Symbol.name a and b' = Symbol.name b
	    val a0 = String.sub(a',0) and b0 = String.sub(b',0)
	 in if Char.isDigit a0
	      then Char.isDigit b0
		andalso (size a' > size b' orelse size a' = size b' andalso a' > b')
	      else Char.isDigit b0 orelse (a' > b')
	end

  (* Tests used to implement the value restriction *)
  (* Based on Ken Cline's version; allows refutable patterns *)
  (* Modified to support CAST, and special binding CASEexp. (ZHONG) *)
  (* Modified to allow applications of lazy val rec Y combinators to
     be nonexpansive. (Taha, DBM) *)

    local open Absyn in

    (* dconRefutable : dcon -> bool
     * a dcon is irrefutable if its datatype has only one data constructor *)
    fun dconRefutable(T.DATACON{sign,...}) =
	case sign
	 of A.CSIG(n,m) => (n+m) > 1 (* ref, etc. dcons considered irrefutable *)
	  | A.CNIL => false (* exn constructor *)

    (* orAlternatives : A.pat -> A.pat list *)
    (* DBM: assumes "|" operator in patterns is right associative
     * this function should only be applied to  *)
    fun orAlternatives (ORpat(p1,p2)) =
	p1 :: orAlternatives p2
      | orAlternatives p = [p]

    (* refutable: A.pat -> bool
     * a pattern is refutable if there exists a value of its type that does
     * not match it, i.e. the "coverage" of the pattern is incomplete *)
    fun refutable pat =
	case pat
	 of VARpat _ => false
	  | WILDpat => false
	  | CONpat (dcon, tyvars) => dconRefutable dcon
	  | RECORDpat {fields, ...} =>
	    List.exists (fn (_,p) => refutable p) fields
	  | APPpat (dcon, _, arg) =>
	    dconRefutable dcon orelse refutable arg
	  | VECTORpat (pats, _) =>
	    List.exists refutable pats
	  | LAYEREDpat (p1, p2) => refutable p1 orelse refutable p2
	  | CONSTRAINTpat (p, _ ) => refutable p
	  | MARKpat (p, _) => refutable p
	  | ORpat (p1, p2) => refutablePats (orAlternatives pat)
	  | _ => true  (* NOPAT, numbers, strings, characters are refutable *)

    (* We don't (yet?) cope with OR patterns. One expects that
     * p1 at least is refutable, else the ORpat is degenerate.
     * together, p1 and/or p2 may be refutable, but ORpat(p1,p2) may
     * not be; e.g. ORpat(true,false). This test is not straitforward! *)

    (* refutablePats : A.pat list -> bool
     * test whether a list of alternative pats is refutable as a whole for their
     * common type, i.e. are there values that don't match any of the pats? *)
    and refutablePats pats = true  (* punting! -- assume all OR patterns refutable *)

    fun isValue (VARexp _) = true
      | isValue (CONexp _) = true
      | isValue (NUMexp _) = true
      | isValue (REALexp _) = true
      | isValue (STRINGexp _) = true
      | isValue (CHARexp _) = true
      | isValue (FNexp _) = true
      | isValue (RECORDexp fields) =
	foldr (fn ((_,exp),x) => x andalso (isValue exp)) true fields
      | isValue (SELECTexp(_, e)) = isValue e
      | isValue (VECTORexp (exps, _)) =
	foldr (fn (exp,x) => x andalso (isValue exp)) true exps
      | isValue (SEQexp nil) = true
      | isValue (SEQexp [e]) = isValue e
      | isValue (SEQexp _) = false
      | isValue (APPexp(rator, rand)) =
	let fun isrefdcon(T.DATACON{rep=A.REF,...}) = true
	      | isrefdcon _ = false
	    fun iscast (V.VALvar {prim, ...}) = PrimopId.isPrimCast prim
	      | iscast _ = false

	    (* LAZY: The following function allows applications of the
	     * fixed-point combinators generated for lazy val recs to
	     * be non-expansive. *)
	    fun issafe(V.VALvar{path=(SymPath.SPATH [s]),...}) =
		(case String.explode (Symbol.name s)
		  of (#"Y" :: #"$" :: _) => true
		   | _ => false)
	      | issafe _ = false

	    fun iscon (CONexp(dcon,_)) = not (isrefdcon dcon)
	      | iscon (MARKexp(e,_)) = iscon e
	      | iscon (VARexp(ref v, _)) = (iscast v) orelse (issafe v)
	      | iscon _ = false
	in if iscon rator then isValue rand
	   else false
	end
      | isValue (CONSTRAINTexp(e,_)) = isValue e
      | isValue (CASEexp(e, (RULE(p,e'))::_, false)) =
	isValue e andalso not(refutable p) andalso isValue e'
        (* DBM: at the point where isValue is used in typecheck.sml,
         * have "val pat = e" declarations been rewritten as
	 * "val bvars = case e of pat => bvars | _ => raise Bind"?
	 * I don't think so. This happens in FLINT/trans/translate
	 * (and/or the match compiler?). In the general case, this is wrong, because
	 * the rhs rule expression e' associated with p may not be a value.
	 * a more general treatment of case expressions would allow the case
         * where the set of all rule patterns is irrefutable and all the rhs
         * expressions are values *)
      | isValue (LETexp(VALRECdec _, e)) = (isValue e) (* special RVB hacks *)
      | isValue (MARKexp(e,_)) = isValue e
      | isValue _ = false

    end (* local *)


    fun isVarTy(T.VARty(ref(T.INSTANTIATED ty))) = isVarTy ty
      | isVarTy(T.VARty _) = true
      | isVarTy(_) = false


  (* sortFields, mapUnZip: two utility functions used in type checking
     (typecheck.sml, mtderiv.sml, reconstruct.sml) *)

    fun sortFields fields =
	ListMergeSort.sort
	    (fn ((AS.LABEL{number=n1,...},_),
		 (AS.LABEL{number=n2,...},_)) => n1>n2)
	    fields

  (* projectField : symbol * ty -> ty option *)
    fun projectField (label: S.symbol, T.CONty(T.RECORDtyc fieldNames, fieldTypes)) =
	let fun search (nil, _) = NONE
	      | search (n::ns, t::ts) =
		if Symbol.eq (label,n) then SOME t
		else search(ns,ts)
	      | search _ = bug "projectField - bad record type"
	 in search (fieldNames, fieldTypes)
	end
      | projectField _ = bug "projectField - not record type"

  (* mapUnZip : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list *)
    fun mapUnZip f nil = (nil, nil)
      | mapUnZip f (hd::tl) =
	 let val (x,y) = f(hd)
	     val (xl,yl) = mapUnZip f tl
	  in (x::xl,y::yl)
	 end

    fun foldTypeEntire f =
	let fun foldTc (tyc, b0) =
	      case tyc
	       of T.GENtyc { kind, ... } =>
		  (case kind of
		       T.DATATYPE{family={members=ms,...},...} => b0
(*             foldl (fn ({dcons, ...},b) => foldl foldDcons b dcons) b0 ms *)
		     | T.ABSTRACT tc => foldTc (tc, b0)
		     | _ => b0)
		| T.DEFtyc{tyfun=T.TYFUN{arity,body}, ...} => foldTy(body, b0)
		| _ => b0

	    and foldDcons({name, rep, domain=NONE}, b0) = b0
	      | foldDcons({domain=SOME ty, ...}, b0) = foldTy(ty, b0)

	    and foldTy (ty, b0) =
	      case ty
	       of T.CONty (tc, tl) =>
		    let val b1 = f(tc, b0)
			val b2 = foldTc(tc, b1)
		     in foldl foldTy b2 tl
		    end
		| T.POLYty {sign, tyfun=T.TYFUN{arity, body}} => foldTy(body, b0)
		| T.VARty(ref(T.INSTANTIATED ty)) => foldTy(ty, b0)
		| _ => b0
	 in foldTy
	end

    fun mapTypeEntire f =
	let fun mapTy ty =
	      case ty
	       of T.CONty (tc, tl) =>
		    T.CONty(f(mapTc, tc), map mapTy tl)
		| T.POLYty {sign, tyfun=T.TYFUN{arity, body}} =>
		    T.POLYty{sign=sign, tyfun=T.TYFUN{arity=arity,body=mapTy body}}
		| T.VARty(ref(T.INSTANTIATED ty)) => mapTy ty
		| _ => ty

	    and mapTc tyc =
	      case tyc
	       of T.GENtyc { stamp, arity, eq, path, kind, stub = _ } =>
		  (case kind of
		       T.DATATYPE{index,family={members,...},...} => tyc
(*
 *  The following code needs to be rewritten !!! (ZHONG)

		   T.GENtyc{stamp=stamp, arity=arity, eq=eq, path=path,
			   kind=DATATYPE {index=index, members=map mapMb members,
					  lambdatyc = ref NONE}}
*)
		     | T.ABSTRACT tc =>
		       T.GENtyc {stamp=stamp, arity=arity, eq=eq, path=path,
			       kind= T.ABSTRACT (mapTc tc),
			       stub = NONE}
		     | _ => tyc)
		| T.DEFtyc{stamp, strict, tyfun, path} =>
		  T.DEFtyc{stamp=stamp, strict=strict, tyfun=mapTf tyfun,
			 path=path}
		| _ => tyc

	     and mapMb {tycname, stamp, arity, dcons, lambdatyc} =
		  {tycname=tycname, stamp=stamp, arity=arity,
		   dcons=(map mapDcons dcons), lambdatyc=ref NONE}

	     and mapDcons (x as {name, rep, domain=NONE}) = x
	       | mapDcons (x as {name, rep, domain=SOME ty}) =
		  {name=name, rep=rep, domain=SOME(mapTy ty)}

	     and mapTf (T.TYFUN{arity, body}) =
		  T.TYFUN{arity=arity, body=mapTy body}

	 in mapTy
	end


  (*
   * Here, using a set implementation should suffice, however,
   * I am using a binary dictionary instead. (ZHONG)
   *)
    local
      structure TycSet = StampMap
    in
      type tycset = T.tycon TycSet.map

      val mkTycSet = fn () => TycSet.empty

      fun addTycSet (tyc as T.GENtyc { stamp, ... }, tycset) =
	    TycSet.insert (tycset, stamp, tyc)
	| addTycSet _ = bug "unexpected tycons in addTycSet"

      fun inTycSet (tyc as T.GENtyc { stamp, ... }, tycset) =
	    isSome (TycSet.find(tycset, stamp))
	| inTycSet _ = false

      fun filterSet (ty, tycs) =
	let fun inList (a::r, tc) = eqTycon(a, tc) orelse inList(r, tc)
	      | inList ([], tc) = false

	    fun pass1 (tc, tset) =
	      if inTycSet(tc, tycs) then
		  (if inList(tset, tc) then tset else tc::tset)
	      else tset
	 in foldTypeEntire pass1 (ty, [])
	end
(*
      val filterSet = fn x =>
	Stats.doPhase(Stats.makePhase "Compiler 034 filterSet") filterSet x
*)
    end (* local TycSet *)

    fun dtSibling (n,tyc as T.GENtyc { kind = T.DATATYPE dt, ... }) =
	let val {index,stamps,freetycs,root, family as {members,...},stripped} = dt
	in
	    if n = index then tyc
	    else let val {tycname,arity,dcons,eq,lazyp,sign} =
			 Vector.sub(members,n)
		     val stamp= Vector.sub(stamps,n)
		 in
		     T.GENtyc {stamp=stamp,
			     arity=arity,eq=eq,path=IP.IPATH[tycname],
			     kind=T.DATATYPE{index=n,stamps=stamps,
					   freetycs=freetycs,
					   root=NONE (*!*),
					   stripped=false,
					   family=family},
			     stub = NONE}
		 end
	end
      | dtSibling _ = bug "dtSibling"

  (* NOTE: this only works (perhaps) for datatype declarations, but not
     specifications. The reason: the root field is used to connect mutually
     recursive datatype specifications together, its information cannot be
     fully recovered in dtSibling. (ZHONG)
   *)
    fun extractDcons (tyc as T.GENtyc { kind = T.DATATYPE dt, ... }) =
	let val {index,freetycs,family as {members,...},...} = dt
	    val {dcons,sign,lazyp,...} = Vector.sub(members,index)
	    fun expandTyc(T.PATHtyc _) =
		bug "expandTyc:PATHtyc" (* use expandTycon? *)
	      | expandTyc(T.RECtyc n) = dtSibling(n,tyc)
	      | expandTyc(T.FREEtyc n) =
		((List.nth(freetycs,n))
		 handle _ => bug "unexpected freetycs in extractDcons")
	      | expandTyc tyc = tyc

	    fun expand ty = mapTypeFull expandTyc ty

	    fun mkDcon({name,rep,domain}) =
		T.DATACON{name = name, rep = rep, sign = sign, lazyp = lazyp,
			typ = dconType (tyc, Option.map expand domain),
			const = case domain of NONE => true | _ => false}

	in map mkDcon dcons
	end
      | extractDcons T.ERRORtyc = bug "extractDcons ERRORtyc"
      | extractDcons (T.DEFtyc _) = bug "extractDcons DEFtyc"
      | extractDcons _ = bug "extractDcons"

    fun mkStrict 0 = []
      | mkStrict n = true :: mkStrict(n-1)

  (* used in ElabSig for datatype replication specs, where the tyc arg
   * is expected to be either a T.GENtyc/DATATYPE or a PATHtyc. *)
    fun wrapDef (tyc as T.DEFtyc _, _) = tyc
      | wrapDef (tyc,s) =
	let val arity = tyconArity tyc
	    val name = tycName tyc
	    val args = boundargs arity
	 in T.DEFtyc{stamp=s,strict=mkStrict arity,path=IP.IPATH[name],
		   tyfun=T.TYFUN{arity=arity,body=T.CONty(tyc,args)}}
	end

  (* eta-reduce a type function: \args.tc args => tc *)
    fun unWrapDef1 (tyc as T.DEFtyc{tyfun=T.TYFUN{body=T.CONty(tyc',args),arity},...}) =
	 let fun formals((T.IBOUND i)::rest,j) = if i=j then formals(rest,j+1) else false
	       | formals(nil,_) = true
	       | formals _ = false
	  in if formals(args,0) then SOME tyc' else NONE
	 end
      | unWrapDef1 tyc = NONE

  (* closure under iterated eta-reduction *)
    fun unWrapDefStar tyc = (case unWrapDef1 tyc
	    of SOME tyc' => unWrapDefStar tyc'
	     | NONE => tyc)

  (* dummyTyGen produces a generator of dummy types with names X0, X1, etc.
   * These are used to to instantiate type metavariables in top-level val
   * decls that are not generalized because of the value restriction. *)
    fun dummyTyGen () : unit -> Types.ty =
	let val count = ref 0
	    fun next () = (count := !count + 1; !count)
	    fun nextTy () =
		let val name = "X"^Int.toString(next())
		 in T.CONty(T.GENtyc{stamp = ST.special name,
				 path = IP.IPATH[S.tycSymbol name],
				 arity = 0, eq = ref T.NO,
				 kind = T.ABSTRACT BT.boolTycon,
				 stub = NONE},
			  [])
		end
	 in nextTy
	end

  (* a crude translation of types to strings *)
    fun tyToString ty = let
	  fun showargs tys = (case tys
		 of nil => ""
		  | [ty] => tyToString ty
		  | ty::tys => concat[tyToString ty, ",", showargs tys]
		(* end case *))
	  in
	    case ty
	     of T.VARty(ref kind) =>
		(case kind
		   of T.INSTANTIATED _ => "<tv:INSTANTIATED>"
		    | T.OPEN _ => "<tv:OPEN>"
		    | T.UBOUND _ => "<tv:UBOUND>"
		    | T.OVLDV _ => "<tv:OVLDV>"
		    | T.OVLDI _ => "<tv:OVLDI>"
		    | T.OVLDW _ => "<tv:OVLDW>"
		    | T.LBOUND _ => "<tv:LBOUND>"
		  (* end case *))
	      | T.IBOUND n => concat["<", Int.toString n, ">"]
	      | T.CONty(tyc,args) =>
		if BT.isArrowType ty
		then concat["(", tyToString (BT.domain ty), " -> ",
			    tyToString (BT.range ty), ")"]
		else if null args
		   then Symbol.name(tycName tyc)
		   else concat["(", showargs args, ") ", Symbol.name(tycName tyc)]
	      | T.POLYty{tyfun=T.TYFUN{body,arity},...} =>
		concat["<P", Int.toString arity, ">[", tyToString body, "]"]
	      | T.WILDCARDty => "<wc>"
	      | T.UNDEFty => "<ud>"
	      | T.MARKty (ty,_) => tyToString ty
	  end

  (* return size and signedness information about integer and word types.  We use a width
   * of zero for IntInf.int.
   *)
    fun numInfo ty = let
	  fun int w = {wid = w, signed = true}
	  fun word w = {wid = w, signed = false}
	  val ty = prune ty
	  in
	    if equalType(ty, BT.intTy) then int Target.defaultIntSz
	    else if equalType(ty, BT.wordTy) then word Target.defaultIntSz
	    else if equalType(ty, BT.intinfTy) then int 0
	    else if equalType(ty, BT.int32Ty) then int 32
	    else if equalType(ty, BT.int64Ty) then int 64
	    else if equalType(ty, BT.word8Ty) then word 8
	    else if equalType(ty, BT.word32Ty) then word 32
	    else if equalType(ty, BT.word64Ty) then word 64
	    else ErrorMsg.impossible(concat[
		"TypeUtil.numInfo(", tyToString ty, ")"
	      ])
	  end

    fun numInRange (n, ty) = let
	    fun pow2 w = IntInf.<<(1, Word.fromInt w)
	    in
	      case numInfo ty
	       of {wid=0, ...} => true (* IntInf.int literals are always in range! *)
		| {wid, signed=true} => let
		    val limit = pow2(wid-1)
		    in
		      (~limit <= n) andalso (n < limit)
		    end
		| {wid, ...} => (n < pow2 wid) (* we assume that n > 0, since it is unsigned *)
	      (* end case *)
	    end

    fun dataconName (T.DATACON {name, ...}) = name

    fun dataconSign (T.DATACON {sign,...}) = sign

    fun dataconIsConst (T.DATACON {const,...}) = const

end (* top local - imports *)
end (* structure TypesUtil *)
