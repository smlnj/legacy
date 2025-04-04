(* COPYRIGHT (c) 1998 Bell Laboratories *)
(* elabtype.sml *)

structure ElabType : ELABTYPE =
struct

local (* imports *)

  structure EM = ErrorMsg
  structure S  = Symbol
  structure SS = SpecialSymbols
  structure SP = SymPath
  structure IP = InvPath
  structure SM = SourceMap
  structure SE = StaticEnv
  structure L  = Lookup
  structure B  = Bindings
  structure T  = Types
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure TS = TyvarSet
  structure EU = ElabUtil
  structure VC = VarCon	     

  open Symbol Absyn Ast Types TypesUtil (* unopen these! *)

in

val debugging : bool ref = ElabControl.etdebugging (* ref false *)

(* say : string -> unit *)
val say = Control_Print.say

(* debugmsg : string -> unit *)
fun debugmsg (msg: string) =
    if !debugging then (say msg; say "\n") else ()

(* bug : string -> 'a *)
fun bug msg = ErrorMsg.impossible("ElabType: " ^ msg)

(* infix arrow tycon *)
val --> = BT.-->
infix -->


(** ELABORATING TYPES and TYPE DECLARATIONS **)

(* elabTyvar : Ast.tyvar -> T.tyvar
 * always produces a UBOUND tyvar *)
fun elabTyvar (tyv: Ast.tyvar) =
    case tyv
      of Ast.Tyv vt => T.mkTyvar (mkUBOUND(vt))
       | Ast.MarkTyv (tyv,region) => elabTyvar tyv  (* discard MarkTy and its region *)

(* elabTyvarList : Ast.tyvar list * SM.region -> T.tyvar list *)
fun elabTyvarList (tyvars: Ast.tyvar list, region) =
    let val tvs = map elabTyvar tyvars
        val names = map (fn (ref(UBOUND{name,...})) => name
                        | _ => bug "elabTyvarList") tvs
     in if EU.checkUniq names
        then tvs  (* tyvar names are unique *) 
        else (EM.errorRegion (region, "duplicate type variable name"); nil)
    end

(* elabType : Ast.ty * SE.staticEnv * SM.region -> T.ty * TS.tyvarset
 * exported *)
fun elabType (ast: Ast.ty, env: SE.staticEnv, region: SM.region)
            : (Types.ty * TS.tyvarset) =
    (case ast
       of VarTy vt =>
	    let val tyv = elabTyvar vt
	     in (VARty tyv, TS.singleton tyv)
	    end
	| ConTy (tyconPath, argtypes) => (* tyconPath : symbol list, argtypes : Ast.ty list *)
	    let val tycon =
		    if length tyconPath = 1 andalso S.name (hd tyconPath) = "->"
		    then BT.arrowTycon
		    else let val arity = length argtypes
			     val path = SP.SPATH tyconPath
			     val pathString = SP.toString path
			  in case L.lookTyc (env, path)
			      of NONE =>
				   (EM.errorRegion (region, "elabType: unbound " ^ pathString);
				    T.ERRORtyc)
			       | SOME tycon =>
				   (if TU.tyconArity tycon = arity (* check for expected tycon arity *)
				    then tycon
				    else (errorRegion (region,
						  concat ["elabType [ConTy]: tycon ",
							  pathString, " arity mismatch, ",
							  "expected arity ", Int.toString arity]);
					  T.ERRORtyc))
			 end		
		val (argtypes', tyvarset) = elabTypeList (argtypes, env, region)
	     in (CONty (tycon, argtypes'), tyvarset)
	    end
	| RecordTy fields =>
	    let val (fields',tyvarset) = elabRecordFields (fields, env, region)
	     in (BT.recordTy (EU.sortRecordFields fields'), tyvarset)
	    end
	| TupleTy ts =>
	    let val (lts1,lvt1) = elabTypeList (ts, env, region)
	     in (BT.tupleTy lts1,lvt1)
	    end
	| MarkTy (ty,region') => elabType (ty, env, region') (* end case *))

(* elabRecordFields : (S.symbol * Ast.ty) list * SE.staticEnv * SM.region
                      -> (S.symbol * T.ty) list * TS.tyvarset 
 * -- not exported *)
and elabRecordFields (fields, env, region: SM.region) =
    let fun folder ((lab,ty), (rest,tyvarset)) =
	      let val (ty', tyvarset') = elabType (ty, env, region)
		  val tyvarsetOp = TS.union (tyvarset', tyvarset)
	       in case tyvarsetOp  (* error checking for incompatible tyvarsets *)
		    of SOME tyvarset' => ((lab, ty')::rest, tyvarset')
		     | NONE => (EM.errorRegion (region, "ElabType: elabRecordFields -- incompatible tyvars");
			        (nil, TS.empty))
	      end
     in foldr folder ([],TS.empty) fields
    end

(* elabTypeList : Ast.ty list * SE.staticEnv * SM.region -> T.ty list * TS.tyvarset
                  -> T.ty list * TS.tyvarset
 * -- not exported *)
and elabTypeList (types: Ast.ty list, env: SE.staticEnv, region: SM.region) =
    let (* folder : Ast.ty * (T.ty list * TS.tyvarset) -> (T.ty list * TS.tyvarset) *)
	fun folder (ty: Ast.ty, (types: T.ty list, tyvarset: TS.tyvarset)) =
	      let val (ty', tyvarset') = elabType (ty, env, region)
	       in case TS.union (tyvarset', tyvarset)
		    of SOME tyvarset'' => (ty' :: types, tyvarset'')
		     | NONE => (EM.errorRegion (region, "ElabType: elabTypeList -- incompatible tyvars");
			        (nil, TS.empty))
	       end
      in foldr folder ([],TS.empty) types (* foldr to maintain the order of the types *)
     end

 (**** DATACON DECLARATIONS ****)
 exception ISREC

 (* elabDB: ([tyc:]T.tycon * [arg:]T.tyvar list * [name:]S.symbol? * [def:]? * [lazyp:]bool)
	    * SE.staticEnv * IP.path * SM.region
	    -> ? *)
(* elaborate a datatype declaration *)	       
 fun elabDB ((tyc, argTyvars, name, def, lazyp),
	     env, rpath:IP.path, region) =
    let val rhs = CONty(tyc, map VARty argTyvars)

	fun checkrec(_,NONE) = ()
	  | checkrec(_,SOME typ) =
	      let fun findname(VarTy _) = ()
		    | findname(ConTy([co],ts)) =
			if Symbol.eq (co,name) then (raise ISREC)
			else app findname ts
		    | findname(ConTy(_,ts)) = app findname ts
		    | findname(RecordTy lbs) = app (fn (_,t) => findname t) lbs
		    | findname(TupleTy ts) = app findname ts
		    | findname(MarkTy(t,_)) = findname t

	       in findname(typ)
	      end

         (* elabConstr : [cname:]S.symbol * [tyOp:]T.ty option  -- the Ast dcon spec
	                -> (S.symbol * bool * T.ty) list * TS.tyvarset *)
	fun elabConstr (cname: S.symbol, tyOp) =
	      (if EU.checkForbiddenCons cname
	       then error (concat["datatype \"", S.name name, "\" has forbidden constructor name: \"",
				 S.name cname, "\""])
	       else ();
	       (case tyOp
		  of SOME ty =>  (* non-constant dcon, where ty is its Ast domain type *)
		       let val (ty', tyvarset) = elabType (ty, env, region)
		        in ((cname, false, (ty' --> rhs)), tyvarset)
		       end
		   | NONE => ((cname,true,rhs),TS.empty)))  (* constant dcon *)

	val arity = length argTyvars

	val isrec = (app checkrec def; false) handle ISREC => true

	val (dcl,tvs) =
	    let fun folder (dcon, (dcons,tyvarset)) =
		      let val (dcon', tyvarset') = elabConstr dcon  (* dcon' : (symbol, bool, ty) *)
			  val u = TS.union (tyvarset', tyvarset)
		      in case u
			   of SOME tyvarset'' =>
			        (dcon' :: dcons, tyvarset'')  (* union successful, tyvarsets compatible *)
			    | NONE => (EM.errorRegion (region, "ElabDB:  -- incompatible tvarsets");
				       (nil, TS.empty))
		      end
	     in foldr folder ([], TS.empty) def
	    end
	val _ = EU.checkBoundTyvars (tvs, argTyvars)
	val _ = TU.bindTyvars argTyvars
	val sdcl = EU.sort3 dcl
	val (reps, sign) = ConRep.infer isrec sdcl
	fun bindDcons ((sym,const,typ),rep) =
	      let val _ = TU.compressTy typ
		  val typ =
		      if arity > 0
		      then POLYty {sign=mkPolySign arity,
				   tyfun=TYFUN{arity=arity,body=typ}}
		      else typ
	       in DATACON{name=sym, const=const, rep=rep,
                          sign=sign, typ=typ, lazyp=lazyp}
	      end
	fun bindDconslist ((r1 as (name,_,_))::l1,r2::l2) =
	      let val dcon = bindDcons (r1,r2)
		  val (dcl,e2) = bindDconslist (l1,l2)
	       in (dcon::dcl,SE.bind(name,B.CONbind dcon,e2))
	      end
	  | bindDconslist ([],[]) = ([],SE.empty)
	  | bindDconslist _ = bug "elabDB.bindDconslist"

     in if length sdcl < length dcl  (* duplicate constructor names *)
	then let fun member(x:string,[]) = false
		   | member(x,y::r) = (x = y) orelse member(x,r)
		 fun dups([],l) = l
		   | dups(x::r,l) =
		       if member(x,r) andalso not(member(x,l))
		       then dups(r,x::l)
		       else dups(r,l)
		 fun add_commas [] = []
		   | add_commas (y as [_]) = y
		   | add_commas (s::r) = s :: "," :: add_commas(r)
		 val duplicates = dups(map (fn (n,_,_) => S.name n) dcl,[])
	      in error
		   (concat["datatype ", S.name name,
			    " has duplicate constructor name(s): ",
			    concat(add_commas(duplicates))])

	     end
	else ();
	bindDconslist(sdcl, reps)
    end


(**** TYPE DECLARATIONS ****)

fun elabTBlist (tbl:Ast.tb list, notwith:bool, env0, rpath, region)
      : T.tycon list * S.symbol list * SE.staticEnv =
    let fun elabTB(tb: Ast.tb, env, region): (T.tycon * symbol) =
	    case tb
	      of Tb {tyc=name, def, tyvars} =>
		   let val tyvars' = elabTyvarList (tyvars, region) (* checks for duplicate tyvars *)
		       val (ty, tyvarset) = elabType (def, env, region)
		       val arity = length tyvars'
		       val _ = EU.checkBoundTyvars (tyvarset, tyvars')
		       val _ = TU.bindTyvars tyvars'
		       val _ = TU.compressTy ty
		       val tycon =
			   DEFtyc{stamp = Stamp.fresh (),
				  path = IP.extend (rpath, name),
				  strict = TU.calcStrictness (arity,ty),
				  tyfun = TYFUN{arity=arity, body=ty}}
		    in (tycon,name)
		   end
	      | MarkTb(tb',region') => elabTB (tb', env, region') (* proceed with new region *)
	fun loop (nil, tycons, names, env) = (rev tycons, rev names, env)
	  | loop (tb::rest, tycons, names, env) =
	      let val env' = if notwith then env0 else SE.atop(env,env0)
		  val (tycon, name) = elabTB (tb,env',region)
	       in loop (rest, tycon::tycons, name::names,
		        SE.bind(name,B.TYCbind tycon,env))
	      end
     in loop (tbl, nil, nil, SE.empty)
    end

fun elabTYPEdec (tbl: Ast.tb list, env, rpath, region) : Absyn.dec * SE.staticEnv =
    let	val _ = debugmsg ">>elabTYPEdec"
	val (tycs, names, env') =
            elabTBlist (tbl, true, env, rpath, region)
	val _ = debugmsg "--elabTYPEdec: elabTBlist done"
     in if EU.checkUniq names
	then ()
	else EM.errorRegion (region, "duplicate type definition");
	debugmsg "<<elabTYPEdec";
	(TYPEdec tycs, env')
    end

fun elabDATATYPEdec ({datatycs,withtycs}, env0, sigContext,
                      sigEntEnv, isFree, rpath, region) = 
    let (* predefine datatypes *)
	val _ = debugmsg ">>elabDATATYPEdec"
        val error = EM.error region
	fun preprocess region (Db{tyc=name,rhs=def,tyvars,lazyp}) =
	    let val tvs = elabTyvarList (tyvars, region)
		val strictName =
		    if lazyp
		    then S.tycSymbol(S.name name ^ "!")
		    else name
		val tyc = GENtyc{path = IP.extend(rpath,strictName),
				 arity = length tyvars,
				 stamp = Stamp.fresh (),
				 eq = ref DATA,
				 kind = TEMP,
				 stub = NONE}
		val binddef =
		    if lazyp then
			   DEFtyc{stamp = Stamp.fresh (),
				  tyfun=TYFUN{arity=length tyvars,
					      body=CONty(BT.suspTycon,
						    [CONty(tyc,map VARty tvs)])},
			          strict=map (fn _ => true) tyvars,
				  path=IP.extend(rpath,name)}

		    else tyc
	     in {tvs=tvs, name=name,def=def,region=region,
		 tyc=tyc, binddef=binddef,lazyp=lazyp,
		 strictName=strictName}
	    end
	  | preprocess _ (MarkDb(db',region')) = preprocess region' db'

        val dbs = map (preprocess region) datatycs
        val _ = debugmsg "--elabDATATYPEdec: preprocessing done"

        val envDTycs = (* staticEnv containing preliminary datatycs *)
	      foldl (fn ({name,binddef,...},env) =>
			   SE.bind(name, B.TYCbind binddef, env))
		       SE.empty dbs
        val _ = debugmsg "--elabDATATYPEdec: envDTycs defined"

	(* elaborate associated withtycs *)
	val (withtycs,withtycNames,envWTycs) =
	    elabTBlist (withtycs, false, SE.atop(envDTycs,env0), rpath, region)
        val _ = debugmsg "--elabDATATYPEdec: withtycs elaborated"

	(* check for duplicate tycon names *)
        val _ = if EU.checkUniq (map #name dbs @ withtycNames)
		then ()
		else EM.errorRegion (region, "duplicate type names in type declaration")
			     
        val _ = debugmsg "--elabDATATYPEdec: uniqueness checked"

        (* add lazy auxiliary withtycs if any *)
        val withtycs = map #binddef (List.filter #lazyp dbs) @ withtycs

	(* staticEnv containing only new datatycs and withtycs *)
	val envTycs = SE.atop(envWTycs, envDTycs)
	(* staticEnv for evaluating the datacon types *)
	val fullEnv = SE.atop(envTycs,env0)
        val _ = debugmsg "--elabDATATYPEdec: envTycs, fullEnv defined"

        val prelimDtycs = map #tyc dbs

        (* the following functions pull out all the flexible components
           inside the domains of the datatypes, and put them into the
           freetycs field in the DATATYPE kind; this way, future
           re-instantiations of the datatypes only need to modify the
           freetycs list, rather than all the domains (ZHONG)
         *)
        val freeTycsRef = ref ([] : tycon list, 0)
        fun regFree tyc =
          let val (ss, n) = !freeTycsRef
              fun h (x::rest, i) =
                   if eqTycon(tyc, x) then FREEtyc (i-1)
                   else h(rest, i-1)
                | h ([], _) =
                   let val _ = (freeTycsRef := (tyc::ss, n+1))
                    in FREEtyc n
                   end
           in h (ss, n)
          end

	fun transTyc (tyc as GENtyc { kind = TEMP, ... }) =
	    let fun g(tyc,i,x::rest) =
		    if eqTycon(tyc,x) then RECtyc i
                    else g(tyc,i+1,rest)
		  | g(tyc,_,nil) = tyc
	    in g(tyc,0,prelimDtycs)
	    end
	  | transTyc (tyc as GENtyc _) =
	    if isFree tyc then regFree tyc else tyc
	  | transTyc (tyc as (DEFtyc _ | PATHtyc _)) =
              if isFree tyc then regFree tyc else tyc
          | transTyc tyc = tyc

	fun transType t =
	    case TU.headReduceType t
	      of CONty(tyc, args) =>
		   CONty(transTyc tyc,map transType args)
	       | POLYty{sign,tyfun=TYFUN{arity,body}} =>
		   POLYty{sign=sign,
			  tyfun=TYFUN{arity=arity,body=transType body}}
	       | MARKty(tyc, region) => transType tyc
	       | t => t

	(* elaborate the definition of a datatype *)
	fun elabRHS ({tvs,name,def,region,tyc,lazyp,binddef,strictName},
		     (i,done)) =
	    let val (datacons,_) =
                      elabDB ((tyc, tvs, name, def, lazyp), fullEnv, rpath, region)
		fun mkDconDesc (DATACON{name,const,rep,sign,typ,lazyp}) =
		    {name=name, rep=rep,
		     domain=
		       if const then NONE
		       else case transType typ
			      of CONty (_,[dom,_]) => SOME dom
                               | POLYty {tyfun=TYFUN{body=CONty(_,[dom,_]),...},
					...} => SOME dom
			       | _ => bug "elabRHS"}
	     in (i+1,
		 {name=name,
		  dconNames=map (fn DATACON{name,...} => name) datacons,
		    (* duplicate names removed *)
		  dcons=datacons,
		  dconDescs=map mkDconDesc datacons,
		  tyc=tyc,
		  index=i,
		  lazyp=lazyp,
		  strictName=strictName} :: done)
	    end

        val (_,dbs') = foldl elabRHS (0,nil) dbs
	val dbs' = rev dbs'
        val _ = debugmsg "--elabDATATYPEdec: RHS elaborated"

        fun mkMember{name,dcons=DATACON { sign, ... } :: _,
		     dconDescs, tyc=GENtyc { stamp, arity, eq, ... },
		     dconNames,index,lazyp,strictName} =
    (* extract common sign from first datacon *)
	    (stamp, {tycname=strictName,dcons=dconDescs,arity=arity,
                     eq=eq,lazyp=lazyp,sign=sign})
	  | mkMember _ = bug "mkMember"

        val (mstamps, members) = ListPair.unzip (map mkMember dbs')

        val nstamps = Vector.fromList mstamps
        val nfamily = {members=Vector.fromList members,
		       properties = PropList.newHolder (),
                       (* lambdatyc=ref NONE, *)
                       mkey = Stamp.fresh ()}
        val nfreetycs =
          let val (x, n) = !freeTycsRef
              val _ = if length x = n then ()  (* sanity check *)
                      else bug "unexpected nfreetycs in elabDATATYPEdec"
           in rev x
          end
        val _ = debugmsg "--elabDATATYPEdec: members defined"

        fun fixDtyc{name,index,
		    tyc as GENtyc {path,arity,stamp,eq,kind,stub},
		    dconNames,dcons,dconDescs,lazyp,strictName} =
	    {old=tyc,
	     name=strictName,
	     new=GENtyc{path=path,arity=arity,stamp=stamp,eq=eq,
			stub=NONE,
			kind=DATATYPE{index=index,
				      stamps=nstamps,
				      family=nfamily,
				      freetycs=nfreetycs,
				      root=NONE,
				      stripped=false}}}
	  | fixDtyc _ = bug "fixDtyc"

        val dtycmap = map fixDtyc dbs'  (* maps prelim to final datatycs *)
        val _ = debugmsg "--elabDATATYPEdec: fixDtycs done"

	val finalDtycs = map #new dtycmap
        val _ = debugmsg "--elabDATATYPEdec: finalDtycs defined"

        val _ = EqTypes.defineEqProps(finalDtycs,sigContext,sigEntEnv)
        val _ = debugmsg "--elabDATATYPEdec: defineEqProps done"

        fun applyMap m =
            let fun sameTyc(GENtyc g1, GENtyc g2) =
		    Stamp.eq(#stamp g1, #stamp g2)
                  | sameTyc(tyc1 as DEFtyc _, tyc2 as DEFtyc _) =
		      equalTycon(tyc1, tyc2)
                  | sameTyc _ = false

                fun f(CONty(tyc, args)) =
	              let fun look({old,new,name}::rest) =
			      if sameTyc(old,tyc) then new else look rest
			    | look nil = tyc
		       in CONty(look m, map (applyMap m) args)
		      end
		  | f (POLYty{sign,tyfun=TYFUN{arity,body}}) =
		      POLYty{sign=sign,tyfun=TYFUN{arity=arity,body=f body}}
		  | f (MARKty(t,_)) = f t
		  | f t = t
             in f
            end

        fun augTycmap (tyc as DEFtyc{tyfun=TYFUN{arity,body},stamp,
                                     strict,path}, tycmap) =
            {old=tyc,name=IP.last (path, SS.errorTycId),
	     new=DEFtyc{tyfun=TYFUN{arity=arity,body=applyMap tycmap body},
			strict=strict,stamp=stamp,path=path}}
	    :: tycmap
	  | augTycmap _ = bug "augTycMap"

        (* use foldl to process the withtycs in their original order *)
        val alltycmap = foldl augTycmap dtycmap withtycs
        val _ = debugmsg "--elabDATATYPEdec: alltycmap defined"

        fun header(_, 0, z) = z
          | header(a::r, n, z) = header(r, n-1, a::z)
          | header([], _, _) = bug "header2 in elabDATATYPEdec"

	val finalWithtycs = map #new (header(alltycmap,length withtycs,[]))
        val _ = debugmsg "--elabDATATYPEdec: finalWithtycs defined"

        fun fixDcon (DATACON{name,const,rep,sign,typ,lazyp}) =
	    DATACON{name=name,const=const,rep=rep,sign=sign,lazyp=lazyp,
		    typ=applyMap alltycmap typ}

        val finalDcons = List.concat(map (map fixDcon) (map #dcons dbs'))
        val _ = debugmsg "--elabDATATYPEdec: finalDcons defined"

        val envDcons = foldl (fn (d as DATACON{name,...},e)=>
			         SE.bind(name,B.CONbind d, e))
	                     SE.empty
	                     finalDcons

        val finalEnv = foldl (fn ({old,name,new},e) =>
			         SE.bind(name,B.TYCbind new,e))
	                     envDcons alltycmap
        val _ = debugmsg "--elabDATATYPEdec: envDcons, finalEnv defined"

     in if EU.checkUniq (List.concat(map #dconNames dbs'))
	then ()
        else EM.errorRegion (region, "duplicate datacon names in datatype declaration");
        debugmsg "<<elabDATATYPEdec";
	(finalDtycs,finalWithtycs,finalDcons,finalEnv)
    end (* fun elabDATATYPEdec0 *)

end (* local *)
end (* structure ElabType *)
