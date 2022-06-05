(* c-calls.sml 
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * SML/NJ interface for calling user C functions (SMLNJ-C)
 *
 * It is critical that this file agree with:
 *         runtime/c-libs/smlnj-ccalls/c-calls.h
 *         runtime/c-libs/smlnj-ccalls/c-calls.c
 *)

functor CCalls (structure CCInfo : CC_INFO) : C_CALLS  = 
    struct
	structure U = Unsafe
	structure UC = U.CInterface

	val maxWordSzB = 4

	type word = Word32.word

	structure C = CCInfo

	(* assert that the C types are not larger than 32 bits *)
	val _ = if (C.longSzB > maxWordSzB) orelse
	           (C.intSzB > maxWordSzB) orelse
		   (C.ptrSzB > maxWordSzB) 
		then 
		    raise General.Fail "size of C word too big for SMLNJ-C"
		else ();

	val NArgs = 15  (* must agree with c-calls.h & c-calls.c *)

	val say : string -> unit = print

	(* implementation of an abstract pointer type *)
	structure CAddress :> sig eqtype caddr
                                  val NULL : caddr
				  val index : (caddr * int) -> caddr
				  val difference : (caddr * caddr) -> word
			      end =
	    struct
		type caddr = word

		val NULL : word = 0w0

		(* index and difference assume pointers to bytes *)
		fun index (p:caddr,i:int) = Word32.+(p,(Word32.fromInt i))
		fun difference (p:caddr,q:caddr) = 
		    Word32.-(p,q)
	    end
	open CAddress

        (* NOTE: order (i.e. naming) of this data type is critical
         * because the C side of the interface (c-calls.c, c-calls.h, etc.)
         * makes assumptions about variant tags.
         *)
	datatype cdata = 
	    Caddr of caddr
	  | Carray of cdata Array.array
	  | Cchar of char
	  | Cdouble of real
	  | Cfloat of real
	  | Cfunction of cdata list -> cdata
	  | Cint of word
	  | Clong of word
	  | Cptr of cdata
	  | Cshort of Word32.word
	  | Cstring of string  (* 'char *' *)
	  | Cstruct of cdata list
	  | Cunion of cdata
	  | Cvector of cdata Vector.vector
	  | Cvoid

	datatype ctype = 
	    CaddrT
	  | CarrayT of (int * ctype)
	  | CcharT
	  | CdoubleT
	  | CfloatT
	  | CfunctionT of (ctype list * ctype)
	  | CintT 
	  | ClongT
	  | CptrT of ctype
	  | CshortT
	  | CstringT 
	  | CstructT of ctype list
	  | CunionT of ctype list
	  | CvectorT of (int * ctype)
	  | CvoidT

	(* ctype' is the rewritten representation of ctype that extends
	 * the latter with size and (sometimes) with alignment information.
         *)
	datatype ctype' = 
	    CaddrT'
	  | CarrayT' of {nelems:int,elemtyp:layout}
	  | CcharT'
	  | CdoubleT'
	  | CfloatT'
	  | CfunctionT' of {argtypes:layout list,rettype:layout}
	  | CintT'
	  | ClongT'
	  | CptrT' of layout
	  | CshortT'
	  | CstringT'
	  | CstructT' of layout list
	  | CunionT' of layout list
	  | CvectorT' of {nelems:int,elemtyp:layout}
	  | CvoidT'
	  | padT'
	withtype layout = {typ:ctype',size:int,align:int option}

	val arrayCode = "A"
	val intCode = "I"
        val shortCode = "i"  (* baby int *)
	val longCode = "L"
	val charCode = "C"
	val doubleCode = "D"
	val floatCode = "R"
	val functionCode = "F" 
	val addrCode = "@"
	val stringCode = "S"
	val openStructCode = "("
	val closeStructCode = ")"
	val openUnionCode = "<"
	val closeUnionCode = ">"
	val vectorCode = "B"
	val voidCode = "V"
	val ptrCode = "P"
	val padCode = "#"

	val cat = String.concat
	val largest = foldr Int.max 0
	val sum = foldr (op +) 0

	fun forAll _ [] = true
	  | forAll p (x::xs) = (p x) andalso (forAll p xs)

	exception EmptyAggregate
	exception AggregateTooBig

	fun hasType (Caddr _) = CaddrT
	  | hasType (Carray a) = 
	    (* how to handle 0-length arrays? *)
	    CarrayT(Array.length a,
		    hasType(Array.sub(a,0)) handle Subscript => CvoidT)
	  | hasType (Cchar _) = CcharT
	  | hasType (Cdouble _) = CdoubleT
	  | hasType (Cfloat _) = CfloatT
	  | hasType (Cshort _) = CshortT
	  | hasType (Cint _) = CintT
	  | hasType (Clong _) = ClongT
	  | hasType (Cptr p) = CptrT (hasType p)
	  | hasType (Cstring _) = CstringT
	  | hasType (Cstruct l) = CstructT (map hasType l)
	  | hasType (Cunion u) = CunionT [hasType u]
	  | hasType (Cvector v) = 
	    (* how to handle 0-length vectors? *)
	    CvectorT(Vector.length v,
		    hasType(Vector.sub(v,0)) handle Subscript => CvoidT)
	  | hasType Cvoid = CvoidT

        fun dataSz (Cint _) = C.intSzB
	  | dataSz (Cshort _) = C.shortSzB
	  | dataSz (Clong _) = C.longSzB
          | dataSz (Cstring s) = (String.size s)*C.charSzB + 1
          | dataSz (Cchar _) = C.charSzB
          | dataSz (Cdouble _) = C.doubleSzB
          | dataSz (Cfloat _) = C.floatSzB

	fun stringSpace (Cstring s) = size s + 1
	  | stringSpace (Carray a) = 
	    ((stringSpace (Array.sub(a,0))) handle Subscript => 0)
	  | stringSpace (Cstruct l) = sum (map stringSpace l)
	  | stringSpace (Cunion u) = stringSpace u
	  | stringSpace (Cvector a) = 
	    ((stringSpace (Vector.sub(a,0))) handle Subscript => 0)
	  | stringSpace _ = 0
		
	(* alignment/padding computations for the C side *)
	(* see "C -- A Reference Manual" by S. Harbison and G. Steele, Jr.
         * for details on how C aligns union/struct members and array elems.
         *)
	(* put these into rewrite if that's the only place they're used *)
	fun roundUp (i,align) = 
	    let val r = Int.rem(i,align)
	    in if r = 0 then i else (i div align + 1) * align
	    end

	fun computePadSz (sz,align) = roundUp(sz,align)-sz
	fun mkPad size = {typ=padT',size=size,align=NONE}

	fun memberAlign init l = 
	    foldr (fn ({align=SOME al,size,typ},b) => Int.max (al,b)
	            | (_,b) => b) init l

        fun foldMemberSize f init = 
	    foldr (fn ({align,size,typ},b) => f(size,b)) init

	fun memberSum x = foldMemberSize (op +) 0 x
	fun memberMax x = foldMemberSize Int.max 0 x

	(* rewrite: ctype -> {typ:ctype',size:int,align:int option} *)
	(* size is the size of the ctype thing
         * NOT including the size of pointed-to sub-structure
         * e.g., sub-structure of Cptrs or Cstrings
         *)
	fun rewrite CaddrT = {typ=CaddrT',size=C.ptrSzB,align=SOME C.ptrSzB}
 	  | rewrite (CarrayT (n,t)) = 
	    let val t' as {align,size,typ} = rewrite t
	    in {typ=CarrayT'{nelems=n,elemtyp=t'},size=n*size,align=align}
	    end
	  | rewrite CcharT = {typ=CcharT',size=C.charSzB,align=SOME C.charSzB}
	  | rewrite CdoubleT = {typ=CdoubleT',size=C.doubleSzB,
				align=SOME C.doubleSzB}
	  | rewrite CfloatT = {typ=CfloatT',size=C.floatSzB,
			       align=SOME C.floatSzB}
	  | rewrite CintT  = {typ=CintT',size=C.intSzB,align=SOME C.intSzB}
	  | rewrite CshortT = {typ=CshortT',size=C.shortSzB,align=SOME C.shortSzB}
	  | rewrite ClongT = {typ=ClongT',size=C.longSzB,align=SOME C.longSzB}
	  | rewrite (CptrT t) = 
	    let val t' = rewrite t
	    in {typ=CptrT' t',size=C.ptrSzB,align=SOME C.ptrSzB}
	    end
	  | rewrite CstringT = {typ=CstringT',size=C.ptrSzB,
				align=SOME C.ptrSzB}
	  | rewrite (CunionT l) = 
	    let val l' = map rewrite l
		val al = memberAlign 0 l'
		val sz = roundUp(memberMax l',al)
	    in {typ=CunionT' l',size=sz,align=SOME al}
	    end
	  | rewrite (CstructT l) = 
	    let val l' = map rewrite l
		val al = memberAlign 0 l'
		fun addPads ([],acc) = []
		  | addPads ((x as {typ,size,align=SOME al})::xs,acc) = 
		    let val slack = computePadSz (acc,al)
			val res = x::(addPads (xs,acc+slack+size))
		    in if slack = 0 then res
		       else mkPad slack :: res
		    end
		val l'' = addPads (l',0)
		val sz = memberSum l''
		val extra = computePadSz (sz,al)
		val l''' = if extra=0 then l'' else l'' @ [mkPad extra]
	    in
		{typ=CstructT' l''',size=roundUp(sz,al),align=SOME al}
	    end
	  | rewrite (CfunctionT (argtypes,rettype)) = 
	    let val atypes = map rewrite argtypes
		val rtype = rewrite rettype
	    in {typ=CfunctionT'{argtypes=map rewrite argtypes,
				rettype=rewrite rettype},
		size=C.ptrSzB,align=SOME C.ptrSzB}
	    end
 	  | rewrite (CvectorT (n,t)) = 
	    let val t' as {align,size,typ} = rewrite t
	    in {typ=CvectorT'{nelems=n,elemtyp=t'},size=n*size,align=align}
	    end
	  | rewrite CvoidT = {typ=CvoidT',size=0,align=NONE}

        fun typeToCtl arg = 
	    let val charRange = 255 (* not 256 since 0 is C string delim *)
		fun  uToS bytes c = (* unsigned int to string *)
		let fun aux (0,i,acc) = if i <> 0 then raise AggregateTooBig
					else acc
		      | aux (n,i,acc) = 
			let val q = i div charRange
			    val r = i mod charRange + 1
			in aux(n-1,q,String.str(Char.chr r) ^ acc)
			end
		in aux (bytes,c,"")
		end
		fun aux {typ=CaddrT',...} = addrCode
		  | aux {typ=CarrayT'{nelems,elemtyp=elemtyp as {size,...}},
			 ...} =  
		    arrayCode^(uToS 2 nelems)^(uToS 2 size)^(aux elemtyp)
		  | aux {typ=CintT',size,...} = intCode^(uToS 1 size)
		  | aux {typ=CshortT',size,...} = shortCode^(uToS 1 size)
		  | aux {typ=ClongT',size,...} = longCode^(uToS 1 size)
		  | aux {typ=CcharT',...} = charCode
		  | aux {typ=CdoubleT',...} = doubleCode
		  | aux {typ=CfloatT',...} = floatCode
		  | aux {typ=CfunctionT'{argtypes,rettype},...} = 
		    functionCode^(uToS 1 (length argtypes))^
		    (cat (map aux argtypes))^(aux rettype)
		  | aux {typ=CptrT' (t as {size,align=SOME al,...}),...} = 
		    ptrCode^(uToS 4 size)^(uToS 1 al)^(aux t)
		  | aux {typ=CstringT',...} = stringCode
		  | aux {typ=CstructT' [],...} = raise EmptyAggregate
		  | aux {typ=CstructT' l,size,...} =  
		    (* need to put size in here (?) *)
		    openStructCode^(cat (map aux l))^closeStructCode
		  | aux {typ=CunionT' [],...} = raise EmptyAggregate
		  | aux {typ=CunionT' l,size,...} = 
		    openUnionCode^(uToS 1 size)^(cat (map aux l))^
		    closeUnionCode
		  | aux {typ=CvectorT'{nelems,elemtyp=elemtyp as {size,...}},
			 ...} =  
		    vectorCode^(uToS 2 nelems)^(uToS 2 size)^(aux elemtyp)
		  | aux {typ=CvoidT',...} = voidCode
		  | aux {typ=padT',size,...} = padCode^(uToS 1 size)
	    in	aux arg
	    end
	        
	val libname = "SMLNJ-CCalls"
	fun cfun x = UC.c_function libname x

(**     (* for debugging *)
	fun cfun s = (print "binding C function '";
		      print s;
		      print "'\n";
		      UC.c_function libname s)
**)

	fun cbind (mf as (moduleName, funName)) = 
	    let val f = UC.bindCFun mf
	    in
		if (U.cast f <> 0) then U.cast f
		else (print ("can't find "^moduleName ^ "." ^ funName^"\n");
		      raise UC.CFunNotFound(moduleName ^ "." ^ funName))
	end

	type arg_desc = string     (* type requirement *)

	fun mkArgDesc t = typeToCtl (rewrite t)

	type dummy = unit

        val CFnDoCCall : ((dummy -> dummy) *                 
		       int *
		       arg_desc list * 
		       arg_desc *
		       cdata list *
		       bool) -> (cdata * caddr list) = cfun "c_call"
	    
	exception BadReturnType of ctype
	exception BadArgumentType of ctype
	exception NotAPtr of ctype
        exception UnimplementedForType
	exception TooManyArgs of int

	(* valid return types are types are "ground" types *)
	fun validRetType CintT = true
	  | validRetType CshortT = C.shortSzB = C.intSzB
	  | validRetType ClongT = C.longSzB = C.intSzB
	  | validRetType CvoidT = true
	  | validRetType CstringT = true
	  | validRetType CaddrT = true
	  | validRetType CfloatT = true
	  | validRetType CdoubleT = true
	  | validRetType (CptrT _) = true
          | validRetType CcharT = true
	  | validRetType _ = false

	fun validFunctionTypes (CfunctionT (args,ret)) = 
	    validRetType ret andalso (forAll validArgType args)
	  | validFunctionTypes (CarrayT(_,t)) = validFunctionTypes t
	  | validFunctionTypes (CptrT t) = validFunctionTypes t
	  | validFunctionTypes (CstructT l) = forAll validFunctionTypes l
	  | validFunctionTypes (CunionT l) = forAll validFunctionTypes l
	  | validFunctionTypes (CvectorT(_,t)) = validFunctionTypes t
	  | validFunctionTypes _ = true
	and validArgType CaddrT = true
	  | validArgType CintT = true
	  | validArgType CshortT = C.shortSzB = C.intSzB
	  | validArgType ClongT = C.longSzB = C.intSzB
	  | validArgType (p as CptrT _) = validFunctionTypes p
	  | validArgType CstringT = true
	  | validArgType (f as CfunctionT _) = validFunctionTypes f
	  | validArgType _ = false

	(* need to do something here about function types *)
	fun validPtr (CptrT _) = true
	  | validPtr CstringT = true
	  | validPtr _ = false

	fun register freeFlag (name:string,args:ctype list,res:ctype) = 
	    let val _ = (validRetType res) orelse (raise (BadReturnType res))
		val _ = app (fn x => ((validArgType x) orelse 
				      (raise (BadArgumentType x));
				      ())) args
		val nargs = length args
                val _ = (nargs > NArgs) andalso (raise (TooManyArgs nargs))
		val f : dummy -> dummy = cbind (libname,name)
		val args' = map mkArgDesc args
		val res' =  mkArgDesc res
	    in
(*		say ("function \"" ^ name ^ "\" registered\n");  *)
		fn x => CFnDoCCall (f,nargs,args',res',x,freeFlag)
 		handle (e as (OS.SysErr (msg,errno))) =>
		         (say ("C call error: "^msg);
			  raise e)
		     | x => raise x
	    end

	fun registerAutoFreeCFn args = #1 o (register true args)
	val registerCFn = register false

	val CFnDatumMLtoC : arg_desc * cdata -> (caddr * caddr list) = 
	                        cfun "datumMLtoC"
	fun datumMLtoC t = 
	    let val _ = (validPtr t) orelse (raise (NotAPtr t))
		val desc = mkArgDesc t
	    in
		fn d => CFnDatumMLtoC (desc,d)
	    end

	val CFnDatumCtoML : arg_desc * caddr -> cdata = cfun "datumCtoML"
	fun datumCtoML t = 
	    let fun noFns (CfunctionT _) = false
		  | noFns (CstructT l) = forAll noFns l
		  | noFns (CunionT l) = forAll noFns l
		  | noFns (CarrayT (_,t)) = noFns t
		  | noFns (CvectorT (_,t)) = noFns t
		  | noFns _ = true
		val _ = (validPtr t) orelse (raise (NotAPtr t))
		val _ = (noFns t) orelse (raise UnimplementedForType)
		val desc = mkArgDesc t
	    in
		fn p => CFnDatumCtoML (desc,p)
	    end


	(* sizeof : ctype -> int *)
        (* returns the number of bytes to represent ctype as 
         * a basic C data structre, NOT including any substructure
         * such as Cstrings or Cptrs
         *)
        val sizeof = #size o rewrite   (* this is overkill *)

	(* sizeofDatum : cdata -> int *)
        (* returns the number of bytes needed to represent cdata
         * as a C data structure, INCLUDING substructure such 
         * as Cstrings and Cptrs
         * 
	 * Only basic (flat) types currently work.
         *)
	val sizeofDatum = dataSz

        local val free' = registerAutoFreeCFn("free",[CaddrT],CvoidT)
        in fun free p = (free' [Caddr p]; ())
        end	

    end (* functor CCalls *)
