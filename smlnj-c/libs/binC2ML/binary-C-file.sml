(* COPYRIGHT (c) 1996  Bell Laboratories, Lucent Technologies
 *
 * binaryC convertor
 *
 * this functor provides functions to convert binary C data 
 * (as stored in e.g. a Word8Vector) to/from structured C data
 * (as used in the SML/NJ-C interface).
 *)

functor  BinaryC(structure C : C_CALLS) : BINARY_C = 
    struct
	structure W8V = Word8Vector
	structure W8 = Word8

	structure C = C
	open C

	val toChar = Char.chr o W8.toInt
	val fromChar = W8.fromInt o Char.ord

	fun vecToCvec v = 
	    let val len = W8V.length v
	    in
		(Cvector(Vector.tabulate(len,
					 fn i => Cchar(toChar(W8V.sub(v,i))))),
		 CvectorT(len,CcharT))
	    end

	fun cVecToVec (cv,n) = 
	    W8V.tabulate (n,fn i => let val Cchar c = Vector.sub(cv,i)
				    in fromChar c
				    end)

	fun okTypeForFile CcharT = true
	  | okTypeForFile CintT = true
	  | okTypeForFile CdoubleT = true
	  | okTypeForFile CfloatT = true
	  | okTypeForFile (CstructT l) = 
	    foldr (fn (x,y) => y andalso (okTypeForFile x)) true l
	  | okTypeForFile (CarrayT(_,t)) = okTypeForFile t
	  | okTypeForFile (CvectorT(_,t)) = okTypeForFile t
	  | okTypeForFile _ = false

	exception NonFlatFileType
	fun fromBits typ v = 
	    let val _ = okTypeForFile typ orelse raise NonFlatFileType
		val (v,vtype) = vecToCvec v
		(* Warning: the next lines perform a cast *)
		val (p,plist) = datumMLtoC (CptrT vtype) (Cptr v)
		val (Cptr res) = datumCtoML (CptrT typ) p
	    in	app free plist;
		res
	    end

	fun toBits typ datum = 
	    let val _ = okTypeForFile typ orelse raise NonFlatFileType
		val szb = sizeof typ
		(* Warning: the next lines perform a cast *)
		val (p,plist) = datumMLtoC (CptrT typ) (Cptr datum)
		val Cptr (Cvector res) = 
		    datumCtoML (CptrT (CvectorT(szb,CcharT))) p
	    in	app free plist;
		cVecToVec (res,szb)
	    end

end  (* functor BinaryCFile *)

