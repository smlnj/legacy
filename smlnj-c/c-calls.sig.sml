(* c-calls.sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Signature for an interface for calling user C functions from SML/NJ.
 *)

signature C_CALLS = 
    sig
	(* an abstract pointer type *)
	eqtype caddr
	val NULL : caddr
        val free : caddr -> unit
	val index : (caddr * int) -> caddr
	val difference : (caddr * caddr) -> Word32.word

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
	  | CstringT                    (* C's null-terminated "char *" *)
	  | CstructT of ctype list
	  | CunionT of ctype list
	  | CvectorT of (int * ctype)
	  | CvoidT

	datatype cdata = 
	    Caddr of caddr
	  | Carray of cdata Array.array              
	  | Cchar of char                            
	  | Cdouble of real
	  | Cfloat of real
	  | Cfunction of cdata list -> cdata
	  | Cint of Word32.word
	  | Clong of Word32.word
	  | Cptr of cdata
	  | Cshort of Word32.word
 	  | Cstring of string           (* C's null-terminated "char *" *)
	  | Cstruct of cdata list
	  | Cunion of cdata
	  | Cvector of cdata Vector.vector
	  | Cvoid

	val datumMLtoC : ctype -> cdata -> (caddr * caddr list)
	val datumCtoML : ctype -> caddr -> cdata

	(* number of bytes ctype uses in the C heap *)
	(* size does not include pointed-to substructure (e.g. Cstring,Cptr) *)
	val sizeof : ctype -> int
	
	(* total number of bytes the datum will occupy on the C heap *)
        (* unlike sizeof, this includes all pointed-to substructure *)
	val sizeofDatum : cdata -> int

	val hasType : cdata -> ctype

	exception EmptyAggregate
	exception AggregateTooBig
	exception BadReturnType of ctype
	exception BadArgumentType of ctype
	exception NotAPtr of ctype
	exception UnimplementedForType

	val registerCFn : (string * ctype list * ctype) -> 
	                  (cdata list -> (cdata * caddr list))

	val registerAutoFreeCFn : (string * ctype list * ctype) -> 
	                          (cdata list -> cdata)
    end (* signature C_CALLS *)







