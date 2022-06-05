(* COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * signature for binaryC convertor
 *
 *)

signature BINARY_C = 
    sig
	exception NonFlatFileType

	structure C : C_CALLS
	
	val fromBits : C.ctype -> Word8Vector.vector -> C.cdata
	val toBits : C.ctype -> C.cdata -> Word8Vector.vector
    end 

