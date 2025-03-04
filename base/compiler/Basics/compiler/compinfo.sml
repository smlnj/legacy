(* Basics/compiler/compinfo.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* This version replaces the compinfo.sml in ElabData/main/compinfo.sml *)

(* The CompInfo structure provides a set of references to resources 
 * required during compilation, such as a (fresh) stamp generator, an Lvar
 * generator (using a revised LambdaVar structure), and various values related
 * to error messages. 
 * DBM: this file will need to be revised when newpp revisions of ErrorMsg, Source,
 * SourceMap are adopted.
 *)

signature COMP_INFO =
sig

  val mkStamp : (unit -> Stamps.stamp) ref
  val mkLvar : (string option -> LambdaVar.lvar) ref

  val sourceName: string ref

  val anyErrors: (unit -> bool) ref
  val error : ErrorMsg.errorFn ref		      
  val errorMatch: (SourceMap.region -> string) ref

  val reset : Source.source -> unit
				  
end (* signature COMP_INFO *)  


structure CompInfo : COMP_INFO =
struct

local (* imports *)

  structure ER = ErrorMsg
  structure SR = Source
  structure SM = SourceMap  
  structure ST = Stamps
  structure LV = LambdaVar

in		   

  val mkStamps = ref (ST.mkStamp)
  val mkLvar = ref (LV.mkLvar)

  val sourceName = ref ""

  val anyErrors = ref (fn () -> false)
  val error : (SM.region -> EM.complainer) ref =
      ref (fn x => raise Fail "CompInfo.error uninitialized")
  val errorMatch : (SM.region -> string) ref =
      ref (fn x => raise Fail "CompInfo.errorMatch uninitialized")

  (* reset : SR.source -> unit
   * (re)initializes the compiler info references based on a given input source *)
  fun reset (source as {fileOpened,...}: SR.source) : unit =
      let val { error, errorMatch, anyErrors } = ErrorMsg.errors source
       in ST.reset ();
	  LV.reset ();					
	  sourceName := fileOpened;
	  mkStamp := ST.mkStamp;
	  mkLvar := LV.mkLvar;
	  anyErrors := (fn () => !anyErrors);
	  error := error;
	  errorMatch := errorMatch
      end

end (* local - imports *)
end (* structure CompInfo *)
