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

  val mkStamp : unit -> Stamps.stamp
  val mkLvar : unit -> LambdaVar.lvar
    (* type will change to "string option -> lvar", with new LambdaVar in dbm branch *)

  val source : unit -> Source.inputSource option

  (* the functions below can in the future be accessed directly from ErrorMsg,
     which will base them on the current source stored here (CompInfo). *)
  val anyErrorsRef: (unit -> bool) ref
  val errorRef : ErrorMsg.errorFn ref		      
  val errorMatchRef: (SourceMap.region -> string) ref

  val reset : Source.inputSource -> unit
				  
end (* signature COMP_INFO *)  


structure CompInfo : COMP_INFO =
struct

local (* imports *)

  structure EM = ErrorMsg
  structure SR = Source
  structure SM = SourceMap  
  structure ST = Stamps
  structure LV = LambdaVar

in		   

  val mkStamp : unit -> ST.stamp = ST.fresh
  val mkLvar: unit -> LV.lvar = LV.mkLvar

  val sourceRef : SR.inputSource option ref = ref NONE
  fun source () : SR.inputSource option = !sourceRef

  val anyErrorsRef = ref (fn () => false)
  val errorRef : (SM.region -> EM.complainer) ref =
      ref (fn x => raise Fail "CompInfo.error uninitialized")
  val errorMatchRef : (SM.region -> string) ref =
      ref (fn x => raise Fail "CompInfo.errorMatch uninitialized")

  (* reset : SR.source -> unit
   * re-initializes the compiler global info for the given input source *)
  fun reset (source : SR.inputSource) : unit =
      let val { error, errorMatch, anyErrors } = ErrorMsg.errors source
       in ST.reset ();  (* reset generator counter for Stamps *)
	  LV.clear ();	(* reset generator counter for LambdaVar, will become reset(). *)
	  sourceRef := SOME source;
          (* The following should in the future be taken directly from 
           * ErrorMsg, which should access the current source from this
	   * structure, CompInfo *)
	  anyErrorsRef := (fn () => !anyErrors);
	  errorRef := error;
	  errorMatchRef := errorMatch
      end

  (* [DBM, 2025.03.05] It would be simpler if the current source was contained in * a ref
  in the ErrorMsg, or here as now done.  This would eliminate the need for the
  ErrorMsg.errors * function. After setting the source ref, we could just call
  ErrorMsg.error, etc.  * directly. The CompInfo.reset function could still take a source
  argument, which * it would then use to reset the value of the source in
  ErrorMsg. Alternatively, * ErrorMsg could depend on CompInfo for access to the source,
  which would be stored * in the CompInfo structure instead of "errorFn", etc. *)

end (* local - imports *)
end (* structure CompInfo *)
