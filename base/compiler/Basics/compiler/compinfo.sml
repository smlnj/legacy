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

  val source : unit -> Source.source

  (* the functions below can in the future be accessed directly from ErrorMsg,
     which will base them on the current source stored here (CompInfo). *)
  val anyErrors: bool ref

  val reset : Source.Source -> unit
				  
end (* signature COMP_INFO *)  


structure CompInfo : COMP_INFO =
struct

local (* imports *)

  structure SR = Source
  structure ST = Stamps
  structure LV = LambdaVar

in		   

  (* contents of sourceRef only set by the CompInfo.reset function, which should
   * be called at the beginning of any compilation. *)

  val sourceRef : SR.source ref = ref (SR.newSource NONE)
  fun source () : SR.source = !sourceRef

  val anyErrors = ref false

  (* reset : SR.source -> unit
   * re-initializes the compiler global info for the given input source *)
  fun reset (source : SR.source) : unit =
        (ST.reset ();  (* reset generator counter for Stamps *)
	 LV.reset ();  (* reset generator counter for LambdaVa *)
         anyErrors := false;
	 sourceRef := source)

(* 
  [DBM, 2025.03.05] It would be simpler if the current source was contained in a ref
  in the ErrorMsg, or here as now done.  This would eliminate the need for the
  ErrorMsg.errors function. After setting the source ref, we could just call
  ErrorMsg.error, etc. directly. The CompInfo.reset function could still take a source
  argument, which it would then use to reset the value of the source in
  ErrorMsg. Alternatively, ErrorMsg could depend on CompInfo for access to the source,
  which would be stored in the CompInfo structure instead of "errorFn", etc.
 
  [DBM, 2025.04.01] This is now done. CompInfo has the source and the anyErrors
  boolean flag. CompInfo.reset(source) sets a new source and resets counters for
  stamps and lambda vars (and other counters in the future).  CompInfo no longer
  provides access to the mlLvar and mkStamp functions, which shoule be accessed
  directly as Stamps.fresh and LambdaVar.mkLvar.
*)

end (* local - imports *)
end (* structure CompInfo *)
