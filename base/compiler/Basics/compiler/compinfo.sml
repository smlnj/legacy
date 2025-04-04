(* Basics/compiler/compinfo.sml  (moved from ElabData/basics, DBM 2025.04.03)
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* This version replaces the compinfo.sml in ElabData/main/compinfo.sml *)

(* The CompInfo structure contains a pair of internal references to resources 
 * required during compilation.
 *  (1) errorRef : bool ref -- set using reportError, queried using errors
 *  (2) sourceRef : source ref -- this contains the current input source, which
 *      remains fixed during a compilation.
 *      The current source is set by the reset function and is accessed using
 *      the source function.
 *)

signature COMP_INFO =
sig

  val reportError : unit -> unit
    (* report an error. This will be recorded in the internal errorRef. value *)
  val errors : unit -> bool
    (* have any errors been reported *)			    

  val source : unit -> Source.source

  val reset : Source.Source -> unit
    (* resets the current source and resets the errorRef to false (no errors) *)
				  
end (* signature COMP_INFO *)  


structure CompInfo : COMP_INFO =
struct

local (* imports *)

  structure SR = Source

in		   
  (* contents of sourceRef only set by the CompInfo.reset function, which should
   * be called at the beginning of any compilation. *)

  val sourceRef : SR.source ref = ref (SR.newSource NONE)
  fun source () : SR.source = !sourceRef

  val errorsRef = ref false
  fun reportError () = (errorsRef := true)
  fun errors () : bool = errorsRef  (* have any errors been reported *)

  (* reset : SR.source -> unit
   * re-initializes the compiler global info for the given input source *)
  fun reset (source : SR.source) : unit =
      (errorsRef := false;
       sourceRef := source)

(* 

  [DBM, 2025.03.05] It would be simpler if the current source was contained in a ref
  in the ErrorMsg, or here as now done.  This would eliminate the need for the
  ErrorMsg.errors function. After setting the source ref, we could just call
  ErrorMsg.error, etc. directly. The CompInfo.reset function still takes a source
  argument, which it uses to reset the value of the internal source ref, which can
  be accessed using CompInfo.source.  ErrorMsg (and some other structures?) get the
  current source from CompInfo using CompInfo.source.
 
  [DBM, 2025.04.01] This is now done. CompInfo has the source and the anyErrors
  boolean flag. CompInfo.reset(source) sets a new source and resets counters for
  stamps and lambda vars (and other counters in the future).  CompInfo no longer
  provides access to the mlLvar and mkStamp functions, which shoule be accessed
  directly as Stamp.fresh and LambdaVar.mkLvar.  Similarly, the Stamp.reset and
  LambdaVar reset functions must be called separately and directly, rather than
  through CompInfo.reset.  This is for convenience, because Stamp and LambdaVar are
  defined in ElabData/basics, while CompInfo is now defined (earlier) in Basics/compiler.

*)

end (* local - imports *)
end (* structure CompInfo *)
