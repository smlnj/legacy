(* errormsg.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* ErrorMsg: error reporting *)

(* [DBM, 2025.03.26] This can be considerably simplified in two ways:
 * (1) Eliminate compilcations due to Ramsey's model of the sourceMap (done).
 * (2) Use the new PrettyPrint library instead of the old PP library.
 * The version of errormsg.s?? in the smlnj[newpp] branch does both of these.
 * This version simplifies by using CompInfo for accessing the anyErrors
 * and errorConsumer values. This means that these don't have to be passed
 * as parameters in many places. They are expected to remain fixed throughout
 * a particular compilation.
 *)

structure ErrorMsg : ERRORMSG =
struct

local (* imports *)

  structure CI = CompInfo
  structure SL = SourceLoc
  structure SM = SourceMap
  structure S = Source
  structure PP = PrettyPrint

in		     

  exception Error  (* since 0.92! *)

  (* severity: WARN : warnings, COMPLAIN : "recoverable errors", TERMINAL : "terminal errors" *)
  datatype severity
    = WARN     (* warning messages, non-fatal. It should be possible for compilation
		* and execution to complete after a warning. *)
    | COMPLAIN (* "recoverable" errors, where static elaboration can attempt to continue,
		* but CompInfo.anyErrors is set to true and compilation terminates after 
                * the elaboration phase. *)
    | TERMINAL (* "terminal" errors, where we cannot continue elaboration, so compilation
		* terminates immediately. *)

  (* bodyPrinter will be replaced with Format.format when we move to the new PrettyPrint library *)
  type bodyPrinter = PP.stream -> unit

  (* val recordError : severity * bool ref -> unit *)
  fun recordError severity : unit = 
      (case severity
 	 of WARN => ()  (* no consequences for warnings *)
          | COMPLAIN =>  CI.anyErrors := true (* allow possibility of error recovery *)
	  | TERMINAL => (CI.anyErrors := true; raise Error))

  (* val nullErrorBody : bodyPrinter -- prints nothing *)
  (* should now be renamed "nullBodyPrinter" *)
  val nullErrorBody : bodyPrinter = (fn (_: PP.stream) => ())

  (* val ppmsg : string * severity * string * bodyPrinter -> unit *)
  fun ppmsg (location: string, severity: severity, msg: string, bodyPrinter: bodyPrinter) =
      case (!BasicControl.printWarnings, severity)
	of (false,WARN) => ()
	 | _ =>
	    PP.with_pp PP.defaultDevice
	      (fn ppstrm =>
		  (PP.openHVBox ppstrm (PP.Rel 0);
		   PP.openHVBox ppstrm (PP.Rel 2);
		   PP.string ppstrm location;
		   PP.string ppstrm  (* print error label *)
		      (case severity
			 of WARN => " Warning: "
			  | (COMPLAIN |  TERMINAL) => " Error: ");
		   PP.string ppstrm msg;
		   bodyPrinter ppstrm;
		   PP.closeBox ppstrm;
		   PP.newline ppstrm;
		   PP.closeBox ppstrm))


  (* locationString : SL.region -> string *)
  (* Map a region in the current source file to a _location string_ to be used in
   * an error message.
   * exported for use in FLINT/trans/translate.sml => should be moved elsewhere? *)
  fun locationString (region: SM.region) : string =
      let val source = CI.source ()
	  val sourceName = S.name source
          val sourceMap = S.sourcemap source
       in (case region
	    of SM.NULLregion => sourceName ^ ":<NULLregion>"
	     | SM.REGION (lo, hi) =>
		 let val (startLoc, endLoc) = SM.regionToLocations sourcemap region
		  in String.concat
		      [sourceName, ":", SL.locationToString startLoc, "-",
		       SL.locationToString endLoc]
		 end)
      end

  (* error : SM.region -> severity -> string -> bodyPrinter -> unit *)
  fun error (region: SM.region) (severity: severity) (msg: string) (body : bodyPrinter) =
      (ppmsg ((locationString region), severity, msg, body);
       recordError severity)

  (* errorRegion : SL.region * string -> unit *)
  fun errorRegion (region: SL.region, msg: string) =
      error region COMPLAIN msg nullErrorBody

  (* errorRegion : SL.region * string -> unit *)
  fun warnRegion (region: SL.region, msg: string) =
      error region WARN msg nullErrorBody

  (* impossible : string -> 'a *)
  (* impossible is commonly used to define a compiler bug error function, usually named "bug". *)
  fun impossible msg =
      (app Control_Print.say ["Error: Compiler bug: ", msg, "\n"];
       Control_Print.flush();
       raise Error)

  (* impossibleWithBody : string -> bodyPrinter -> 'a *)
  fun impossibleWithBody (msg: string) (bodyPrinter: bodyPrinter) =
      (PP.with_pp (PP.defaultDevice) (fn ppstrm =>
        (PP.string ppstrm "Error: Compiler bug: ";
         PP.string ppstrm msg;
         bodyPrinter ppstrm;
         PP.newline ppstrm));
       raise Error)

end (* top local - imports *)
end (* structure ErrorMsg *)
