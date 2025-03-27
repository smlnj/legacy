(* errormsg.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* ErrorMsg: error reporting *)

(* [DBM, 2025.03.26] This can be considerably simplified in two ways:
 * (1) Eliminate compilcations due to Ramsey's model of the sourceMap.
 * (2) Use the new PrettyPrint library instead of the old PP library.
 * The version of errormsg.s?? in the smlnj[newpp] branch does both of these.
 * This version simplifies by using CompInfo for accessing the anyErrors
 * and errorConsumer values. This means that these don't have to be passed
 * as parameters in many places. They are expected to remain fixed throughout
 * a particular compilation.
 *)

structure ErrorMsg : ERRORMSG =
struct

  structure S = Source
  structure SM = SourceMap
  structure PP = PrettyPrint
  structure CI = CompInfo

  exception Error  (* since 0.92! *)

  (* severity: WARN : warnings, COMPLAIN : "recoverable errors", TERMINAL : "terminal errors" *)
  datatype severity
    = WARN     (* warning messages, non-fatal. It should be possible for compilation
		* and exectuion to complete after a warning. *)
    | COMPLAIN (* "recoverable" errors, where static elaboration can attempt to continue,
		* but CompInfo.anyErrors is set to true and compilation terminates after 
                * the elaboration phase. *)
    | TERMINAL (* "terminal" errors, where we cannot continue elaboration, so compilation
		* terminates immediately. *)

  (* bodyPrinter will be defined as Format.format with new PrettyPrint library *)

  type bodyPrinter = PP.stream -> unit

  type complainer = severity -> string -> bodyPrinter -> unit

  type errorFn = SM.region -> severity -> string -> bodyPrinter -> unit

  (* errors: the error record *)
  type errors = {error: SM.region -> complainer,
                 errorMatch: SM.region -> string,
                 anyErrors: bool ref}

  (* val defaultConsumer : unit -> PP.device *)
  fun defaultConsumer () = PP.defaultDevice

  (* val nullErrorBody : bodyPrinter -- prints nothing *)
  val nullErrorBody : bodyPrinter = (fn (_: PP.stream) => ())

  (* val ppmsg : string * severity * string * bodyPrinter -> unit *)
  fun ppmsg (location: string, severity: severity, msg: string, bodyPrinter: bodyPrinter) =
      case (!BasicControl.printWarnings, severity)
	of (false,WARN) => ()
	 | _ =>
	    PP.with_pp CI.errorConsumer
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

  (* val recordError : severity * bool ref -> unit *)
  fun record severity : unit = 
      (case severity
 	 of WARN => ()
          | COMPLAIN =>  CI.anyErrors := true
	  | TERMINAL => (CI.anyErrors := true; raise Error))

(* [Ramsey, OBSOLETE] With the advent of source-map resynchronization
 * (a.k.a ( *#line...* ) comments), a contiguous region as seen by the compiler
 * can correspond to one or more contiguous segments in source code, with
 * different segments possibly lying in different source files.
 * We can imagine myriad ways of displaying such information, but we
 * confine ourselves to two:
 *  * When there's just one source region, we have what we had in the old
 *    compiler, and we display it the same way:
 *
 *      name:line.col or
 *      name:line1.col1-line2.col2
 *
 *  * When the region spans two or more source segments, we use an ellipsis instead
 *    of a dash, and if not all regions are from the same file, we provide
 *    the file names of both endpoints (even if the endpoints are the same
 *    file).
 *
 * [DBM] The matchErrorString function can be significantly simplified given that #line comments
 *    will no longer be supported by the lexer. See errormsg.sml in newpp branch of smlnj.
 *    See the revised and simplified version of errormsg.sml in the smlnj[newpp] branch,
 *    which should eventually replace this version.
 *)

  (* val impossible : string -> 'a *)
  (* impossible is commonly used to define a compiler bug error function, usually named "bug".
   * This uses the hardwired default consumer instead of CI.errorConsumer! *)
  fun impossible msg =
      (app Control_Print.say ["Error: Compiler bug: ",msg,"\n"];
       Control_Print.flush();
       raise Error)

  (* val regionToString : SM.region -> string *)
  (* This is much complicated by the Ramsey NoWeb stuff. *)
  fun regionToString (region: SM.region) : string =
      let val sr : int = SM.start region  (* p1 -> sr *)
	  val er : int = SM.end region    (* p2 -> er *)
	  val {sourceMap,file,...} = CI.inputSource
	  val fileName0 =
	      case file
	        of SOME f => f
		 | NONE => "stdIn"
          fun shortpoint ({line, column,...} : SM.sourceloc, l) =
              Int.toString line :: "." :: Int.toString column :: l
          fun showpoint (p as {fileName,...} : SM.sourceloc, l) =
              fileName :: ":" :: shortpoint (p, l)
          fun allfiles(f, (src: SM.sourceloc, _)::l) =
              f = #fileName src andalso allfiles(f, l)
            | allfiles(f, []) = true
          fun lastpos [(_, hi)] = hi
            | lastpos (h::t) = lastpos t
            | lastpos [] = impossible "lastpos botch in ErrorMsg.regionToString"
       in concat
           (case SM.fileregion sourceMap (p1, p2)
              of [(lo, hi)] =>
                    if p1+1 >= p2
		    then showpoint (lo, [])
                    else showpoint (lo, "-" :: shortpoint (hi, []))
               | (lo, _) :: rest =>
                    if allfiles(#fileName lo, rest)
		    then showpoint(lo, "..." :: shortpoint(lastpos rest, []))
                    else showpoint(lo, "..." :: showpoint (lastpos rest, []))
               | [] => [fileName0, ":<nullRegion>"])
      end

  (* val error : SM.region -> severity -> string -> bodyPrinter -> unit *)
  fun error (region: SM.region) (severity: severity) (msg: string) (body : bodyPrinter) =
      (ppmsg ((regionToString region), severity, msg, body);
       record severity)

  (* val errorNoSource : string * severity * string * bodyPrinter -> unit *)
  fun errorNoSource (args as (_, severity, _, _)) =
      (ppmsg args; record severity)

  (* val errorNoFile : SM.region * severity * string * bodyPrinter -> unit *)
  fun errorNoFile (region: SM.region, severity: severity, msg: string, bodyPrinter: bodyPrinter) : unit =
      (ppmsg (if SM.startRegion > 0
	      then SM.regionToString region  (* concat[Int.toString p1, "-", Int.toString p2] *)
              else "",
              severity, msg, bodyPrinter);
       record severity)

  (* val impossibleWithBody : string -> bodyPrinter -> 'a *)
  fun impossibleWithBody (msg: string) (body: PP.stream -> unit) =
      (PP.with_pp (defaultConsumer()) (fn ppstrm =>
        (PP.string ppstrm "Error: Compiler bug: ";
         PP.string ppstrm msg;
         body ppstrm;
         PP.newline ppstrm));
       raise Error)

(* obsolete
  (* val errors : S.inputSource -> errors *)
  fun errors (source: S.inputSource) =
      {error = error source,
       errorMatch = regionToString source,
       anyErrors = #anyErrors source}

  (* val anyErrors : errors -> bool *)
  fun anyErrors ({anyErrors, ...}: errors) = !anyErrors
*)
  (* val errorsNoFile : PP.device * bool ref -> errors *)
  fun errorsNoFile (consumer,any) =
      {error = errorNoFile (consumer,any),
       errorMatch = fn _ => "Match",
       anyErrors = any}

end  (* structure ErrorMsg *)
