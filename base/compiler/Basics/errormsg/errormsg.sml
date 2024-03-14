(* errormsg.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ErrorMsg : ERRORMSG =
struct

  structure S = Source
  structure SM = SourceMap
  structure PP = PrettyPrint

 (* error reporting *)

  exception Error  (* was Syntax, changed to Error in 0.92 *)

  (* severity: WARN : warnings, COMPLAIN : "recoverable errors", TERMINAL : "terminal errors" *)
  datatype severity
    = WARN     (* warning messages, non-fatal *)
    | COMPLAIN (* "recoverable" errors, where static elaboration can continue *)
    | TERMINAL (* "terminal" errors, where we cannot continue elaboration *)

  type complainer = severity -> string -> (PP.stream -> unit) -> unit

  type errorFn = SM.region -> complainer

  (* errors: the error record *)
  type errors = {error: SM.region -> complainer,
                 errorMatch: SM.region -> string,
                 anyErrors: bool ref}

  (* val defaultConsumer : unit -> PP.device *)
  fun defaultConsumer () = PP.defaultDevice

  (* val nullErrorBody : PP.stream -> unit *)
  val nullErrorBody = (fn (ppstrm: PP.stream) => ())

  (* val ppmsg : PP.device * string * severity * string * (PP.stream -> unit) -> unit *)
  fun ppmsg (errConsumer: PP.device, location, severity, msg, body) =
      case (!BasicControl.printWarnings, severity)
	of (false,WARN) => ()
	 | _ =>
	    PP.with_pp errConsumer
	      (fn ppstrm =>
		  (PP.openHVBox ppstrm (PP.Rel 0);
		   PP.openHVBox ppstrm (PP.Rel 2);
		   PP.string ppstrm location;
		   PP.string ppstrm  (* print error label *)
		      (case severity
			 of WARN => " Warning: "
			  | (COMPLAIN |  TERMINAL) => " Error: ");
		   PP.string ppstrm msg;
		   body ppstrm;
		   PP.closeBox ppstrm;
		   PP.newline ppstrm;
		   PP.closeBox ppstrm))

  (* val record : severity * bool ref -> unit *)
  fun record ((COMPLAIN | TERMINAL), anyErrors) = anyErrors := true
    | record (WARN, _) = ()

  fun record (severity, anyErrors) : unit = 
      (case severity
 	 of WARN => ()
          | COMPLAIN =>  anyErrors := true
	  | TERMINAL => (anyErrors := true; raise Error))

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
 *)

  (* val impossible : string -> 'a *)
  (* impossible is commonly used to define a compiler bug error function, usually named "bug" *)
  fun impossible msg =
      (app Control_Print.say ["Error: Compiler bug: ",msg,"\n"];
       Control_Print.flush();
       raise Error)

  (* val matchErrorString : S.inputSource -> SM.region -> string *)
  fun matchErrorString ({sourceMap,fileOpened,...}: S.inputSource)
                      ((p1,p2): SM.region) : string =
      let fun shortpoint ({line, column,...} : SM. sourceloc, l) =
              Int.toString line :: "." :: Int.toString column :: l
          fun showpoint (p as {fileName,...} : SM.sourceloc, l) =
              Pathnames.trim fileName :: ":" :: shortpoint (p, l)
          fun allfiles(f, (src: SM.sourceloc, _)::l) =
              f = #fileName src andalso allfiles(f, l)
            | allfiles(f, []) = true
          fun lastpos [(_, hi)] = hi
            | lastpos (h::t) = lastpos t
            | lastpos [] = impossible "lastpos botch in ErrorMsg.matchErrorString"
       in concat
           (case SM.fileregion sourceMap (p1, p2)
              of [(lo, hi)] =>
                    if p1+1 >= p2 then showpoint (lo, [])
                    else showpoint (lo, "-" :: shortpoint (hi, []))
               | (lo, _) :: rest =>
                    if allfiles(#fileName lo, rest) then
                      showpoint(lo, "..." :: shortpoint(lastpos rest, []))
                    else
                      showpoint(lo, "..." :: showpoint (lastpos rest, []))
               | [] => [Pathnames.trim fileOpened, ":<nullRegion>"])
      end

  (* val error : S.inputSource -> SM.region -> complainer *)
  fun error (source as {anyErrors, errConsumer,...}: S.inputSource)
            ((p1,p2): SM.region) (severity:severity)
            (msg: string) (body : PP.stream -> unit) =
      (ppmsg (errConsumer, (matchErrorString source (p1,p2)), severity, msg, body);
       record(severity,anyErrors))

  (* val errorNoSource : PP.device * bool ref -> string -> complainer *)
  fun errorNoSource (consumer, anyErrors) (location: string) sev msg body =
      (ppmsg (consumer, location, sev, msg, body); record (sev, anyErrors))

  (* val errorNoFile : PP.device * bool ref -> SM.region -> complainer *)
  fun errorNoFile (errConsumer,anyErrors) ((p1,p2): SM.region) severity msg body =
      (ppmsg (errConsumer,
              if p2>0
	      then concat[Int.toString p1, "-", Int.toString p2]
              else "",
              severity, msg, body);
       record (severity, anyErrors))

  (* val impossibleWithBody : string -> (PP.stream -> unit) -> 'a *)
  fun impossibleWithBody (msg: string) (body: PP.stream -> unit) =
      (PP.with_pp (defaultConsumer()) (fn ppstrm =>
        (PP.string ppstrm "Error: Compiler bug: ";
         PP.string ppstrm msg;
         body ppstrm;
         PP.newline ppstrm));
       raise Error)

  (* val errors : S.inputSource -> errors *)
  fun errors (source: S.inputSource) =
      {error = error source,
       errorMatch = matchErrorString source,
       anyErrors = #anyErrors source}

  (* val anyErrors : errors -> bool *)
  fun anyErrors ({anyErrors, ...}: errors) = !anyErrors

  (* val errorsNoFile : PP.device * bool ref -> errors *)
  fun errorsNoFile (consumer,any) =
      {error = errorNoFile (consumer,any),
       errorMatch = fn _ => "Match",
       anyErrors = any}

end  (* structure ErrorMsg *)
