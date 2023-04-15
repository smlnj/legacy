(* mutecompiler.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Mutecompiler : MUTECOMPILER =
  struct

  (* compile and execute the contents of a file. *)
  local    
    val printingLimitRefs =
          let open Control.Print
          in [
              printDepth, (* default: 5 , max 10 *)
              printLength, (* default: 12 , max 16 *)
              stringDepth, (* default: 70 *)
              intinfDepth (* default: 70 *)
              ]
          end;

    val savedControlPrintOut = ref NONE;

    val savedPrintingLimitSettings = ref NONE;
        
    val compilerOutputPreviousFullLines = ref ([] : string list);
    
    val compilerOutputCurrentLine = ref ([] : string list);



  in

    val printlineLimit = ref 5;

    val compilerMuted = ref false;

        (* 121 *)
    fun isNewline #"\n" = true
      | isNewline _ = false;

    (* 122 *)
    fun push stack item = stack := item :: ! stack; 

    (* 221 *)
    fun installPrintingLimitSettings settings =
          ListPair.app (op :=) (printingLimitRefs, settings);

    (* 11 *)
    fun saveControlPrintOut () = 
            if isSome (! savedControlPrintOut)
            then ()
            else savedControlPrintOut := SOME (! Control.Print.out);

    (* 12 *)
    fun stashCompilerOutput string
      = case String.fields isNewline string   
         of nil => raise (Fail "impossible ") (* 121 *)
          | [chunk] => push compilerOutputCurrentLine chunk (* 122 *)
          | chunk :: lines
            => (if chunk <> "" then push compilerOutputCurrentLine chunk else ();
                push compilerOutputPreviousFullLines
                     (String.concat (rev (! compilerOutputCurrentLine)));
                let val (last :: others) = rev lines
                in app (push compilerOutputPreviousFullLines)
                       (rev others);
                   compilerOutputCurrentLine
                   := (if last <> "" then [last] else [])
                end);
        
    (* 13 *)
    fun savePrintingLimits () =
            if isSome (! savedPrintingLimitSettings)
            then ()
            else savedPrintingLimitSettings := SOME (map ! printingLimitRefs);

    (* 14 *)
    fun lowerPrintingLimitsToMin () =
          List.app (fn r => r := 0) printingLimitRefs;

    (* 21 *)
    fun restoreControlPrintOut () =
            case ! savedControlPrintOut of
                NONE => ()
              | SOME value => (savedControlPrintOut := NONE;
                               Control.Print.out := value);
    (* 22 *)
    fun restorePrintingLimits () =
            case ! savedPrintingLimitSettings of
                NONE => ()
              | SOME settings => (savedPrintingLimitSettings := NONE;
                                  installPrintingLimitSettings settings); (* 221*)
    (* 311 *)
    fun outputFlush f s = (TextIO.output (f, s); TextIO.flushOut f);

    (* 31 *)
    val printStdErr = outputFlush TextIO.stdErr;

    (* 1 *)
    fun silenceCompiler () =
           (compilerMuted := true;
            saveControlPrintOut ();  (* 11 *)
            Control.Print.out := { flush = fn () => (), say = stashCompilerOutput }; (* 12 *)
            savePrintingLimits (); (* 13 *)
            lowerPrintingLimitsToMin ()); (* 14 *)

    (* 2 *)    
    fun unsilenceCompiler () = (compilerMuted := false;
                              restoreControlPrintOut (); (* 21 *)
                              restorePrintingLimits ()); (* 22 *)
                                
    (* dummy function to silence the autoloading messages *)
    fun mcdummyfn () = ( );
    
    (* 3 *)
    fun printStashedCompilerOutput ()
      = let val completeLines = length (! compilerOutputPreviousFullLines)
            val partialLine = 0 < length (! compilerOutputCurrentLine)
            val partialLines = if partialLine then 1 else 0
            val stashedOutput = 0 < completeLines orelse partialLine
        in if stashedOutput andalso (!compilerMuted)
          then (printStdErr ("___________________________________________________________________\n");
                 let val linesShown
                       = ((if partialLine then [String.concat (rev (! compilerOutputCurrentLine))] else [])
                          @ List.take (! compilerOutputPreviousFullLines,
                                       Int.min (completeLines,
                                                !printlineLimit - partialLines)))
                     val numLinesShown = length linesShown
                     val last = completeLines + partialLines
                     val first = last - numLinesShown + 1
                 in printStdErr (String.concat ["The last " ^ Int.toString (!printlineLimit) ^ " lines " ^ Int.toString first ^ " through " ^ Int.toString last ^ " of suppressed compiler messages are:\n"]);
                    foldr (fn (line, ()) => printStdErr (line ^ "\n"))
                          ()
                          linesShown
                 end;
                 printStdErr ("_____________End of suppressed compiler messages.__________________\n")
                 )
            else ()
        end;
  end

  end (* structure Mutecompiler *)