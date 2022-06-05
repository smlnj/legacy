(* interactive.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * interactive tactic tree manager. 
 *)

functor InteractiveTT (structure TTS : TTREE_SUPPORT) : INTERACTIVE_TT =
struct

structure TTS = TTS
structure TTM = TTreeManager(structure S = TTS)


fun error_handler evt = 
    let val err_msg = CML.sync evt
    in
	(case err_msg
	    of TTM.TacticApplicationError m => CIO.print "Tactic Application Error \n"
	     | TTM.TacticParseError => CIO.print "Tactic Parse Error \n"
             | _ => CIO.print "unknown execption \n");
	error_handler evt 
    end

fun layout ttree menu server = let
	val root = Widget.mkRoot server
        val tw =  TTM.mkTTreeWidget (ttree,menu,root) 
	val evt = TTM.evtOf tw
	val shell = Shell.mkShell 
	              (TTM.widgetOf tw, 
		       NONE, 
		       {win_name = SOME "Tactic Tree", 
			icon_name = SOME "Tactic Tree"})
	in
	  CML.spawn (fn () => error_handler evt); Shell.init shell
	end
	  handle MLXError.XERROR str =>
	    XDebug.trace (XDebug.errorTM, fn () => ["main: XERROR " ^ str ^ "\n"])


fun create (goal,server,tactic_menu_extension) =
      let val ttree = TTM.mkTTreeState goal
      in
        RunCML.doit (fn () => layout ttree tactic_menu_extension server,SOME 10);
        ttree
      end

fun view (ttree, server, tactic_menu_extension) = ( 
       RunCML.doit (fn () => layout ttree tactic_menu_extension server,SOME 10);
       ttree)

val extract_event = TTM.extract_event
val extract_tactic_text = TTM.extract_tactic_text
val extract_text = TTM.extract_text

end



