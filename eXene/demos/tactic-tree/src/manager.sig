(* manager.sig 
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * signature for tactic tree manager. 
 *)

signature TTREE_MANAGER =
  sig
    structure S : TTREE_SUPPORT

    exception TacticParseError 
    exception TacticApplicationError of exn 
    exception FailedValidation of exn 
    exception ExtractDoesNotAchieve
    exception TacticTreeIncomplete

    type ttree_state
    type ttree_widget 

    val mkTTreeWidget :
          ttree_state * (string * S.tactic) list * Widget.root ->ttree_widget
    val widgetOf : ttree_widget -> Widget.widget
    val evtOf : ttree_widget -> exn CML.event


    val mkTTreeState : S.goal -> ttree_state
    val extract_event : ttree_state -> S.event
    val extract_tactic_text : ttree_state -> string 
    val extract_text : ttree_state * string -> unit

  end
