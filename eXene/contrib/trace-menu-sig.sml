(* ######################################################################
   #                     CML_TRACE_MENU.SIG                             #
   ###################################################################### *)

(* An eXene interface to the CMLTrace system.

   AUTHOR:  Clifford Krumvieda
            Department of Computer Science
            Cornell University
            Ithaca, NY 14850
            cliff@cs.cornell.edu
 *)

(* ######################################################################

   Trace menus:  The mkTraceMenu function can be used to create a menu of 
    certain CMLTrace modules.  Each line in the menu consists of a box
    and a module name; the box has a checkmark in it if its module is
    being traced.  Clicking in the box toggles the checkmark and trace
    status.  
   The second argument to mkTraceMenu is a list of module names that 
    determine the "frontier" of modules appearing in the menu.  A 
    typical value is ["/"].

   ###################################################################### *)

signature CML_TRACE_MENU = sig

  structure W : WIDGET

  type trace_menu

  val widgetOf : trace_menu -> W.widget
  val mkTraceMenu : W.root -> string list -> trace_menu

end
