(* ttree.sig
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * signature for tactic trees 
 *)

signature TTREE =
sig

structure S : TTREE_SUPPORT 
structure T: TREE

exception EmptyTree 
exception EventDoesNotAchieve
exception Impossible

type ttree
type ttree_state

datatype view_mode = Subtree | Node | Local

datatype format = Plain | Highlight | Special

datatype action = 
      MoveToParent
    | MoveToChild
    | MoveToTop
    | MoveLeft
    | MoveRight
    | MoveToNode of int
    | HighlightSubtree
    | ApplyTacticToNode of int	
    | Delete
    | Display
    | Elide 
    | ApplyTactic of S.tactic * string 
    | ChangeMode 

type text_block 

datatype display_instruction = 
      ClearFrom of int
    | DisplayText of text_block
    | DoNothing
    | InsertText of text_block
    | Scroll of int * int



val mkTTree : S.goal -> ttree_state             
val do_action : ttree_state * action -> (display_instruction list)
val synthesize_event : ttree_state -> S.event 
val synthesize_tactic_text : ttree_state -> string
val synthesize_text : ttree_state * string -> unit


end 



