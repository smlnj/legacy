(* ttree.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * functor for producing tactic trees 
 *)


functor TTree(S : TTREE_SUPPORT) : TTREE = 
struct 

structure S = S 
exception EmptyTree 
exception EventDoesNotAchieve
exception Impossible

datatype format = Highlight | Plain | Special

structure TS = 
    struct 
        datatype display_info = 
	   DisplayBlock of 
              {goal :string list, 
	       by_line : string list, 
	       global_range : int * int, (* start,size *)
	       local_range : int,  (* size *)
	       elision_test : bool,
	       elision_strings : string list,
	       elision_depth : int, (* if elision_depth > 0 then node is invisible *)
	       tree_depth : int,
	       form : format} 

        datatype goal_value = 
            Incomplete of S.goal 
	  | Complete of S.goal * string * S.validation

	type node_value = goal_value * display_info 
    end 

structure T = TreeFun(TS)

open TS

datatype view_mode = Subtree | Node | Local

type ttree = T.tree
type ttree_node = T.node_id
type ttree_state = ttree ref * view_mode ref

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

type text_block = ((string list) * format * (int * int)) 

datatype display_instruction = 
      ClearFrom of int
    | Scroll of int * int
    | InsertText of text_block
    | DisplayText of text_block 
    | DoNothing


datatype redraw_action =
    CurrentNode of format 
  | CurrentSubtree of format 
  | DrawAboveNode of view_mode
  | ChildrenOnly
  | DrawElidedSubtree of view_mode
  | BeneathSubtree of redraw_action (* redraw entire current subtree and beneath it *)
  | LeavingLocalView  (* redraw with current node highlighted, change all node except 
	              root to have their format to be Plain.*)
  | InsertByline
  | DrawSubtree
  | UnhighlightSubtree
  | OptimizeHighlight of format


datatype modify_display_block =
    ToggleElision of ttree 
  | SetFormat of ttree_node * format
  | SubtreeRemoved of ttree
  | InitializeChildren of S.goal list 
                            (* create start in global view , elision_depth, tree_depth, form 
			       for each subgoal in the list*)
  | InsertTactic of ttree * string
  | RelocateNode of ttree_node * int     (* a new start position *)

    
(*** MAGIC NUMBERS **)
val visible = 0
val elided = true
val top_level = 1
val top_of_screen = 0
val off_screen = ~1
val empty_elision = []
    
(**********************************)

fun prettyPrinter (display_string,tree_depth) = let 
    fun border_maker size = 
	if size = 0  then "" else S.indentation ^ (border_maker (size-1))
	
    fun string_to_list str border = let
	fun remove_newlines str [] = [str]
	  | remove_newlines str (x::l) = if x = "\n" then str::(remove_newlines "" l)
					 else  remove_newlines (str^x) l
    in
	map (fn s => border ^ s) 
            (fold (op @) (map (remove_newlines "" ) (map explode str)) [])
    end
in
    string_to_list [display_string] (border_maker tree_depth)
end

(**** DISPLAY FUNCTIONS   *****)

fun currentNodeElided tt = let
    val (_,DisplayBlock{elision_test,...}) = T.current_node_value tt
in
    (elision_test = elided)
end

fun lineAtCurrentNode (current_node, line_num) = let
    val (_,DisplayBlock{global_range=(start,size),...}) = T.get_value current_node
in
    (start <= line_num) andalso (line_num <= start+size-1)
end

fun currentNodeVisible tt = let
    val (_,DisplayBlock{elision_depth,...}) = T.current_node_value tt
in
    elision_depth = visible
end

fun nodeElided node = let
    val (_,DisplayBlock{elision_test,...}) = T.get_value node
in
    elision_test = elided
end

fun changeDisplayValue (tt,display_bloc) = let
    val  (goal_val,_) = T.current_node_value tt
in
    T.change_node_value (tt,(goal_val, display_bloc))
end

fun assignDisplayValue (node,display_bloc) = let 
    val  (goal_val,_) = T.get_value node
in
    (T.get_value_ref node) := (goal_val,display_bloc)
end

fun desendantOf (tt, node) = let
    val (_,DisplayBlock{global_range=(node_start,_),...}) = T.get_value node 

    fun compareAncestor (ref T.Ground) = false
      | compareAncestor ancestor = let 
	    val (_,DisplayBlock{global_range=(ancestor_start,_),...}) = T.get_value ancestor
	in
	    if node_start = ancestor_start 
		then true 
	    else if node_start > ancestor_start (*node is beneath ancestor *)
		     then false
		 else compareAncestor (T.get_parent ancestor)
	end
in
    compareAncestor (T.current_subtree tt)
end
fun initializeRoot goal = let
    val goal_list = prettyPrinter ((S.goal_to_string goal),top_level)
    val by_line = prettyPrinter ((S.unrefined),top_level) 
    val size = length (goal_list @ by_line) 
in
    DisplayBlock{goal = goal_list, by_line = by_line, global_range = (top_of_screen,size), 
		 local_range = size, elision_test = (not elided), elision_strings = empty_elision,
		 elision_depth = visible, tree_depth = top_level,
		 form = Highlight}
end

(*************** modifyDisplay (ToggleElision tt) ******************)
(* The current node is either elided or unelided. The elision depths of the nodes in the
   current subtree are changed to reflect the elision operation. *)

fun modifyDisplay (ToggleElision tt) = let 
    val (_,DisplayBlock{goal,by_line,global_range = (start,_),local_range,elision_test,
			elision_strings,elision_depth,tree_depth,form}) = T.current_node_value tt

    val elision_test = not elision_test 
    val (elision_strings,delta) = if elision_test = elided 
			      then (prettyPrinter ((S.elision),tree_depth),1) 
			  else (empty_elision,~1)

    val global_size = local_range + (length elision_strings)

    fun changeElisionDepth node = let
	val (_,DisplayBlock{goal,by_line,global_range,local_range,elision_test,elision_strings,
			elision_depth,tree_depth,form}) = T.get_value node

	val disp_bloc = DisplayBlock{goal = goal, by_line = by_line, global_range = global_range,
				      local_range = local_range, elision_test = elision_test, 
				      elision_strings = elision_strings, 
				      elision_depth = elision_depth + delta,
				      tree_depth = tree_depth, form = form}
    in
	assignDisplayValue(node,disp_bloc)
    end

in
    T.preorder tt (changeElisionDepth,(),T.get_children_list,fn (_,_) => ()) (T.get_children_list 
								  (T.current_subtree tt)) ;

    DisplayBlock{goal = goal, by_line = by_line, global_range = (start,global_size),
		 local_range = local_range, elision_test = elision_test, 
		 elision_strings = elision_strings, elision_depth = elision_depth,
		 tree_depth = tree_depth, form = form}
end
	
(************ modifyDisplay (SubtreeRemoved tt) ************)
(* Create a display block for the current node whose subtree has been deleted *) 

  | modifyDisplay (SubtreeRemoved tt) = let
	val (_,DisplayBlock{goal,global_range=(start,size),elision_test,elision_strings,
			    elision_depth,tree_depth,form,...}) = T.current_node_value tt 

	val by_line = prettyPrinter ((S.unrefined),tree_depth)
	val local_size = (length goal) + (length by_line)
	val global_size = local_size + (length elision_strings)

    in
	DisplayBlock{goal = goal, by_line = by_line, global_range = (start,global_size), 
		     local_range = local_size, elision_test = elision_test, 
		     elision_strings = elision_strings, elision_depth = elision_depth,
		     tree_depth = tree_depth, form = form}
    end

(*******************************)

  | modifyDisplay (SetFormat (node,form)) = let
	val (_,DisplayBlock{goal,by_line,global_range,local_range,elision_test,elision_strings,
			    elision_depth,tree_depth,...}) = T.get_value node
    in
	DisplayBlock{goal = goal, by_line = by_line, global_range = global_range,
		     local_range = local_range, elision_test = elision_test, 
		     elision_strings = elision_strings, elision_depth = elision_depth,
		     tree_depth = tree_depth, form = form}
    end
  
(*******************************)
  
(********* modifyDisplay (InsertTactic (tt,tactic_str)) ***********)
(*  Create a by_line with tactic_str as the tactic *)

  | modifyDisplay (InsertTactic (tt,tactic_str)) = let 
    val (_,DisplayBlock{goal,global_range=(start,_),elision_test, elision_strings,elision_depth,
			tree_depth,form,...}) = T.current_node_value tt

    val	by_line = prettyPrinter ((S.refined ^ tactic_str),tree_depth)
    val local_size = (length by_line) + (length goal)
    val global_size = local_size + (length elision_strings)
    in
	DisplayBlock{goal = goal, by_line = by_line, global_range = (start,global_size), 
			  local_range = local_size, elision_test = elision_test,
			  elision_strings = elision_strings, elision_depth = elision_depth, 
			  tree_depth = tree_depth, form = Plain}
    end

(********* modifyDisplay (RelocateNode (node,new_start)) ***********)
(*  Change the start position of the node to new_start *)

  | modifyDisplay (RelocateNode (node,new_start)) = let
	val (_,DisplayBlock{goal,by_line,global_range=(_,size),local_range,elision_test,
			    elision_strings,elision_depth,tree_depth,form}) = T.get_value node
    in	   
	DisplayBlock{goal=goal,by_line=by_line,global_range=(new_start,size),
		     local_range=local_range,elision_test=elision_test,
		     elision_strings=elision_strings,elision_depth=elision_depth,
		     tree_depth=tree_depth,form=form}
    end
(********************************************************************************************)

(******* initializeChildren (node,subgoals)   *******)

(* Create a list of display blocks, one corresponding to each subgoal in subgoals.*)  

fun initializeChildren (node, (subgoal::subgoals)) = let
    val (_,DisplayBlock{global_range=(start,size),elision_test,tree_depth,elision_depth,
			...}) = T.get_value node
	
    val new_elision_depth = if elision_test = elided
				then elision_depth + 1   (* make child invisble, parent elided*)
			    else elision_depth  (* child invisible if parent invisible *)    
				
    val new_tree_depth = tree_depth + 1

    fun getChildren (subgoal::subgoal_list) new_start = let

	val goal_list = prettyPrinter ((S.goal_to_string subgoal),new_tree_depth)
	val by_line = prettyPrinter ((S.unrefined),new_tree_depth)
	val size = length (goal_list @ by_line)
    in     
	(DisplayBlock{goal = goal_list, by_line = by_line, global_range = (new_start,size), 
		      local_range = size, elision_test = (not elided), elision_strings = empty_elision, 
		      elision_depth = new_elision_depth, tree_depth = new_tree_depth,
		      form = Plain})::(getChildren subgoal_list (new_start+size))
    end
      | getChildren [] _ = []
in
    getChildren (subgoal::subgoals) (start+size)
end

  | initializeChildren (_,[]) = []

(***************************)
(* This function changes the starts of all the nodes in a subtree.  Only visible nodes are
   changed.
*) 

 fun updateSubtreeStarts [] _ = ()

   | updateSubtreeStarts (node::rest) start = 

      updateSubtreeStarts 
            ((T.get_children_list node) @ rest)
	    (let val  (_,DisplayBlock disp_bloc) = T.get_value node
	    in 
		 if (#elision_depth disp_bloc) > visible
		     then start
		 else 
		     (assignDisplayValue (node,modifyDisplay (RelocateNode (node,start)));
		     (#2 (#global_range disp_bloc)) + start)
	    end)
					    
(*********)
(** This function traverses the subtree of a node's right sibling and the right siblings of
    its ancestors.  The start position of each node is changed during a visit.  ****)
(*************)
   
fun updateBeneathSubtree tt ((node as (ref (T.TreeNode m))),start) = let

    fun visitNode (node,new_start) = let 
	val (_,DisplayBlock{goal,by_line,elision_strings,global_range=(_,size),elision_depth,...}) =
	                                                                    T.get_value node
    in 
	assignDisplayValue (node,modifyDisplay (RelocateNode (node,new_start)));
	assignDisplayValue (node,modifyDisplay (SetFormat (node,Plain)));
        new_start+size
    end
    fun updateSubtree ([],start) (ref T.Ground) = ()

      | updateSubtree ([],start) next_subtree = 
	             updateSubtree ([next_subtree],start) (T.get_ancestor_right_sibling next_subtree)

      | updateSubtree ((node::rest),start) next_subtree  = 
	   updateSubtree (((T.get_children_list node) @ rest),(visitNode (node,start))) next_subtree
	
    fun scrollInstr node = let
	val (_,DisplayBlock{global_range = (node_start,_),...}) = T.get_value node
    in
	[Scroll (node_start,start - node_start)]
    end
	   
    val (scroll_instr,_) = (scrollInstr node,updateSubtree ([node],start) (T.get_ancestor_right_sibling node))

in
    scroll_instr 
end

(*****  REDRAW   *****)

(**** redraw the current node highligted or unhighlighted. ******)
fun redraw tt (CurrentNode form) = let

    val current_node = T.current_subtree tt
    val (_,DisplayBlock{goal,by_line,elision_strings,global_range=(start,size),elision_depth,...}) =
	                                              T.current_node_value tt
in
    if elision_depth > visible 
	then []
    else 
	(assignDisplayValue(current_node, modifyDisplay (SetFormat (current_node,form)));
	[DisplayText (goal @ by_line @ elision_strings, form,(start,start+size))])
end
(*********************)
(*
      If a tactic was applied and subgoals were produced, then the current node should be unhighlighted. Otherwise
      it should be left highlighted, and the tactic string inserted in its display.  If the subtree was removed,
      then the tactic string should be inserted.
 *)

  | redraw tt InsertByline = let
	val (_,DisplayBlock{by_line,goal,elision_strings,elision_test,global_range=(start,size),form,...}) = 
	                                                                       T.current_node_value tt

	fun childDisplayInstrList (child::child_list) = let
	    val (_,DisplayBlock{by_line,goal,global_range=(start,size),...}) = T.get_value child
	in
	    (DisplayText (goal @ by_line, Plain,(start,start+size)))::(childDisplayInstrList child_list)
	end
	  | childDisplayInstrList [] = []

in
(* NOTE: If a tactic is applied to the current node which is elided then its children do not
         have to be drawn. Therefore, only the by line has to be drawn.  However if a tactic is 
	 applied by the middle mouse button to an elided node other than the current node, then 
	 the node is not highlighted.  Therefore, that entire node has to be redrawn.
*)

    if elision_test = elided 
	then 
	    if form = Plain  
		then [DisplayText(goal@by_line@elision_strings,Highlight,(start,start+size))]
	    else [InsertText (by_line,Highlight,(start+(length goal),start+(length goal)+(length by_line)))]
    else 
	case (T.get_children_list (T.current_subtree tt))
	    of [] =>

		(* A tactic can be applied to the current node or some other node via the middle mouse button.  
		   Checking the format of the node determines which node the tactic was applied.
                *)

		if form = Plain
		    then [DisplayText(goal@by_line@elision_strings,Highlight,(start,start+size))]
		else [InsertText (by_line,Highlight,(start+(length goal),start+(length goal)+(length by_line)))]

	     | child_list => 
		   (DisplayText (goal@by_line,Plain,(start,start+size)))::(childDisplayInstrList child_list)
end
(**************************)

  | redraw tt UnhighlightSubtree = let 
	fun displayNode node = let 
	    val (_,DisplayBlock{goal,by_line,elision_strings,elision_depth,
				 global_range = (start,size),...}) = T.get_value node
	in 
	    if elision_depth > visible
		then DoNothing
	    else
		(assignDisplayValue (node, modifyDisplay (SetFormat (node,Plain)));
		DisplayText (goal @ by_line @ elision_strings, Plain ,(start,start+size)))
	end

	fun getChildren node = let 
	    val (_,DisplayBlock{elision_depth,elision_test,goal,by_line,elision_strings,
				global_range = (start,size),...}) = T.get_value node
	    
	in 
	    if (elision_depth > visible) orelse (elision_test = elided)
		then []
	    else T.get_children_list node
	end
	val children_list = (fn [] => [] | (x::l) => l) (T.get_children_list (T.current_subtree tt))
							   
    in
	(displayNode (T.current_subtree tt))::(T.preorder tt (displayNode,[],getChildren,op ::) children_list)
    end
 
  | redraw tt DrawSubtree = let 
 
	fun displayNode node = let 
	    val (_,DisplayBlock{goal,by_line,elision_strings,elision_depth,
				 global_range = (start,size),...}) = T.get_value node
	in 
	    if elision_depth > visible
		then DoNothing
	    else
		(assignDisplayValue (node, modifyDisplay (SetFormat (node,Plain)));
		DisplayText (goal @ by_line @ elision_strings, Plain ,(start,start+size)))
	end

	fun getChildren node = let 
	    val (_,DisplayBlock{elision_depth,elision_test,goal,by_line,elision_strings,
				global_range = (start,size),...}) = T.get_value node
	    
	in 
	    if (elision_depth > visible) orelse (elision_test = elided)
		then []
	    else T.get_children_list node
	end
    in
	(fn ((DisplayText(t,_,r))::l) => (DisplayText(t,Highlight,r))::l | _ => []) 
	                           (T.preorder tt (displayNode,[],getChildren,op ::) [T.current_subtree tt])
    end
 
(**** redraw the current subtree highligted or unhighlighted. ******)

  | redraw tt (OptimizeHighlight new_form) = let
(* If new_form = Special then make the current subtree Plain except for part of it which will be the new
   current subtree.  Only at most one node will have format Special. When that node is encountered in this
   function, its format is changed to Highlight.
*)
	fun displayNode node = let 
	    val (_,DisplayBlock{goal,by_line,elision_strings,elision_depth,form,
				 global_range = (start,size),...}) = T.get_value node
	in 
	    if (elision_depth > visible) orelse (form = Special)
		then DoNothing
	    else 
		(assignDisplayValue (node, modifyDisplay (SetFormat (node,new_form)));
		 DisplayText (goal @ by_line @ elision_strings,new_form,(start,start+size)))
	end

	fun getChildren node = let 
	    val (_,DisplayBlock{elision_depth,elision_test,goal,by_line,elision_strings,form,
				global_range = (start,size),...}) = T.get_value node
	    
	in 
	    if form = Special
		then (assignDisplayValue (node, modifyDisplay (SetFormat (node,Highlight)));[])
	    else if (elision_depth > visible) orelse (elision_test = elided)
		     then []
		 else T.get_children_list node
	end
    in
	T.preorder tt (displayNode,[],getChildren,op ::) [T.current_subtree tt]
    end

(**** redraw the current subtree highligted or unhighlighted. ******)
 
  | redraw tt (CurrentSubtree form ) = let 
	fun displayNode node = let 
	    val (_,DisplayBlock{goal,by_line,elision_strings,elision_depth,
				 global_range = (start,size),...}) = T.get_value node
	in 
	    if elision_depth > visible
		then DoNothing
	    else
		(assignDisplayValue (node, modifyDisplay (SetFormat (node,form)));
		DisplayText (goal @ by_line @ elision_strings, form,(start,start+size)))
	end

	fun getChildren node = let 
	    val (_,DisplayBlock{elision_depth,elision_test,goal,by_line,elision_strings,
				global_range = (start,size),...}) = T.get_value node
	    
	in 
	    if (elision_depth > visible) orelse (elision_test = elided)
		then []
	    else T.get_children_list node
	end
    in
	T.preorder tt (displayNode,[],getChildren,op ::) [T.current_subtree tt]
    end

(****  redraw current node with chidren--display routine for local view   ***)

  | redraw tt ChildrenOnly = let
	val current_node = T.current_subtree tt 
        val num_of_indentations = #tree_depth 
	                   ((fn (_,(DisplayBlock d)) => d) (T.current_node_value tt))

	fun stripOffBorder ([],_) = []
	  | stripOffBorder (str::str_list,num_of_indentations) = 
		(implode(nthtail(explode str,(String.length S.indentation) 
				 * num_of_indentations)))::
		(stripOffBorder (str_list,num_of_indentations))
 

	fun localViewDisplay [] _ = []

	  | localViewDisplay (node::node_list) start = let

	    val (_,DisplayBlock{goal,by_line,local_range,...}) = T.get_value node
		
	    in	
		assignDisplayValue(node,modifyDisplay (SetFormat (node,Plain)));
		(DisplayText(stripOffBorder(goal@by_line,num_of_indentations), Plain, 
			     (start,start+local_range)))::
		(localViewDisplay node_list (start + local_range))
	    end    
    in
	[ClearFrom top_of_screen] @ (localViewDisplay ([current_node] @ 
						  (T.get_children_list current_node)) top_of_screen)
    end					     
(*********************)
    
(**** redraw nodes above current node--this requires moving some text
      beneath the current subtree down  
****)


  | redraw tt (DrawAboveNode view) = let
	fun currentNodeVisible tt = let  

	    (* mark the left siblings as being off the screen *)
	    fun markLeftSiblings (ref T.Ground) = ()
	      | markLeftSiblings node = 

		(assignDisplayValue(node,modifyDisplay (RelocateNode (node,off_screen))); 
		 markLeftSiblings (T.get_left_sibling node))
		
	    val (_,DisplayBlock{global_range=(start,_),...}) = T.current_node_value tt
	in
	   if (start = off_screen)
	       then (markLeftSiblings (T.get_left_sibling (T.current_subtree tt)); false)
	   else true
	       
	end   
    in 
	case ((currentNodeVisible tt),view)

	    of (true,Subtree) => redraw tt (OptimizeHighlight Highlight)
		          
	     | (true,Node) => redraw tt (CurrentNode Highlight)

	                  (* DON'T OPTIMIZE HIGHLIGHTING HERE *)
	     | (false,Subtree) => (updateSubtreeStarts [T.current_subtree tt] top_of_screen;
				   redraw tt (BeneathSubtree (CurrentSubtree Highlight)))
		                  

	     | (false,Node) => (updateSubtreeStarts [T.current_subtree tt] top_of_screen;
				redraw tt (BeneathSubtree DrawSubtree))

    end
(*********************)


(**** redraw nodes that are drawn beneath the current subtree.
      The node beneath the current subtree are the subtrees of its right siblings and the
       subtrees of the right siblings of its ancestors.  Also make sure that the format of 
       each node is set to Plain.
*****)

  | redraw tt (BeneathSubtree redraw_action) = let
	val current_node = T.current_subtree tt

(* find the new start of the first node beneath the current subtree.  The first node beneath the
   current subtree is located after the bottom rightmost node of the current subtree. 
*)

	fun getStart node = let 

	    val (_,DisplayBlock{elision_test,global_range = (start,size),...}) = T.get_value node

	    fun rightMostChild [] = start + size
	      | rightMostChild (node::[]) = getStart node 
	      | rightMostChild (x::y::rest) = getStart (hd (rev (y::rest)))
	                                                                  

	in
	    case (elision_test=elided)
		of true => start + size 
		 | false => rightMostChild (T.get_children_list node)
	end 


	val start =  getStart current_node 
    in
(* Subtract the start of the first non empty right sibling of the 
   ancestor closest to the current node from the 
   start + size of the rightmost node in the current subtree. 
*) 
(* debuggin stuff 
       CIO.print ("updateBeneathSubtree: starting line number of node                  immedietely following current subtree -> "^
		   ( Integer.makestring start) ^ "\n");
*)
	if (T.at_top tt) (* deleting, applying or eliding *) 
                         (* at the root will cause all nodes to be redrawn. *)
	    then [ClearFrom start] @ (redraw tt redraw_action)

	else 
(* right sibling or right sibling of an ancestor is the node immedietly 
beneath the current subtree 
*)
	    (case (T.get_ancestor_right_sibling current_node)
(* 
		 of (ref T.Ground) =>(CIO.print("updateBeneathSubtree: all right siblings of ancestors or empty\n");
				       [ClearFrom start])
*)
		 of (ref T.Ground) =>[ClearFrom start]
	          | node => updateBeneathSubtree tt (node,start)) @ (redraw tt redraw_action)
    end
(*********************)

  | redraw tt (DrawElidedSubtree view_mode) = let
	val (_,DisplayBlock{elision_test=root_elision_test,global_range,...}) = 
	                  T.current_node_value tt
        
	fun updateStart (node,new_start) = let 
	    val (_,DisplayBlock{goal,by_line,elision_strings,elision_depth,elision_test,
				global_range=(_,size),...}) = T.get_value node
	                                                                    
	in 
	    assignDisplayValue (node,modifyDisplay (RelocateNode (node,new_start)));

	    if (root_elision_test=elided) orelse (elision_depth > visible) 
	       	then new_start
	    else new_start+size

	end

	fun updateSubtreeStarts [] _ = ()
	  | updateSubtreeStarts (node::rest) start = 
	    updateSubtreeStarts ((T.get_children_list node) @ rest) (updateStart (node,start))

    in
	updateSubtreeStarts [T.current_subtree tt] (#1 global_range) ;
	case (view_mode,root_elision_test = elided)
	    of (Node,true) => redraw tt (BeneathSubtree (CurrentNode Highlight))
	     | (Node,_) => redraw tt ((BeneathSubtree DrawSubtree))
	     | (Subtree,true) => redraw tt (BeneathSubtree (CurrentNode Highlight))
	     | (Subtree,_)  => redraw tt (BeneathSubtree (CurrentSubtree Highlight))
    end
    
(**** Redraw the entire tree starting from the root changing the start position of each node.****)

  | redraw tt LeavingLocalView = let
        val current_node = T.current_subtree tt
	    
	(* NOTE: The immediate left sibling and ancestors of a current node are marked as being off the screen
	   if the current node is at the top of the screen.  When a marked node is redrawn, its left siblings are
           marked. This occurs in redraw DrawAboveNode, since a marked node can only be redrawn in this function.
	*)

        fun setOffScreen (ref T.Ground) = ()
	  | setOffScreen node = 
		(assignDisplayValue(node,modifyDisplay (RelocateNode (node,off_screen))); 
		 setOffScreen (T.get_parent node))

	fun visitNode (node,new_start) = let 
	    val (_,DisplayBlock{goal,by_line,elision_strings,global_range=(_,size),elision_depth,...}) =
	                                                                    T.get_value node
	in 
	    assignDisplayValue (node,modifyDisplay (RelocateNode (node,new_start)));
	    assignDisplayValue (node,modifyDisplay (SetFormat (node,Plain)));

	    if (elision_depth > visible) 
		then (DoNothing,new_start)
	    else 
		(DisplayText(goal @ by_line @ elision_strings, Plain,(new_start,new_start+size)),
		 (new_start+size))
	end

	fun updateSubtree ([],start) (ref T.Ground) = []

	  | updateSubtree ([],start) next_subtree = 
	             updateSubtree ([next_subtree],start) (T.get_ancestor_right_sibling next_subtree)
										     

	  | updateSubtree ((node::rest),start) next_subtree  = 
	    (fn (x,y) =>(x::(updateSubtree (((T.get_children_list node) @ rest),y) next_subtree)))
	                 (visitNode (node,start))
    in	
	(case (T.get_left_sibling current_node)
	    of (ref T.Ground) => setOffScreen (T.get_parent current_node)
	     | left_sibling => setOffScreen (left_sibling));

	(* updateSubtreeStarts [current_node] top_of_screen;*)
	[ClearFrom top_of_screen] @ 
	((fn (DisplayText(t,_,r))::l => (DisplayText(t,Highlight,r))::l  | _ => [])
	         (updateSubtree ([current_node],top_of_screen) (T.get_ancestor_right_sibling current_node)))
    end

(****************************************************************************)

(* The following function creates display blocks for a list of subgoals. It returns
   the subgoals with their display blocks.
*)
fun getChildrenNodeVal tt [] = []
  | getChildrenNodeVal tt (subgoal_list) = let
	fun combine ([],[]) = []
	  | combine ((x::xs),(y::ys)) = (x,y)::(combine(xs,ys))
    in
	combine(subgoal_list,(initializeChildren ((T.current_subtree tt),
								subgoal_list)))
    end	    

(********************************************************)
(* returns the node which is drawn on the give line number. 
*)

fun findNode (current_node,Local,line_num) = let

    fun testNode (node,position) = let
	val (_,DisplayBlock{local_range,...}) = T.get_value node
    in
	if (position <= line_num) andalso (line_num <= position+local_range)
	    then (SOME node, position)
	else (NONE,position+local_range)
    end

    fun search ((node::node_list),current_position) = 
	(case (testNode (node,current_position))
	    of (NONE,new_position) => search (node_list,new_position) 
	     | (found_node,_) => found_node)

      | search ([],_) = NONE
in
    search (current_node::(T.get_children_list current_node),top_of_screen)
end

  | findNode (current_node,_,line_num) = let
	fun testNode line_num node = let
	    val (_,DisplayBlock{global_range=(start,size),elision_depth,...}) = T.get_value node
	in
(* debugging stuff 
	   (CIO.print ("findNode: line number of mouse pointer -> " ^ Integer.makestring(line_num) ^ 
			" \n starting line  of current node ->" ^ Integer.makestring(start) ^ 
			" \n last line of current node -> " ^ Integer.makestring(start+size) ^ "\n"));
*) 
	   (elision_depth = visible) andalso (start > off_screen) andalso 
	   ((start <= line_num) andalso (line_num < (start + size)))
	end


	fun getNextNode (ref T.Ground) = (ref T.Ground)
	  | getNextNode node  = T.get_ancestor_right_sibling node

	fun start_node node = let 
	    val(_,DisplayBlock{global_range = (start,_),...}) = T.get_value node
	in
	    if line_num >= start 
		then node 
	    else start_node (T.get_parent node)
	end
	
        fun search (ref T.Ground) = NONE
	  | search node = 
	         case (T.search_subtree node (testNode line_num))
		     of NONE => search (getNextNode node)
	              | found_node => found_node
    in
	search (start_node current_node)
    end
    

fun delete_subtree tt = let
    val (goal_val,disp_val) = case (T.current_node_value tt)
                  of ((TS.Incomplete g),d) => (g,d)
                   | ((TS.Complete(g,_,_),d)) => (g,d) 
    val new_tt = T.delete_children tt 			 
in  
    T.change_node_value(new_tt, ((TS.Incomplete goal_val),
					   modifyDisplay (SubtreeRemoved new_tt)))
end 


fun apply_tactic (tt,tact,tact_str) = 
      (let val (goal_val,disp_val) = case (T.current_node_value tt)
                     of ((TS.Incomplete g),d) => (g,d) 
                      | ((TS.Complete(g,_,_)),d) => (g,d)

           val (subgoals,valid) = S.apply_tactic(tact,goal_val) 

       in 
	  T.change_node_value(tt,((TS.Complete(goal_val,tact_str,valid)),
					   (modifyDisplay (InsertTactic (tt,tact_str)))));
	  T.change_children tt (map (fn (g,d) => ((TS.Incomplete g),d)) 
				(getChildrenNodeVal tt subgoals))
       end; 
	tt)

fun elide tt = changeDisplayValue (tt, modifyDisplay (ToggleElision tt))

fun do_action((tt,vm),MoveToChild) = 
    (case (!vm)
	 of Local => redraw (T.move_to_children (!tt)) ChildrenOnly
          | Subtree => if (currentNodeElided (!tt)) orelse ((T.get_children_list (T.current_subtree (!tt))) = []) 
			   then []
		       else 
			   (redraw (!tt) UnhighlightSubtree) @ (T.move_to_children (!tt); []) 

          | Node => if (currentNodeElided (!tt)) orelse ((T.get_children_list (T.current_subtree (!tt))) = []) 
			then []
		    else 			    
			(redraw (!tt) (CurrentNode Plain)) @  
			(redraw (T.move_to_children (!tt)) (CurrentNode Highlight)))
    
  | do_action ((tt,vm),MoveToTop) =
    if T.at_top (!tt) 
	then []
    else
    (case (!vm) 
	 of Local => redraw (T.move_to_top (!tt)) ChildrenOnly
          | Node  => (redraw (!tt) (CurrentNode Plain)) @ (redraw (T.move_to_top (!tt)) (DrawAboveNode Node))
	  | Subtree => 
		(changeDisplayValue ((!tt), modifyDisplay(SetFormat(T.current_subtree (!tt),Special)));
		redraw (T.move_to_top (!tt)) (DrawAboveNode Subtree)))

  | do_action ((tt,vm),MoveToParent) = 
    if T.is_null_tree (T.get_parent (T.current_subtree (!tt)))
	then []
    else
    (case (!vm) 
	 of Local => redraw (T.move_to_parent (!tt)) ChildrenOnly
          | Node  => (redraw (!tt) (CurrentNode Plain)) @ (redraw (T.move_to_parent (!tt)) (DrawAboveNode Node))
	  | Subtree => 
		(changeDisplayValue ((!tt), modifyDisplay(SetFormat(T.current_subtree (!tt),Special)));
		redraw (T.move_to_parent (!tt)) (DrawAboveNode Subtree)))	

  | do_action ((tt,vm),MoveLeft) =
    if T.is_null_tree (T.get_left_sibling (T.current_subtree (!tt)))
	then []
    else
    (case (!vm) 
	 of Local => redraw (T.move_to_left_sibling (!tt)) ChildrenOnly
	 
          | global => 
		let val disp_info = if global = Subtree 
					then redraw (!tt) (CurrentSubtree Plain) 
				    else redraw (!tt) (CurrentNode Plain)
		in
		    disp_info @ redraw (T.move_to_left_sibling (!tt)) (DrawAboveNode global)
		end)

  | do_action ((tt,vm),MoveRight) = 
    if T.is_null_tree (T.get_right_sibling (T.current_subtree (!tt)))
	then []
    else
    (case (!vm) 
	 of Local => redraw (T.move_to_right_sibling (!tt)) ChildrenOnly

	  | Subtree => (redraw (!tt) (CurrentSubtree Plain)) @  
		       (redraw (T.move_to_right_sibling (!tt)) (CurrentSubtree Highlight))
		         
	  | Node  => (redraw (!tt) (CurrentNode Plain)) @  
		     (redraw (T.move_to_right_sibling (!tt)) (CurrentNode Highlight)))

  | do_action ((tt,vm),HighlightSubtree) = 
    (case (!vm)

	(*  It is assumed that the entire current subtree is unhighlighted *)

	of Node => redraw (!tt) (CurrentNode Highlight)
	 | Subtree => redraw (!tt) (CurrentSubtree Highlight)
	 | _  => [])

  | do_action ((tt,vm),(MoveToNode line_num)) = let
	val some_node = findNode (T.current_subtree (!tt),(!vm),line_num)

	fun subtreeHighlighted node = let
	    val (_,DisplayBlock{form,... }) = T.get_value node
	in
	    form = Highlight
	end
    in
	(case ((!vm),some_node)
	     of (Local,SOME node) => redraw (T.move_to_subtree (!tt) node) ChildrenOnly

	      | (Subtree,SOME node) => 
		    let val current_node = T.current_subtree (!tt)
		    in 
			if (lineAtCurrentNode (current_node,line_num))
			    then []

    (* if node is highlighted then it is an ancestor of the current node *)

			else if (subtreeHighlighted node)  
				 then 

     (* Set the format of node found to Special so that its subtree remains highlighted
	when unhighlighting current subtree. 
     *)
				     (assignDisplayValue (node, modifyDisplay (SetFormat (node,Special)));
				     (redraw (!tt) (OptimizeHighlight Plain)) @ (T.move_to_subtree (!tt) node;[]))
			     else
				 if (desendantOf(!tt,node)) 
				     then 
					 (changeDisplayValue (!tt, modifyDisplay (SetFormat (T.current_subtree (!tt),Special)));
					  redraw (T.move_to_subtree (!tt) node) (OptimizeHighlight Highlight))
				 else
				     (redraw (!tt) (CurrentSubtree Plain)) @
				     (redraw (T.move_to_subtree (!tt) node) (CurrentSubtree Highlight))
		    end

	      | (Node,SOME node) => if (lineAtCurrentNode (T.current_subtree (!tt),line_num))
					   then []
				    else 
					(redraw (!tt) (CurrentNode Plain)) @ 
					(redraw (T.move_to_subtree (!tt) node) (CurrentNode Highlight)) 
						  
	     | (_,NONE) => [])
    end

(* Does the same thing as do_action MoveToNode but doesn't perform any highlighting *)

  | do_action ((tt,vm),(ApplyTacticToNode line_num)) = let 
	val some_node = findNode (T.current_subtree (!tt),(!vm),line_num)
    in
	(case ((!vm),some_node)
	     of (Local,SOME node) => redraw (T.move_to_subtree (!tt) node) ChildrenOnly
	      | (Subtree,SOME node) => if (lineAtCurrentNode (T.current_subtree (!tt),line_num))
					then []
				    else (redraw (!tt) (CurrentSubtree Plain)) @ (T.move_to_subtree (!tt) node; [])

	      | (Node,SOME node) => if (lineAtCurrentNode (T.current_subtree (!tt),line_num))
					then []
				    else (redraw (!tt) (CurrentNode Plain)) @ (T.move_to_subtree (!tt) node; [])
	      | (_,NONE) => [])
    end

  | do_action ((tt,vm),Delete) = 
    (case (!vm) 
	 of Local => redraw (delete_subtree (!tt)) ChildrenOnly
	  | _ => redraw (delete_subtree (!tt)) (BeneathSubtree InsertByline))

  | do_action ((tt,vm),Elide) =
    (case (!vm) 
	 of Local => (elide (!tt); [])
          | global  => redraw (elide (!tt)) (DrawElidedSubtree global)) 
	 
  | do_action ((tt,vm),ApplyTactic(tact,tactic_str)) = let
	val new_tt = apply_tactic ((!tt),tact,tactic_str)
    in
	case (!vm) 
	    of Local => redraw new_tt ChildrenOnly
	     | _  => (redraw new_tt (BeneathSubtree InsertByline)) @
			(if not (currentNodeElided new_tt)
			     then redraw (T.move_to_children new_tt) (CurrentNode Highlight)
			 else [])

    end

  | do_action ((tt,vm),ChangeMode) =
    (case (!vm) 
	 of Local => (vm := Node ; 
		      if currentNodeVisible (!tt) 
			  then redraw (!tt) LeavingLocalView
		      else redraw (T.move_to_top (!tt)) LeavingLocalView)

	  | Subtree => (vm := Local ; redraw (!tt) ChildrenOnly)
	  | Node => (vm := Subtree ; redraw (!tt) (CurrentSubtree Highlight)))

  | do_action ((tt,vm),Display) = let
	val new_tt = T.move_to_top (!tt)
    in
	updateSubtreeStarts [T.current_subtree new_tt] top_of_screen;
	(case (!vm) 
	     of Local => redraw new_tt  ChildrenOnly
	      | Subtree => redraw new_tt (CurrentSubtree Highlight)
	      | Node => redraw new_tt DrawSubtree)
    end

fun goal_of_ttree tt = 
    case (T.current_node_value tt) 
      of (Incomplete g,_) => g
       | (Complete(g,_,_),_) => g 

fun synthesize_event (tt,vm) = 
     let val result_event = 
           T.bottom_up((!tt),
		  fn (TS.Complete(_,_,valid),_) => (S.apply_validation (valid,[])),
		  fn ((TS.Complete(_,_,valid),_),evt_list) => 
		                              (S.apply_validation (valid,evt_list)))
     in if S.achieves(goal_of_ttree (!tt),result_event) 
        then result_event 
        else raise EventDoesNotAchieve 
     end 

fun synthesize_tactic_text (tt,vm) = 
      T.bottom_up((!tt),
		  (fn (TS.Complete(_,T,_),_) => T 
                    |  (TS.Incomplete _,_) => S.id_tac_text), 
		  (fn ((TS.Complete(_,T1,_),_),rl) => 
                          (case rl 
                                   of [T2] => if T2 = S.id_tac_text 
				              then T1 
					      else T1 ^ " " ^ S.then_text ^ " " ^ T2 
                                    | _ => if (fold (fn (s,r) => 
						      if (s = S.id_tac_text) andalso r 
						      then true else false) 
                                                    rl 
						    true)
                                           then T1
					   else T1 ^ " " 
					      ^ S.thenl_text ^ "[" 
					      ^ (fold (fn (s,r) => 
						       if r = "" then s else s ^ "," ^ r) 
						      rl 
						      "")
					      ^ "]")))

fun synthesize_text((tt,vm),outfile) = let
    val out_stream = IO.open_out outfile 
    fun print_node node = let
	val (_,DisplayBlock{goal,by_line,elision_strings,elision_depth,...}) = T.get_value node

        val output_strings = 
	    if elision_depth=visible 
		then (goal @ by_line @ elision_strings) 
	    else []

    in 
	app (fn y => IO.output(out_stream,(y ^ "\n"))) output_strings
    end
in
    T.preorder (!tt) (print_node,(),T.get_children_list,fn (_,_) => ()) [T.get_tree (!tt)];
    IO.close_out out_stream
end


fun mkTTree main_goal  = 
         (ref (T.new_tree (TS.Incomplete main_goal, 
			   initializeRoot main_goal)), 
	  ref Node)
				     
end 
