(* tree.sig
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * signature for tree data structure and manipulation functions 
 *)

signature TREE_SUPPORT =
sig 

type node_value

end 


signature TREE =
sig

structure S : TREE_SUPPORT 

exception EmptyTree

type tree
type node_id

datatype tree_node =  
           Ground
         | TreeNode of {value: S.node_value ref,
			current_subtree : bool ref, 
			parent : tree_node ref,
                        left_sibling : tree_node ref,
                        right_sibling : tree_node ref,
			children : (tree_node ref) list ref 
	                }

(* modification functions *) 
val new_tree : S.node_value -> tree 
val delete_children : tree -> tree 
val change_node_value : tree * S.node_value -> tree
val change_children : tree -> (S.node_value list) -> tree
val current_node_value : tree -> S.node_value
val node_apply : (S.node_value -> 'a) -> node_id -> 'a

(* query functions *) 
val at_top : tree -> bool
val at_ground : tree -> bool
val is_null_tree : node_id -> bool

(* navigation functions *) 
val move_to_top : tree -> tree 
val move_to_parent : tree -> tree 
val move_to_left_sibling : tree -> tree 
val move_to_right_sibling : tree -> tree 
val move_to_children : tree -> tree 
val move_to_subtree : tree -> node_id -> tree
val move_to_ancestor : tree -> (S.node_value -> 'a) -> (('a * 'a) -> bool) * 'a -> tree

(* traversal functions *)
val bottom_up : (tree * (S.node_value -> 'a) * ((S.node_value * ('a list )) ->'a)) -> 'a 
val preorder : tree -> (node_id -> 'b) * 'd * (node_id -> node_id list) * ('b * 'd -> 'd) -> node_id list -> 'd
val search_subtree : node_id -> (node_id -> bool) -> node_id option

(* access functions *)
val current_subtree : tree -> node_id 
val get_value : node_id -> S.node_value
val get_value_ref : node_id -> S.node_value ref
val get_parent : node_id -> node_id
val get_left_sibling : node_id -> node_id
val get_right_sibling : node_id -> node_id
val get_children : node_id -> node_id list ref
val get_children_list : node_id -> node_id list
val get_subtree : tree -> node_id 
val get_tree : tree -> node_id
val get_ground : unit -> node_id
val get_ancestor_right_sibling : node_id -> node_id

end 



