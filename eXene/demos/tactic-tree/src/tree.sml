(* tree.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * functor for tree data structure and manipulation functions 
 *)

functor TreeFun (S : TREE_SUPPORT) : TREE = 
struct 

structure S = S 

exception EmptyTree

datatype tree_node =  
           Ground
         | TreeNode of {value: S.node_value ref,
			current_subtree : bool ref, 
			parent : tree_node ref,
                        left_sibling : tree_node ref,
                        right_sibling : tree_node ref,
			children : (tree_node ref) list ref 
	                }
	   
type tree = 
    {root : tree_node ref, subtree : tree_node ref} ref 

type node_id = tree_node ref 

(* basic functions *) 

fun get_subtree (ts:tree) =  #subtree(!ts)
fun get_subtree_node (ts:tree) = !(get_subtree ts)
fun get_tree (ts:tree) =  #root(!ts)
fun get_tree_node (ts:tree) = !(get_tree ts)

fun get_value t =
       case !t
	 of Ground => raise EmptyTree
          | (TreeNode r) => !(#value r)
       
fun get_value_ref t =
       case !t
	 of Ground => raise EmptyTree
          | (TreeNode r) => #value r

fun get_subtree_flag t =
       case !t
	 of Ground => raise EmptyTree
          | (TreeNode r) => #current_subtree r
       
fun get_parent_node t = 
       case !t
	 of Ground => raise EmptyTree
          | (TreeNode r) => !(#parent r)

fun get_parent t = 
       case !t
	 of Ground => raise EmptyTree 
          | (TreeNode r) => #parent r
       

fun get_left_sibling t = 
       case !t
	 of Ground => raise EmptyTree
          | (TreeNode r) => #left_sibling r
	
fun get_right_sibling t = 
       case !t
	 of Ground => raise EmptyTree
          | (TreeNode r) => #right_sibling r
       
fun get_children t = 
       case !t
	 of Ground => raise EmptyTree 
          | (TreeNode r) => #children r 

fun get_children_list t = 
       case !t
	 of Ground => raise EmptyTree 
	          | (TreeNode r) => !(#children r)

fun get_ground () = ref Ground

fun get_ancestor_right_sibling (ref Ground) = (ref Ground)
  | get_ancestor_right_sibling (node as (ref (TreeNode r))) = 	    
         case (get_right_sibling node)
	     of (ref Ground) => get_ancestor_right_sibling (get_parent node)
	      | right_sibling  => right_sibling

fun current_node_value tt = get_value (get_subtree tt)

fun new_tree g  = 
	let val new = ref (TreeNode {value = ref g,
				     current_subtree = ref true,
				     parent = ref Ground,
				     left_sibling = ref Ground,
				     right_sibling = ref Ground,
				     children = ref [] })
 
	in 
	    ref {root = new,subtree = ref (!new)}
        end 

fun delete_children ts = 
     (let val f = get_children (get_subtree ts)
       in f := []  end ;
     ts)

val current_subtree  = get_subtree

fun is_null_tree t = case !t of Ground => true | _ => false
fun at_ground (t:tree)  = is_null_tree (#subtree (!t))

fun at_top ts  = is_null_tree (get_parent (get_subtree ts))

fun move_to_top ts = 
     (let val st = get_subtree ts 
          val t = get_tree ts 
      in ((get_subtree_flag st) := false; (get_subtree_flag t) := true; st := !t)
      end ;ts)

fun move_to_parent ts = 
     (let val t = get_subtree ts 
        in let val p = (get_parent t)
            in if is_null_tree p 
               then () 
               else ((get_subtree_flag t) := false; (get_subtree_flag p) := true; t := !p)
            end 
        end ;ts)
		      
fun move_to_left_sibling ts = 
     (let val t = get_subtree ts 
        in let val r = get_left_sibling t 
	   in if is_null_tree r 
	      then () 
              else ((get_subtree_flag t) := false; (get_subtree_flag r) := true; t := !r) end 
        end ;
     ts)

fun move_to_right_sibling ts = 
     (let val t = get_subtree ts 
        in let val r = get_right_sibling t 
	   in if is_null_tree r 
              then () 
              else ((get_subtree_flag t) := false; 
		    (get_subtree_flag r) := true; 
		    t := !r) 
            end 
        end ;
     ts)


fun move_to_children ts  = 
     (let val t = get_subtree ts 
        in (case get_children_list t 
	     of [] => () 
	      | (r ::_) => ((get_subtree_flag t) := false; 
		            (get_subtree_flag r) := true; 
		            t := !r) )
        end ;
     ts)

fun move_to_subtree ts r  = 
     (let val t = get_subtree ts 
        in ((get_subtree_flag t) := false; 
            (get_subtree_flag r) := true; 
	    t := !r) 
	end; ts)

fun move_to_ancestor tt gval (test,key) = 
	let fun move_up t = 
		if test (key,gval(current_node_value t)) orelse (at_top t)
		    then t
		    else move_up (move_to_parent t)
	
	in move_up (move_to_parent tt) end

fun change_children ts [] = ((get_children (get_subtree ts)) := []; ts)
  | change_children ts (hd::tl) = 
    (let val t = get_subtree ts
         val new_p = ref (!t )
         val first = ref (TreeNode{value = ref hd,
				   parent = new_p,
				   current_subtree = ref false, 
				   left_sibling = ref Ground,
				   right_sibling = ref Ground,
				   children = ref []})
         fun mk_siblings (left,[]) = [left]
           | mk_siblings (left,v::l) = 
                 let val new = ref 
		     (TreeNode{value = ref v,
			      parent = new_p,
			      current_subtree = ref false, 
			      left_sibling = left,
			      right_sibling = ref Ground,
			      children = ref[]})
		 in (get_right_sibling left) := (!new);
		     left::(mk_siblings(new,l))
		 end 
     in (get_children t) := (mk_siblings (first,tl)) end;
     ts)

fun change_node_value (tt,v) = ((get_value_ref (get_subtree tt)) := v; tt) 

fun bottom_up (ts,f1,f2) = 
     let fun aux t = 
       case !t 
	 of Ground => raise EmptyTree
	  | (TreeNode r) => 
	        let val kids = !(#children r)
		in case kids 
		     of [] => (f1 (!(#value r)))
		      | _  => (f2 ((!(#value r)),map aux kids))
                end
     in aux (get_tree ts) end 

fun node_apply f n_id = f (get_value n_id)

fun preorder (dt:tree) (visit,g,h,oper) []  = g
 |  preorder dt (visit,g,h,oper) (node::stack) = 
	oper ((visit node), preorder dt (visit,g,h,oper) ((h node) @ stack))

fun search_subtree (node as (ref (TreeNode n))) test_func = let
    val right_sibling = get_right_sibling node
    val left_most_child = (hd (get_children_list node) handle Hd => (ref Ground))
			   
	  
in
    case (test_func node)
	of false => (case (search_subtree left_most_child test_func)
			of NONE => (search_subtree right_sibling test_func)
		        | found_node => found_node)
       | true => SOME node
end

  | search_subtree (ref Ground) _ = NONE

end


