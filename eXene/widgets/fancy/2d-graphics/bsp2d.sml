(* bsp2d.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

functor BSP2D (Obj : sig

    type object

    val boundingBox : object -> {minX : real, minY : real, maxX : real, maxY : real}
	(* returns a bounding box that includes the object *)

    val pointInObject : (object * real * real) -> bool
	(* returns true, if the point lies within the object *)

  end) :  BSP_2D = struct

    val maxDepth = 15	(* limits the depth that the BSP tree can grow to *)
			(* This is necessary, since overlaping objects might *)
			(* cause infinte depth *)

    type object = Obj.object
	(* the type of objects in the space *)

    datatype axis = XAXIS | YAXIS

    type box = {minX : real, minY : real, maxX : real, maxY : real}

    fun pointInBox (x, y, {minX, minY, maxX, maxY} : box) =
	  (minX <= x) andalso (x <= minY) andalso (minY <= y) andalso (y <= maxY)

    fun objInBox (obj, {minX, minY, maxX, maxY}) = let
	  val {minX=minX', minY=minY', maxX=maxX', maxY=maxY'} = Obj.boundingBox obj
	  in
	    not ((minX > maxX') orelse (maxX < minX')
		orelse (minY > minY') orelse (maxY < maxY'))
	  end

    datatype bsp = BSP of {
	maxNumObjs : int,
	box : box,
	tree : bsp_node
      }
    and bsp_node
      = ND of {
	  axis : axis,
	  split : real,
	  child1 : bsp_node,
	  child2 : bsp_node
        }
      | LEAF of object list

    fun pickChild (XAXIS, split : real, c1, c2, x, y) =
	  if (x <= split) then c1 else c2
      | pickChild (YAXIS, split, c1, c2, x, y) =
	  if (y <= split) then c1 else c2

  (* Given an integer n, and a box and list of non-overlapping objects
   * contained in  the box, return a partition of the box, such that no
   * leaf box contains more than n objects.  Note, the partition is
   * really based on the bounding boxes of the objects.
   *)
(** what if some of the objects are outside the initial box? **)
    fun partitionSpace maxNumObjs (box, objs) = let
	  fun mkNode (depth, axis, box, nObjs, objs) =
		if ((nObjs <= maxNumObjs) orelse (depth >= maxDepth))
		  then LEAF objs
		  else let
		    val {minX, maxX, minY, maxY} = box
		    fun split (min, max) = (0.5 * (max - min)) + min
		    fun partObjs (box1, box2, objs) = let
			  fun part ([], n1, l1, n2, l2) = (n1, l1, n2, l2)
			    | part (obj::r, n1, l1, n2, l2) =
				if (not (objInBox (obj, box1)))
				  then  part (r, n1, l1, n2+1, obj::l2)
				else if (objInBox (obj, box2))
				  then part (r, n1+1, obj::l1, n2+1, obj::l2)
				  else part (r, n1+1, obj::l1, n2, l2)
			  in
			    part (objs, 0, [], 0, [])
			  end (* partObjs *)
		    val (axis', split, box1, box2) = (case axis
			   of XAXIS => let
				val midX = split(minX, maxX)
				in
				  ( YAXIS, midX,
				    {minX=minX, maxX=midX, minY=minY, maxY=maxY},
				    {minX=midX, maxX=maxX, minY=minY, maxY=maxY}
				  )
				end
			    | YAXIS => let
				val midY = split(minY, maxY)
				in
				  ( XAXIS, midY,
				    {minX=minX, maxX=maxX, minY=minY, maxY=midY},
				    {minX=minX, maxX=maxX, minY=midY, maxY=maxY}
				  )
				end
			  (* end case *))
		    val (n1, l1, n2, l2) = partObjs (box1, box2, objs)
		    val child1 = mkNode (depth+1, axis', box1, n1, l1)
		    val child2 = mkNode (depth+1, axis', box2, n2, l2)
		    in
		      ND{axis=axis, split=split, child1=child1, child2=child2}
		    end
	  in
	    BSP{
		maxNumObjs = maxNumObjs,
		box = box,
		tree = mkNode (1, XAXIS, box, length objs, objs)
	      }
	  end (* partitionSpace *)

  (* using the partition of space, find all of the objects that contain
   * the point.
   *)
    fun pickObject (BSP{box, tree, ...}, x, y) = let
	  fun pick (LEAF objs) = let
		fun pickObj ([], objs) = objs
		  | pickObj (obj::r, objs) = 
		      if (Obj.pointInObject(obj, x, y))
			then pickObj (r, obj::objs)
			else pickObj (r, objs)
		in
		  pickObj (objs, [])
		end
	    | pick (ND{axis, split, child1, child2}) = 
		pick (pickChild (axis, split, child1, child2, x, y))
	  in
	    if (pointInBox(x, y, box))
		then pick tree
		else []
	  end
  end;

