(* pen-rep.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * The internal representation of pen values.
 *)

structure PenRep : sig

    datatype pen_val_rep	    (* internal representation of pen component *)
				    (* values *)
      = PVRep_default
      | PVRep_wire of word		(* a value's wire representation *)
      | PVRep_pixmap of XProtTypes.pixmap_id
      | PVRep_point of Geometry.point
      | PVRep_rects of (XProtTypes.rect_order * Geometry.rect list)
      | PVRep_dashes of int list

    datatype pen = PEN of {	    (* a read-only drawing context.  This is *)
				    (* mapped onto a X-server GC by the GC-server *)
	mask : word,			(* a bitmask specifying which entries have *)
					(* non-default values. *)
	vals : pen_val_rep Vector.vector(* the value vector (read-only) *)
      }

    val numPenSlots : int

    val defaultPen : pen

    val penMatch : (word * pen * pen) -> bool

  end = struct

    structure G = Geometry
    structure XTy = XProtTypes

    val & = Word.andb
    val >> = Word.>>
    infix & >>

    datatype pen_val_rep	    (* internal representation of pen component *)
				    (* values *)
      = PVRep_default
      | PVRep_wire of word		(* a value's wire representation *)
      | PVRep_pixmap of XTy.pixmap_id
      | PVRep_point of G.point
      | PVRep_rects of (XTy.rect_order * G.rect list)
      | PVRep_dashes of int list

    datatype pen = PEN of {	    (* a read-only drawing context.  This is *)
				    (* mapped onto a X-server GC by the GC-server *)
	mask : word,			(* a bitmask specifying which entries have *)
					(* non-default values. *)
	vals : pen_val_rep Vector.vector(* the value vector (read-only) *)
      }

    val numPenSlots = 19

    val defaultPen = PEN{
	    mask = 0w0,
	    vals = Vector.tabulate (numPenSlots, fn _ => PVRep_default)
	  }

  (* test two pens to see if they match on a subset of their values. *)
    fun penMatch (0w0, _, _) = true
      | penMatch (usedMask, PEN{mask=m1, vals=v1}, PEN{mask=m2, vals=v2}) =
	  (v1 = v2) (* first test for same object *)
	  orelse let
	    val m = (usedMask & m1)
	    in
	      (m = (m2 & usedMask))
		andalso let
		  fun matchVal (PVRep_wire a, PVRep_wire b) = (a = b)
		    | matchVal (PVRep_pixmap(XTy.XID a), PVRep_pixmap(XTy.XID b)) =
			(a = b)
		    | matchVal (PVRep_point a , PVRep_point b) = (a = b)
		    | matchVal (PVRep_rects(o1, rl1), PVRep_rects(o2, rl2)) = let
			fun eq ([], []) = true
			  | eq ((a : G.rect)::ra, b::rb) = (a=b) andalso eq(ra, rb)
			  | eq _ = false
			in
			  (o1 = o2) andalso eq(rl1, rl2)
			end
		    | matchVal (PVRep_dashes a, PVRep_dashes b) = let
			fun eq ([], []) = true
			  | eq ((a : int)::ra, b::rb) = (a=b) andalso eq(ra, rb)
			  | eq _ = false
			in
			  eq(a, b)
			end
		    | matchVal _ = false
		  fun matchVals (0w0, _) = true
		    | matchVals (m, i) = 
			(((m & 0w1) = 0w0)
			  orelse matchVal(Vector.sub(v1, i), Vector.sub(v2, i)))
			andalso matchVals(m >> 0w1, i+1)
		  in
		    matchVals(m, 0)
		  end
	    end

  end (* PenRep *)
