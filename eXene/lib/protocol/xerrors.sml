(* xerrors.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * These are ML types for representing the X11 protocol error messages.
 *)

structure XErrors =
  struct
    local open XProtTypes in

    datatype xerror = XErr of {
	    kind : xerr_kind,
	    major_op : Word8.word,	(* major op-code (8 bits) *)
	    minor_op : word		(* minor op-code (16 bits) *)
	  }

    and xerr_kind
      = BadRequest			(* bad request code *)
      | BadValue of string		(* int parameter out of range *)
      | BadWindow of xid		(* parameter not a Window *)
      | BadPixmap of xid		(* parameter not a Pixmap *)
      | BadAtom of xid			(* parameter not an Atom *)
      | BadCursor of xid		(* parameter not a Cursor *)
      | BadFont of xid			(* parameter not a Font *)
      | BadMatch			(* parameter mismatch *)
      | BadDrawable of xid		(* parameter not a Pixmap or Window *)
      | BadAccess			(* depending on context: *)
					(*   - key/button already grabbed *)
					(*   - attempt to free an illegal cmap entry *)
					(*   - attempt to store into a read-only *)
					(*     cmap entry. *)
					(*   - attempt to modify the access control *)
					(*     list from other than the local host. *)
      | BadAlloc			(* insufficient resources *)
      | BadColor of xid			(* no such colormap *)
      | BadGC of xid			(* parameter not a GC *)
      | BadIDChoice of xid		(* choice not in range or already used *)
      | BadName				(* font or color name doesn't exist *)
      | BadLength			(* Request length incorrect *)
      | BadImplementation		(* server is defective *)

    val firstExtensionError = 0w128 : Word8.word
    val lastExtensionError = 0w255 : Word8.word

    end (* local open XTypes in *)
  end (* XErrors *)
