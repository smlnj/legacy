(* styles-sig.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

signature STYLES =
  sig

    type comp_name = Quark.quark
    type attr_name = Quark.quark

    type style_name = comp_name list
	(* A style_name is a key for searching a style database.  A style
	 * name is a non-empty list of non-null component names taken from
	 * the following character set:  A..Z a..z 0..9 _ -
	 *)
    type style_view
	(* a style_view is a search key for finding attributes in a style.
	 * It consists of a name and an ordered list of aliases.
	 *)

    type style

    exception BadStyleName

    val extendName : (style_name * string) -> style_name
	(* extend a style name by the component *)

    val mkView : {name : style_name, aliases : style_name list} -> style_view
	(* make a style_view from a name and list of aliases; the order of the
	 * list defines the search order.
	 *)

    val nameOfView : style_view -> style_name
	(* return the name part of the view *)

    val aliasesOfView : style_view -> style_name list
	(* return the list of aliases that defines the view. *)

    val extendView : (style_view * string) -> style_view
	(* extend each of the names in the view by the component *)

    val concatViews : (style_view * style_view) -> style_view
	(* concatenate two views; the first view has priority over the second. *)

    val appendAlias : (style_view * style_name) -> style_view
    val prependAlias : (style_name * style_view) -> style_view
	(* add a alias to the back or front of a view *)

    val emptyStyle : EXeneBase.screen -> style
	(* create an empty style *)

    val style : style -> style
	(* create a style that is the logical child of another style *)

(* NOTE: we may want to distinguish between "dynamic" and "static" attributes *)

    type attr_spec = {attr : attr_name, value : string}

    val addAttrs : style -> (style_name * attr_spec list) list -> unit
	(* add a list of (attribute, value) pairs to a style; this will propagate
	 * to any listeners.
	 *)

    val deleteAttr : style -> (style_name * attr_name) -> unit
	(* delete an attribute value from a style *)

    val mkStyle : style -> (style_name * attr_spec list) list -> style
	(* create a new style from an existing style and a list of attribute
	 * value definitions.  This is equivalent to (addAttrs o style).
	 *)

    val findAttr : style -> (style_view * attr_name list)
	  -> (attr_name * string option) list
	(* lookup the given attribute in the given style.  This returns NONE,
	 * if the attribute is undefined. Note that the partial application of
	 * this function computes a list of fingers into the style database
	 * so that the actual searching for attributes will be faster.
	 *)

    datatype attr_change
      = ADD_ATTR of string
      | CHANGE_ATTR of string
      | DELETE_ATTR

    val listen : style -> (style_view * attr_name list)
	  -> (attr_name * attr_change CML.event) list
	(* express an interest in changes to an attribute in a style.  This
	 * event will be enabled once for each change to the style that occurs
	 * after the event is created.
	 *)

  end; (* STYLES *)
