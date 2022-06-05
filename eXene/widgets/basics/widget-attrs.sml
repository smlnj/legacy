(* widget-attrs.sml
 *
 * COPYRIGHT (c) 1991,1994 by AT&T Bell Laboratories.
 *
 * High-level view of widget attributes.
 *)

signature WIDGET_ATTRS =
  sig
    exception InvalidAttr of string

    type attr_spec = Attrs.attr_name * Attrs.attr_type * Attrs.attr_value
    type arg = Attrs.attr_name * Attrs.attr_value

    type view
    type attrs

    val attrs : (view * attr_spec list * arg list) -> attrs
    val findAttr : attrs -> Attrs.attr_name -> Attrs.attr_value

  end (* WIDGET_ATTRS *)

structure WidgetAttrs : WIDGET_ATTRS =
  struct
    exception InvalidAttr of string

    type attr_spec = Attrs.attr_name * Attrs.attr_type * Attrs.attr_value
    type arg = Attrs.attr_name * Attrs.attr_value
    type view = Styles.style_view * Styles.style
    datatype attrs = ATTRS of {lookup : Attrs.attr_name -> Attrs.attr_value}

    structure QuarkTbl = HashTableFn (struct
	type hash_key = Quark.quark
	val hashVal = Quark.hash
	val sameKey = Quark.same
      end)

    fun okay (attrSpecs : attr_spec list) n =
          List.find (fn s => Quark.same(n,#1 s)) attrSpecs
     
    fun add (okay,tbl) (n,v) =
          case okay n of
            SOME (_,t,_) => QuarkTbl.insert tbl (n,(v,t))
          | NONE => ()
    
    fun attrs ((name,style),attrSpecs, []) =
            ATTRS{lookup = Styles.findAttrs style (name, attrSpecs)}
      | attrs ((name,style),attrSpecs, args) = let
          val cvt = Attrs.cvtAttrValue (Styles.ctxtOf style)
          val base = Styles.findAttrs style (name, attrSpecs)
          val tbl = QuarkTbl.mkTable (8, Fail "widget-attrs")
          fun lookup n = case QuarkTbl.find tbl n of
                           SOME v => cvt v
                         | NONE => base n
          in
            app (add (okay attrSpecs,tbl)) args;
            ATTRS{lookup = lookup}
          end

    fun findAttr (ATTRS{lookup}) name = 
          (lookup name) handle _ => raise InvalidAttr (Quark.stringOf name)

  end (* WidgetAttrs *)
