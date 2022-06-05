(* styles.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

(*abstraction Styles : STYLES =*)
structure Styles =
  struct

    structure Q = Quark
    structure AV = AttrValue
    structure PRS = ParseResourceSpecs

    exception BadStyleName

    exception BadStyleSpec of string * int

  (* a style_name is a key for searching a style database *)
    type style_name = {
	name : Quark.quark list,
	hash : int
      }

    fun extHash (hash, comp) =
	  Bits.andb(Bits.lshift(hash, 1), 0xffffff) + Q.hash comp

    fun styleName sl = let
	  fun chkName ([], comps, hash) = {name = rev comps, hash = hash}
	    | chkName (s::r, comps, hash) = let
		val comp = PRS.checkCompName s
		in
		  chkName (r, comp::comps, extHash(hash, comp))
		end
	  in
	    (chkName (sl, [], 0)) handle _ => raise BadStyleName
	  end

  (* compare two style names for equality *)
    fun sameStyleName ({name=n1, hash=h1} : style_name, {name=n2, hash=h2}) = let
	  fun cmp ([], []) = true
	    | cmp (q1::r1, q2::r2) = Quark.same(q1, q2) andalso cmp(r1, r2)
	    | cmp _ = false
	  in
	    (h1 = h2) andalso cmp(n1, n2)
	  end

  (* a style_view is a search key for finding attributes in a style.
   * It consists of a name and an ordered list of aliases.
   *)
    datatype style_view = SV of {
	name : style_name,
	aliases : style_name list
      }


  (* make a style_view from a name and list of aliases; the order of the
   * list defines the search order.
   *)
    val mkView = SV

  (* return the name part of the view *)
    fun nameOfView (SV{name, ...}) = name

  (* return the list of aliases that defines the view. *)
    fun aliasesOfView (SV{aliases, ...}) = aliases

  (* extend each of the names in the view by the component *)
    fun extendView (SV{name, aliases}, comp) = let
	  val compQ = PRS.checkCompName comp
	  fun ext {name, hash} = {
		  name = name @ [compQ],
		  hash = extHash(hash, compQ)
		}
	  in
	    SV{name = ext name, aliases = map ext aliases}
	  end

  (* concatenate two views; the first view has priority over the second. *)
    fun concatViews (SV{name=n1, aliases=a1}, SV{name=n2, aliases=a2}) =
	  SV{name = n1, aliases = a1 @ (n2::a2)}

  (* add a alias to the back or front of a view *)
    fun appendAlias (SV{name, aliases}, alias) =
	  SV{name=name, aliases = aliases@[alias]}
    fun prependAlias (SV{name, aliases}, alias) =
	  SV{name=name, aliases = alias::aliases}


  (*** attributes in the database ***)
    datatype attr = ATTR of {
	rawValue : string,
	cache : AV.attr_value ref
      }

    fun mkAttr rawValue = ATTR{
	    rawValue = rawValue,
	    cache = ref AV.AV_NoValue
	  }

  (* extract the value from an attribute object, performing
   * the conversion, if necessary, and caching the result.
   *)
    fun getAttrValue scr = let
	  val cvtValue = AV.cvtAttrValue scr
	  fun get (ATTR{rawValue, cache}, attrTy) = let
		val cacheVal = !cache
		in
		  case (cacheVal, attrTy)
		   of (AV.AV_Str _, AV.AT_Str) => cacheVal
		    | (AV.AV_Int _, AV.AT_Int) => cacheVal
		    | (AV.AV_Real _, AV.AT_Real) => cacheVal
		    | (AV.AV_Bool _, AV.AT_Bool) => cacheVal
		    | (AV.AV_Font _, AV.AT_Font) => cacheVal
		    | (AV.AV_Color _, AV.AT_Color) => cacheVal
		    | _ => (let val cvtVal = cvtValue (rawValue, attrTy)
			in
			  cache := cvtVal; cvtVal
			end
			  handle _ => AV.AV_NoValue)
		  (* end case *)
		end
	  in
	    get
	  end


  (*** The resource database tables ***)

    structure QuarkTbl = HashTable (struct
	type hash_key = Q.quark
	val hashVal = Q.hash
	val sameKey = Q.same
      end)

  (* maps on quarks *)
    type 'a qmap = 'a QuarkTbl.hash_table

    fun findQuark (tbl, q) = QuarkTbl.peek tbl q
    fun insQuark (tbl, q, v) = QuarkTbl.insert tbl (q, v)
    fun empty tbl = (QuarkTbl.numItems tbl = 0)


    type binding = PRS.binding

    datatype db_tbl = DBTBL of {
	tight : db_tbl qmap,
	loose : db_tbl qmap,		(* entries of the form "*path.attr:" *)
	attrs : (attr * binding) qmap	(* entries of the form "[*]attr:" *)
      }

    fun newDBTbl () = DBTBL{
	    tight = QuarkTbl.mkTable (8, Fail "db_tbl.tight"),
	    loose = QuarkTbl.mkTable (8, Fail "db_tbl.loose"),
	    attrs = QuarkTbl.mkTable (8, Fail "db_tbl.attrs")
	  }

  (* given a database and a component name path, find the list of
   * attribute binding tables keyed by the path.
   *)
    fun findAttrTbls (DBTBL{tight, loose, attrs}, path) = let
	  fun findLooseAttr attrTbl attrQ = (case findQuark(attrTbl, attrQ)
		 of (SOME(attr, LOOSE)) => (SOME attr)
		  | _ => NONE
		(* end case *))
	  fun findAttr attrTbl attrQ = (case findQuark(attrTbl, attrQ)
		 of (SOME(attr, LOOSE)) => (SOME attr)
		  | _ => NONE
		(* end case *))
	  fun find (tight, loose, attrs, [], tbls) =
		if (empty attrs) then tbls else (findAttr attrs)::tbls
	    | find (tight, loose, attrs, comp::r, tbls) = let
		val tbls' = (case (findQuark(tight, comp))
		       of NONE => tbls
			| (SOME(DBTBL{tight, loose, attrs})) =>
			    find (tight, loose, attrs, r, tbls)
		      (* end case *))
		fun findLoose ([], tbls) = tbls
		  | findLoose (comp::r, tbls) = (case findQuark(loose, comp)
		       of NONE => findLoose (r, tbls)
			| (SOME(DBTBL{tight, loose, attrs})) =>
			    findLoose (r, find (tight, loose, attrs, r, tbls))
		      (* end case *))
		val tbls'' = if (empty loose) then tbls' else findLoose(r, tbls')
		in
		  if (empty attrs) then tbls'' else (findLooseAttr attrs)::tbls''
		end
	  val tbls = rev (find (tight, loose, attrs, path, []))
(** NOTE: we may want to just return a list of tables, instead of a composite
 ** function, since views consist of a name plus aliases.
 **)
	  fun search attr = let
		fun search' [] = NONE
		  | search' (tbl::r) = (case (tbl attr)
		       of NONE => search' r
			| someVal => someVal
		      (* end case *))
		in
		  search' tbls
		end
	  in
	    search
	  end (* findAttrTbls *)

  (* insert an attribute binding into the database *)
    fun insertAttr (db, isLoose, path, name, attr) = let
	  fun find (tbl, comp) = (case findQuark(tbl, comp)
		 of (SOME db) => db
		  | NONE => let val db = newDBTbl()
		      in
			insQuark (tbl, comp, db); db
		      end
		(* end case *))
	  fun insert (DBTBL{tight, loose, attrs}, bind, path) = (
		case (bind, path)
		 of (PRS.TIGHT, (PRS.Name comp, bind)::r) =>
		      insert (find (tight, comp), bind, r)
		  | (PRS.LOOSE, (PRS.Name comp, bind)::r) =>
		      insert (find (loose, comp), bind, r)
		  | (_, (PRS.Wild, _)::_) =>
		      raise Fail "wildcard components not implemented"
		  | (_, []) => insQuark (attrs, name, (attr, bind))
		(* end case *))
	  in
	    insert (db, if isLoose then PRS.LOOSE else PRS.TIGHT, path)
	  end (* insertRsrcSpec *)


  (*** The database with view cache ***)
    datatype db = DB of {
	db : db_tbl,
	cache : (style_name * (PRS.attr_name -> attr option)) Weak.weak list ref
      }

    fun mkDB () = DB{
	    db = newDBTbl(),
	    cache = ref []
	  }

  (* this is a temporary function for building resource data bases by hand *)
    fun insertRsrcSpec (DB{db, cache}, {loose, path, attr, value}) = (
	  insertAttr (db, loose, path, attr, mkAttr value);
	  cache := [])

  (* given a database and a style view (name + aliases) construct the lookup
   * function for the view.
   *)
    fun constructView (DB{db, cache}, SV{name, aliases}) = let
	(* probe the cache for a binding for name; remove any stale
	 * cache entries that are encountered.
	 *)
	  fun probeCache name = let
		fun probe ([], l) = (rev l, NONE)
		  | probe (w::r, l) = (case (Weak.strong w)
		       of NONE => probe (r, l)
			| (SOME(name', binding)) =>
			    if (sameStyleName(name, name'))
			      then (w :: ((rev l) @ r), SOME binding)
			      else probe (r, w::l)
		      (* end case *))
		val (cache', result) = probe (!cache, [])
		in
		  cache := cache';  result
		end
	(* add a binding to the cache *)
	  fun addToCache item = cache := (Weak.weak item) :: !cache
	(* find the attribute tables for a name *)
	  fun findTbls (name : style_name) = (case (probeCache name)
		 of NONE => let
		      val tbls = findAttrTbls (db, #name name)
		      in
			addToCache (name, tbls);
			tbls
		      end
		  | (SOME tbls) => tbls
		(* end case *))
	(* search for an attribute in this view *)
	  fun findAttr attrName = let
		fun search [] = NONE
		  | search (name::r) = (case (findTbls name attrName)
		       of NONE => search r
			| attr => attr
		      (* end case *))
		in
		  search (name::aliases)
		end
	  in
	    findAttr
	  end


  (*** Styles ***)

    datatype req_msg
      = FindAttrs of {
	    key : style_view,
	    targets : (PRS.attr_name * AV.attr_type) list,
	    reply : (PRS.attr_name * AV.attr_value) list CML.cond_var
	  }

    datatype style = STY of {
	scr : EXeneBase.screen,
	reqCh : req_msg CML.chan
      }

  (* spawn a style server for the given screen and database *)
    fun mkStyleServer (scr, db) = let
	  val ch = CML.channel()
	  val getAttrValue = getAttrValue scr
	  fun findAttr key = let
		val find = constructView (db, key)
		in
		  fn (attrName, attrTy) => (case (find attrName)
		     of NONE => (attrName, AV.AV_NoValue)
		      | (SOME attr) => (attrName, getAttrValue (attr, attrTy))
		    (* end case *))
		end
	  fun server () = (
		case (CML.accept ch)
		 of (FindAttrs{key, targets, reply}) => let
		      val results = map (findAttr key) targets
		      in
			CML.writeVar (reply, results)
		      end
		(* end case *);
		server ())
	  in
	    CML.spawn server;
	    STY{
		reqCh = ch, scr = scr
	      }
	  end (* mkStyleServer *)

  (* create an empty style *)
    fun emptyStyle scr = mkStyleServer (scr, mkDB ())

  (* create a style, initializing it from a list of strings.  This
   * is for testing purposes.
   *)
    fun styleFromStrings (scr, sl) = let
	  val db = mkDB()
	  fun parse str = let
		val (PRS.RsrcSpec{loose, path, attr, value, ...}) =
		      PRS.parseRsrcSpec str
		in
		  insertRsrcSpec (db, {
		      loose=loose, path=path, attr=attr, value=value
		    })
		end handle PRS.BadSpec i => raise BadStyleSpec(str,i)
	  in
	    app parse sl;
	    mkStyleServer (scr, db)
	  end

  (* applicative maps from attribute names to attribute values *)
    structure QuarkMap = BinaryDict (struct
	type ord_key = Q.quark
	val cmpKey = Q.cmp
      end)

  (* *)
    fun findAttrs (STY{reqCh, scr, ...})  (name, queries) = let
	  val cvtValue = AV.cvtAttrValue scr
	  fun unzip ([], attrReqs, defaults) = (attrReqs, defaults)
	    | unzip ((attrName, attrTy, default)::r, attrReqs, defaults) =
		unzip(r, (attrName, attrTy)::attrReqs, (default, attrTy)::defaults)
	  fun zip ([], [], attrMap) = attrMap
	    | zip ((attrName, attrVal)::r1, (default, attrTy)::r2, attrMap) = (
		case (attrVal, default)
		 of (AV.AV_NoValue, NONE) => zip (r1, r2, attrMap)
		  | (AV.AV_NoValue, SOME v) =>
			zip (r1, r2,
			  QuarkMap.insert (attrMap, attrName, cvtValue(v, attrTy))
		      (* end case *))
		  | _ => zip (r1, r2, QuarkMap.insert (attrMap, attrName, attrVal))
		(* end case *))
	  val (attrReqs, defaults) = unzip (queries, [], [])
	  val replyV = CML.condVar()
	  val _ = CML.send (reqCh, FindAttrs{
		  key=name, targets=attrReqs, reply=replyV
		})
	  val map = zip (CML.readVar replyV, defaults, QuarkMap.mkDict())
	  fun find attr = (case (QuarkMap.peek (map, attr))
		 of NONE => AV.AV_NoValue
		  | (SOME v) => v
		(* end case *))
	  in
	    find
	  end (* findAttrs *)

(*****************************************************
    val style : style -> style
(* create a style that is the logical child of another style *)

(* NOTE: we may want to distinguish between "dynamic" and "static" attributes *)

    type attr_spec = {attr : string, value : string}

    val addResourceSpecs : style -> (string * string) list -> unit
	(* add a list of resource specifications to the style *)

    val addAttrs : style -> (style_name * attr_spec list) -> unit
	(* add a list of (attribute, value) pairs to a style; this will propagate
	 * to any listeners.
	 *)

    val deleteAttr : style -> (style_name * string) -> unit
	(* delete an attribute value from a style *)

    val mkStyle : style -> (style_name * attr_spec list) -> style
	(* create a new style from an existing style and a list of attribute
	 * value definitions.
	 *)

    val findAttr : style -> style_view -> string option
	(* lookup the given attribute in the given style *)

    datatype attr_change
      = ADD_ATTR of string
      | CHANGE_ATTR of string
      | DELETE_ATTR

    val listen : style -> style_view -> attr_change CML.event
	(* express an interest in changes to an attribute in a style.  This
	 * event will be enabled once for each change to the style that occurs
	 * after the event is created.
	 *)
*****************************************************)

  end; (* Styles *)
