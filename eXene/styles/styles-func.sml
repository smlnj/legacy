(* styles-func.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

signature ATTRS =
  sig
    type attr_type
    type attr_value
    type attr_ctxt

    exception NoConversion
    exception BadAttrValue

    val AV_NoValue : attr_value
    val sameValue : attr_value * attr_value -> bool
    val sameType : attr_value * attr_type -> bool
    val cvtString : attr_ctxt -> (string * attr_type) -> attr_value
    val cvtAttrValue : attr_ctxt -> (attr_value * attr_type) -> attr_value
  end

functor StylesFunc (AV : ATTRS) (* : STYLES *) =
  struct

    structure Weak = SMLofNJ.Weak
    structure Q = Quark
    structure PRS = ParseResourceSpecs

    exception BadStyleName

  (* a style_name is a key for searching a style database *)
    type style_name = {
    name : Quark.quark list,
    hash : word
      }

    fun extHash (hash, comp) =
      Word.andb(Word.<<(hash, 0w1), 0wxffffff) + Q.hash comp

    fun styleName sl = let
      fun chkName ([], comps, hash) = {name = rev comps, hash = hash}
        | chkName (s::r, comps, hash) = let
        val comp = PRS.checkCompName s
        in
          chkName (r, comp::comps, extHash(hash, comp))
        end
      in
        (chkName (sl, [], 0w0)) handle _ => raise BadStyleName
      end

  (* compare two style names for equality *)
    fun sameStyleName ({name=n1, hash=h1} : style_name, {name=n2, hash=h2}) = let
      fun cmp ([], []) = true
        | cmp (q1::r1, q2::r2) = Quark.same(q1, q2) andalso cmp(r1, r2)
        | cmp _ = false
      in
        (h1 = h2) andalso cmp(n1, n2)
      end

  (* extend a style name with a component *)
    fun extendStyleName ({name, hash} : style_name, comp) : style_name = let
      val compQ = Quark.quark comp
      in
        { name = name @ [compQ],
          hash = extHash(hash, compQ)
        }
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
    fun getAttrValue ctxt = let
      val cvtValue = AV.cvtString ctxt
      fun get (ATTR{rawValue, cache}, attrTy) = let
        val cacheVal = !cache
        in
                  if AV.sameType (cacheVal, attrTy) then cacheVal
                  else let 
                    val cvtVal = cvtValue (rawValue, attrTy)
            in cache := cvtVal; cvtVal end handle _ => AV.AV_NoValue
        end
      in
        get
      end


  (*** The resource database tables ***)

    structure QuarkTbl = HashTableFn (struct
    type hash_key = Q.quark
    val hashVal = Q.hash
    val sameKey = Q.same
      end)

  (* maps on quarks *)
    type 'a qmap = 'a QuarkTbl.hash_table

    fun findQuark (tbl, q) = QuarkTbl.find tbl q
    fun insQuark (tbl, q, v) = QuarkTbl.insert tbl (q, v)
    fun empty tbl = (QuarkTbl.numItems tbl = 0)


    type binding = PRS.binding

    datatype db_tbl = DBTBL of {
    tight : db_tbl qmap,
    loose : db_tbl qmap,        (* entries of the form "*path.attr:" *)
    attrs : (attr * binding) qmap   (* entries of the form "[*]attr:" *)
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
        reply : (PRS.attr_name * AV.attr_value) list SyncVar.ivar
      }
(* added ddeboer May 2004 *)
      | GetDb of db SyncVar.ivar
(* end additions by ddeboer *)

    datatype style = STY of {
    ctxt : AV.attr_ctxt,
    reqCh : req_msg CML.chan
      }

    fun ctxtOf (STY{ctxt,...}) = ctxt

  (* spawn a style server for the given context and database *)
    fun mkStyleServer (ctxt, db) = let
      val ch = CML.channel()
      val getAttrValue = getAttrValue ctxt
      fun findAttr key = let
        val find = constructView (db, key)
        in
          fn (attrName, attrTy) => (case (find attrName)
             of NONE => (attrName, AV.AV_NoValue)
              | (SOME attr) => (attrName, getAttrValue (attr, attrTy))
            (* end case *))
        end
      fun server () = (
        case (CML.recv ch)
         of (FindAttrs{key, targets, reply}) => let
              val results = map (findAttr key) targets
              in
            SyncVar.iPut (reply, results)
              end
(* added ddeboer May 2004 *)
          | (GetDb(reply)) => (SyncVar.iPut(reply,db))
(* end additions by ddeboer *)
        (* end case *);
        server ())
      in
        CML.spawn server;
        STY{
        reqCh = ch, ctxt = ctxt
          }
      end (* mkStyleServer *)

  (* create an empty style *)
    fun emptyStyle ctxt = mkStyleServer (ctxt, mkDB ())

  (* create a style, initializing it from a list of strings.  This
   * is for testing purposes.
   *)
    fun styleFromStrings (ctxt, sl) = let
      val db = mkDB()
      fun parse str = let
        val (PRS.RsrcSpec{loose, path, attr, value, ...}) =
              PRS.parseRsrcSpec str
        in
          insertRsrcSpec (db, {
              loose=loose, path=path, attr=attr, value=value
            })
        end
      in
        app parse sl;
        mkStyleServer (ctxt, db)
      end

  (* applicative maps from attribute names to attribute values *)
    structure QuarkMap = BinaryMapFn (struct
    type ord_key = Q.quark
    val compare = Q.cmp
      end)

  (* *)
    fun findAttrs (STY{reqCh, ctxt, ...})  (name, queries) = let
      val cvtValue = AV.cvtAttrValue ctxt
      fun unzip ([], attrReqs, defaults) = (attrReqs, defaults)
        | unzip ((attrName, attrTy, default)::r, attrReqs, defaults) =
        unzip(r, (attrName, attrTy)::attrReqs, (default, attrTy)::defaults)
      fun zip ([], [], attrMap) = attrMap
        | zip ((attrName, attrVal)::r1, (dflt, attrTy)::r2, attrMap) =
                if AV.sameValue(attrVal,AV.AV_NoValue) then
          if AV.sameValue(dflt, AV.AV_NoValue) 
                    then zip (r1, r2, attrMap)
            else zip (r1, r2,
              QuarkMap.insert (attrMap, attrName, cvtValue(dflt, attrTy)))
        else zip (r1, r2, QuarkMap.insert (attrMap, attrName, attrVal))
      val (attrReqs, defaults) = unzip (queries, [], [])
      val replyV = SyncVar.iVar()
      val _ = CML.send (reqCh, FindAttrs{
          key=name, targets=attrReqs, reply=replyV
        })
      val map = zip (SyncVar.iGet replyV, defaults, QuarkMap.empty)
      fun find attr = (case (QuarkMap.find (map, attr))
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

    (* Additions by ddeboer, May 2004. 
     * Dusty deBoer, KSU CIS 705, Spring 2004. *)
        
    (* utility function: list the resource specs from a db. 
     * a resource spec is roughly:
     * PRS.RsrcSpec{loose:bool,path:(PRS.component*PRS.binding)list,attr:PRS.attr_name,value:string,ext:(false)}
     *)
    fun listRsrcSpecs (DB{db,cache}) =
        let
        fun lstSpcs (DBTBL{tight,loose,attrs},pth) =
            (* list specs from attrs; that is the easy part. *)
            (let
            val (qabLst: (Quark.quark * (attr * binding)) list) = QuarkTbl.listItemsi attrs
            val (rscSpL: PRS.resource_spec list) = 
                List.map 
                    (fn (qu,(ATTR{rawValue,...},bind)) => 
                        PRS.RsrcSpec{loose=(case bind of PRS.LOOSE=>true | PRS.TIGHT=>false),
                            path=pth,attr=qu,value=rawValue,ext=false}) 
                    qabLst
            val (loosqtLst: (Quark.quark * db_tbl) list) = 
                    QuarkTbl.listItemsi loose
            val (loostpLst: (db_tbl * (PRS.component * PRS.binding) list) list) =
                    List.map (fn (q,t) => (t,pth@[(PRS.Name q,PRS.LOOSE)])) loosqtLst
            val (loosRscSpL: PRS.resource_spec list) = 
                    List.concat (List.map lstSpcs loostpLst)
            val (tghtqtLst: (Quark.quark * db_tbl) list) = 
                    QuarkTbl.listItemsi tight
            val (tghttpLst: (db_tbl * (PRS.component * PRS.binding) list) list) =
                    List.map (fn (q,t) => (t,pth@[(PRS.Name q,PRS.TIGHT)])) tghtqtLst
            val (tghtRscSpL: PRS.resource_spec list) = 
                    List.concat (List.map lstSpcs tghttpLst)    
            in (rscSpL@loosRscSpL@tghtRscSpL) end)
        in lstSpcs (db,[]) end
    
    (* another utility function - get the resource specs from a style, then convert them
     * to strings. This could be used to write a style back to a database, as in
     * XrmPutFileDatabase().
     *)
    fun stringsFromStyle (STY{reqCh,ctxt}) =
        let
        val replyV = SyncVar.iVar()
        val _ = CML.send(reqCh,GetDb(replyV))
        val db = SyncVar.iGet replyV
        in  
            List.map
                (fn PRS.RsrcSpec{loose,path,attr,value,...} => 
                    (String.concat 
                        (List.map 
                            (fn (PRS.Name cn,b) => 
                                (case b of PRS.LOOSE => "*" | PRS.TIGHT => ".")^(Quark.stringOf cn))
                            path))^
                    (if loose then "*" else ".")^(Quark.stringOf attr)^":"^value)
            (listRsrcSpecs db)
        end;
    
    (* mergeStyles(sourceStyle: style, targetStyle: style) -> mergedStyle: style
     * 
     * mergedStyle should consist of the same resource specifications that would
     * exist in targetStyle if all resource specifications of sourceStyle were
     * inserted into targetStyle. That is, in particular, a tight binding of a
     * particular resource specification in targetStyle would not be overwritten
     * by a loose binding of the same specification in sourceStyle.
     *
     * The behavior of this should be similar to XrmMergeDatabases(db1,db2) of Xlib;
     * in particular, resources specified in db1 should override those in db2.
     *)
    fun mergeStyles (STY{reqCh=rc1,ctxt=ctxt1},STY{reqCh=rc2,ctxt=ctxt2}) =
        let
        val repv1 = SyncVar.iVar()
        val repv2 = SyncVar.iVar()
        val _ = CML.send(rc1,GetDb(repv1))
        val _ = CML.send(rc2,GetDb(repv2))
        val (db1: db) = SyncVar.iGet repv1
        val (db2: db) = SyncVar.iGet repv2
        val rsrcsp1 = listRsrcSpecs db1
        fun insRsrcSpcs ([]) = ()
          | insRsrcSpcs (PRS.RsrcSpec{loose,path,attr,value,...}::rs) =
                (insertRsrcSpec (db2,{loose=loose,path=path,attr=attr,value=value});
                insRsrcSpcs rs)
        in
            (insRsrcSpcs rsrcsp1;
            mkStyleServer(ctxt2,db2))
        end

    (*
    fun mergeStyles (STY{reqCh=rc1,ctxt=ctxt1},STY{reqCh=rc2,ctxt=ctxt2}) =
        let
        val repv1 = SyncVar.iVar()
        val repv2 = SyncVar.iVar()
        val _ = CML.send(rc1,GetDb(repv1))
        val _ = CML.send(rc2,GetDb(repv2))
        val (db1: db) = SyncVar.iGet repv1
        val (db2: db) = SyncVar.iGet repv2
        * insert every entry in quarktable1 into quarktable2 *
        fun qtMerge (ht1,ht2) =
            (List.app (fn (k,v) => (QuarkTbl.insert ht2 (k,v))) (QuarkTbl.listItemsi ht1))
        * merge: insert all attribute values from db1 into db2 *
        fun dbMerge (DBTBL{tight=tght1,loose=loos1,attrs=attr1},
                     DBTBL{tight=tght2,loose=loos2,attrs=attr2}) =
                        (qtMerge(attr1,attr2);dbMerge(tght1,tght2);dbMerge(loos1,loos2))
        in (dbMerge(db1,db2); mkStyleServer(ctxt2,db2)) end
    *)
    
    (**
     * Parsing of command line arguments:
     *----------------------------------
     *)
    (* options specified on the command line may be of two types:
     * - a "named" option, such as "x" and "y" in "add -x 1 -y 3" where "x" and "y" are simple
     *   arguments to the "add" program that adds them together, and where the "add" program
     *   simply wishes to determine the value of "x" and "y", or
     * - a "resource spec" option, such as "foreground" in "xapp -foreground black" where the
     *   "xapp" wishes to obtain a resource specification like "*foreground: black" from these
     *   command line arguments.
     *)
    (* Named options should be typically useful in obtaining input for 
     * processing by an application, as opposed to X resource specification
     * values. For example, "-filename foo" will probably be used by an
     * application in some process, while "-background bar" is an X resource
     * to be used in some graphical display.
     * For further details see eXene/styles/styles-func.sml.
     *)
    datatype optName 
        = OPT_NAMED of string   (* custom options: retrieve by name *)
        | OPT_RESSPEC of string (* resource options: convert to a style *)
        
    type argName = string (* option spec string in argv *)
    datatype optKind
        = OPT_NOARG of string (* as XrmoptionNoArg. optname will assume this value if argName is specified in argv *)
        | OPT_ISARG     (* as XrmoptionIsArg:     value is option string itself *)
        | OPT_STICKYARG (* as XrmoptionStickyArg: value is chars immediately following option *)
        | OPT_SEPARG    (* as XrmoptionSepArg:    value is next argument in argv *)
        | OPT_RESARG    (* as XrmoptionResArg:    resource and value in next argument in argv *)
        | OPT_SKIPARG   (* as XrmSkipArg:         ignore this option and next argument in argv *)
        | OPT_SKIPLINE  (* as XrmSkipLine:        ignore this option and the rest of argv *)
    datatype optVal
        = OPT_ATTRVAL of (string * AV.attr_type)
        | OPT_STRING of string
    (* option specification table: name for searching, name in argv, kind of option, and type of option *)
    type optSpec = (optName * argName * optKind * AV.attr_type) list
    (* command line argument strings, with optSpec, will be converted into a optDb *)
    type optDb = (optName * optVal) list
    
    (* parseCommand: optSpec -> (string list) -> (optDb * string list) 
     * parseCommand proceeds through the string list of command line arguments,
     * adding any recognizable options from optSpec to the optDb. Any unrecognized
     * arguments (that is, arguments not recognized as unique prefixes of an option
     * in optSpec) are returned as a string list, along with the optDb produced.
     * Future improvement: figure out a way for these unrecognized arguments to be
     * somehow marked as to their position in the original argument list, in case
     * position is important.
     *)
     
    fun parseCommand (os: optSpec) [] = ([],[])
      | parseCommand (os: optSpec) (s::sl) =
            (let
            fun mkOptRec (optNam,optVal:string,attrType:AV.attr_type) =
                (case optNam of
                    OPT_NAMED(n) =>
                        (optNam,OPT_ATTRVAL(optVal,attrType))
                  | OPT_RESSPEC(n) =>
                        (optNam,OPT_STRING(optVal)))
            in
            (case ((List.filter 
                    (fn (_,an,_,_) => ((String.isPrefix s an) orelse (String.isPrefix an s))) 
                    os): (optName * argName * optKind * AV.attr_type) list) of
                ([]:optSpec) => 
                    (let
                     val (od,ua) = (parseCommand (os) sl)
                     in (od,s::ua) end)
              | ([(on,an,OPT_NOARG(av),at)]:optSpec) =>
                    (let
                     val (od,ua) = (parseCommand (os) sl)
                     in ((mkOptRec(on,av,at))::od,ua) end)
              | ([(on,an,OPT_ISARG,at)]:optSpec) =>
                    (let
                     val (od,ua) = (parseCommand (os) sl)
                     in ((mkOptRec(on,an,at))::od,ua) end)
              | ([(on,an,OPT_STICKYARG,at)]:optSpec) =>
                    (let
                     val la = String.size(s)
                     val lo = String.size(an)
                     val sv = (if la>lo then String.substring(s,(lo),(la-lo)) else "")
                     val (od,ua) = (parseCommand (os) sl)
                     in ((mkOptRec(on,sv,at))::od,ua) end)
              | ([(on,an,OPT_SEPARG,at)]:optSpec) =>
                    (case sl of
                        sv::svs =>
                            (let
                            val (od,ua) = (parseCommand (os) svs)
                            in ((mkOptRec(on,sv,at))::od,ua) end)
                      | [] =>
                            (let
                            val (od,ua) = (parseCommand (os) sl)
                            in (od,s::ua) end))
              | ([(on,an,OPT_RESARG,at)]:optSpec) =>
                    (case sl of
                        sv::svs =>
                            (let
                            val (bcol::(acol::_)) = (String.tokens (fn c => (c=(#":"))) sv)
                            val (od,ua) = (parseCommand (os) svs)
                            in ((mkOptRec(on,sv,at))::
                                (OPT_RESSPEC(bcol),OPT_STRING(acol))::od,ua) end)
                      | [] =>
                            (let
                            val (od,ua) = (parseCommand (os) sl)
                            in (od,s::ua) end))
              | ([(on,an,OPT_SKIPARG,at)]:optSpec) =>
                    (case sl of
                         sv::svs =>
                             (let
                             val (od,ua) = (parseCommand (os) svs)
                             in (od,ua) end)
                       | [] =>
                             (let
                             val (od,ua) = (parseCommand (os) sl)
                             in (od,s::ua) end))
              | ([(on,an,OPT_SKIPLINE,at)]:optSpec) => ([],[])
              (* ambiguous argument s *)
              | (_:optSpec) => (let
                      val (od,ua) = (parseCommand (os) sl)
                      in (od,s::ua) end)) 
           end)
    
    (* findNamedOpt: optDb -> optName -> AV.attr_value list 
     * find the attribute values of the "named" command line arguments.
     * this will return a list of _all_ arguments with the given name, with
     * the last argument value given on the command line as the head of the
     * list.
     * this allows an application to process named arguments in several ways -
     * it may wish that later arguments take precedence over earlier arguments,
     * in which case it may use only the head of the value list (if it exists).
     * otherwise, if the application wishes to obtain all of the argument values,
     * it may do this also (by working with the whole list).
     
     OPT_ATTRVAL(AV.cvtString ctxt (optVal,attrType))
     *)
    fun findNamedOpt od (OPT_NAMED(on)) ctxt =
        let
        fun filt (OPT_NAMED(n),v) = (n=on)
          | filt (_,_) = false
        in
            (List.rev 
                (List.map (fn (n,v) => 
                    (case v of OPT_ATTRVAL(v,t) => 
                        (AV.cvtString ctxt (v,t)) | _ => AV.AV_NoValue))
                (List.filter filt od)))
        end
      | findNamedOpt od (OPT_RESSPEC(on)) ctxt = []
        
    fun findNamedOptStrings od (OPT_NAMED(on)) =
        let
        fun filt (OPT_NAMED(n),v) = (n=on)
          | filt (_,_) = false
        in
            (List.rev 
                (List.map (fn (n,v) => 
                    (case v of OPT_ATTRVAL(v,t) => v | _ => ""))
                (List.filter filt od)))
        end
      | findNamedOptStrings od (OPT_RESSPEC(on)) = []
      
    (* styleFromOptDb: create a style from resource specifications in optDb.
     *)
    fun styleFromOptDb (ctxt,od) =
        let
        fun filt (OPT_RESSPEC(n),v) = true
          | filt (_,_) = false
        fun rovToStr(OPT_RESSPEC(n),OPT_STRING(v)) = (n^":"^v)
          | rovToStr(_,_) = ""
        val strLst = List.map (rovToStr) (List.filter filt od)
        in
            styleFromStrings(ctxt,strLst)
        end
    
    (* a utility function that returns a string outlining the valid command
     * line arguments in optSpec. *)
    fun helpStrFromOptSpec (os:optSpec) =
        let
        val argLst = (List.map (fn (_,ar,_,_) => ar:string) os)
        val hlpStr = ("["^(String.concatWith "|" argLst)^"]")
        in ("Valid options:\n"^hlpStr^"\n") end
        
(* end additions by ddeboer. *)

  end; (* Styles *)
