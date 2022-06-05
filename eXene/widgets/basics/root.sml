(* root.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 *
 * Definitions for widget root.
 *)

signature ROOT =
  sig

    structure EXB : EXENE_BASE

    type root
    type style

    val mkRoot : (string * EXB.authentication option) -> root
    val delRoot : root -> unit
    val sameRoot : root * root -> bool
    val displayOf : root -> EXB.display
    val screenOf : root -> EXB.screen
    val shades : root -> EXB.color -> WidgetBase.shades
    val tile : root -> string -> EXB.tile
    val colorOf : root -> EXB.color_spec -> EXB.color
    val openFont : root -> string -> EXB.font
    val stdCursor : root -> EXeneBase.std_cursor -> EXeneBase.cursor
    val ringBell : root -> int -> unit

    val sizeOfScr    : root -> Geometry.size
    val sizeMMOfScr  : root -> Geometry.size
    val depthOfScr   : root -> int

    val isMonochrome : root -> bool

    val styleOf : root -> style
    val styleFromStrings : root * string list -> style

    (* Additions by ddeboer (KSU CIS 705, Spring 2004) *)
    
    (* was/is included for testing purposes: disabled because can be unreliable.
    val stringsFromStyle : style -> string list
    *)
    
    (* mergeStyles(style1,style2): merge style1 with style2,
     * giving precedence first to tight bindings, then to resources
     * of style1.
     *)
    val mergeStyles : style * style -> style
    
    (* styleFromXRDB: return a style created from the properties
     * loaded by xrdb into the X-server
     *)
    val styleFromXRDB : root -> style
    
    (* command line option specification and parsing: 
     * see eXene/styles/styles-func.sml for details. *)
    type optName 
    type argName
    type optKind
    type optSpec
    type optDb
    type attr_value
    
    (* parseCommand: given a root and an option spec, create an option db
     * from command line arguments. *)
    val parseCommand : optSpec -> string list -> optDb * string list
    
    (* findNamedOpt: given an option db and a named option (an option to
     * be used for purposes other than resource specification), return a 
     * list of Attrs.attr_values. This list is ordered such that the last
     * value to appear on the command line appears first in this list, so
     * that the application may choose to use the first value only, or it
     * may choose to use all values given.
     * Named options should be typically useful in obtaining input for 
     * processing by an application, as opposed to X resource specification
     * values. For example, "-filename foo" will probably be used by an
     * application in some process, while "-background bar" is an X resource
     * to be used in some graphical display.
     * For further details see eXene/styles/styles-func.sml.
     *)
    val findNamedOpt : optDb -> optName -> root -> attr_value list
    
    val findNamedOptStrings : optDb -> optName -> string list
    
    (* styleFromOptDb: create a style from resource specifications in optDb.
     *)
    val styleFromOptDb : root * optDb -> style
    
    (* a utility function that returns a string outlining the valid command
     * line arguments in optSpec. *)
    val helpStrFromOptSpec : optSpec -> string
    (* end additions; but see below *)
  end (* ROOT *)

structure Root =
  struct

    structure EXB = EXeneBase

  (* Root object, corresponding to display/screen pair.
   *  server = ""          => "unix:0.0"
   *         = ":d"        => "unix:d.0"
   *         = "host:d"    => "host:d.0"
   *         = "host:d.s"  => "host:d.s"
   * where host is an internet address (e.g., "128.84.254.97") or "unix".
   *
   * At present, screen is always the default screen.
   *)
    type style = Styles.style

    datatype root = Root of {
      id : unit ref,
      scr : EXB.screen,
      mkshade : EXB.color -> ShadeServer.shades,
      mktile : string -> EXB.tile,
      style : style,
      idGen : unit -> int
    }

    val initImages = [
          (Quark.quark "lightGray", Images.lightGray),
          (Quark.quark "gray", Images.gray)
        ]

    fun mkRoot (server, auth) = let
          val scr = EXB.defaultScreenOf (EXB.openDisplay (server, auth))
          val idChan = CML.channel ()
          fun idLoop i = (CML.send(idChan,i);idLoop(i+1))
          val is = ImageServer.mkImageServer initImages
          val ts = TileServer.mkTileServer (scr,ImageServer.getImage is)
          val ss = ShadeServer.mkShadeServer scr
          val tilef = TileServer.getTile ts
          in
            CML.spawn (fn () => idLoop 0);
            Root {
                id = ref (), 
                scr = scr, 
                style = Styles.emptyStyle {scr=scr,tilef=tilef}, 
                mktile = tilef,
                mkshade = ShadeServer.getShades ss,
                idGen = fn () => CML.recv idChan}
          end

    fun screenOf (Root {scr,...}) = scr
    fun displayOf (Root {scr,...}) = EXB.displayOfScr scr
    fun delRoot root = EXB.closeDisplay (displayOf root)
    fun sameRoot (Root {id,...},Root{id=id',...}) = id = id'
    fun shades (Root{mkshade,...}) c = mkshade c
    fun tile (Root{mktile,...}) s = mktile s
    fun colorOf (Root{scr,...}) color_spec = EXB.colorOfScr scr color_spec
    fun openFont (Root{scr,...}) = Font.openFont (EXB.displayOfScr scr)
    fun stdCursor (Root{scr,...}) = EXB.stdCursor (EXB.displayOfScr scr)
    fun ringBell (Root{scr,...}) = EXB.ringBell (EXB.displayOfScr scr)
    fun sizeOfScr (Root{scr,...}) = EXB.sizeOfScr scr
    fun sizeMMOfScr (Root{scr,...}) = EXB.sizeMMOfScr scr
    fun depthOfScr (Root{scr,...}) = EXB.depthOfScr scr

    fun styleOf (Root {style,...}) = style

    fun isMonochrome (Root{scr,...}) = 
          EXB.displayClassOfScr scr = EXB.StaticGray andalso 
          EXB.depthOfScr scr = 1

    fun styleFromStrings (Root{scr,mktile,...}, sl) =
          Styles.styleFromStrings ({scr=scr,tilef=mktile},sl)

    (* Additions by ddeboer: (KSU CIS 705, Spring 2004) *)
    
    fun stringsFromStyle sty = Styles.stringsFromStyle sty
    fun mergeStyles (sty1,sty2) = Styles.mergeStyles (sty1,sty2)
    fun styleFromXRDB root =
        (let
        val dpy = displayOf(root)
        val scr = EXB.defaultScreenOf dpy
        val stl = ICCC.xrdbOfScr scr
        in (
            (*(TextIO.print ("XRDB strings:\n"^(String.concatWith "\n" stl)^"\n"));*)
            styleFromStrings(root,stl)) end)
    
    type optName = Styles.optName
    type argName = Styles.argName
    type optKind = Styles.optKind
    type optSpec = Styles.optSpec
    type optDb   = Styles.optDb
    type attr_value = Attrs.attr_value
    
    fun parseCommand (oSpec) sl =
            Styles.parseCommand (oSpec) sl
            
    fun findNamedOpt (oDb:Styles.optDb) (oNam:Styles.optName) (Root{scr,mktile,...}) =
            (Styles.findNamedOpt oDb oNam {scr=scr,tilef=mktile})
            
    fun styleFromOptDb (Root{scr,mktile,...},oDb) =
            (Styles.styleFromOptDb ({scr=scr,tilef=mktile},oDb))
            
    fun findNamedOptStrings (oDb:Styles.optDb) (oNam:Styles.optName) =
            (Styles.findNamedOptStrings oDb oNam)
            
    fun helpStrFromOptSpec(oSpec) = Styles.helpStrFromOptSpec(oSpec)
    (* end additions *)
    
  end (* Root *)
