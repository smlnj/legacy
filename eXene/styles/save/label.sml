(* this is a test of the styles *)

structure L = Label
structure W = Widget
structure A = StdAttrs
structure AV = AttrValue

fun getString (attrDef, attr) = (case (attrDef attr)
       of (AV.AV_Str s) => SOME s
	| _ => NONE
      (* end case *))

fun getFont (attrDef, attr) = (case (attrDef attr)
       of (AV.AV_Font f) => SOME f
	| _ => NONE
      (* end case *))

fun getColor (attrDef, attr) = (case (attrDef attr)
       of (AV.AV_Color c) => SOME c
	| _ => NONE
      (* end case *))


val buttonAttrs = [
	(A.attr_label,		AV.AT_Str,	SOME "hello world"),
	(A.attr_font,		AV.AT_Str,	NONE),
	(A.attr_background,	AV.AT_Color,	SOME "bisque"),
	(A.attr_foreground,	AV.AT_Color,	SOME "blueviolet")
      ];

fun myButton root (name, sty) = let
      val attrDef = Styles.findAttrs sty (name, buttonAttrs)
      val (SOME label) = getString (attrDef, A.attr_label)
      in
	L.mkLabel root {
	    label	= label,
	    font	= getString (attrDef, A.attr_font),
	    foregrnd	= getColor (attrDef, A.attr_foreground),
	    backgrnd	= getColor (attrDef, A.attr_background),
	    align	= W.HCenter
	  }
      end;

fun doit resources = let
      fun example root = let
	    val sty = Styles.styleFromStrings (Widget.screenOf root, resources)
	    val butName = Styles.mkView {
		    name = Styles.styleName ["button"],
		    aliases = [Styles.styleName ["Button"]]
		  }
            val shell = Shell.mkShell (
		    L.widgetOf(myButton root (butName, sty)),
	            NONE,
		    {win_name = SOME "label", icon_name = SOME "label"})
            in
	      Shell.init shell; ()
            end
      in
	RunEXene.run example
      end;

val resources1 = [
	"button.background: lightgoldenrodyellow\n",
	"button.foreground: forestgreen\n"
      ];

val resources2 = [
	"Button.background: lightpink1\n",
	"button.foreground: mediumvioletred\n",
	"*label: help me!!!"
      ];

val resources3 = [
	"Button.background: lightpink1\n",
	"button.foreground: mediumvioletred\n",
	"*font: -*-rock-*-*-*-*-18-*-*-*-*-*-*-*\n"
      ];

