(* ml-viewer.sml
 *
 * COPYRIGHT (c) 1993 AT&T Bell Laboratories
 *
 * A viewer widget for ML code.
 *)

signature ML_VIEWER =
  sig

    val openViewer : Widget.root -> {
	    file : string,
	    module : string,
	    loc : int,
	    range : {first : int, last : int} option
	  } -> unit

  end; (* ML_VIEWER *)

structure MLViewer : ML_VIEWER =
  struct

    structure Load = LoadFile(CIO)
    structure EXB = EXeneBase
    structure V = Viewer

    fun openViewer root {file, module, loc, range} = let
(*
val _ = CIO.print (Format.format "openViewer: file = %s, module = %s, loc = %d\n" [
Format.STR file, Format.STR module, Format.INT loc])
*)
	  val cv = CML.condVar()
	  val _ = CML.spawn (fn () => CML.writeVar (cv, Load.loadFile (file, range)))
	  val openFont = Font.openFont (Widget.displayOf root)
	  val font1 = openFont "-*-courier-medium-r-*-*-20-*-*-*-*-*-*-*"
	  val font2 = openFont "-*-courier-medium-o-*-*-20-*-*-*-*-*-*-*"
	  val font3 = openFont "-*-courier-bold-r-*-*-20-*-*-*-*-*-*-*"
	  val viewer = Viewer.mkViewer root {
		  src = CML.readVar cv,
		  font = font1,
		  backgrnd = EXeneBase.CMS_Name "wheat1",
		  comm_face = V.Face{font=SOME font2, color = SOME(EXB.CMS_Name "red")},
		  kw_face = V.Face{font=SOME font3, color = SOME(EXB.black)},
		  sym_face = V.Face{font=SOME font3, color = SOME(EXB.black)},
		  id_face = V.Face{font=NONE, color = SOME(EXB.CMS_Name "blue")}
		}
	  val initLoc = (case range
		 of NONE => loc-1
		  | SOME{first, last} => loc-first
		(* end case *))
	  val (widget, evt) =
		Widget.filterCmd (ScrollViewer.mkViewer root (viewer, initLoc))
	  val quitBtn = Button.mkTextBtn root {
		  rounded = true,
		  backgrnd = NONE, foregrnd = NONE,
		  label = "Close view"
		}
	  val widget' = Box.widgetOf (Box.mkLayout root (Box.VtCenter [
		  Box.HzCenter [
		      Box.Glue{nat=2, min=2, max=SOME 2},
		      Box.VtCenter [
			  Box.Glue{nat=2, min=2, max=SOME 2},
		          Box.WBox(Button.widgetOf quitBtn),
			  Box.Glue{nat=2, min=2, max=SOME 2}
			],
		      Box.Glue{nat=1000, min=1, max=NONE}
		    ],
		  Box.WBox(Divider.mkHorzDivider root {color=NONE, width=1}),
		  Box.WBox widget
		]))
	  val shell = Shell.mkShell (widget', NONE, {
		  win_name=SOME ("ML-viewer: " ^ file),
		  icon_name=SOME "ML-viewer"
		})
	  fun cmdMonitor () = let
		val (cmdEvt, cmdCh) = CML.sync evt
		val quitEvt = Button.evtOf quitBtn
		fun handleCmd msg = (
		      case (Interact.msgBodyOf msg)
		       of Interact.CI_OwnDeath => Shell.destroy shell
			| _ => ()
		      (* end case *);
		      CML.send (cmdCh, msg))
		fun handleBtn (Button.BtnUp _) = Shell.destroy shell
		  | handleBtn _ = ()
		fun loop () = (
		      CML.select [
			  CML.wrap (cmdEvt, handleCmd),
			  CML.wrap (quitEvt, handleBtn)
			];
		      loop ())
		in
		  loop ()
		end
          in
	    Shell.init shell;
	    CML.spawn cmdMonitor;
	    ()
          end

  end; (* MLViewer *)

