(* abstract.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * make the eXene interface abstract.
 *)

structure EXene :> sig

    structure Geometry : GEOMETRY
    structure EXeneBase : EXENE_BASE
    structure Font : FONT
    structure Drawing : DRAWING
    structure ICCC : ICCC
    structure Interact : INTERACT
    structure EXeneWin : EXENE_WIN
    structure StdCursor : STD_CURSOR

    sharing Geometry = EXeneBase.G = Drawing.G = ICCC.G = Interact.G = EXeneWin.G
    sharing EXeneBase = Font.EXB = Drawing.EXB = ICCC.EXB = Interact.EXB
		= EXeneWin.EXB = StdCursor.EXB
    sharing ICCC = EXeneWin.ICCC
    sharing Interact = EXeneWin.Interact

    sharing type Font.font = EXeneBase.font
    sharing type EXeneWin.window = Drawing.window = EXeneBase.window
    sharing type Drawing.pixmap = EXeneBase.pixmap
    sharing type Drawing.tile = EXeneBase.tile
    sharing type Drawing.font = EXeneBase.font
    sharing type Drawing.color = EXeneBase.color
    sharing type ICCC.atom = EXeneBase.atom

  end = struct

    structure Geometry : GEOMETRY = Geometry

    structure EXeneBase : EXENE_BASE =
      struct
	structure G = Geometry

	structure XTime = XTime

	open EXeneVersion

	exception BadAddr = XDisplay.BadAddr

	open XProtTypes DrawTypes FontBase Display Cursor ColorServer Pixmap Image Tile
	open HashWindow

        local fun copyRGB (RGB rgb) = CMS_RGB rgb in
	val white = copyRGB whiteRGB
	val black = copyRGB blackRGB
	end

      end

    structure Font : FONT =
      struct
	structure EXB = EXeneBase
	open XProtTypes FontBase FontServer Display
      end

    structure Drawing : DRAWING =
      struct
	structure G = Geometry
	structure EXB = EXeneBase
	open XProtTypes FontBase ColorServer DrawTypes PenRep Pen Draw
      end

    structure ICCC : ICCC =
      struct
	structure G = Geometry
	structure EXB = EXeneBase
	open Property StdAtoms XProps

	val internAtom = XAtoms.internAtom
	val lookupAtom = XAtoms.lookupAtom
	val nameOfAtom = XAtoms.nameOfAtom

	type selection_handle = Selection.selection_handle
	val acquireSelection = Selection.acquireSelection
	val selectionOf = Selection.selectionOf
	val timeOf = Selection.timeOf
	val selectionReqEvt = Selection.selectionReqEvt
	val selectionRelEvt = Selection.selectionRelEvt
	val releaseSelection = Selection.releaseSelection
	val requestSelection = Selection.requestSelection
      end

    structure Interact : INTERACT =
      struct
	structure G = Geometry
	structure EXB = EXeneBase
	open KeyBut XProtTypes WindowEnv KeysymTranslation
      end

    structure EXeneWin : EXENE_WIN =
      struct
	structure G = Geometry
	structure EXB = EXeneBase
	structure ICCC = ICCC
	structure Interact = Interact
	open XProtTypes XProps Window HashWindow
      end

    structure StdCursor : STD_CURSOR =
      struct
	structure EXB = EXeneBase
	open Cursor
      end

  end; (* abstraction EXene *)
