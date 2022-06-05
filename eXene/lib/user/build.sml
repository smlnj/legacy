(* build.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Bind the eXene library structures at top-level.
 *)

structure Geometry : GEOMETRY = EXene.Geometry
structure EXeneBase : EXENE_BASE = EXene.EXeneBase
structure Font : FONT = EXene.Font
structure Drawing : DRAWING = EXene.Drawing
structure ICCC : ICCC = EXene.ICCC
structure Interact : INTERACT = EXene.Interact
structure EXeneWin : EXENE_WIN = EXene.EXeneWin
structure StdCursor : STD_CURSOR = EXene.StdCursor

