(* text-pool-sig.sml
 *
 *)

signature TEXT_POOL =
  sig

    structure TextCanvas : TEXT_CANVAS

    type typeball
      sharing type TextCanvas.typeball = typeball

    type text_pool

    val resize : (text_pool * Geometry.size) -> unit
	(* notify the text-pool of a change in the associated canvas's size;
	 * this is called by TextDisplay.resize.
	 *)

    val numRows : text_pool -> int				(* ??? *)
	(* return number of rows in the text pool.  Note that this should
	 * cover the canvas (i.e., there shouldn't be pixels in the canvas
	 * that are not mapped to a row).
	 *)

    val maxCols : text_pool -> int
	(* return the maximum number of displayed columns in any row *)

    val getRow : text_pool -> int
	  -> {at : Geometry.point, elems : TextCanvas.text_elem list}
	(* return the text of the given row *)

    val getText : text_pool -> {row : int, start : int, stop : int}
	  -> {at : Geometry.point, elems : TextCanvas.text_elem list}
	(* return the text elements in the given row between the start and
	 * stop character positions (inclusive), along with the origin of
	 * the first element.
	 *)

    val getRowHt : (text_pool * int) -> int
	(* return the height of the given row *)

    val getRowScent : (text_pool * int) -> {ascent : int, descent : int}
	(* return the ascent and descent of the given row *)

    val baselineOfRow : (text_pool * int) -> int
	(* return the y-coordinate of a row's baseline; this is the same as
	 * the y-coordinate (rowToY) plus the ascent.
	 *)

    val pixelRngToRowRng : (text_pool * int * int) -> (int * int)
	(* given an inclusive range of pixels in the y-dimension, return the
	 * minimum inclusive range of rows covered by the pixel range.
	 *)

    val pixelRngToColRng : (text_pool * int * int * int) -> (int * int)
	(* given a row and an inclusive range of pixels in the x-dimension,
	 * return the minimum inclusive range of columns covered in the
	 * row by the pixel range.
	 *)

    val rowToY : (text_pool * int) -> int
	(* return the y-coordinate of the top of a row. *)

    val coordToX : (text_pool * TextCanvas.char_coord) -> int
	(* return the x-coordinate of a character coordinate *)

    val coordToPt : (text_pool * TextCanvas.char_coord) -> Geometry.point
	(* map a character coordinate to the origin of its bounding rectangle. *)

    val coordToRect : (text_pool * TextCanvas.char_coord) -> Geometry.rect
	(* map a character coordinate into a rectangle bounding its contents.
	 * The height of the rectangle is the row height (even if the character
	 * height is smaller).
	 *)

    val coordToElem : (text_pool * TextCanvas.char_coord) -> TextCanvas.text_elem
	(* map a character coordinate onto the corresponding single-character 
	 * typeballed type element
	 *)

    val xPosToCoord : (text_pool * int * int) -> TextCanvas.char_coord
	(* given a row and x-coordinate, return the full character coordinate *)

    val ptToCoord : (text_pool * Geometry.point) -> TextCanvas.char_coord
	(* map a point to a character coordinate *)

  end; (* TEXT_POOL *)

