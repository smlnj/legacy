(* bitmap-io-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This module provides code to read and write depth-1 images
 * stored in X11 bitmap file format (see XReadBitmapFile(3X)).
 *)

signature BITMAP_IO =
  sig
    exception BitmapFileInvalid

    val readBitmap : TextIO.instream -> {
	    hot_spot : Geometry.point option,
	    image : EXeneBase.image
	  }
	(* read an X11 format bitmap image from the given instream. Raise
	 * BitmapFileInvalid, if the input file is badly formatted.
	 *)

    exception NotBitmap

    val writeBitmap : (TextIO.outstream * string * {
	    hot_spot : Geometry.point option, image : EXeneBase.image
	  }) -> unit
	(* write a bitmap with the given name to the given output stream.
	 * Raise the exception NotBitmap, if the image is not a depth-1 bitmap,
	 * and raise the exception EXeneBase.BadImageData, if the data does
	 * not match the given width and height.
	 *)

  end; (* BITMAP_IO *)
