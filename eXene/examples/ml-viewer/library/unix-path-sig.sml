(* unix-path-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

signature UNIX_PATH =
  sig

    type path_list
      sharing type path_list = Pathname.path_list

    val getWD : unit -> string
	(* get the current working directory. *)

    val getPath : unit -> path_list
	(* get the user's PATH environment variable. *)

    val mkAbsolutePath : string -> string
    val mkRelativePath : string -> string

    datatype access = A_READ | A_WRITE | A_EXEC
      sharing type System.Unsafe.SysIO.access = access
    datatype file_type = F_REGULAR | F_DIR | F_SYMLINK | F_SOCK | F_CHR | F_BLK
      sharing type System.Unsafe.SysIO.file_type = file_type

    exception NoSuchFile
	(* this is Pathname.NoSuchFile *)

    val findFile : (path_list * access list) -> string -> string
    val findFiles : (path_list * access list) -> string -> string list

    val findFileOfType : (path_list * file_type * access list) -> string -> string
    val findFilesOfType : (path_list * file_type * access list) -> string -> string list

  end (* UNIX_PATH *)
