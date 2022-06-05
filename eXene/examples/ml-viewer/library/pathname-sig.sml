(* pathname-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Routines for manipulating UNIX paths.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *
 *)

signature PATHNAME =
  sig

    val makePath : {dir : string, name : string, ext : string} -> string
	(* construct a pathname out of its constituent parts *)
    val splitPath : string -> {dir : string, name : string, ext : string}
	(* split a pathname into its constituent parts *)

    val dirOfPath : string -> string
	(* return the directory part of a pathname *)
    val nameOfPath : string -> string
	(* return the name part of a pathname *)
    val extOfPath : string -> string
	(* return the extension part of a pathname *)
    val baseOfPath : string * string -> string
	(* return the name part of the path and strip the specified extension. *)

    val defaultExt : {path : string, ext : string} -> string
	(* if path has a non-null name and does not have a extension, then add ".ext" *)

    val pathExplode : string -> string list
	(* return a list of path components (e.g., "abc/def" --> ["abc", "def"]).
	 * If the path is a full path, then the first element will be "".
	 *)

    val pathImplode : string list -> string
	(* concatenate a list of path components, inserting "/" between elements. *)

    val isAbsolutePath : string -> bool
	(* return true, if path begins with "/" *)

    val mkAbsolutePath : (string * string) -> string
	(* make the first argument be an absolute path as viewed from the
	 * second argument (which must be an absolute path).
	 *)
    val mkRelativePath : (string * string) -> string
	(* make the first argument be a relative path as viewed from the
	 * second argument (which must be an absolute path).
	 *)

  (* Search path lists *)

    type path_list

    val mkSearchPath : string -> path_list
	(* make a search path list from a string of the form "p1:p2:...:pn" *)
    val pathsOfList : path_list -> string list
	(* return the paths that make up a path_list. *)
    val appendPath : (path_list * string) -> path_list
	(* append a path onto a search path list *)
    val prependPath : (string * path_list) -> path_list
	(* prepend a path onto a search path list. *)

    exception NoSuchFile

    val findFile : (path_list * (string -> bool)) -> string -> string
	(* return the first path p in the pathlist, such that p^name satisfies
	 * the predicate.
	 *)
    val findFiles : (path_list * (string -> bool)) -> string -> string list
	(* return the list of paths p in the pathlist, such that p^name satisfies
	 * the predicate.
	*)

  end (* PATHNAME *)
