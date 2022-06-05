(* pathname.sml
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

structure Pathname : PATHNAME =
  struct

    fun badArg (func, msg) = LibBase.badArg{module="Pathname", func=func, msg=msg}

    fun suffix (s, i) = substring(s, i, size s - i)

    fun isDot (s, i) = (ordof(s, i) = (* "." *)46)
    fun isSlash (s, i) = (ordof(s, i) = (* "/" *)47)

  (* return the index of the start of the extension, or NONE *)
    fun findExt p = let
	  fun find i = if isDot(p, i)
		  then if isDot(p, i-1)
		    then skipDots (i-2)
		  else if isSlash(p, i-1)
		    then NONE
		    else SOME(i+1)
		  else find(i-1)
	  and skipDots i = if isDot(p, i) then skipDots(i-1) else find i
	  in
	    (skipDots (size p - 1)) handle _ => NONE
	  end (* findExt *)

  (* return the index of the start of the name, or NONE.  extStart is the
   * start of the extension part (if known).
   *)
    fun findName (p, extStart) = let
	  fun find i = if isSlash(p, i) then SOME(i+1) else find(i-1)
	  in
	    (case extStart
	     of SOME i => find(i-1)
	      | NONE => let val n = size p - 1
		  in
		    if isSlash(p, n)
		      then NONE
		      else find(n-1)
		  end
	    (* end case *))
	      handle _ => SOME 0
	  end

  (* construct a pathname out of its constituent parts *)
    fun makePath {dir, name="", ext=""} = dir
      | makePath {dir, name="", ...} = badArg("makePath", "bad extension")
      | makePath {dir, name, ext=""} = (dir ^ name)
      | makePath {dir, name, ext} = implode[dir, name, ".", ext]

  (* split a pathname into its constituent parts *)
    fun splitPath "" = {dir="", name="", ext=""}
      | splitPath p = let
	  val extIndex = findExt p
	  in
	    case (findName (p, extIndex), extIndex)
	     of (NONE, NONE) => {dir = p, name = "", ext = ""}
	      | (SOME 0, NONE) => {dir = "", name = p, ext = ""}
	      | (SOME i, NONE) =>
		  {dir = substring(p, 0, i), name = suffix(p, i), ext = ""}
	      | (SOME i, SOME j) => {
		    dir = substring(p, 0, i),
		    name = substring(p, i, j-(i+1)),
		    ext = suffix(p, j)
		  }
	      | _ => raise LibBase.Impossible "[Pathname.splitPath]"
	    (* end case *)
	  end

  (* return the directory part of a pathname *)
    fun dirOfPath p = (case findName(p, NONE)
	   of NONE => p
	    | (SOME i) => substring(p, 0, i)
	  (* end case *))

  (* return the name part of a pathname *)
    fun nameOfPath p = let
	  val extIndex = findExt p
	  in
	    case (findName (p, extIndex), extIndex)
	     of (NONE, NONE) => ""
	      | (SOME 0, NONE) => p
	      | (SOME i, NONE) => suffix(p, i)
	      | (SOME i, SOME j) => substring(p, i, j-(i+1))
	      | _ => raise LibBase.Impossible "[Pathname.nameOfPath]"
	    (* end case *)
	  end

  (* return the extension part of a pathname *)
    fun extOfPath p = (case (findExt p)
	   of NONE => ""
	    | (SOME i) => suffix(p, i)
	  (* end case *))

  (* return the name part of the path and strip the specified extension. *)
    fun baseOfPath (path, ext) = (case (findName (path, NONE), ext)
	   of (NONE, _) => ""
	    | (SOME 0, "") => path
	    | (SOME i, "") => suffix(path, i)
	    | (SOME i, ext) => let
		val pathSz = size path
		val nameSz = pathSz - i
		val extSz = size ext
		fun match (i, j) = ((j >= extSz)
		      orelse (ordof(path, i) = ordof(ext, j))
			andalso match(i+1, j+1))
		in
		  if (extSz > nameSz)
		    then suffix (path, i)
		  else if (match (pathSz - extSz, 0))
		    then substring (path, i, nameSz-extSz)
		    else suffix (path, i)
		end
	  (* end case *))

  (* if path has a non-null name and does not have an extension, then add ".ext" *)
    fun defaultExt {path, ext} = let
	  val extIndex = findExt path
	  in
	    case (findName (path, extIndex), extIndex)
	     of (NONE, NONE) => path
	      | (SOME _, NONE) => implode[path, ".", ext]
	      | (SOME _, SOME _) => path
	      | _ => raise LibBase.Impossible "[Pathname.nameOfPath]"
	    (* end case *)
	  end

  (* return a list of path components (e.g., "abc/def" --> ["abc", "def"]).
   * If the path is a full path, then the first element will be "".
   *)
    fun pathExplode s = let
	  val n = (size s)-1
	  fun exp (~1, j, p) = if (j = n) then [s] else substring(s, 0, j+1)::p
	    | exp (i, j, p) = if isSlash(s, i)
		then exp (i-1, i-1, substring(s, i+1, j-i)::p)
		else exp (i-1, j, p)
	  in
	    exp (n, n, [])
	  end

  (* concatenate a list of path components, inserting "/" between elements. *)
    fun pathImplode l = let
	  fun mkPath ([], _) = ""
	    | mkPath ([c], p) = implode(rev(c::p))
	    | mkPath (c::r, p) = mkPath(r, "/"::c::p)
	  in
	    mkPath (l, [])
	  end

  (* return true, if path begins with "/" *)
    fun isAbsolutePath s = (isSlash (s, 0)) handle _ => false

  (* make the first argument be an absolute path as viewed from the
   * second argument (which must be an absolute path).
   *)
    fun mkAbsolutePath (p1, p2) = if (isAbsolutePath p2)
	  then if (isAbsolutePath p1)
	    then p1
	    else implode[p2, "/", p1]
	  else badArg("mkAbsolutePath", "expected absolute path")

  (* make the first argument be a relative path as viewed from the
   * second argument (which must be an absolute path).
   *)
    fun mkRelativePath (p1, p2) = if (isAbsolutePath p2)
	  then if (isAbsolutePath p1)
	    then let
	      fun strip ([], []) = "."
		| strip (l, []) = dotDot(l, [])
		| strip ([], l) = pathImplode l
		| strip (l1 as (x1::r1), l2 as (x2::r2)) = if (x1 = x2)
		    then strip(r1, r2)
		    else dotDot (l1, l2)
	      and dotDot ([], l) = pathImplode l
		| dotDot (_::r, l) = dotDot(r, ".."::l)
	      in
		strip (pathExplode p2, pathExplode p1)
	      end
	    else p1
	  else badArg("mkRelativePath", "expected absolute path")

  (** Path lists **)

    datatype path_list = PathList of string list

  (* make sure that a search path element is terminated with a "/" *)
    fun termPath p = if (ordof(p, String.size p - 1) <> (* "/" *) 47)
	  then p ^ "/"
	  else p

  (* make a search path list from a string of the form "p1:p2:...:pn" *)
    fun mkSearchPath s = let
	  val len = String.size s
	  fun addPath (i, j, l) = if (i >= j)
		then l
		else (termPath(substring(s, i, j-i)) :: l)
	  fun split (~1, j, l) = addPath(0, j, l)
	    | split (i, j, l) = if (ordof(s, i) = (* ":" *)58)
		then split (i-1, i, addPath(i+1, j, l))
		else split (i-1, j, l)
	  in
	    PathList(split (len-1, len, []))
	  end

  (* return the paths that make up a path_list. *)
    fun pathsOfList (PathList l) = l

  (* append a path onto a search path list *)
    fun appendPath (pl, "") = pl
      | appendPath (PathList l, p) = PathList(l @ [termPath p])

  (* prepend a path onto a search path list. *)
    fun prependPath ("", pl) = pl
      | prependPath (p, PathList l) = PathList((termPath p) :: l)

    exception NoSuchFile

  (* return the first path p in the pathlist, such that p^name satisfies
   * the predicate.
   *)
    fun findFile (PathList l, pred) fname = let
	  fun find [] = raise NoSuchFile
	    | find (p::r) = let val pn = p^fname
		in
		  if (pred pn) then pn else find r
		end
	  in
	    if (isAbsolutePath fname)
	      then if (pred fname) then fname else raise NoSuchFile
	      else find l
	  end

  (* return the list of paths p in the pathlist, such that p^name satisfies
   * the predicate.
   *)
    fun findFiles (PathList l, pred) fname = let
	  fun find ([], l) = rev l
	    | find (p::r, l) = let val pn = p^fname
		in
		  if (pred pn) then find (r, pn::l) else find (r, l)
		end
	  in
	    if (isAbsolutePath fname)
              then if (pred fname) then [fname] else []
              else find (l, [])
	  end

  end; (* Pathname *)
