(* unix-path.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

structure UnixPath : UNIX_PATH =
  struct

    type path_list = Pathname.path_list

    structure S : sig
	datatype access = A_READ | A_WRITE | A_EXEC
        datatype file_type = F_REGULAR | F_DIR | F_SYMLINK | F_SOCK | F_CHR | F_BLK
      end = System.Unsafe.SysIO
    open S

    exception NoSuchFile = Pathname.NoSuchFile

    fun getWD () = let
	  val (inS, outS) = IO.execute ("/bin/pwd", [])
	  fun close () = (close_in inS; close_out outS)
	  in
	    let val path = (IO.input_line inS) in
	      close();
	      path
	    end
	      handle ex => (close(); raise ex)
	  end

    fun getPath () = let
	  val path = (case (UnixEnv.getEnv "PATH") of (SOME p) => p | _ => "")
	  in
	    Pathname.mkSearchPath path
	  end (* getPath *)

    fun mkAbsolutePath path = if (Pathname.isAbsolutePath path)
	    then path
	    else implode[getWD(), "/", path]

    fun mkRelativePath path = if (Pathname.isAbsolutePath path)
	    then let
	      fun strip ([], []) = "."
		| strip (l, []) = dotDot(l, [])
		| strip ([], l) = Pathname.pathImplode l
		| strip (l1 as (x1::r1), l2 as (x2::r2)) = if (x1 = x2)
		    then strip(r1, r2)
		    else dotDot (l1, l2)
	      and dotDot ([], l) = Pathname.pathImplode l
		| dotDot (_::r, l) = dotDot(r, ".."::l)
	      in
		strip (Pathname.pathExplode(getWD()), Pathname.pathExplode path)
	      end
	    else path

    local
    (* NOTE: the handle is to work around a bug in access() in pre-0.93
     * versions of the runtime system.
     *)
      fun access mode pathname =
	    (System.Unsafe.SysIO.access(pathname, mode))
	      handle _ => false
      fun accessAndType (mode, ftype) pathname = (
	    System.Unsafe.SysIO.access(pathname, mode)
	    andalso System.Unsafe.SysIO.ftype(System.Unsafe.SysIO.PATH pathname) = ftype)
	      handle _ => false
    in
    fun findFile (pl, mode) = Pathname.findFile (pl, access mode)
    fun findFiles (pl, mode) = Pathname.findFiles (pl, access mode)
    fun findFileOfType (pl, ftype, mode) =
	  Pathname.findFile (pl, accessAndType(mode, ftype))
    fun findFilesOfType (pl, ftype, mode) =
	  Pathname.findFiles (pl, accessAndType(mode, ftype))
    end (* local *)

  end (* UnixPath *)
