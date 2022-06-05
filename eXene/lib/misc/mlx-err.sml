(* mlx-err.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure MLXError =
  struct
    exception XERROR of string
    fun impossible s = raise (XERROR s)
    fun xerror s = raise (XERROR s)
    val noWarning = ref false
    fun warning s = if (!noWarning) then () else (TextIO.output(TextIO.stdErr, s))
  end
