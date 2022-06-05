(* smlnj-const.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure SMLNJConstant =
  struct
    type const = int
    fun toString(n) = ""
    fun valueOf(n) = MLRiscErrorMsg.impossible ("SMLNJConstant")
    fun hash(n) = 0w0
    fun == (x : const,y : const) = false
  end
