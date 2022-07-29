(* basis-unix.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This file rebinds Unix-specific basis module names to their 2004 versions.
 *)

signature POSIX = POSIX_2004
signature POSIX_TTY = POSIX_TTY_2004
structure Posix : POSIX = Posix
