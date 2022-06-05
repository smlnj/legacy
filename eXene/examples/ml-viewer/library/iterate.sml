(* iterate.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 *)

structure Iterate : ITERATE =
  struct

    fun iterate f cnt init = let 
          open LibBase
          fun iter (0,v) = v
            | iter (n,v) = iter(n-1,f v)
          in
            if cnt < 0 
              then badArg {module="Iterate",func="iterate",msg="count < 0"}
              else iter (cnt,init)
          end
        
    fun repeat f cnt init = let 
          open LibBase
          fun iter (n,v) = if n = cnt then v else iter(n+1,f(n,v))
          in
            if cnt < 0 
              then badArg {module="Iterate",func="repeat",msg="count < 0"}
              else iter (0,init)
          end
        
  end (* Iterate *)
