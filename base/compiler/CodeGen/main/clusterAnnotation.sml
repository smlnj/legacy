(* This file is a temporary hack that records if the virtual 
 * frame pointer is being used for the current cluster compilation.
 * 
 * useVfp is required for spilling, however, at the current time the
 * spill callbacks only take the block annotations and nothing else.
 * Therefore the spill routine checks this variable to decide which
 * base pointer to use.
 *
 * Eventually the spill call backs will take a datatype indicating the
 * source of the annotation, and the cluster annotation will also be
 * bundled as an input. But until then ...
 *)

structure ClusterAnnotation = struct
  val useVfp = ref false
end
