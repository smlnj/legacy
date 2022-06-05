(* cpsRegions.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature CPS_REGION =
sig
  structure PT : POINTS_TO = PointsTo

  type region = PT.region

  val stack     : region
  val spill     : region
  val readonly  : region
  val memory    : region
  val storelist : region
  val real      : region

  val toString  : region -> string

  val reset     : unit -> unit
end
