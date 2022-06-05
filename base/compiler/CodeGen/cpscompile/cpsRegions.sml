(* cpsRegions.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CPSRegions : CPS_REGION =
struct
  structure PT = PointsTo
  structure C  = CellsBasis

  type region = PT.region

  val memoryCell    = PT.TOP{id=C.mem 128, name="rw", mutable=true}
  val readonlyCell  = PT.TOP{id=C.mem 129, name="ro", mutable=false}
  val stackCell     = PT.TOP{id=C.mem 130, name="stack", mutable=true}
  val spillCell     = PT.TOP{id=C.mem 131, name="spill", mutable=true}
  val realCell      = PT.TOP{id=C.mem 132, name="real", mutable=false}
  val storelistCell = PT.TOP{id=C.mem 133, name="storelist", mutable=true}

  val memory     = ref memoryCell
  val readonly   = ref readonlyCell
  val stack      = ref stackCell
  val spill      = ref spillCell
  val real       = ref realCell
  val storelist  = ref storelistCell

  fun reset() =
      (memory    := memoryCell;
       readonly  := readonlyCell;
       stack     := stackCell;
       spill     := spillCell;
       real      := realCell;
       storelist := storelistCell
      )

  val toString   = PT.toString

end
