(* center-ptr-bm.sml
 * this file created by bm2mlx
 * from:  mit/cntr_ptr mit/cntr_ptrmsk
 * on: Wed Mar  6 15:24:56 EST 1991
 *)
structure CenterPtrBM =
  struct
    val cntr_ptr = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\000\000",
                "\001\128",
                "\001\128",
                "\003\192",
                "\003\192",
                "\007\224",
                "\007\224",
                "\015\240",
                "\015\240",
                "\025\152",
                "\017\136",
                "\001\128",
                "\001\128",
                "\001\128",
                "\001\128",
                "\000\000"
              ]]
          }
    val cntr_ptrmsk = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\003\192",
                "\003\192",
                "\007\224",
                "\007\224",
                "\015\240",
                "\015\240",
                "\031\248",
                "\031\248",
                "\063\252",
                "\063\252",
                "\063\252",
                "\059\220",
                "\003\192",
                "\003\192",
                "\003\192",
                "\003\192"
              ]]
          }
  end (* CenterPtrBM *)
