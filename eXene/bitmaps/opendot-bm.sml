(* opendot-bm.sml
 * this file created by bm2mlx
 * from:  mit/opendot mit/opendotMask
 * on: Wed Mar  6 15:25:21 EST 1991
 *)
structure OpenDotBM =
  struct
    val opendot = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\000\000",
                "\000\000",
                "\000\000",
                "\000\000",
                "\000\000",
                "\003\128",
                "\006\192",
                "\004\064",
                "\006\192",
                "\003\128",
                "\000\000",
                "\000\000",
                "\000\000",
                "\000\000",
                "\000\000",
                "\000\000"
              ]]
          }
    val opendotMask = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\000\000",
                "\000\000",
                "\000\000",
                "\000\000",
                "\007\192",
                "\015\224",
                "\015\224",
                "\015\224",
                "\015\224",
                "\015\224",
                "\007\192",
                "\000\000",
                "\000\000",
                "\000\000",
                "\000\000",
                "\000\000"
              ]]
          }
  end (* OpenDotBM *)
