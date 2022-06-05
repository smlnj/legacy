(* dimple-bm.sml
 * this file created by bm2mlx
 * from:  mit/dimple1 mit/dimple3
 * on: Wed Mar  6 15:22:28 EST 1991
 *)
structure DimpleBM =
  struct
    val dimple1 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\170\170",
                "\000\000",
                "\170\170",
                "\000\000",
                "\170\170",
                "\000\000",
                "\170\170",
                "\000\000",
                "\170\170",
                "\000\000",
                "\170\170",
                "\000\000",
                "\170\170",
                "\000\000",
                "\170\170",
                "\000\000"
              ]]
          }
    val dimple3 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\136\136",
                "\000\000",
                "\000\000",
                "\000\000",
                "\136\136",
                "\000\000",
                "\000\000",
                "\000\000",
                "\136\136",
                "\000\000",
                "\000\000",
                "\000\000",
                "\136\136",
                "\000\000",
                "\000\000",
                "\000\000"
              ]]
          }
  end (* DimpleBM *)
