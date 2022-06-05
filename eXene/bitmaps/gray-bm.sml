(* gray-bm.sml
 * this file created by bm2mlx
 * from:  mit/flipped_gray mit/gray1 mit/gray3 mit/light_gray
 * on: Wed Mar  6 15:23:02 EST 1991
 *)
structure GrayBM =
  struct
    val flipped_gray = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=4, ht=2},
            data = [[
                "\224",
                "\176"
              ]]
          }
    val gray1 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=2, ht=2},
            data = [[
                "\128",
                "\064"
              ]]
          }
    val gray3 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=4, ht=4},
            data = [[
                "\128",
                "\000",
                "\032",
                "\000"
              ]]
          }
    val light_gray = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=4, ht=2},
            data = [[
                "\016",
                "\064"
              ]]
          }
  end (* GrayBM *)
