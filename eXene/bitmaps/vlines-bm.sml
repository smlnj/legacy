(* vlines-bm.sml
 * this file created by bm2mlx
 * from:  mit/vlines2 mit/vlines3
 * on: Wed Mar  6 15:24:16 EST 1991
 *)
structure VLinesBM =
  struct
    val vlines2 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=2, ht=1},
            data = [[
                "\128"
              ]]
          }
    val vlines3 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=3, ht=1},
            data = [[
                "\064"
              ]]
          }
  end (* VLinesBM *)
