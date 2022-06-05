(* hlines-bm.sml
 * this file created by bm2mlx
 * from:  mit/hlines2 mit/hlines3
 * on: Wed Mar  6 15:23:11 EST 1991
 *)
structure HLinesBM =
  struct
    val hlines2 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=1, ht=2},
            data = [[
                "\128",
                "\000"
              ]]
          }
    val hlines3 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=1, ht=3},
            data = [[
                "\000",
                "\128",
                "\000"
              ]]
          }
  end (* HLinesBM *)
