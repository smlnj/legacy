(* black-bm.sml
 * this file created by bm2mlx
 * from:  mit/black
 * on: Wed Mar  6 15:22:12 EST 1991
 *)
structure BlackBM =
  struct
    val black = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\255\255",
                "\255\255",
                "\255\255",
                "\255\255",
                "\255\255",
                "\255\255",
                "\255\255",
                "\255\255",
                "\255\255",
                "\255\255",
                "\255\255",
                "\255\255",
                "\255\255",
                "\255\255",
                "\255\255",
                "\255\255"
              ]]
          }
  end (* BlackBM *)
