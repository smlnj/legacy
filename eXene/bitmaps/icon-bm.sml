(* icon-bm.sml
 * this file created by bm2mlx
 * from:  mit/icon
 * on: Wed Mar  6 15:23:17 EST 1991
 *)
structure IconBM =
  struct
    val icon = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\255\255",
                "\213\085",
                "\170\171",
                "\213\085",
                "\160\011",
                "\208\005",
                "\160\011",
                "\208\005",
                "\160\011",
                "\208\005",
                "\160\011",
                "\208\005",
                "\170\171",
                "\213\085",
                "\170\171",
                "\255\255"
              ]]
          }
  end (* IconBM *)
