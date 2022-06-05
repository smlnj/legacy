(* stipple-bm.sml
 * this file created by bm2mlx
 * from:  mit/stipple
 * on: Wed Mar  6 15:24:01 EST 1991
 *)
structure StippleBM =
  struct
    val stipple = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=4},
            data = [[
                "\170\170",
                "\119\119",
                "\170\170",
                "\093\221"
              ]]
          }
  end (* StippleBM *)
