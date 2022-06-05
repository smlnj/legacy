(* xlogo16-bm.sml
 * this file created by bm2mlx
 * from:  mit/xlogo16
 * on: Wed Mar  6 15:24:44 EST 1991
 *)
structure XLogo16BM =
  struct
    val xlogo16 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\240\001",
                "\120\001",
                "\060\002",
                "\030\004",
                "\030\008",
                "\015\016",
                "\007\144",
                "\003\160",
                "\003\064",
                "\002\224",
                "\004\240",
                "\004\120",
                "\008\120",
                "\016\060",
                "\032\030",
                "\064\015"
              ]]
          }
  end (* XLogo16BM *)
