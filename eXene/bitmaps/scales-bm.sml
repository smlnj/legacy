(* scales-bm.sml
 * this file created by bm2mlx
 * from:  mit/scales
 * on: Wed Mar  6 15:23:54 EST 1991
 *)
structure ScalesBM =
  struct
    val scales = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\008\008",
                "\008\008",
                "\020\020",
                "\227\227",
                "\128\128",
                "\128\128",
                "\065\065",
                "\062\062",
                "\008\008",
                "\008\008",
                "\020\020",
                "\227\227",
                "\128\128",
                "\128\128",
                "\065\065",
                "\062\062"
              ]]
          }
  end (* ScalesBM *)
