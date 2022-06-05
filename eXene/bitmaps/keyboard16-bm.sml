(* keyboard16-bm.sml
 * this file created by bm2mlx
 * from:  mit/keyboard16
 * on: Wed Mar  6 15:23:20 EST 1991
 *)
structure Keyboard16BM =
  struct
    val keyboard16 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\000\000",
                "\000\000",
                "\015\240",
                "\016\008",
                "\016\008",
                "\016\008",
                "\016\008",
                "\015\240",
                "\000\000",
                "\000\000",
                "\015\240",
                "\021\088",
                "\042\172",
                "\063\252",
                "\000\000",
                "\000\000"
              ]]
          }
  end (* Keyboard16BM *)
