(* xlogo11-bm.sml
 * this file created by bm2mlx
 * from:  mit/xlogo11
 * on: Wed Mar  6 15:24:40 EST 1991
 *)
structure XLogo11BM =
  struct
    val xlogo11 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=11, ht=11},
            data = [[
                "\240\032",
                "\240\064",
                "\120\128",
                "\060\128",
                "\029\000",
                "\026\000",
                "\023\000",
                "\039\128",
                "\035\192",
                "\067\192",
                "\129\224"
              ]]
          }
  end (* XLogo11BM *)
