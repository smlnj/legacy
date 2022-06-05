(* wide-weave-bm.sml
 * this file created by bm2mlx
 * from:  mit/wide_weave
 * on: Wed Mar  6 15:24:25 EST 1991
 *)
structure WideWeaveBM =
  struct
    val wide_weave = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\136\136",
                "\029\029",
                "\062\062",
                "\092\092",
                "\136\136",
                "\197\197",
                "\227\227",
                "\209\209",
                "\136\136",
                "\029\029",
                "\062\062",
                "\092\092",
                "\136\136",
                "\197\197",
                "\227\227",
                "\209\209"
              ]]
          }
  end (* WideWeaveBM *)
