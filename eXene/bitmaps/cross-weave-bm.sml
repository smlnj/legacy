(* cross-weave-bm.sml
 * this file created by bm2mlx
 * from:  mit/cross_weave
 * on: Wed Mar  6 15:22:25 EST 1991
 *)
structure CrossWeaveBM =
  struct
    val cross_weave = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\170\170",
                "\017\017",
                "\170\170",
                "\068\068",
                "\170\170",
                "\017\017",
                "\170\170",
                "\068\068",
                "\170\170",
                "\017\017",
                "\170\170",
                "\068\068",
                "\170\170",
                "\017\017",
                "\170\170",
                "\068\068"
              ]]
          }
  end (* CrossWeaveBM *)
