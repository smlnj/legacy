(* images.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure Images =
  struct
    val lightGray = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [map Byte.stringToBytes [
                "\136\136","\"\"","\^Q\^Q","DD",
                "\136\136","\"\"","\^Q\^Q","DD",
                "\136\136","\"\"","\^Q\^Q","DD",
                "\136\136","\"\"","\^Q\^Q","DD"
              ]]
          }

    val gray = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [map Byte.stringToBytes [
                "UU","\170\170","UU","\170\170",
                "UU","\170\170","UU","\170\170",
                "UU","\170\170","UU","\170\170",
                "UU","\170\170","UU","\170\170"
              ]]
          }

    val darkGray = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [map Byte.stringToBytes [
                "\221\221","ww","\221\221","ww",
                "\221\221","ww","\221\221","ww",
                "\221\221","ww","\221\221","ww",
                "\221\221","ww","\221\221","ww"
              ]]
          }

  end
