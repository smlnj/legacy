structure WriteBitmap :
  sig
    exception NotBitmap

    val writeBitmapFile : (TextIO.outstream * string) -> 
          (EXeneBase.image * int option * int option) -> unit
  end =
  struct
    exception NotBitmap
    open Geometry TextIO

    val << = Bits.lshift
    val >> = Bits.rshift
    val & = Bits.andb
    val ++ = Bits.orb
    infix << >> & ++

    val flip = #["0","8","4","c","2","a","6","e",
                 "1","9","5","d","3","b","7","f"]

    val fmt1 = Format.format "#define %swidth %d\n#define %sheight %d\n"
    val fmt2 = Format.format "#define %s%s_hot %d\n"

    val bytesPerRow = 12

    fun writeBitmapFile (outs,name) 
            (EXeneBase.IMAGE{sz,data=[data]},hotx,hoty) = let
          open Format
          val name = case name of "" => "" | _ => name^"_"
          val pr = outputc outs
          val SIZE{wid,ht} = sz
          fun doChar (c,byte) = let
                val v = ord c
                in
                  if byte = 0 then pr "   0x"
                  else if byte mod bytesPerRow = 0 then pr ",\n   0x"
                  else pr ", 0x";
                  pr (Vector.sub(flip,v & 0xF));
                  pr (Vector.sub(flip,v >> 4));
                  byte+1
                end
          fun doRow (row,byte) = revfold doChar (explode row) byte
          in
            pr (fmt1 [STR name, INT wid, STR name, INT ht]);
            case hotx of
              SOME x => pr(fmt2 [STR name, STR "x", INT x])
            | NONE => ();
            case hoty of
              SOME y => pr(fmt2 [STR name, STR "y", INT y])
            | NONE => ();
            app pr ["static char ",name,"bits[] = {\n"];
            revfold doRow data 0;
            pr "};\n"
          end
      | writeBitmapFile _ (EXeneBase.IMAGE{data,...},_,_) = raise NotBitmap
  end
