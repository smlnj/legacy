structure ReadBitmap :
  sig
    exception BadImageData

    val readBitmapFile : TextIO.instream -> 
          (EXeneBase.image * int option * int option)
  end =
  struct
    exception BadImageData = EXeneBase.BadImageData

    open Geometry TextIO

    val << = Bits.lshift
    val >> = Bits.rshift
    val & = Bits.andb
    val ++ = Bits.orb
    infix << >> & ++

    val flip0_9 = #[0x0,0x8,0x4,0xc,0x2,0xa,6,0xe,0x1,0x9]
    val flipA_F = #[0x5,0xd,0x3,0xb,0x7,0xf]

    val sfmt1 = Format.scan "#define %s %d"
    val sfmt2 = Format.scan "static char %s = {"
    val sfmt3 = Format.scan "static unsigned char %s = {"

    fun isSuffix (sfx,s,j) = let
          fun loop (i,j) = 
                (ordof(sfx,i) = ordof(s,j)) andalso loop(i+1,j+1)
          val start = (size s) - (size sfx)
          in 
            if (size sfx) <> (size s - j) then false
            else (loop (0,j)) handle _ => true
          end

    fun done (wid,ht,hotx,hoty,[]) = raise BadImageData
      | done (wid,ht,hotx,hoty,data) =
            (EXeneBase.IMAGE{sz=SIZE{wid=wid,ht=ht},data = [data]},hotx,hoty)

    fun cvt x =
          if ((48 <= x) andalso (x <= 57)) (* '0'..'9' *)
            then Vector.sub(flip0_9,x - 48)
              else if ((65 <= x) andalso (x <= 70)) (* 'A'..'F' *)
            then Vector.sub(flipA_F,x - 65)
          else if ((97 <= x) andalso (x <= 102)) (* 'a'..'f' *)
            then Vector.sub(flipA_F,x - 97)
          else raise BadImageData

    fun doDefine (nt,value,vals) = let
          val type_idx = ((StringUtil.revindex "_" (nt,size nt))+1)
                           handle _ => 0
          in
            if isSuffix("width",nt,type_idx) 
              then (value,#2 vals, #3 vals, #4 vals, #5 vals)
            else if isSuffix("height",nt,type_idx) 
              then (#1 vals, value, #3 vals, #4 vals, #5 vals)
            else if isSuffix("hot",nt,type_idx) 
              then if type_idx < 2
                     then vals
                   else if isSuffix("x_hot",nt,type_idx-2)
                     then (#1 vals,#2 vals, SOME value, #4 vals, #5 vals)
                   else if isSuffix("y_hot",nt,type_idx-2)
                     then (#1 vals,#2 vals, #3 vals, SOME value, #5 vals)
                   else vals
            else vals
          end

    fun readBitmapFile ins = let
          open Format CType
          fun wrap f l = (f l) handle _ => []
          val scan1 = wrap sfmt1
          val scan2 = wrap sfmt2
          val scan3 = wrap sfmt3
          val ord_0 = ord "0"
          val ord_x = ord "x"

          fun getc () = (ord(input(ins,1))) handle _ => raise BadImageData
            
          fun getChar () = let
                fun get () = 
                  if getc () <> ord_x then raise BadImageData
                  else let
                    val i1 = cvt(getc())
                    val i2 = cvt(getc())
                    in
                      if isXDigitOrd (getc()) then raise BadImageData
                      else chr ((i2 << 4) ++ i1)
                    end
                fun skip c = if c <> ord_0 then skip(getc()) else get ()
                in skip(getc()) end
            
          fun doData (nt, vals as (wid,ht,_,_,_)) = let
                val type_idx = ((StringUtil.revindex "_" (nt,size nt))+1)
                                 handle _ => 0
                fun getLine (0,l) = implode(rev l)
                  | getLine (i,l) = getLine(i-1,(getChar())::l)
                in
                  if not(isSuffix("bits[]",nt,type_idx)) then vals
                  else let
                    val bytes_per_line = ((wid + 7) >> 3)
                    fun loop (0,l) = rev l
                      | loop (i,l) = (
                          loop(i-1,getLine(bytes_per_line,[])::l)
                        )
                    in
                      if wid <= 0 andalso ht <= 0 then raise BadImageData
                      else (#1 vals,#2 vals, #3 vals, #4 vals, loop(ht,[]))
                    end
                end

          fun tryScan (vals,line) =
                case scan1 line of 
                  [STR name_type, INT value] => doDefine(name_type,value,vals)
                | _ => case scan2 line of
                         [STR name_type] => doData (name_type,vals)
                       | _ => case scan3 line of
                                [STR name_type] => doData (name_type,vals)
                              | _ => vals

          fun read (arg as (wid,ht,hotx,hoty,data)) =
                case input_line ins
                 of "" => done arg
                  | line => read(tryScan (arg,line))
          in read (0,0, NONE, NONE,[]) end
  end
