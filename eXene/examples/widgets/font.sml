(* simple.sml
 *
 * COPYRIGHT (c) 1991,1995 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)
structure TestFont : 
  sig
    val doit' : string list * string -> unit
    val doit : unit -> unit
    val main : string list * 'a -> unit
  end = 
  struct
    structure W = Widget
    structure F = Format

    fun printInfo (msg, Font.CharInfo info) = (
          CIO.print msg;
          CIO.print(F.format ": lb %d rb %d wd %d\n"
            [F.INT (#left_bearing info), F.INT(#right_bearing info),
             F.INT (#char_wid info)])
        )

    fun fontInfo font = let
          val {min_bounds,max_bounds, min_char, max_char} = Font.fontInfoOf font
          val charInfo = Font.charInfoOf font
          fun loop c = 
                if c > max_char then ()
                else let
                  val info = charInfo c
                  in
                    printInfo(makestring c,info);
                    loop (c+1)
                  end handle Font.NoCharInfo => loop (c+1)
          in
            CIO.print(F.format "min_char = %d max_char = %d\n" [F.INT min_char,
              F.INT max_char]); 
            printInfo ("min_bounds",min_bounds);
            printInfo ("max_bounds",max_bounds);
            loop min_char
          end

    val fname = "-Adobe-Helvetica-Bold-R-Normal--*-120-*"

    fun goodbye root = let
          fun quit () = (W.delRoot root; RunCML.shutdown())
          val font = W.openFont root fname
          in
            fontInfo font;
            quit ()
          end
  
    fun doit' (debugFlags, server) = (
          XDebug.init debugFlags;
          RunEXene.runWArgs goodbye {dpy= SOME server,timeq=NONE}
        )
  
    fun doit () = RunEXene.run goodbye
  
    fun main (prog::server::_,_) = doit'([], server)
      | main _ = doit ()
  
  end (* Simple *)
