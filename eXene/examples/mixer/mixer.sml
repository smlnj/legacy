(* mixer.sml
 *
 * COPYRIGHT (c) 1991,1995 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)
structure Mixer :
  sig
    val doit' : string list * string -> unit
    val doit : unit -> unit
    val main : string list * 'a -> unit
  end = 
  struct
    structure W = Widget
    structure A = Attrs
    structure CS = ColorState

    open Geometry

    val resources = ["*background: gray"]

    val maxcolor = 0w65535
    val medcolor = maxcolor div 0w2
    val mincolor = 0w0 

    val border_width = 4
    val slider_width = 20 
    val hue_box_dim = 25
    val big_spot_ht = 400
    val big_spot_wid = 150

    val hglue = Box.Glue {nat=5, min=5, max=SOME 5}
    val vglue = Box.Glue {nat=5, min=1, max=NONE}
	  
    val pause = Time.fromMilliseconds 500

    val redc = W.EXB.RGB{red=medcolor, green=0w0, blue=0w0}
    val greenc = W.EXB.RGB{red=0w0, green=medcolor, blue=0w0}
    val bluec = W.EXB.RGB{red=0w0, green=0w0, blue=medcolor}
    val blackc = W.EXB.RGB{red=0w0, green=0w0, blue=0w0}

    fun mk_red n = W.EXB.RGB{red = n, green=mincolor, blue=mincolor}
    fun mk_green n = W.EXB.RGB{red = mincolor, green=n, blue=mincolor}
    fun mk_blue n = W.EXB.RGB{red = mincolor, green=mincolor, blue=n}

    fun makeMixer (root, view) = let
          val white = W.EXB.whiteOfScr (Root.screenOf root)
          val colorOf = Root.colorOf root
          fun quit () = let
                fun q () = (CML.sync(CML.timeOutEvt pause); 
                            Root.delRoot root; 
                            RunCML.shutdown OS.Process.success)
                in CML.spawn q; () end
       
          val switch = Toggle.toggleSwitch (root,view,[]) (fn _ => quit ())
          val switch_line = Box.HzCenter [
                              vglue,
                              Box.WBox (Toggle.widgetOf switch),
                              hglue
                            ]
   
          fun mkDisplayBox c w = let
                val args = [(A.attr_background, A.AV_Color c),
                            (A.attr_borderWidth, A.AV_Int border_width)]
                val dpy = Frame.frame (root,view,args) (Shape.mkRigid w)
                in
                  Box.HzCenter [vglue,Box.WBox (Frame.widgetOf dpy),vglue]
                end
   
          fun paintSpot spot c = 
                (Spot.setSpot spot c) 
                handle _ => (TextIO.print "out of color cells\n"; quit())
   
          val spot = Spot.spot (root,view) 
                       {color = blackc, ht = big_spot_ht, wid =big_spot_wid}
          val paint = paintSpot spot 
          val color_screen = mkDisplayBox white (Spot.widgetOf spot)
   
          val cc = CS.mkColorState blackc
          val send_cc = CS.sendChangeColor cc 
          val cc_evt = CS.evtOfColorState cc
          fun painter () = painter (paint (CML.sync cc_evt)) 
   
          fun mkcolorcomplex (W.EXB.RGB c) mk_color mkmsg = let 
                val color = colorOf (W.EXB.CMS_RGB c)
                val l_args = [(A.attr_label, A.AV_Str "          0"),
                              (A.attr_background, A.AV_Color color)]
                val label = Label.label (root,view,l_args)
                val display = mkDisplayBox color (Label.widgetOf label)
                val s_args = [(A.attr_isVertical, A.AV_Bool true),
                              (A.attr_background, A.AV_Str "gray"),
                              (A.attr_width, A.AV_Int slider_width),
                              (A.attr_fromValue, A.AV_Int 0),
                              (A.attr_toValue, A.AV_Int(Word.toIntX maxcolor))]
                val slider = Slider.slider (root, view, s_args)
                val spot = Spot.spot (root,view) 
                             {color = blackc,ht = hue_box_dim,wid = hue_box_dim}
                val screen = mkDisplayBox white (Spot.widgetOf spot)
                val line = Box.HzCenter [
                               hglue, 
                               screen, 
                               hglue, 
                               Box.WBox (Slider.widgetOf slider), 
                               hglue,
                               display, 
                               hglue
                             ]
   
                val set = Label.setLabel label
                val evt = CML.wrap(Slider.evtOf slider, Word.fromInt)
                val paint = paintSpot spot 
                fun printer_loop () = let
                      val n = CML.sync evt
                      in 
                        set (Label.Text (Word.fmt StringCvt.DEC n));
                        paint (mk_color n);
                        send_cc (mkmsg n);
                        printer_loop ()
                      end 
                in (line, printer_loop) end 
   
          val (red_line, red_printer_loop) = 
                mkcolorcomplex redc mk_red CS.ChangeR
          val (green_line, green_printer_loop) = 
                mkcolorcomplex greenc mk_green CS.ChangeG
          val (blue_line, blue_printer_loop) = 
                mkcolorcomplex bluec mk_blue CS.ChangeB
   
          in
            CML.spawn red_printer_loop; 
            CML.spawn green_printer_loop;
            CML.spawn blue_printer_loop ;
            CML.spawn painter;
            Box.widgetOf (Box.mkLayout root (Box.VtCenter [
                vglue,
                color_screen, 
                vglue,        
                switch_line,
                vglue,
                red_line,
                vglue,
                green_line,
                vglue,
                blue_line,
                vglue
             ]))
          end (* end makeMixer *)


    fun init root = let
          val style = W.styleFromStrings (root, resources)
          val name = Styles.mkView {name = Styles.styleName [],
                                    aliases = [Styles.styleName []]}
          val view = (name,style)
          val mix = makeMixer (root,view)
          val args = [(Attrs.attr_title, Attrs.AV_Str "RGB Mixer"),
                      (Attrs.attr_iconName, Attrs.AV_Str "MIX")]
          val shell = Shell.shell (root,view,args) mix
          in Shell.init shell end

    fun doit' (debugFlags, server) = (
          XDebug.init debugFlags;
          RunEXene.runWArgs init {dpy= SOME server,timeq=NONE}
        )
  
    fun doit () = RunEXene.run init

    fun main (prog::server::_,_) = doit' ([], server)
      | main _ = doit ()

  end; (* Mixer *)
