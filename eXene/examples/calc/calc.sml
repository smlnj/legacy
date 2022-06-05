(* calc.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * The calculator interface.
 *)

signature CALC =
  sig
    structure W : WIDGET
    val mkCalc : W.root * W.view * W.arg list -> W.widget
  end (* CALC *)

structure Calc : CALC =
  struct

    structure W = Widget
    structure A = Attrs

    fun mkDpyLine w =
	  Box.HzCenter [
	      Box.Glue{nat=5, min=0, max=NONE},
	      Box.WBox (Shape.mkRigid w),
	      Box.Glue{nat=5, min=0, max=NONE}
	    ]

    fun mkSwitchLine sw = Box.HzCenter [
	    Box.Glue{nat=5, min=0, max=NONE},
	    Box.WBox sw,
	    Box.Glue{nat=5, min=5, max=SOME 5}
	  ]

    val fname = "-Adobe-Helvetica-Bold-R-Normal--*-120-*"

    fun mkLine (root,view,args) itemlist = let
	  val hglue = Box.Glue {nat=5, min=5, max=SOME 5}
	  fun addBox ((name,act), l) = let
                val args = [(A.attr_label, A.AV_Str name),
                            (A.attr_font, A.AV_Str fname)]
		val fw = Button.textCmd (root,view,args) act
		in
		  hglue::(Box.WBox (Button.widgetOf fw))::l
		end
	  val boxlist = List.foldr addBox [hglue] itemlist
	  in
	    (Box.HzCenter boxlist)
	  end

    fun mkCalc (root,view,args) = let
          val dispArgs = [(A.attr_label, A.AV_Str "          0"),
                          (A.attr_relief, A.AV_Relief W.Sunken),
                          (A.attr_halign, A.AV_HAlign W.HRight)]
	  val display = Label.label (root,view,dispArgs)
	  val display_line = mkDpyLine (Label.widgetOf display)
	  fun quit _ = let
	        val pause = Time.fromMilliseconds 500
                fun cleanup () = (
		      CML.sync(CML.timeOutEvt pause); 
                      W.delRoot root; 
                      RunCML.shutdown OS.Process.success)
                in (CML.spawn cleanup; ()) end
	  val sw = Toggle.widgetOf (Toggle.toggleSwitch (root,view,args) quit)
	  val switch_line = mkSwitchLine sw
	  val acc = Acc.mkAcc ()
	  val send_acc = Acc.sendAcc acc
	  fun printer () = let
		val acc_evt = Acc.evtOf acc
		fun set l = Label.setLabel display (Label.Text l)
		fun loop () = loop (case (CML.sync acc_evt)
		       of Acc.OVal v => set (Int.toString v)
			| Acc.OInfinity => set "Infinity"
			| Acc.OOverflow => set "Overflow"
		      (* end case *))
		in
		  loop ()
		end (* printer *)
	  fun opfn msg () = send_acc msg
	  val line1 = mkLine (root,view,args) [
		  ("7", opfn (Acc.Val 7)), ("8", opfn (Acc.Val 8)),
		  ("9", opfn (Acc.Val 9)), ("+", opfn (Acc.Op Acc.Plus))]
	  val line2 = mkLine (root,view,args) [
		  ("4", opfn (Acc.Val 4)), ("5", opfn (Acc.Val 5)),
		  ("6", opfn (Acc.Val 6)), ("-", opfn (Acc.Op Acc.Minus))]
	  val line3 = mkLine (root,view,args) [
		  ("1", opfn (Acc.Val 1)), ("2", opfn (Acc.Val 2)),
		  ("3", opfn (Acc.Val 3)), ("*", opfn (Acc.Op Acc.Times))]
	  val line4 = mkLine (root,view,args) [
		  ("C", opfn (Acc.Clear)), ("0", opfn (Acc.Val 0)),
		  ("=", opfn (Acc.Equal)), ("/", opfn (Acc.Op Acc.Divide))]
	  val vglue = Box.Glue {nat=5, min=1, max=NONE}
	  in
	    CML.spawn printer;
	    Box.widgetOf (Box.layout (root,view,args) (Box.VtCenter [
		vglue,
		display_line,
		vglue,
		switch_line,
		vglue,
		line1,
		vglue,
		line2,
		vglue,
		line3,
		vglue,
		line4,
		vglue
	      ]))
	  end (* end mkCalc *)

  end (* Calc *)
