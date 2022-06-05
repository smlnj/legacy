(* color-state.sml
 *
 * COPYRIGHT (c) 1991,1995 by AT&T Bell Laboratories.  
 * See COPYRIGHT file for details.
 *)

signature COLORSTATE =
  sig

    structure W : WIDGET

    datatype change_color_msg
      = ChangeR of word
      | ChangeG of word
      | ChangeB of word
      | ChangeRGB of W.EXB.rgb
    
    type color_state

    val mkColorState : W.EXB.rgb -> color_state
    val sendChangeColor : color_state -> change_color_msg -> unit
    val evtOfColorState : color_state -> W.EXB.rgb CML.event

  end;

structure ColorState : COLORSTATE = 
  struct

    structure W = Widget
 
    type rgb = W.EXB.rgb

    datatype change_color_msg
      = ChangeR of word 
      | ChangeG of word 
      | ChangeB of word
      | ChangeRGB of rgb

    datatype color_state = 
             ColorState of (change_color_msg CML.chan * rgb CML.chan)

    fun mkColorState initial_color = let 
	  val in_chan = CML.channel()
	  val out_chan = CML.channel()
	  fun get_msg () = CML.recv in_chan
	  fun send_color c = CML.send(out_chan,c)

          fun change_color (W.EXB.RGB{red=r,green=g,blue=b}, msg) = let 
                val new_color = (case msg
		       of (ChangeR n) => W.EXB.RGB{red=n,green=g,blue=b}
			| (ChangeG n) => W.EXB.RGB{red=r,green=n,blue=b}
			| (ChangeB n) => W.EXB.RGB{red=r,green=g,blue=n}
			| (ChangeRGB r) => r
		      (* end case *))
                in
		  send_color new_color; new_color
		end

	  fun loop color = loop(change_color(color, get_msg ()))

	  in
	    CML.spawn (fn () => loop initial_color);
	    ColorState(in_chan, out_chan)
	  end 

    fun sendChangeColor (ColorState(in_chan, _)) msg = CML.send (in_chan, msg)

    fun evtOfColorState (ColorState(_, out_chan)) = CML.recvEvt out_chan

end; (* ColorState *)
