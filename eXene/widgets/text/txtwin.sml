(* txtwin.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * One-line vtty.
 *)

signature TXTWIN = 
  sig

    structure W : WIDGET

    type txtwin
  
    val mkTxtWin : W.root -> (W.EXB.color option * W.EXB.color option) 
      -> txtwin

  end (* TXTWIN *)

structure TxtWin : TXTWIN = struct

  structure EXB = EXeneBase
  structure W = Widget
  structure CA = CharArray

  open CML Geometry EXeneBase EXeneWin Interact Drawing Widget

  type fn_table = {
    deletec:string -> unit,
    insert:string -> unit,
    reset:unit -> unit,
    set_cur_pos:int -> unit,
    set_cursor:bool -> unit,
    set_size:size -> int
  }

  type txtwin = (int -> size) * (point -> int) 
    * (window * size -> fn_table)

  datatype rqst =
    SetSize of size
  | Insert of string
  | SetCurPos of int
  | SetCursor of bool
  | Reset
  | Delete of string

  fun mkTxtWin root (foregrnd, backgrnd) = let
    val scr = screenOf root

    val dfltBackColor = whiteOfScr scr
    val dfltForeColor = blackOfScr scr

    val forec = 
      case foregrnd of
        NONE => dfltForeColor
      | SOME c => c

    val pen = newPen [PV_Foreground forec]
    val bltpen = newPen []  (* workaround for blt op bug. *)

    val fontName = "9x15"
    val font = W.openFont root fontName
    val {ascent=fonta,descent=fontd} = Font.fontHt font
    val fonth = fonta + fontd;
    val Font.CharInfo{left_bearing=lb,char_wid=fontw,...} = 
			Font.charInfoOf font (Char.ord #"A")
    val xdelta = 1
    val endpad = 2
    fun num_chars x = (x - endpad) div fontw

    fun realize (win,sz as SIZE{wid,ht}) = let
      val reqchan = channel () and repchan = channel () 
      val src = WSRC win
      val dst = drawableOfWin win
      fun text arg = drawString dst pen font arg

      fun draw_cursor pen p = let
        val x = p * fontw
      in
        drawSeg dst pen (LINE(PT{x=x,y=0},PT{x=x,y=fonth-1}))
      end

      fun copy (r,p) = pixelBltEvt dst bltpen {src=src,src_rect=r,dst_pos=p}

      val (clear, cursor_off) =
        case backgrnd of
          NONE => (
            clearArea dst,
            fn p => clearArea dst (RECT{x=p*fontw,y=0,wid=1,ht=fonth})
          )
        | SOME c => 
          let
            val clrpen = (newPen [PV_Foreground c])
          in
            (fillRect dst clrpen, draw_cursor clrpen)
          end
      
      val cursor_on = draw_cursor pen

      fun main (len, SIZE{wid, ht}) = let
        val buf = CA.array (len, #" ")

        fun clear_buf () = let
          fun cb 0 = CA.update (buf, 0, #" ")
            | cb i = (CA.update (buf, i, #" "); cb(i-1))
        in
          cb (len-1)
        end

        fun del_buf (p, c) = let
          fun shft i = 
            if i = len then ()
            else (CA.update(buf, i-1, CA.sub(buf, i)); shft (i+1))
        in
          shft p;
          if size c = 1
	    then CA.update(buf, len-1, String.sub(c, 0))
	    else CA.update(buf, len-1, #" ")
        end

        fun ins_buf (p, c) = let
          fun shft i = 
            if i = p then ()
            else (CA.update(buf, i, CA.sub(buf, i-1)); shft (i-1))
        in
          shft (len-1);
          CA.update(buf, p, c)
        end

        fun ins_str_buf (p, s) = let
          val slen = size s
          val endp = p + slen - 1
          fun shft i = 
            if i = endp then ()
            else (CA.update(buf, i, CA.sub(buf, i-slen)); shft (i-1))
          fun do_update (p, i) = 
            (CA.update(buf, p, (String.sub(s,i))); do_update(p+1,i+1))
        in
          shft (len-1);
          (do_update (p,0)) handle Ord => ()
        end

        fun redraw (p, on_off) = (
          text (PT{x=0,y=fonta}, CA.vector buf);
	        if on_off then cursor_on p else ()
        )

        fun loop (curp,on_off) = 
          case recv reqchan of
            SetSize (sz as SIZE{wid,ht}) =>
            let
	            val len' = num_chars wid
            in
	            send (repchan, len');
	            main (len', sz)
            end
          | Insert s => 
            if curp >= len then loop(curp,on_off)
            else (case size s of
	            0 => loop(curp,on_off)
            | 1 => 
	            let
                val x = curp*fontw
	              val bw = (len - curp - 1)*fontw + endpad
              in
                ins_buf (curp, String.sub(s, 0));
                let
                  val bevt = copy(RECT{x=x,y=0,wid=bw,ht=ht},PT{x=x+fontw,y=0})
                in
                  clear (RECT{x=x,y=0,wid=fontw,ht=ht});
                  text (PT{x=x,y=fonta},s);
                  case sync bevt of
                    [] => ()
                  | _ => redraw(curp+1,on_off)
                end;
                loop (curp+1, on_off)
              end
            | slen =>
	            let
                val cnt = Int.min (slen, len - curp)
                val x = curp*fontw
	              val space = cnt*fontw
	              val bw = (len - curp - cnt)*fontw + endpad
              in
                ins_str_buf (curp, substring(s,0,cnt));
                let
                  val bevt = copy(RECT{x=x,y=0,wid=bw,ht=ht},PT{x=x+space,y=0})
                in
                  clear (RECT{x=x,y=0,wid=space,ht=ht});
                  text (PT{x=x,y=fonta},substring(s,0,cnt));
                  case sync bevt of
                    [] => ()
                  | _ => redraw(curp+cnt,on_off)
                end;
                loop (curp+cnt, on_off)
              end
            )
          | SetCurPos curp' =>
            if curp' >= 0 andalso curp' <= len then
            (
	            if on_off then (
	              cursor_off curp;
	              cursor_on curp'
	            )
	            else ();
	            loop (curp', on_off)
            )
            else loop (curp,on_off)
          | SetCursor on_off' => 
            (
              if on_off' <> on_off then
	              if on_off' then cursor_on curp
	              else cursor_off curp
              else ();
	            loop (curp, on_off')
            )
          | Reset => (
	            clear (RECT{x=0,y=0,wid=wid,ht=ht});
              clear_buf ();
	            loop (0,false)
            )
          | Delete c =>
            if curp > 0 then
	            let
	              val x = curp*fontw
	              val endx = (len-1)*fontw
              in
                del_buf (curp, c);
                let
                  val bevt =
	                  copy (RECT{x=x,y=0,wid=wid-x,ht=ht},PT{x=x-fontw,y=0})
                in
	                if curp = len then
	                  clear (RECT{x=endx+1,y=0,wid=wid-endx,ht=ht})
	                else
	                  clear (RECT{x=endx,y=0,wid=wid-endx,ht=ht});
	                if size c = 1 then text (PT{x=endx,y=fonta},c) else ();
                  case sync bevt of
                    [] => ()
                  | _ => redraw(curp-1,on_off)
                end;
	              loop (curp-1, on_off)
	            end
            else loop (curp,on_off)
      in
        loop (0, false)
      end

    in
      XDebug.xspawn ("txtwin", fn () => (main(num_chars wid,sz);()));
      {
        set_size = fn sz => (send(reqchan,SetSize sz); recv repchan),
        set_cur_pos = fn v => send(reqchan, SetCurPos v),
        set_cursor = fn v => send(reqchan, SetCursor v),
        insert = fn c => send(reqchan, Insert c),
        reset = fn () => send(reqchan, Reset),
        deletec = fn c => send(reqchan, Delete c)
      }
    end

    fun pttochar (PT{x,y}) = (x+xdelta) div fontw
    fun sizer n = SIZE{ht=fonth, wid=n*fontw + endpad}
  in
    (sizer, pttochar, realize)
  end

end (* TxtWin *)

