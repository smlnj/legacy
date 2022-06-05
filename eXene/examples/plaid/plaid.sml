(* plaid.sml
 *
 * COPYRIGHT (c) 1991,1995 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)
structure Plaid :
  sig
    val doit' : string list * string -> unit
    val doit : unit -> unit
    val main : string list * 'a -> unit
  end = 
  struct
    structure W = Widget
    structure D = Drawing
    structure I = Interact

    open Geometry

    val emptyRect = RECT{x=0,y=0,wid=0,ht=0}
    fun middleOfRect (RECT{x,y,wid,ht}) = PT{x=x+(wid div 2),y=y+(ht div 2)}
    fun centerRect (RECT{wid,ht,...},PT{x,y}) = 
      RECT{wid=wid,ht=ht,x=x-(wid div 2),y=y-(ht div 2)}
    fun symmetricDifference (r,r') = let
      fun difference (r as RECT{x,y,wid,ht},r',acc) = let
        val RECT{x=ix,y=iy,wid=iwid,ht=iht} = intersection(r,r')
        val icx = ix+iwid
        val icy = iy+iht
        fun pare (x,y,cx,cy,acc) =
          if x < ix 
            then pare(ix,y,cx,cy,(RECT{x=x,y=y,ht=cy-y,wid=ix-x})::acc)
          else if y < iy 
            then pare(x,iy,cx,cy,(RECT{x=x,y=y,ht=iy-y,wid=cx-x})::acc)
          else if icx < cx 
            then pare(x,y,icx,cy,(RECT{x=icx,y=y,ht=cy-y,wid=cx-icx})::acc)
          else if icy < cy 
            then pare(x,y,cx,icy,(RECT{x=x,y=icy,ht=cy-icy,wid=cx-x})::acc)
          else acc
        in
          pare(x,y,x+wid,y+ht,acc)
        end handle Intersection => r::acc
      in
        difference(r',r,difference(r,r',[]))
      end

    fun mkPlaid root = let
      val scr = W.screenOf root

      val pen = 
        D.newPen [D.PV_Foreground W.EXB.color1, D.PV_Function D.OP_Xor]
      val idlePen = pen 
      val timer = CML.timeOutEvt(Time.fromMilliseconds 100)

      fun realizePlaid {win,sz,env} = let
        val drawwin = D.drawableOfWin win
        val autodrawwin = D.feedback drawwin
        val idleFill = D.fillRects drawwin idlePen
        val fill = D.fillRects autodrawwin idlePen
        val I.InEnv{m,ci,...} = I.ignoreKey env

        fun doActive (size as SIZE{wid,ht}) = let
          val middle as (PT{x=midx,y=midy}) = 
            middleOfRect (mkRect(originPt,size))

          fun adjust (arg as (PT{x,y},PT{x=dx,y=dy})) =
            if x < 0 then adjust(PT{x= ~x,y=y},PT{x= ~dx,y=dy})
            else if x >= wid then adjust(PT{x=2*wid - x - 2,y=y},PT{x= ~dx,y=dy})
            else if y < 0 then adjust(PT{x=x,y= ~y},PT{x=dx,y= ~dy})
            else if y >= ht then adjust(PT{x=x,y=2*ht - y - 2},PT{x=dx,y= ~dy})
            else arg
         
          fun update {p,deltaP,prevRect,oddCycle} = let
            val (p' as PT{x,y},deltaP') = adjust (addPt(p,deltaP),deltaP)
            val rect = RECT{ x = 0, y = 0,
                wid = 2*abs(x - midx), ht=2*abs(y - midy)}
            val rect = centerRect(rect, middle)
            in
              if oddCycle then fill(symmetricDifference(rect,prevRect)) else ();
              {p=p',deltaP=deltaP',prevRect=rect,oddCycle=not oddCycle}
            end

          fun handleCI (I.CI_Redraw _) = startOver ()
            | handleCI (I.CI_Resize (RECT{wid,ht,...})) = 
                doActive (SIZE{wid=wid,ht=ht})
            | handleCI _ = ()

          and loop me = 
            CML.select [
              CML.wrap(timer, fn () => loop(update me)),
              CML.wrap(m, fn evt => 
                case I.msgBodyOf(evt) of 
                  I.MOUSE_FirstDown _ => doIdle size
                | _ => loop me),
              CML.wrap(ci, handleCI o I.msgBodyOf)
            ]

          and startOver () = (
            D.clearDrawable drawwin;
            loop {p=middle,prevRect=emptyRect,oddCycle=false,deltaP=PT{x=1,y=1}}
          )
          in
            startOver ()
          end

        and doIdle (size as SIZE{wid,ht}) = let

          fun redraw () = let
            val bnd = Int.min(wid,ht) div 2
            fun loop i = 
              if i > bnd then ()
              else (
                idleFill [
                  RECT{x=i,y=i,wid=1,ht=ht-(2*i)},
                  RECT{x=wid - i - 1,y=i,wid=1,ht=ht-(2*i)},
                  RECT{x=i,y=i,wid=wid-(2*i),ht=1},
                  RECT{x=i,y=ht - i - 1,wid=wid-(2*i),ht=1}
                ];
                loop (i+2)
              )
            in
              D.clearDrawable drawwin;
              loop 1
            end

          fun handleCI (I.CI_Redraw _) = redraw ()
            | handleCI (I.CI_Resize (RECT{wid,ht,...})) = 
                doIdle (SIZE{wid=wid,ht=ht})
            | handleCI _ = ()

          fun loop () = 
            CML.select [
              CML.wrap(m, fn evt => 
                case I.msgBodyOf(evt) of 
                  I.MOUSE_FirstDown _ => doActive size
                | _ => loop ()),
              CML.wrap(ci, loop o handleCI o I.msgBodyOf)
            ]
          in
            loop ()
          end
        in
          CML.spawn (fn () => doIdle sz); ()
        end
      val size = W.fixBounds(300,200)
      in
        Shape.mkFlex (W.mkWidget{
            boundsOf = fn () => size,
            args = fn () => {background = NONE},
            root = root,
            realize = realizePlaid
          })
      end

    fun plaid root = let
          val style = W.styleFromStrings (root, [])
          val name = Styles.mkView {name = Styles.styleName [],
                                    aliases = [Styles.styleName []]}
          val view = (name,style)
          val plaid = mkPlaid root
          val args = [(Attrs.attr_title, Attrs.AV_Str "Plaid"),
                      (Attrs.attr_iconName, Attrs.AV_Str "Plaid")]
          val shell = Shell.shell (root,view,args) plaid
          in Shell.init shell end

    fun doit' (debugFlags, server) = (
          XDebug.init debugFlags;
          RunEXene.runWArgs plaid {dpy= SOME server,timeq=NONE}
        )
  
    fun doit () = RunEXene.run plaid

    fun main (prog::server::_,_) = doit' ([], server)
      | main _ = doit ()
  end

