(* scene.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

signature SCENE =
  sig
    structure W : WIDGET

    type scene

    val mkScene : W.root -> int -> scene
    val widgetOf : scene -> W.widget
    val start : scene -> unit
    val up : scene -> unit
    val dive : scene -> unit
    val wave : scene -> unit

  end

structure Scene : SCENE =
  struct
    structure W = Widget

    open CML Geometry Widget EXeneBase Drawing Interact

    datatype rqst = Start | Up | Wave | Dive

    datatype scene = SCENE of {
       widget : widget,
       reqchan : rqst chan
      }

    datatype position = Gone | Top | Step of int

    val topMargin = 48
    val bottomMargin = 48
    val personHt = 32
    val poleWid = 4
    val platformWidth = 12
    val platformDepth = 2
    val climbIncr = 9

    val landData = (16, [[
      "0x8888", "0x2222", "0x1111", "0x4444",
      "0x8888", "0x2222", "0x1111", "0x4444",
      "0x8888", "0x2222", "0x1111", "0x4444",
      "0x8888", "0x2222", "0x1111", "0x4444"
    ]])

    val waterData = (16, [[
      "0x5555", "0xaaaa", "0x5555", "0xaaaa",
      "0x5555", "0xaaaa", "0x5555", "0xaaaa",
      "0x5555", "0xaaaa", "0x5555", "0xaaaa",
      "0x5555", "0xaaaa", "0x5555", "0xaaaa"
    ]])

    fun mkScene root steps = let
      val scr = screenOf root
      val reqchan = channel ()
      val reqevt = receive reqchan
      val realizeVar = condVar ()
      val natHt = topMargin + bottomMargin + platformDepth + (steps * personHt)
      val bounds = {
          x_dim = DIM{base=0,incr=1,min=80,nat=180,max=NONE},
          y_dim = DIM{base=0,incr=1,min=natHt,nat=natHt,max=NONE}
        }
      val widget = mkWidget {
          root = root,
          boundsOf = fn () => bounds,
          realize = fn arg => writeVar(realizeVar, arg)
        }

      val imageArray = Array.arrayoflist(map (Images.mkImage scr) Images.images)
      val diveImage = Array.sub(imageArray,Images.diveIndex)
      val topImage = Array.sub(imageArray,Images.topIndex)
      val top2Image = Array.sub(imageArray,Images.topIndex+1)
      val top3Image = Array.sub(imageArray,Images.topIndex+2)
      val top4Image = Array.sub(imageArray,Images.topIndex+3)
      val waveList = 
            [topImage, top2Image, top3Image, top4Image, top3Image, top2Image]
      val climb1 = Array.sub(imageArray,Images.climbIndex)
      val climb2 = Array.sub(imageArray,Images.climbIndex+1)
      val climb3 = Array.sub(imageArray,Images.climbIndex+2)
      val climb4 = Array.sub(imageArray,Images.climbIndex+3)
      val SIZE{ht=climbHt,...} = sizeOfTile(#data climb1)
      val climbBound = topMargin + climbIncr + climbHt - 1
      val standImage = Array.sub(imageArray,Images.standIndex)

      val landTile = createTileFromAsciiData scr landData
      val waterTile = createTileFromAsciiData scr waterData

      val towerPen = 
        newPen [PV_Foreground (blackOfScr scr), PV_Background (whiteOfScr scr)]
      val imagePen = towerPen
      val waterPen = updatePen (towerPen, [PV_FillStyle_Stippled, PV_Stipple waterTile])
      val landPen = updatePen (towerPen, [PV_FillStyle_Stippled, PV_Stipple landTile])

      val splashList = Splash.mkSplashes (root, waterTile)

      val timer = timeout (Time.fromMilliseconds 40)
      fun pause () = sync timer

      fun realize {win,sz,env} pos = let
        val InEnv{ci,...} = ignoreInput env
        val drawwin = drawableOfWin win
        val auto_drawwin = feedback drawwin
        fun init (SIZE{wid,ht},pos) = let

          val midx = wid div 2

          val land = {
            shape = ConvexShape,
            verts = [
              PT{x=midx,y=ht-bottomMargin},
              PT{x=wid-1,y=ht-bottomMargin},
              PT{x=wid-1,y=ht-1},
              PT{x=midx-bottomMargin,y=ht-1}
            ]
          }
          val water = {
            shape = ConvexShape,
            verts = [
              PT{x=midx,y=ht-bottomMargin},
              PT{x=0,y=ht-bottomMargin},
              PT{x=0,y=ht-1},
              PT{x=midx-bottomMargin,y=ht-1}
            ]
          }
          val poleHt = ht-(bottomMargin+topMargin)
          val platform = 
            RECT{x=midx-((platformWidth - poleWid) div 2),
              y=topMargin,ht=platformDepth,wid=platformWidth}
          val pole = RECT{x=midx,y=topMargin,wid=poleWid, ht=poleHt}

          val top = PT{x=midx,y=topMargin-1}

          fun drawTower () = 
            (fillRect drawwin towerPen pole;
             fillRect drawwin towerPen platform)

          fun drawLandscape () =
            (fillPolygon drawwin waterPen water;
             fillPolygon drawwin landPen land)

          val stepx = midx+poleWid
          val stepy = ht-(bottomMargin+1)
          val step = poleHt div steps
          fun stepPt i = PT{x=stepx,y=stepy-(step*i)}
          val divePt = PT{y=stepy+1,x=midx-32}
          val splashPt = subPt(divePt,PT{x=0,y=1})

          fun putTop () = 
            Images.setImage (auto_drawwin,imagePen) (topImage,top)
          fun clearTop () =
            Images.clearImage auto_drawwin (topImage,top)
          fun putStep 0 =
              Images.setImage (auto_drawwin,imagePen) (standImage,stepPt 0)
            | putStep i =
              Images.setImage (auto_drawwin,imagePen) (climb1,stepPt i)
          fun clearStep 0 =
              Images.clearImage auto_drawwin (standImage,stepPt 0)
            | clearStep i =
              Images.clearImage auto_drawwin (climb1,stepPt i)

          fun doImage (image,pt) = let
            open Images
            in
              setImage (auto_drawwin,imagePen) (image,pt);
              pause();
              clearImage drawwin (image,pt)
            end
  
          fun wave () = let
            fun cycle _ = app (fn i => doImage(i,top)) waveList
            fun repeat 0 = ()
              | repeat i = (cycle();repeat(i-1))
            in
              repeat 4
            end

          fun splash () = app (fn i => doImage(i,splashPt)) splashList

          fun makeDive i = let
            open Images
            val PT{x=diveX,y=diveY} = divePt
            val initX = midx-2;
            val initY = (stepy-(step*i)) + 16
            val delX = initX - diveX
            val delY = initY - diveY
            val incr = 6
            fun xOfY y = (y*delX + initX*delY - initY*delX) div delY
            fun dive (pt as PT{x,y}) =
              if y >= diveY then ()
              else (
                setImage (auto_drawwin,imagePen) (diveImage,pt);
                pause ();
                clearImage auto_drawwin (diveImage,pt);
                dive(PT{x= xOfY(y+incr),y=y+incr})
              )
            in
              dive (PT{x=initX,y=initY})
            end

          fun climb i = let
            val pt0 = stepPt i
            val PT{y=y1,...} = stepPt (i+1)
            val ybound = max(y1,climbBound)
            fun loop (pt as PT{y,x}) =
              if y <= ybound then ()
              else let 
                val pt' = PT{x=x,y=y-climbIncr}
                in
                  doImage (climb1,pt);
                  doImage (climb2,pt);
                  doImage (climb3,pt');
                  doImage (climb4,pt');
                  loop pt'
                end
            in
              loop pt0
            end

          fun redraw(_,pos) = (
             clearDrawable drawwin;
             drawTower ();
             drawLandscape ();
             case pos of 
               Gone => ()
             | Top => putTop ()
             | Step i => putStep i
          )

          fun handleCI(CI_Redraw rlist,pos) = redraw(rlist,pos)
            | handleCI(CI_Resize (RECT{wid,ht,...}),pos) = 
                init(SIZE{wid=wid,ht=ht},pos)
            | handleCI _ = ()

          fun doGone () = let
            fun handleReq Start = climbing 0
              | handleReq _ = doGone ()
            in
              select[
                wrap(ci, fn evt => doGone(handleCI(msgBodyOf evt,Gone))),
                wrap(reqevt, handleReq)
              ]
            end
            
          and doTop () = let
            fun handleReq Start = (clearTop(); climbing 0)
              | handleReq _ = ()
            fun loop () =
              select[
                wrap(ci, fn evt => loop(handleCI(msgBodyOf evt,Top))),
                wrap(reqevt, fn req => loop(handleReq req))
              ]
            in
              wave ();
              putTop ();
              loop ()
            end
            
          and climbing i = let
            fun toTop () = (clearStep i; climb i; doTop())
            fun doClimb s = (clearStep i; climb i; climbing s)
            fun handleReq Start = 
                  if i = 0 then () else (clearStep i; climbing 0)
              | handleReq Wave = toTop ()
              | handleReq Dive = (clearStep i; doDive i)
              | handleReq Up = 
                  if i+1 < steps then doClimb(i+1) else toTop ()
            fun loop () = 
              select[
                wrap(ci, fn evt => loop(handleCI(msgBodyOf evt,Step i))),
                wrap(reqevt, loop o handleReq)
              ]
            in
              putStep i;
              loop ()
            end

          and doDive i = (makeDive i; splash (); doGone ())
            
          in
            case pos of
              Gone => doGone ()
            | Top => doTop ()
            | Step i => climbing i
          end
        in
          init (sz,pos)
        end

      fun scene pos = let
        fun handleReq (Start,_) = Step 0
          | handleReq (Dive,_) = Gone
          | handleReq (Wave,_) = Top
          | handleReq (Up,Step i) = if i+1 < steps then Step(i+1) else Top
          | handleReq (Up,p) = p
        in
          select[
            wrap(reqevt, fn msg => scene(handleReq(msg,pos))),
            wrap(readVarEvt realizeVar, fn arg => realize arg pos)
          ]
        end
      in
        spawn (fn () => scene (Step 0));
        SCENE{
          widget = widget,
          reqchan = reqchan
        }
      end

    fun widgetOf (SCENE{widget,...}) = widget
    fun start (SCENE{reqchan,...}) = send(reqchan,Start)
    fun up (SCENE{reqchan,...}) = send(reqchan,Up)
    fun dive (SCENE{reqchan,...}) = send(reqchan,Dive)
    fun wave (SCENE{reqchan,...}) = send(reqchan,Wave)

  end

