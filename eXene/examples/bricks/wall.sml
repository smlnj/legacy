(* wall.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

signature WALL = 
  sig
    structure U : BRICK_UTIL
    structure W : WIDGET

    type Wall

    val mkWall : W.root -> (int * int) -> Wall
    val widgetOf : Wall -> W.widget
    val startGame : (Wall * U.Difficulty) -> unit
    val difficultyOf : Wall -> U.Difficulty
    val setRange : (Wall * U.Range) -> unit
  end

structure Wall : WALL =
  struct
    structure U = BrickUtil
    structure W = Widget

    open Geometry Widget U Brick

    val SafeZone = 3

    fun realToRGB(r,g,b) = let
      fun scale v = Word.fromInt(Real.trunc(65535.0 * v))
    in
      W.EXB.CMS_RGB{red=scale r, green = scale g, blue = scale b}
    end

    val brickRed = realToRGB(0.970077, 0.291340, 0.066498)
    val yellow = realToRGB(1.0, 1.0, 0.0)
    val lightGrey = realToRGB(0.8, 0.8, 0.8)
    val darkGrey = realToRGB(0.2, 0.2, 0.2)
    val mediumGrey = realToRGB(0.7, 0.7, 0.7)
    val cyan = realToRGB(0.0, 1.0, 1.0)

    local open Interact in
      fun leftMouse (MButton b) = b = 1
      fun rightMouse (MButton b) = b >= 3
      fun middleMouse (MButton b) = b = 2
    end

    datatype rqst = 
        Start of Difficulty
      | SetRange of Range
      | GetDifficulty of Difficulty CML.chan

    datatype Wall = WALL of {
        reqChan : rqst CML.chan,
        widget : widget
      }


    fun mkWallWidget (root,color,msgwin,bricks) = let
      open Box
      val endGlue = Glue{nat=0,min=0,max=NONE}
      val halfBrick = BrickSizeH div 2
      val startGlue = Glue{nat=halfBrick,min=halfBrick,max=SOME halfBrick}

      fun boxCol [] = [endGlue]
        | boxCol (b::rest) = (WBox(Brick.widgetOf b))::(boxCol rest)

      fun boxRow (_, []) = []
        | boxRow (y, r::rest) =
            if y mod 2 = 0 
              then (HzCenter(boxCol r))::(boxRow(y+1,rest))
              else (HzCenter(startGlue::(boxCol r)))::(boxRow(y+1,rest))
      val wallView = mkLayout root (VtCenter (
          (WBox msgwin)::
          ((boxRow (0, bricks))@[endGlue])))
      in
        Background.mkBackground{widget=widgetOf wallView, color = SOME color}
      end

    fun mkWall root (xSize,ySize) = let
      val scr = screenOf root
      val reqChan = CML.channel ()
      val brickChan = CML.channel ()
      val cvtColor = W.EXB.colorOfScr scr 
      val palette = {
          brick = cvtColor brickRed,
          mark = cvtColor yellow,
          concrete = cvtColor lightGrey,
          darkLines = cvtColor darkGrey,
          lightLines = cvtColor mediumGrey,
          highlightLines = cvtColor cyan
        }
      val noBrick = newNoBrick root palette

      val mainMsg = "ClickLeft: remove bad bricks. " ^
           "ClickRight or ShiftClickLeft: mark/unmark bricks."

      val msgArea = Label.mkLabel root 
        {label="",font=SOME BrickFont,foregrnd=NONE, 
          backgrnd=NONE, align=HCenter}
      fun setMsg txt = Label.setLabel msgArea (Label.Text txt)

      fun mkRow y = let
        fun mkCol x = if x = xSize then [] 
                      else (newBrick root (PT{x=x,y=y},brickChan,palette))::(mkCol (x+1))
      in
        if y = ySize then [] else (mkCol 0)::(mkRow (y+1))
      end
      val bricklist = mkRow 0
      val brick = Array.fromList(map Array.fromList bricklist)
    
      fun brickAt (PT{x,y}) =
        (Array.sub(Array.sub(brick,y),x)) handle _ => noBrick

      val widget = 
        mkWallWidget (root,#concrete palette,Label.widgetOf msgArea, bricklist)

      local
        val random = Rand.mkRandom 0w1
      in
        fun randx () = Rand.range (0,xSize-1) (random())
        fun randy () = Rand.range (0,ySize-1) (random())
      end

      fun setupGame diff = let
        val range = if cmpDifficulty(diff,Desperate) >= 0 then Long else Short
        fun chooseGood () = let
          val goodCnt = 
            Real.trunc(real(xSize*ySize*(difficultyProbability diff))/100.0)
          fun loop (0, cnt) = cnt
            | loop (i, cnt) = let
	        val rx = randx ()
	        val ry = randy ()
                val b = brickAt(PT{x=rx,y=ry})
                in
	          if ((rx>=SafeZone) orelse (ry>=SafeZone)) 
                      andalso (not (isGood b))
                    then (setGood b; loop(i-1,cnt+1))
                    else loop(i-1,cnt)
                end
          in
            Array.app (fn row => Array.app reset row) brick;
            loop (goodCnt, 0)
          end
        val goodCnt = chooseGood ()
        val badCnt = xSize*ySize-goodCnt
        val delta = showAndFlood(brickAt originPt,brickAt)
        in
          setMsg mainMsg;
          (range,badCnt-delta)
        end

      fun gameLost () = (
        Array.app (fn row => Array.app (fn b => endShow(b,brickAt)) row) brick;
        setMsg("OOPS! That was a perfectly good brick!");
        endGame (Normal,Short)
      )

      and gameWon () = (
        setMsg("NO BAD BRICKS LEFT! Skateboarding is now safe.");
        endGame (Normal,Short)
      )

      and startGame difficulty = let
        val (range,badBricks) = setupGame difficulty
        exception GameLost and GameWon

        local
          open Format
          val diffName = difficultyName difficulty
          val format1 = diffName^" Game:  %d bad bricks left"
          val format2 = 
            diffName ^ " Game:  %d out of %d unknown neighbors are good;"
              ^ "   %d bad bricks left"
          
        in
          fun gameStatus (good, unknown, bad) =
            if good = unknown
              then setMsg(format format1 [INT bad])
              else setMsg(format format2 [INT good, INT unknown, INT bad]) 
        end

        fun markBfn b = 
          if stateOf b = UnknownState 
            then if isGood b then raise GameLost else showAndFlood (b,brickAt)
            else 0
        val markBad = neighborCount markBfn
        fun markGfn b = if stateOf b = UnknownState then toggleMarking b else ()
        val markGood = enumerateNeighbors markGfn

        fun autoBrick(brick, range) = let
          val bad = neighborBadCount(brick, range, brickAt)
          val ok = neighborOKCount(brick, range, brickAt)
          val (unknown, good) = 
            case range of
              Short => (6 - (bad + ok), stateVal(stateOf brick))
            | _ => (18 - (bad + ok), neighborGoodCount (brick, Long, brickAt))
          in
            if unknown = 0 then 0
            else if good <= ok then markBad(brick, range, brickAt)
            else if unknown = good - ok then (markGood(brick, range, brickAt);0)
            else 0
          end

        fun brickAction (mbttn, b, me as (range,badcnt)) =
          if rightMouse mbttn then (toggleMarking b; me)
          else if stateOf b <> OKState then
            if isGood b then raise GameLost
            else let
              val delta =
                if stateOf b = UnknownState then showAndFlood (b,brickAt)
                else if leftMouse mbttn orelse cmpDifficulty(difficulty,Hard) < 0 
                  then autoBrick(b, Short)
	        else autoBrick(b, Long)
              val badcnt' = badcnt - delta
              in
                gameStatus(0, 0, badcnt');
                if badcnt' = 0 then raise GameWon else (range,badcnt')
              end
          else me

        fun adjustRange (m,me as (_,badcnt)) =
          if cmpDifficulty(difficulty,Desperate) < 0 
            orelse leftMouse m then (Short,badcnt)
          else if middleMouse m then (Long,badcnt)
          else me

        fun brickHighlightOn (b,me as (range,badcnt)) =
          if isShown b then (
            enumerateNeighbors highlightOn (b,range,brickAt);
            if range = Long
              then setText (b, Int.toString(neighborGoodCount (b,Long,brickAt)))
              else ();
            let
              val bad = neighborBadCount(b, range, brickAt)
              val ok = neighborOKCount(b, range, brickAt)
              val (unknown, good) = 
                case range of
                  Short => (6 - (bad + ok), stateVal(stateOf b))
                | _ => (18 - (bad + ok), neighborGoodCount(b,Long,brickAt))
            in
              gameStatus(good - ok, unknown, badcnt)
            end;
            me
          )
          else me

        fun brickHighlightOff (b,me as (range, _)) =
          if isShown b then (
            enumerateNeighbors highlightOff (b, range, brickAt);
            if range = Long
              then setText (b, Int.toString(neighborGoodCount (b,Short,brickAt)))
              else ();
            me
          ) else me

        fun handleBrick (Down (m,b), me) = 
              brickHighlightOn(brickAt b,adjustRange(m,me))
          | handleBrick (Up(m,b),me) = let
              val brick = brickAt b
              in
                brickAction(m,brick,brickHighlightOff(brick,me))
              end
          | handleBrick (Cancel b,me) = 
              brickHighlightOff(brickAt b,me)

        fun handleReq (Start d,_) = startGame d
          | handleReq (SetRange r',(_,b)) = (r',b)
          | handleReq (GetDifficulty c,s) = (CML.send(c,difficulty); s)

        fun loop (me as (r,bad)) =
          loop (CML.select [
            CML.wrap(CML.recvEvt reqChan, fn msg => handleReq(msg,me)),
            CML.wrap(CML.recvEvt brickChan, fn msg => handleBrick(msg,me))
          ])
        in
          (loop (range,badBricks)) 
            handle GameWon => gameWon() | GameLost => gameLost()
        end

      and endGame (me as (d,r)) = let
        fun handleReq (Start d',_) = startGame d'
          | handleReq (SetRange r',(d,_)) = endGame(d,r')
          | handleReq (GetDifficulty c,s) = (CML.send(c,d); endGame s)
        in
          CML.select [
            CML.wrap(CML.recvEvt reqChan, fn msg => handleReq(msg,me)),
            CML.wrap(CML.recvEvt brickChan, fn _ => endGame me)
          ]
        end

      in
        CML.spawn (fn () => (endGame(Normal, Short);()));
        WALL {widget=widget,reqChan=reqChan}
      end

    fun widgetOf (WALL{widget,...}) = widget
    fun startGame (WALL{reqChan,...},d) = CML.send(reqChan, Start d)
    fun difficultyOf (WALL{reqChan,...}) = let
      val repchan = CML.channel ()
      in
        CML.send (reqChan, GetDifficulty repchan);
        CML.recv repchan
      end
    fun setRange (WALL{reqChan,...},range) = CML.send(reqChan, SetRange range)

  end (* structure Wall *)
