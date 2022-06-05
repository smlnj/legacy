(* brick.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

signature BRICK =
  sig
    structure W : WIDGET
    structure U : BRICK_UTIL

    type Brick

    val newBrick : W.root -> (U.Position * U.MseEvt CML.chan * U.Palette) 
          -> Brick
    val newNoBrick : W.root -> U.Palette -> Brick
    val widgetOf : Brick -> W.widget
    val setGood : Brick -> unit
    val reset : Brick -> unit
    val stateOf : Brick -> U.State
    val isShown : Brick -> bool
    val isGood : Brick -> bool

    val enumerateNeighbors : 
      (Brick -> unit) -> (Brick * U.Range * (U.Position -> Brick)) -> unit
    val neighborCount :
      (Brick -> int) -> (Brick * U.Range * (U.Position -> Brick)) -> int
    val neighborGoodCount : (Brick * U.Range * (U.Position -> Brick)) -> int
    val neighborBadCount : (Brick * U.Range * (U.Position -> Brick)) -> int
    val neighborOKCount : (Brick * U.Range * (U.Position -> Brick)) -> int

    val showAndFlood : Brick * (U.Position -> Brick) -> int
    val endShow : Brick * (U.Position -> Brick) -> unit

    val highlightOn : Brick -> unit
    val highlightOff : Brick -> unit
    val toggleMarking : Brick -> unit
    val setText : Brick * string -> unit

  end

structure Brick : BRICK =
  struct

    structure W = Widget
    structure U = BrickUtil

    open CML Geometry Widget U BrickView

    datatype Brick = BRICK of { 
        bv : brick_view,
        p : Position,
        good : bool ref,
        shown : bool ref,
        state : State ref
      }

    fun setGood (BRICK{good,...}) = good := true
    fun setState (BRICK{state,...},s) = state := s
    fun setShown (BRICK{shown,...}) = shown := true
    fun viewOf (BRICK{bv,...}) = bv
    fun stateOf (BRICK{state,...}) = !state
    fun isShown (BRICK{shown,...}) = !shown
    fun isGood (BRICK{good,...}) = !good
    fun positionOf (BRICK{p,...}) = p
    fun widgetOf (BRICK{bv,...}) = BrickView.widgetOf bv

    fun isBad b =
      case stateOf b of
        NoBrickState => true
      | BadState _ => true
      | _ => false

    fun enumerateNeighbors no (brick, range, brickAt) = let
      val p = positionOf brick
      in
        no(brickAt(WestOf p));
        no(brickAt(NorthWestOf p));
        no(brickAt(NorthEastOf p));
        no(brickAt(EastOf p));
        no(brickAt(SouthEastOf p));
        no(brickAt(SouthWestOf p));
        if range = Long then (
          no(brickAt(WestOf(WestOf p)));
          no(brickAt(WestOf(NorthWestOf p)));
          no(brickAt(NorthWestOf(NorthWestOf p)));
          no(brickAt(NorthWestOf(NorthEastOf p)));
          no(brickAt(NorthEastOf(NorthEastOf p)));
          no(brickAt(NorthEastOf(EastOf p)));
          no(brickAt(EastOf(EastOf p)));
          no(brickAt(EastOf(SouthEastOf p)));
          no(brickAt(SouthEastOf(SouthEastOf p)));
          no(brickAt(SouthEastOf(SouthWestOf p)));
          no(brickAt(SouthWestOf(SouthWestOf p)));
          no(brickAt(SouthWestOf(WestOf p)))
        )
        else ()
      end

    fun neighborCount pred (brick, range, brickAt) = let
      val cnt = ref 0
      fun inc v = cnt := (!cnt) + v
    in
      enumerateNeighbors (inc o pred) (brick, range, brickAt);
      !cnt
    end
    val neighborGoodCount = neighborCount (fn b => if isGood b then 1 else 0)
    val neighborBadCount = neighborCount (fn b => if isBad b then 1 else 0)
    val neighborOKCount = 
      neighborCount (fn b => case stateOf b of OKState => 1 | _ => 0)

    fun endShow (b, brickAt) =
      if isShown b orelse isGood b then ()
      else let
        val cnt = neighborGoodCount(b,Short,brickAt)
      in
        setState(b,BadState cnt);
        endView (viewOf b) (Int.toString cnt);
        setShown b
        (* original decrements bad brick count *)
      end

    fun show (b, brickAt) = let
      val cnt = neighborGoodCount(b,Short,brickAt)
      in
        setState(b, BadState cnt);
        setShown b;
        showView (viewOf b) (Int.toString cnt)
      end


    fun showAndFlood (b, brickAt) = let
      fun showAF (b,cnt) =
        if isShown b then cnt
        else let
          val p = positionOf b
          val cnt' = if isGood b then cnt else (show (b,brickAt); cnt+1)
        in
          case stateOf b of
            BadState 0 =>
              (showAF(brickAt(SouthWestOf p),
                showAF(brickAt(SouthEastOf p),
                  showAF(brickAt(EastOf p),
                    showAF(brickAt(NorthEastOf p),
                      showAF(brickAt(NorthWestOf p),
                        showAF(brickAt(WestOf p),cnt')))))))
          | _ => cnt'
        end
      in
        showAF (b,0)
      end
          
    fun highlightOn (BRICK{bv,...}) = BrickView.highlightOn bv
    fun highlightOff (BRICK{bv,...}) = BrickView.highlightOff bv

    fun toggleMarking b =
      case stateOf b of
        UnknownState => (markView (viewOf b); setState(b,OKState))
      | OKState => (normView (viewOf b); setState(b,UnknownState))
      | _ => ()

    fun setText (BRICK{bv,...},txt) = BrickView.setText bv txt

    fun newBrick root (arg as (pt,_,_)) =
      BRICK { 
        bv = BrickView.mkBrickView root arg,
        p = pt,
        good = ref false,
        shown = ref false,
        state = ref UnknownState
      }

    fun newNoBrick root palette =
      BRICK { 
        bv = BrickView.mkBrickView root (originPt,channel(),palette),
        p = originPt,
        good = ref false,
        shown = ref true,
        state = ref NoBrickState
      }
    fun reset (BRICK{bv,state,good,shown,...}) =
      (state := UnknownState; shown := false; good := false; normView bv)

  end
