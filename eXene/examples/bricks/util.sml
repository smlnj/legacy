(* util.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

signature BRICK_UTIL =
  sig

    structure G : GEOMETRY
    structure Interact : INTERACT

    val BrickSizeV : int
    val BrickSizeH : int

    val BrickFont : string

    datatype Difficulty = Easy | Normal | Hard | Desperate | Ridiculous | Absurd
    val difficultyList : Difficulty list
    datatype Range = Short | Long

    type Palette

    val difficultyProbability : Difficulty -> int
    val difficultyName : Difficulty -> string
    val cmpDifficulty : (Difficulty * Difficulty) -> int 

    datatype State =
        NoBrickState
      | UnknownState
      | OKState
      | BadState of int   (* number of good brick neighbors *)
  
    val stateVal : State -> int

    type Position
  
    val WestOf : Position -> Position
    val NorthWestOf : Position -> Position
    val NorthEastOf : Position -> Position
    val EastOf : Position -> Position
    val SouthEastOf : Position -> Position
    val SouthWestOf : Position -> Position

    datatype MseEvt = 
        Down of (Interact.mbutton * Position)
      | Up of (Interact.mbutton * Position)
      | Cancel of Position
  end

structure BrickUtil : BRICK_UTIL =
  struct

    structure G = Geometry
    structure Interact = Interact

    val BrickSizeV = 16
    val BrickSizeH = 48
    val BrickFont = 
      "-adobe-helvetica-medium-r-normal--10-100-*-*-p-56-iso8859-1"

    datatype Difficulty = Easy | Normal | Hard | Desperate | Ridiculous | Absurd
    val difficultyList = [Easy, Normal, Hard, Desperate, Ridiculous, Absurd]
    datatype Range = Short | Long

    type Palette = {
          brick : Widget.EXB.color,
          mark : Widget.EXB.color,
          concrete : Widget.EXB.color,
          darkLines : Widget.EXB.color,
          lightLines : Widget.EXB.color,
          highlightLines : Widget.EXB.color
        }

    fun difficultyProbability d = 
      case d of
        Easy => 15
      | Normal => 20
      | Hard => 25
      | Desperate => 30
      | Ridiculous => 40
      | Absurd=> 50
  
    fun difficultyName d = 
      case d of
        Easy => "Easy"
      | Normal => "Normal"
      | Hard => "Hard"
      | Desperate => "Desperate"
      | Ridiculous => "Ridiculous"
      | Absurd=> "Absurd"
  
    fun cmpDifficulty (d1,d2) = 
      (difficultyProbability d1)-(difficultyProbability d2)

    datatype State =
        NoBrickState
      | UnknownState
      | OKState
      | BadState of int   (* number of good brick neighbors *)
  
    fun stateVal NoBrickState = ~3
      | stateVal UnknownState = ~2
      | stateVal OKState = ~1
      | stateVal (BadState c) = c
  
    type Position = Geometry.point
  
    local open Geometry in
      fun WestOf (PT{x,y}) = PT{x=x-1,y=y}
      fun NorthWestOf (PT{x,y}) = PT{x=x-1+(y mod 2),y=y-1}
      fun NorthEastOf (PT{x,y}) = PT{x=x+(y mod 2),y=y-1}
      fun EastOf (PT{x,y}) = PT{x=x+1,y=y}
      fun SouthEastOf (PT{x,y}) = PT{x=x+(y mod 2),y=y+1}
      fun SouthWestOf (PT{x,y}) = PT{x=x-1+(y mod 2),y=y+1}
    end

    datatype MseEvt = 
        Down of (Interact.mbutton * Position)
      | Up of (Interact.mbutton * Position)
      | Cancel of Position
  end

