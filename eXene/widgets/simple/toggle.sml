(* toggle.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Common toggles.
 *)

signature TOGGLE = 
  sig

    structure W : WIDGET
  
    type toggle = ToggleType.toggle
  
    val widgetOf : toggle -> W.widget
    val getState : toggle -> bool
    val setState : (toggle * bool) -> unit
    val setActive : (toggle * bool) -> unit
    val getActive : toggle -> bool
  
    val radioButton : (W.root * W.view * W.arg list) -> (bool -> unit) -> toggle
    val checkButton : (W.root * W.view * W.arg list) -> (bool -> unit) -> toggle
    val labelButton : (W.root * W.view * W.arg list) -> (bool -> unit) -> toggle
    val toggleCheck : (W.root * W.view * W.arg list) -> (bool -> unit) -> toggle
    val toggleText : (W.root * W.view * W.arg list) -> (bool -> unit) -> toggle
    val toggleSwitch : (W.root * W.view * W.arg list) -> (bool -> unit) -> toggle
    val toggleCircle : (W.root * W.view * W.arg list) -> (bool -> unit) -> toggle

    val mkToggleCheck : W.root -> {
      state : W.wstate,
      action : bool -> unit,
      color : W.EXB.color option,
      sz : int
    } -> toggle
  
    val mkToggleText : W.root -> {
      state : W.wstate,
      rounded : bool,
      action : bool -> unit,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      label : string
    } -> toggle
  
    val mkToggleSwitch : W.root -> {
      state : W.wstate,
      action : bool -> unit,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option
    } -> toggle
  
    val mkToggleCircle : W.root -> {
      state : W.wstate,
      action : bool -> unit,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      radius : int
    } -> toggle
  
    val mkToggleIcon : W.root -> {
      state : W.wstate,
      action : bool -> unit,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      icon : W.EXB.tile
    } -> toggle
  
  end (* TOGGLE *)

structure Toggle : TOGGLE = 
  struct

    structure W = Widget

    open ToggleType

    structure CheckToggle = ToggleCtrl (CheckView)
    val toggleCheck = CheckToggle.commandToggle

    structure TextToggle = ToggleCtrl (TextView)
    val toggleText = TextToggle.commandToggle

    structure CircleToggle = ToggleCtrl (CircleView)
    val toggleCircle = CircleToggle.commandToggle

    structure LabelToggle = ToggleCtrl (LabelBttnView)
    val labelButton = LabelToggle.commandToggle
    fun checkButton (root,view,args) =
          labelButton(root,view,args@[(Attrs.attr_type,Attrs.AV_Str "check")])
    fun radioButton (root,view,args) =
          labelButton(root,view,args@[(Attrs.attr_type,Attrs.AV_Str "radio")])

    structure SwitchToggle = ToggleCtrl (SwitchView)
    val toggleSwitch = SwitchToggle.commandToggle

    fun addState (s,l) = let
          open Attrs
          fun set a = (attr_state,AV_Bool a)
          in
            case s of
              W.Active a => (set a)::(attr_active, AV_Bool true)::l
            | W.Inactive a => (set a)::(attr_active, AV_Bool false)::l
          end

    fun mkToggleSwitch root {action, state, foregrnd, backgrnd} = let
          open Attrs
          val name = Styles.mkView{name= Styles.styleName ["toggleSwitch"],
                                   aliases = []}
          val args = addState(state,[])
          val args = case foregrnd of
                       NONE => args
                     | SOME c => (attr_foreground, AV_Color c)::args
          val args = case backgrnd of
                       NONE => args
                     | SOME c => (attr_background, AV_Color c)::args
          in toggleSwitch (root,(name,W.styleOf root),args) action end

    fun mkToggleCheck root {state, sz, action, color} = let
          open Attrs
          val name = Styles.mkView{name= Styles.styleName ["toggleCheck"],
                                   aliases = []}
          val args = addState(state,[(attr_width,AV_Int sz)])
          val args = case color of
                       NONE => args
                     | SOME c => (attr_color, AV_Color c)::args
          in toggleCheck (root,(name,W.styleOf root),args) action end

    fun mkToggleCircle root {state, radius, action, foregrnd, backgrnd} = let
          open Attrs
          val name = Styles.mkView{name= Styles.styleName ["toggleCircle"],
                                   aliases = []}
          val args = addState(state,[(attr_width,AV_Int (2*radius))])
          val args = case foregrnd of
                       NONE => args
                     | SOME c => (attr_foreground, AV_Color c)::args
          val args = case backgrnd of
                       NONE => args
                     | SOME c => (attr_background, AV_Color c)::args
          in toggleCircle (root,(name,W.styleOf root),args) action end

    fun mkToggleIcon root {state, icon,action,foregrnd,backgrnd} = let
          open Attrs
          val name = Styles.mkView{name= Styles.styleName ["toggleIcon"],
                                   aliases = []}
          val args = addState(state,[(attr_tile,AV_Tile icon)])
          val args = case foregrnd of
                       NONE => args
                     | SOME c => (attr_foreground, AV_Color c)::args
          val args = case backgrnd of
                       NONE => args
                     | SOME c => (attr_background, AV_Color c)::args
          in labelButton (root,(name,W.styleOf root),args) action end

    fun mkToggleText root {state,rounded,label,action,foregrnd,backgrnd} = let
          open Attrs
          val name = Styles.mkView{name= Styles.styleName ["toggleIcon"],
                                   aliases = []}
          val args = [(attr_label,AV_Str label),(attr_rounded,AV_Bool rounded)]
          val args = addState(state,args)
          val args = case foregrnd of
                       NONE => args
                     | SOME c => (attr_foreground, AV_Color c)::args
          val args = case backgrnd of
                       NONE => args
                     | SOME c => (attr_background, AV_Color c)::args
          in toggleText (root,(name,W.styleOf root),args) action end

  end (* Toggle *)
