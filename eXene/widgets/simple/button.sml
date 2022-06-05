(* button.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Common buttons.
 *)

signature BUTTON = 
  sig

    structure W : WIDGET

    datatype button_act
      = BtnDown of Interact.mbutton 
      | BtnUp of Interact.mbutton
      | BtnReady
      | BtnNormal

    datatype arrow_dir = AD_Up | AD_Down | AD_Left | AD_Right

    type button = ButtonType.button

    val evtOf : button -> button_act CML.event
    val widgetOf : button -> W.widget
    val setActive : (button * bool) -> unit
    val getActive : button -> bool

    val arrowBtn : (W.root * W.view * W.arg list) -> button
    val arrowCmd : (W.root * W.view * W.arg list) -> (unit -> unit) -> button
    val labelBtn : (W.root * W.view * W.arg list) -> button
    val labelCmd : (W.root * W.view * W.arg list) -> (unit -> unit) -> button
    val textBtn : (W.root * W.view * W.arg list) -> button
    val textCmd : (W.root * W.view * W.arg list) -> (unit -> unit) -> button

    val mkArrowBtn : W.root -> {
      backgrnd : W.EXB.color option,
      dir : arrow_dir,
      foregrnd : W.EXB.color option,
      sz : int
    } -> button

    val mkArrowCmd : W.root -> {
      action : unit -> unit,
      backgrnd : W.EXB.color option,
      dir : arrow_dir,
      foregrnd : W.EXB.color option,
      sz : int
    } -> button

    val mkTextBtn : W.root -> {
      rounded : bool,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      label : string
    } -> button

    val mkTextCmd : W.root -> {
      rounded : bool,
      action : unit -> unit,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      label : string
    } -> button

  end (* BUTTON *)

structure Button : BUTTON = 
  struct

    structure W : sig
        datatype arrow_dir = AD_Up | AD_Down | AD_Left | AD_Right
      end = Widget
    open W
    structure W = Widget

    open ButtonType

    structure ArrowBtn = ButtonCtrl (ArrowView)
    val arrowBtn = ArrowBtn.button
    val arrowCmd = ArrowBtn.commandBtn

    structure TextBtn = ButtonCtrl (TextView)
    val textBtn = TextBtn.button
    val textCmd = TextBtn.commandBtn

    structure LabelBtn = ButtonCtrl (LabelBttnView)
    val labelBtn = LabelBtn.button
    val labelCmd = LabelBtn.commandBtn

    fun mkTextBtn root {rounded, label, foregrnd, backgrnd} = let
          open Attrs
          val name = Styles.mkView{name= Styles.styleName ["textButton"],
                                   aliases = []}
          val args = [(attr_rounded, AV_Bool rounded),
                      (attr_label, AV_Str label)]
          val args = case foregrnd of
                       NONE => args
                     | SOME c => (attr_foreground, AV_Color c)::args
          val args = case backgrnd of
                       NONE => args
                     | SOME c => (attr_background, AV_Color c)::args
          in 
            textBtn (root,(name,Widget.styleOf root),args)
          end
  
    fun mkTextCmd root {action, rounded, label, foregrnd, backgrnd} = let
          open Attrs
          val name = Styles.mkView{name= Styles.styleName ["textCommand"],
                                   aliases = []}
          val args = [(attr_rounded, AV_Bool rounded),
                      (attr_label, AV_Str label)]
          val args = case foregrnd of
                       NONE => args
                     | SOME c => (attr_foreground, AV_Color c)::args
          val args = case backgrnd of
                       NONE => args
                     | SOME c => (attr_background, AV_Color c)::args
          in 
            textCmd (root,(name,Widget.styleOf root),args) action
          end
  
    fun mkArrowBtn root {dir, sz, foregrnd, backgrnd} = let
          open Attrs
          val name = Styles.mkView{name= Styles.styleName ["arrowButton"],
                                   aliases = []}
          val args = [(attr_width, AV_Int sz),
                      (attr_arrowDir, AV_ArrowDir dir)]
          val args = case foregrnd of
                       NONE => args
                     | SOME c => (attr_foreground, AV_Color c)::args
          val args = case backgrnd of
                       NONE => args
                     | SOME c => (attr_background, AV_Color c)::args
          in 
            arrowBtn (root,(name,Widget.styleOf root),args)
          end
  
    fun mkArrowCmd root {action, dir, sz, foregrnd, backgrnd} = let
          open Attrs
          val name = Styles.mkView{name= Styles.styleName ["arrowCommand"],
                                   aliases = []}
          val args = [(attr_width, AV_Int sz),
                      (attr_arrowDir, AV_ArrowDir dir)]
          val args = case foregrnd of
                       NONE => args
                     | SOME c => (attr_foreground, AV_Color c)::args
          val args = case backgrnd of
                       NONE => args
                     | SOME c => (attr_background, AV_Color c)::args
          in 
            arrowCmd (root,(name,Widget.styleOf root),args) action
          end
  
  end (* Button *)
