(* menu-button.sml
 *
 * COPYRIGHT (c) 1997 AT&T Research.
 *)

signature MENU_BUTTON = 
  sig
    structure W : WIDGET
    structure CML : CML
    structure Menu : SIMPLE_MENU

    val mkMenuButton : W.root -> (string * '1a Menu.menu)
	  -> (W.widget * '1a CML.event)
  end

structure MenuButton : MENU_BUTTON = 
  struct

    structure W = Widget
    structure CML = CML
    structure Menu = SimpleMenu

    open CML Geometry Interact Widget

    fun mkMenuButton root (label, menu) = let
      open Attrs
      val wChan = channel () and rChan = channel ()
      val allBttns = map MButton [1,2,3,4,5]
      val name = Styles.mkView{name=Styles.styleName["menuButton"],
                              aliases=[]}
      val args = [(attr_relief,AV_Relief Flat),(attr_label,AV_Str label)]
      val bttn = Toggle.labelButton (root,(name,styleOf root),args) (fn _ => ())
      fun prefn () = Toggle.setState (bttn,true)
      fun postfn () = Toggle.setState (bttn,false)
      fun query arg = (send(wChan, arg); recv rChan)

      fun pos (Menu.WI{scr_pt=PT{x=sx,y=sy}, pt=PT{x,y}, time, but},SIZE{ht,...}) = 
        Menu.Absolute(PT{x=sx-x,y=sy-y+ht+1})

      val (widget, evt) = 
        SimpleMenu.buttonMenu (Toggle.widgetOf bttn, allBttns, menu, query)

      fun menuRealize {win, sz, env} = let
        val InEnv{m,ci,...} = env
        val mChan = channel() and cChan = channel ()
 
        fun handleMouse msg = (
          case msgBodyOf msg of
            MOUSE_FirstDown _ => prefn ()
          | MOUSE_LastUp _ => postfn ()
          | _ => ();
          send(mChan,msg))

        fun handleCI (msg,sz) =
          case msgBodyOf msg of
            CI_Resize (RECT{wid,ht,...}) => 
              (send(cChan,msg);SIZE{wid=wid,ht=ht})
          | _ => (send(cChan,msg);sz)

        fun loop sz = loop(select [
            wrap (m, fn evt => (handleMouse evt; sz)),
            wrap (ci, fn evt => handleCI(evt,sz)),
            wrap (recvEvt wChan, fn msg => (send(rChan,pos(msg,sz));sz))
          ])
        in
          spawn (fn () => loop sz);
          realizeFn widget {
            win=win,
            sz=sz,
            env=replaceCI(replaceMouse(env,recvEvt mChan),recvEvt cChan)
          }
        end

      val menuWidget = mkWidget {
          root = root,
          args = fn () => {background = NONE},
          realize = menuRealize,
          boundsOf = boundsFn widget
        }
      in
        (menuWidget, evt)
      end
  end


